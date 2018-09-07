{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Backend.WebApi where

import Text.URI (URI)
import qualified Text.URI as Uri
import qualified Text.URI.QQ as Uri
import Data.Maybe (listToMaybe)
import Control.Lens((^.))
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import Data.Text (Text)
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Method as Http
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as Aeson
import qualified Snap.Core as Snap
import qualified Data.Map as Map

import Snap.Core (MonadSnap, route)
import Tezos.Block (VeryBlockLike(..))
import Backend.CachedNodeRPC
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Lazy as LBS


import Tezos.NodeRPC.Network (NodeRPCContext (..), nodeRPCImpl, HasNodeRPC)
import Tezos.NodeRPC
import Tezos.Base58Check (toBase58, fromBase58)
import Tezos.Types

snapHead :: (MonadIO m, MonadReader r m, HasNodeDataSource r) => m (Either Text VeryBlockLike)
snapHead = maybe (Left "cache not ready") pure <$> dataSourceHead


v1PublicApi :: forall m. MonadSnap m => NodeDataSource -> m ()
v1PublicApi dataSrc = route
  [ ("chain",                Snap.writeLBS $ Aeson.encode chain)
  , ( chainTXT <> "/params",    writeJSON $ pure . pure)
  , ( chainTXT <> "/head",      writeJSON $ const snapHead )
  , ( chainTXT <> "/lca",       writeJSON $ const snapBranchPoint )
  , ( chainTXT <> "/ancestors", writeJSON $ const snapAncestors )
  , ( chainTXT <> "/block",     writeJSON $ const snapBlock )
  ]
  where
    chain = _nodeDataSource_chain dataSrc
    chainTXT = toBase58 chain
    -- writeJSON :: forall m r a. (MonadSnap m, Aeson.ToJSON a, MonadReader r m, HasNodeDataSource r) => m (Either Text a) -> _ ()
    writeJSON :: forall a. Aeson.ToJSON a => (ProtoInfo -> ReaderT NodeDataSource m (Either Text a)) -> m ()
    writeJSON x = do
      liftIO (MVar.tryReadMVar (_nodeDataSource_parameters dataSrc)) >>= \case
        Nothing -> Snap.modifyResponse (Snap.setResponseCode 503) *> Snap.writeLBS "Cache Not Ready"
        Just params -> either sulk (Snap.writeLBS . Aeson.encode) =<< runReaderT (x params) dataSrc

    sulk :: Text -> m ()
    sulk msg = Snap.modifyResponse (Snap.setResponseCode 400) *> Snap.writeLBS (LBS.fromStrict $ T.encodeUtf8 msg)


snapBranchPoint :: (MonadSnap m, MonadIO m, MonadReader r m, HasNodeDataSource r) => m (Either Text VeryBlockLike)
snapBranchPoint = withCache (Left "nocache") $ \_proto -> do
  Map.lookup "block" <$> Snap.getQueryParams >>= \case
    Nothing -> return $ Left "bad param"
    Just blockBS -> case traverse fromBase58 blockBS of
      Left err -> return $ Left $ T.pack $ show err
      Right (b1:b2:bs) -> branchPoint b1 b2 >>= \case
        Nothing -> return $ Left "not found"
        Just b' -> return $ Right b'
      Right _ -> return $ Left "not enough blocks requested"


snapAncestors :: (MonadSnap m, MonadIO m, MonadReader r m, HasNodeDataSource r) => m (Either Text [BlockHash])
snapAncestors = withCache (Left "nocache") $ \_proto -> runExceptT $ do
  branchBS <- maybe (throwError "missing param:branch") return =<< (listToMaybe <=< Map.lookup "branch") <$> Snap.liftSnap Snap.getQueryParams
  branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS

  levelBS <- maybe (throwError "missing param:level") return =<< (listToMaybe <=< Map.lookup "level") <$> Snap.liftSnap Snap.getQueryParams
  level :: RawLevel <- either (throwError . T.pack . show) return $ Aeson.eitherDecode $ LBS.fromStrict levelBS

  either (throwError . T.pack . show ) return =<< runExceptT (ancestors level branch)

snapBlock :: (MonadSnap m, MonadIO m, MonadReader r m, HasNodeDataSource r) => m (Either Text VeryBlockLike)
snapBlock = withCache (Left "nocache") $ \_proto -> runExceptT $ do
  blockBS <- maybe (throwError "missing param:block") return =<< (listToMaybe <=< Map.lookup "block") <$> Snap.liftSnap Snap.getQueryParams
  block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

  maybe (throwError "block unknown") return =<< lookupBlock block
