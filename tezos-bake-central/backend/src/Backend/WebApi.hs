{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.WebApi where

import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Snap.Core as Snap

import Backend.CachedNodeRPC
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Lazy as LBS
import Snap.Core (MonadSnap, route)
import Tezos.Block (VeryBlockLike (..))

import Tezos.Base58Check (fromBase58, toBase58)
import Tezos.Types

snapHead :: (MonadIO m, MonadReader r m, HasNodeDataSource r) => m (Either Text VeryBlockLike)
snapHead = maybe (Left "cache not ready") pure <$> dataSourceHead

v1PublicApi :: forall m. MonadSnap m => NodeDataSource -> m ()
v1PublicApi dataSrc = route $ fmap (first ("api/v1/" <>))
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

    writeJSON :: forall a. Aeson.ToJSON a => (ProtoInfo -> ReaderT NodeDataSource m (Either Text a)) -> m ()
    writeJSON x = do
      liftIO (MVar.tryReadMVar (_nodeDataSource_parameters dataSrc)) >>= \case
        Nothing -> Snap.modifyResponse (Snap.setResponseCode 503) *> Snap.writeLBS "Cache Not Ready"
        Just params -> either sulk (Snap.writeLBS . Aeson.encode) =<< runReaderT (x params) dataSrc

    sulk :: Text -> m ()
    sulk msg = Snap.modifyResponse (Snap.setResponseCode 400) *> Snap.writeLBS (LBS.fromStrict $ T.encodeUtf8 msg)


snapBranchPoint :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text VeryBlockLike)
snapBranchPoint = withCache (Left "nocache") $ \_proto -> do
  Map.lookup "block" <$> Snap.getQueryParams >>= \case
    Nothing -> return $ Left "bad param"
    Just blockBS -> case traverse fromBase58 blockBS of
      Left err -> return $ Left $ T.pack $ show err
      Right (b1:b2:_) -> branchPoint b1 b2 >>= \case
        Nothing -> return $ Left "not found"
        Just b' -> return $ Right b'
      Right _ -> return $ Left "not enough blocks requested"


snapAncestors :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text [BlockHash])
snapAncestors = withCache (Left "nocache") $ \_proto -> runExceptT $ do
  branchBS <- maybe (throwError "missing param:branch") return =<< (listToMaybe <=< Map.lookup "branch") <$> Snap.liftSnap Snap.getQueryParams
  branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS

  levelBS <- maybe (throwError "missing param:level") return =<< (listToMaybe <=< Map.lookup "level") <$> Snap.liftSnap Snap.getQueryParams
  blockLevel :: RawLevel <- either (throwError . T.pack . show) return $ Aeson.eitherDecode $ LBS.fromStrict levelBS

  either (throwError . T.pack . show ) return =<< runExceptT (ancestors blockLevel branch)

snapBlock :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text VeryBlockLike)
snapBlock = withCache (Left "nocache") $ \_proto -> runExceptT $ do
  blockBS <- maybe (throwError "missing param:block") return =<< (listToMaybe <=< Map.lookup "block") <$> Snap.liftSnap Snap.getQueryParams
  block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

  maybe (throwError "block unknown") return =<< lookupBlock block
