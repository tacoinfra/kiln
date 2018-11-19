{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- TODO do everywhere
{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.WebApi where

import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT(), runExceptT, throwError, MonadError)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Semigroup ((<>))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Sequence (Seq())
import qualified Snap.Core as Snap

import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Snap.Core (MonadSnap, route)
import Tezos.Block (VeryBlockLike (..))

import Tezos.Base58Check (fromBase58, toBase58)
import Tezos.Types
import Tezos.NodeRPC.Types

import Backend.CachedNodeRPC
import Common.Schema (BlockBaker)

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
  , ( chainTXT <> "/baking-rights",    writeJSON $ const snapBakingRights )
  , ( chainTXT <> "/endorsing-rights", writeJSON $ const snapEndorsingRights )
  , ( chainTXT <> "/block-baker", writeJSON $ const snapBlockBaker )
  ]
  where
    chain = _nodeDataSource_chain dataSrc
    chainTXT = toBase58 chain

    writeJSON :: forall a. Aeson.ToJSON a => (ProtoInfo -> ReaderT NodeDataSource m (Either Text a)) -> m ()
    writeJSON x = do
      liftIO (MVar.tryReadMVar (_nodeDataSource_parameters dataSrc)) >>= \case
        Nothing -> Snap.modifyResponse (Snap.setResponseCode 503) *> Snap.writeLBS "Cache Not Ready"
        Just ps -> either sulk (Snap.writeLBS . Aeson.encode) =<< runReaderT (x ps) dataSrc

    sulk :: Text -> m ()
    sulk msg = Snap.modifyResponse (Snap.setResponseCode 400) *> Snap.writeLBS (LBS.fromStrict $ T.encodeUtf8 msg)


snapBranchPoint :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text VeryBlockLike)
snapBranchPoint = withCache (Left "nocache") $ \_proto -> runExceptT $ do
  blockBS <- asTextMaybe "missing param:block" $ params "block"
  case traverse fromBase58 blockBS of
    Left err -> throwError $ T.pack $ show err
    Right (b1:b2:_) -> branchPoint b1 b2 >>= \case
      Nothing -> throwError "not found"
      Just b' -> return  b'
    Right _ -> throwError "not enough blocks requested"


asTextExcept :: forall e m b. (Show e, MonadError Text m) => ExceptT e m b -> m b
asTextExcept x = either (throwError . T.pack . show ) return =<< runExceptT x
asTextMaybe :: MonadError Text m => Text -> m (Maybe b) -> m b
asTextMaybe msg x = maybe (throwError msg) return =<< x

requiredParam :: (MonadError Text m, MonadSnap m) => String -> m BS.ByteString
requiredParam paramName = maybe (throwError $ "missing param:" <> T.pack paramName) return =<< (listToMaybe <=< Map.lookup (fromString paramName)) <$> Snap.liftSnap Snap.getQueryParams

params :: (MonadError Text m, MonadSnap m) => BS.ByteString -> m (Maybe [BS.ByteString])
params paramName =  Map.lookup paramName <$> Snap.liftSnap Snap.getQueryParams

snapAncestors :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text [BlockHash])
snapAncestors = withCache (Left "nocache") $ \_proto -> runExceptT $ do
  branchBS <- requiredParam "branch"
  branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS

  levelBS <- requiredParam "level"
  blockLevel :: RawLevel <- either (throwError . T.pack . show) return $ Aeson.eitherDecodeStrict' levelBS

  either (throwError . T.pack . show ) return =<< runExceptT (ancestors blockLevel branch)

snapBlock :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text VeryBlockLike)
snapBlock = withCache (Left "nocache") $ \_proto -> runExceptT $ do
  blockBS <- requiredParam "block"
  block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

  maybe (throwError "block unknown") return =<< lookupBlock block

snapBakingRights :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text (Seq BakingRights))
snapBakingRights = withCache (Left "nocache") $ \_proto -> runExceptT $ do
  branchBS <- requiredParam "branch"
  branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS

  levelBS <- requiredParam "level"
  blockLevel :: RawLevel <- either (throwError . T.pack . show) return $ Aeson.eitherDecodeStrict' levelBS

  asTextExcept @RpcError $ nodeQueryDataSource $ NodeQuery_BakingRights branch blockLevel

snapEndorsingRights :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text (Seq EndorsingRights))
snapEndorsingRights = withCache (Left "nocache") $ \_proto -> runExceptT $ do
  branchBS <- requiredParam "branch"
  branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS

  levelBS <- requiredParam "level"
  blockLevel :: RawLevel <- either (throwError . T.pack . show) return $ Aeson.eitherDecodeStrict' levelBS

  asTextExcept @RpcError $ nodeQueryDataSource $ NodeQuery_EndorsingRights branch blockLevel

snapBlockBaker :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text BlockBaker)
snapBlockBaker = withCache (Left "nocache") $ \_proto -> runExceptT $ do
  branchBS <- requiredParam "branch"
  branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS

  levelBS <- requiredParam "level"
  blockLevel :: RawLevel <- either (throwError . T.pack . show) return $ Aeson.eitherDecodeStrict' levelBS

  asTextExcept @RpcError $ nodeQueryDataSource $ NodeQuery_BlockBaker branch blockLevel
