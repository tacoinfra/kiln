{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.WebApi where

import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Reader (ReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Sequence (Seq)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Core (MonadSnap, route)
import qualified Snap.Core as Snap

import Tezos.Base58Check (fromBase58, toBase58)
import Tezos.Block (VeryBlockLike (..))
import Tezos.Types

import Backend.CachedNodeRPC
import Backend.STM (atomicallyWith)
import Common.Schema (BlockBaker, CacheDelegateInfo, CacheError)
import ExtraPrelude

snapHead :: (MonadIO m, MonadReader r m, HasNodeDataSource r) => m (Either Text VeryBlockLike)
snapHead = do
  nds <- asks (^. nodeDataSource)
  liftIO $ atomically $ maybe (Left "cache not ready") pure <$> dataSourceHead nds

v1PublicApi :: forall m. MonadSnap m => NodeDataSource -> m ()
v1PublicApi dataSrc = route $ fmap (first ("api/v1/" <>))
  [ ("chain",                Snap.writeLBS $ Aeson.encode chain)
  , ( chainTXT <> "/params",    writeJSON $ pure . pure)
  , ( chainTXT <> "/head",      writeJSON $ const snapHead )
  , ( chainTXT <> "/lca",       writeJSON $ const snapBranchPoint )
  , ( chainTXT <> "/ancestors", writeJSON $ const snapAncestors )
  , ( chainTXT <> "/ballot", writeJSON $ const snapBallots )
  , ( chainTXT <> "/proposal", writeJSON $ const snapProposals )
  , ( chainTXT <> "/block",     writeJSON $ const snapVeryBlockLike )
  , ( chainTXT <> "/block", writeJSON $ const snapBlock )
  , ( chainTXT <> "/baking-rights",    writeJSON $ const snapBakingRights )
  , ( chainTXT <> "/endorsing-rights", writeJSON $ const snapEndorsingRights )
  , ( chainTXT <> "/block-baker", writeJSON $ const snapBlockBaker )
  , ( chainTXT <> "/delegate-info", writeJSON $ const snapDelegateInfo )
  ]
  where
    chain = _nodeDataSource_chain dataSrc
    chainTXT = toBase58 chain

    writeJSON :: forall a. Aeson.ToJSON a => (ProtoInfo -> ReaderT NodeDataSource m (Either Text a)) -> m ()
    writeJSON x = do
      liftIO (readTVarIO (_nodeDataSource_parameters dataSrc)) >>= \case
        Nothing -> Snap.modifyResponse (Snap.setResponseCode 503) *> Snap.writeLBS "Cache Not Ready"
        Just ps -> either sulk (Snap.writeLBS . Aeson.encode) =<< runReaderT (x ps) dataSrc

    sulk :: Text -> m ()
    sulk msg = Snap.modifyResponse (Snap.setResponseCode 400) *> Snap.writeLBS (LBS.fromStrict $ T.encodeUtf8 msg)


snapBranchPoint :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text VeryBlockLike)
snapBranchPoint = do
  withCacheIO (Left "nocache") $ \_proto -> runExceptT $ do
    blockBS <- asTextMaybe "missing param:block" $ params "block"
    case traverse fromBase58 blockBS of
      Left err -> throwError $ T.pack $ show err
      Right (b1:b2:_) -> atomicallyWith (branchPoint b1 b2) >>= \case
        Nothing -> throwError "not found"
        Just b' -> return b'
      Right _ -> throwError "not enough blocks requested"

asTextExcept :: forall e m b. (Show e, MonadError Text m) => ExceptT e m b -> m b
asTextExcept x = either (throwError . T.pack . show ) return =<< runExceptT x

asTextMaybe :: MonadError Text m => Text -> m (Maybe b) -> m b
asTextMaybe msg x = maybe (throwError msg) return =<< x

requiredParam :: (MonadError Text m, MonadSnap m) => Snap.Snap Snap.Params -> String -> m BS.ByteString
requiredParam getParam paramName = maybe (throwError $ "missing param:" <> T.pack paramName) return =<< (listToMaybe <=< Map.lookup (fromString paramName)) <$> Snap.liftSnap getParam

requiredQueryParam :: (MonadError Text m, MonadSnap m) => String -> m BS.ByteString
requiredQueryParam = requiredParam Snap.getQueryParams

requiredPathParam :: (MonadError Text m, MonadSnap m) => String -> m BS.ByteString
requiredPathParam = requiredParam Snap.getParams

params :: (MonadError Text m, MonadSnap m) => BS.ByteString -> m (Maybe [BS.ByteString])
params paramName = Map.lookup paramName <$> Snap.liftSnap Snap.getQueryParams

snapAncestors :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text [BlockHash])
snapAncestors = withCacheIO (Left "nocache") $ \_proto -> runExceptT $ do
  branchBS <- requiredQueryParam "branch"
  levelBS <- requiredQueryParam "level"

  branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS
  blockLevel :: RawLevel <- either (throwError . T.pack . show) return $ Aeson.eitherDecodeStrict' levelBS

  either (throwError . T.pack . show ) return =<< runExceptT (ancestors blockLevel branch)

snapVeryBlockLike :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text VeryBlockLike)
snapVeryBlockLike = do
  nds <- asks (^. nodeDataSource)
  withCacheIO (Left "nocache") $ \_proto -> runExceptT $ do
    blockBS <- requiredQueryParam "block"
    block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

    maybe (throwError "block unknown") return =<< liftIO (atomically $ lookupBlock nds block)

dummyLevel :: RawLevel
dummyLevel = RawLevel 0

snapBallots :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text Ballots)
snapBallots = do
  withCacheIO (Left "nocache") $ \_proto -> runExceptT $ do
    blockBS <- requiredQueryParam "block"
    block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

    asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_Ballots block dummyLevel

snapProposals :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text (Seq ProposalVotes))
snapProposals = do
  withCacheIO (Left "nocache") $ \_proto -> runExceptT $ do
    blockBS <- requiredQueryParam "block"
    block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

    asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_Proposals block dummyLevel

snapBlock :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text Block)
snapBlock = do
  withCacheIO (Left "nocache") $ \_proto -> runExceptT $ do
    blockBS <- requiredQueryParam "hash"
    block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

    asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_Block block dummyLevel

snapBakingRights :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text (Seq BakingRights))
snapBakingRights = withCacheIO (Left "nocache") $ \_proto -> runExceptT $ do
  branchBS <- requiredQueryParam "branch"
  levelBS <- requiredQueryParam "level"

  branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS
  blockLevel :: RawLevel <- either (throwError . T.pack . show) return $ Aeson.eitherDecodeStrict' levelBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_BakingRights branch blockLevel

snapEndorsingRights :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text (Seq EndorsingRights))
snapEndorsingRights = withCacheIO (Left "nocache") $ \_proto -> runExceptT $ do
  branchBS <- requiredQueryParam "branch"
  levelBS <- requiredQueryParam "level"

  branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS
  blockLevel :: RawLevel <- either (throwError . T.pack . show) return $ Aeson.eitherDecodeStrict' levelBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_EndorsingRights branch blockLevel

snapBlockBaker :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text BlockBaker)
snapBlockBaker = withCacheIO (Left "nocache") $ \_proto -> runExceptT $ do
  branchBS <- requiredQueryParam "branch"
  levelBS <- requiredQueryParam "level"

  branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS
  blockLevel :: RawLevel <- either (throwError . T.pack . show) return $ Aeson.eitherDecodeStrict' levelBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_BlockBaker branch blockLevel

snapDelegateInfo :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text CacheDelegateInfo)
snapDelegateInfo = do
  nds <- asks (^. nodeDataSource)
  withCacheIO (Left "nocache") $ \_proto -> runExceptT $ do
    branchBS <- requiredQueryParam "branch"
    delegateBS <- requiredQueryParam "delegate"

    branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS
    delegate <- either (throwError . T.pack . show) return $ tryReadPublicKeyHash delegateBS
    blockLevel <- maybe (throwError "block unknown") (return . view level) =<< liftIO (atomically $ lookupBlock nds branch)

    asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_DelegateInfo branch blockLevel delegate

withCacheIO
  :: forall a r m. (MonadIO m, MonadReader r m, HasNodeDataSource r)
  => a -> (ProtoInfo -> m a) -> m a
withCacheIO dft action = do
  dsrc <- asks (^. nodeDataSource)
  protoInfo <- liftIO $ readTVarIO $ _nodeDataSource_parameters dsrc
  fromMaybe dft <$> traverse action protoInfo
