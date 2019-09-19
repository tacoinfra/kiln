{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.WebApi where

import Control.Concurrent.STM (atomically)
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
import Rhyolite.Backend.Logging (runLoggingEnv)
import Snap.Core (MonadSnap, route)
import qualified Snap.Core as Snap

import Tezos.Base58Check (fromBase58, toBase58)
import Tezos.Block (VeryBlockLike (..))
import Tezos.Operation (Ballot)
import Tezos.PublicKey
import Tezos.Types

import Backend.CachedNodeRPC
import Backend.STM (atomicallyWith)
import Common.Schema (CacheDelegateInfo(..), CacheError(..), ProtocolIndex)
import ExtraPrelude

snapHead :: (MonadIO m, MonadReader r m, HasNodeDataSource r) => m (Either Text (WithProtocolHash VeryBlockLike))
snapHead = do
  nds <- asks (^. nodeDataSource)
  liftIO $ atomically $ maybe (Left "cache not ready") pure <$> dataSourceHead nds

v3PublicApi :: forall m. MonadSnap m => NodeDataSource -> m ()
v3PublicApi dataSrc = route $ fmap (first ("api/v3/" <>))
  [ ("chain",                Snap.writeLBS $ Aeson.encode chain)
  , ( chainTXT <> "/account", writeJSON snapAccount )
  , ( chainTXT <> "/ancestors", writeJSON snapAncestors )
  , ( chainTXT <> "/baking-rights", writeJSON snapBakingRights )
  , ( chainTXT <> "/ballot", writeJSON snapBallot )
  , ( chainTXT <> "/ballots", writeJSON snapBallots )
  , ( chainTXT <> "/block-full", writeJSON snapBlock )
  , ( chainTXT <> "/block-header", writeJSON snapBlockHeader )
  , ( chainTXT <> "/current-proposal", writeJSON snapCurrentProposal )
  , ( chainTXT <> "/current-quorum", writeJSON snapCurrentQuorum )
  , ( chainTXT <> "/delegate-info", writeJSON snapDelegateInfo )
  , ( chainTXT <> "/endorsing-rights", writeJSON snapEndorsingRights )
  , ( chainTXT <> "/head",      writeJSON snapHead )
  , ( chainTXT <> "/lca",       writeJSON snapBranchPoint )
  , ( chainTXT <> "/listings", writeJSON snapListings )
  , ( chainTXT <> "/proposals", writeJSON snapProposals )
  , ( chainTXT <> "/proposal-vote", writeJSON snapProposalVote )
  , ( chainTXT <> "/protocol-index", writeJSON snapProtocolIndex )
  , ( chainTXT <> "/public-key", writeJSON snapPublicKey )
  ]
  where
    chain = _nodeDataSource_chain dataSrc
    chainTXT = toBase58 chain

    writeJSON :: forall a. Aeson.ToJSON a => ReaderT NodeDataSource m (Either Text a) -> m ()
    writeJSON x = either sulk (Snap.writeLBS . Aeson.encode) =<< runReaderT x dataSrc

    sulk :: Text -> m ()
    sulk msg = Snap.modifyResponse (Snap.setResponseCode 400) *> Snap.writeLBS (LBS.fromStrict $ T.encodeUtf8 msg)


snapBranchPoint :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text BlockSpine)
snapBranchPoint = runExceptT $ do
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
snapAncestors = runExceptT $ do
  branchBS <- requiredQueryParam "branch"
  levelBS <- requiredQueryParam "level"

  branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS
  blockLevel :: RawLevel <- either (throwError . T.pack . show) return $ Aeson.eitherDecodeStrict' levelBS

  either (throwError . T.pack . show ) return =<< runExceptT (ancestors blockLevel branch)

snapBallots :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text Ballots)
snapBallots = runExceptT $ do
  blockBS <- requiredQueryParam "block"
  block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_Ballots block

snapBallot :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text (Maybe Ballot))
snapBallot = runExceptT $ do
  blockBS <- requiredQueryParam "block"
  pkhBS <- requiredQueryParam "pkh"
  block <- either (throwError . T.pack . show) return $ fromBase58 blockBS
  pkh <- either (throwError . T.pack . show) return $ tryReadPublicKeyHash pkhBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_Ballot block pkh

snapProposals :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text (Seq ProposalVotes))
snapProposals = runExceptT $ do
  blockBS <- requiredQueryParam "block"
  block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_Proposals block

snapBlock :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text Block)
snapBlock = runExceptT $ do
  blockBS <- requiredQueryParam "hash"
  block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_Block block

snapBlockHeader :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text BlockHeader)
snapBlockHeader = runExceptT $ do
  blockBS <- requiredQueryParam "hash"
  block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_BlockHeader block

snapBakingRights :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text (Seq BakingRights))
snapBakingRights = snapRights NodeQueryIx_BakingRights

snapEndorsingRights :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text (Seq EndorsingRights))
snapEndorsingRights = snapRights NodeQueryIx_EndorsingRights

snapRights :: forall a r m .
  ( MonadSnap m
  , MonadReader r m
  , HasNodeDataSource r
  , Aeson.FromJSON a, Aeson.ToJSON a
  )
  => (BlockHash -> RawLevel -> NodeQueryIx a)
  -> m (Either Text a)
snapRights f = do
  mBranch <- runExceptT $ do
    branchBS <- requiredQueryParam "branch"
    levelBS <- requiredQueryParam "level"

    branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS
    blockLevel :: RawLevel <- either (throwError . T.pack . show) return $ Aeson.eitherDecodeStrict' levelBS
    pure (branch, blockLevel)

  dsrc <- asks (^. nodeDataSource)
  let
    -- To do this without liftIO we would have to add MonadBaseNoPureAborts instance for MonadSnap
    runNodeQueryIx x = liftIO $ runLoggingEnv (_nodeDataSource_logger dsrc) $ flip runReaderT dsrc $ runExceptT (runNodeQueryT x)
  fmap join $ for mBranch $ \(branch, blockLevel) -> do
    res :: Either CacheError a <- runNodeQueryIx $ nodeQueryIx $ f branch blockLevel
    case res of
      Left e -> pure $ Left $ tshow e
      Right v -> pure $ Right v

snapAccount :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text Account)
snapAccount = runExceptT $ do
  blockBS <- requiredQueryParam "block"
  pkhBS <- requiredQueryParam "pkh"

  block <- either (throwError . T.pack . show) return $ fromBase58 blockBS
  pkh <- either (throwError . T.pack . show) return $ tryReadPublicKeyHash pkhBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_Account block (Implicit pkh)

snapProposalVote :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text (Set ProtocolHash))
snapProposalVote = runExceptT $ do
  blockBS <- requiredQueryParam "block"
  pkhBS <- requiredQueryParam "pkh"

  block <- either (throwError . T.pack . show) return $ fromBase58 blockBS
  pkh <- either (throwError . T.pack . show) return $ tryReadPublicKeyHash pkhBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_ProposalVote block pkh

snapListings :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text (Seq VoterDelegate))
snapListings = runExceptT $ do
  blockBS <- requiredQueryParam "block"
  block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_Listings block

snapCurrentProposal :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text (Maybe ProtocolHash))
snapCurrentProposal = runExceptT $ do
  blockBS <- requiredQueryParam "block"

  block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_CurrentProposal block

snapCurrentQuorum :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text Int)
snapCurrentQuorum = runExceptT $ do
  blockBS <- requiredQueryParam "block"
  block <- either (throwError . T.pack . show) return $ fromBase58 blockBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_CurrentQuorum block

snapDelegateInfo :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text DelegateInfo)
snapDelegateInfo = runExceptT $ do
  nds <- asks (^. nodeDataSource)
  branchBS <- requiredQueryParam "branch"
  delegateBS <- requiredQueryParam "delegate"

  branch <- either (throwError . T.pack . show) return $ fromBase58 branchBS
  delegate <- either (throwError . T.pack . show) return $ tryReadPublicKeyHash delegateBS
  blockLevel <- maybe (throwError "block unknown") (return . view level) =<< liftIO (atomically $ lookupBlock nds branch)

  cd <- asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_DelegateInfo branch blockLevel delegate
  pure $ DelegateInfo
    { _delegateInfo_balance = _cacheDelegateInfo_balance cd
    , _delegateInfo_frozenBalance = _cacheDelegateInfo_frozenBalance cd
    , _delegateInfo_frozenBalanceByCycle = _cacheDelegateInfo_frozenBalanceByCycle cd
    , _delegateInfo_stakingBalance = _cacheDelegateInfo_stakingBalance cd
    , _delegateInfo_delegatedContracts = mempty
    , _delegateInfo_delegatedBalance = _cacheDelegateInfo_delegatedBalance cd
    , _delegateInfo_deactivated = _cacheDelegateInfo_deactivated cd
    , _delegateInfo_gracePeriod = _cacheDelegateInfo_gracePeriod cd
    }

snapPublicKey :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text PublicKey)
snapPublicKey = runExceptT $ do
  contractIdBS <- requiredQueryParam "contract-id"
  contractId <- either (throwError . T.pack . show) return $ tryReadContractId contractIdBS

  asTextExcept @CacheError $ nodeQueryDataSource $ NodeQuery_PublicKey contractId

snapProtocolIndex :: (MonadSnap m, MonadReader r m, HasNodeDataSource r) => m (Either Text ProtocolIndex)
snapProtocolIndex = do
  mProtocol <- runExceptT $ do
    protocolBS <- requiredQueryParam "protocol"

    either (throwError . T.pack . show) return $ fromBase58 protocolBS

  dsrc <- asks (^. nodeDataSource)
  -- To do this without liftIO we would have to add MonadBaseNoPureAborts instance for MonadSnap
  let runNodeQuery x = liftIO $ runLoggingEnv (_nodeDataSource_logger dsrc) $ flip runReaderT dsrc $ runExceptT (runNodeQueryT x)
  fmap join $ for mProtocol $ \protocol -> do
    res :: Either CacheError ProtocolIndex <- runNodeQuery $ do
      headBlk <- maybe (nqThrowError CacheError_NotEnoughHistory) pure =<< nqAtomically (dataSourceHead dsrc)
      getProtocolIndex (headBlk ^. hash) protocol
    case res of
      Left e -> pure $ Left $ tshow e
      Right v -> pure $ Right v
