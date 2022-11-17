{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-} -- for MonadError instance

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

-- TODO: move this to ~lib?
module Backend.NodeRPC where

import Prelude hiding (cycle, round)
import Control.Arrow (left)
import Control.Concurrent.STM (
    STM,
    TQueue,
    TVar,
    atomically,
    readTVar,
    retry,
    writeTQueue,
  )

import Control.Exception.Safe (Exception, MonadMask, withException)
import Control.Lens (re, review, (<>~))
import Control.Lens.TH (makeLenses)
import Control.Monad (ap)
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (ExitCase (..), MonadCatch, MonadThrow, bracket, catch, generalBracket, mask, throwM, uninterruptibleMask)
import Control.Monad.Error.Lens (catching)
import Control.Monad.Except (ExceptT (..), MonadError, catchError, liftEither, runExceptT, throwError)
import Control.Monad.Logger (LoggingT, MonadLoggerIO, MonadLogger, logDebug, logDebugSH, logError, monadLoggerLog)
import Control.Monad.Reader (local, reader)
import qualified Control.Monad.State as S
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson.GADT (deriveJSONGADT)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Either (rights)
import Data.Foldable (find)
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Int (Int32)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..))
import Data.Pool (Pool)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (singleton, (<|))
import qualified Data.Set as Set
import Data.String.Here.Interpolated (i)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Id.Class
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple.LargeObjects as PG
import qualified Database.PostgreSQL.Simple as PG
import Named
import qualified Network.HTTP.Client as Http
import Rhyolite.Backend.DB (MonadBaseNoPureAborts, runDb, project1)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject, withLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, executeMany, queryQ, sql)
import Rhyolite.Backend.DB.Serializable
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Rhyolite.Schema (LargeObjectId (..))
import Safe (headMay)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.NodeRPC
import Tezos.Types hiding (Block)
import qualified Tezos.Lima.Types as Lima

import Backend.Common (LedgerQuery, timeout')
import Backend.Schema
import Backend.STM (MonadSTM (liftSTM), newTVar', readTVar', writeTVar')
import Common.Schema
import ExtraPrelude

import Orphans.Instances ()

-- This exception should be impossible, but that depends on the node
-- working correctly.  The information inside is just the arguments of
-- the request you would have made to end up with it.
data NoRightsException = NoRightsException BlockHash RawLevel Round
  deriving (Eq, Ord, Show, Typeable)

instance Exception NoRightsException

data NodeQuery a where
  NodeQuery_ProtocolConstants :: BlockQuery -> NodeQuery ProtoInfo
  -- Query only baking rights with zero 'round'.
  NodeQuery_BakingRights      :: BlockQuery -> Set RawLevel -> NodeQuery (Seq BakingRightsCrossCompat)
  NodeQuery_EndorsingRights   :: BlockQuery -> Set RawLevel -> NodeQuery (Seq EndorsingRightsCrossCompat)
  NodeQuery_Account           :: BlockQuery -> ContractId -> NodeQuery AccountCrossCompat
  NodeQuery_Ballots           :: BlockQuery -> NodeQuery BallotsCrossCompat
  NodeQuery_Ballot            :: BlockQuery -> PublicKeyHash -> NodeQuery (Maybe Ballot)
  NodeQuery_ProposalVote      :: BlockQuery -> PublicKeyHash -> NodeQuery (Set ProtocolHash)
  NodeQuery_Listings          :: BlockQuery -> NodeQuery VoterListingsCrossCompat
  NodeQuery_Proposals         :: BlockQuery -> NodeQuery ProposalVotesListCrossCompat
  NodeQuery_CurrentProposal   :: BlockQuery -> NodeQuery (Maybe ProtocolHash)
  NodeQuery_CurrentQuorum     :: BlockQuery -> NodeQuery Int
  NodeQuery_Block             :: BlockQuery -> NodeQuery BlockCrossCompat
  NodeQuery_BlockHeader       :: BlockQuery -> NodeQuery BlockHeader
  NodeQuery_DelegateInfo      :: BlockQuery -> RawLevel -> PublicKeyHash -> NodeQuery CacheDelegateInfo
  NodeQuery_ParticipationInfo :: BlockQuery -> RawLevel -> PublicKeyHash -> NodeQuery ParticipationInfo
  NodeQuery_Balance           :: BlockQuery -> RawLevel -> PublicKeyHash -> NodeQuery Tez
  NodeQuery_Blocks            :: BlockHash -> RawLevel -> NodeQuery (Seq BlockHash)
  NodeQuery_Round             :: BlockQuery -> NodeQuery Int32
deriving instance Show (NodeQuery a)
deriving instance Typeable (NodeQuery a)

-- @NodeQuery@ smart constructors that allow arbitrary @ToBlockQuery@ arguments.
-- It's hard to use this constraint in GADT constructors directly because it'll
-- require to manually defined @GEq@ and @GCompare@ instances since TH is unable
-- to generate instances for GADTs with existentially parameterized constructors:(
nodeQuery_ProtocolConstants :: ToBlockQuery blk => blk -> NodeQuery ProtoInfo
nodeQuery_BakingRights      :: ToBlockQuery blk => blk -> Set RawLevel -> NodeQuery (Seq BakingRightsCrossCompat)
nodeQuery_EndorsingRights   :: ToBlockQuery blk => blk -> Set RawLevel -> NodeQuery (Seq EndorsingRightsCrossCompat)
nodeQuery_Account           :: ToBlockQuery blk => blk -> ContractId -> NodeQuery AccountCrossCompat
nodeQuery_Ballots           :: ToBlockQuery blk => blk -> NodeQuery BallotsCrossCompat
nodeQuery_Ballot            :: ToBlockQuery blk => blk -> PublicKeyHash -> NodeQuery (Maybe Ballot)
nodeQuery_ProposalVote      :: ToBlockQuery blk => blk -> PublicKeyHash -> NodeQuery (Set ProtocolHash)
nodeQuery_Listings          :: ToBlockQuery blk => blk -> NodeQuery VoterListingsCrossCompat
nodeQuery_Proposals         :: ToBlockQuery blk => blk -> NodeQuery ProposalVotesListCrossCompat
nodeQuery_CurrentProposal   :: ToBlockQuery blk => blk -> NodeQuery (Maybe ProtocolHash)
nodeQuery_CurrentQuorum     :: ToBlockQuery blk => blk -> NodeQuery Int
nodeQuery_Block             :: ToBlockQuery blk => blk -> NodeQuery BlockCrossCompat
nodeQuery_BlockHeader       :: ToBlockQuery blk => blk -> NodeQuery BlockHeader
nodeQuery_DelegateInfo      :: ToBlockQuery blk => blk -> RawLevel -> PublicKeyHash -> NodeQuery CacheDelegateInfo
nodeQuery_ParticipationInfo :: ToBlockQuery blk => blk -> RawLevel -> PublicKeyHash -> NodeQuery ParticipationInfo
nodeQuery_Balance           :: ToBlockQuery blk => blk -> RawLevel -> PublicKeyHash -> NodeQuery Tez
nodeQuery_Round             :: ToBlockQuery blk => blk -> NodeQuery Int32
nodeQuery_ProtocolConstants = NodeQuery_ProtocolConstants . toBlockQuery
nodeQuery_BakingRights blk = NodeQuery_BakingRights (toBlockQuery blk)
nodeQuery_EndorsingRights blk = NodeQuery_EndorsingRights (toBlockQuery blk)
nodeQuery_Account blk = NodeQuery_Account (toBlockQuery blk)
nodeQuery_Ballots blk = NodeQuery_Ballots (toBlockQuery blk)
nodeQuery_Ballot blk = NodeQuery_Ballot (toBlockQuery blk)
nodeQuery_ProposalVote blk = NodeQuery_ProposalVote (toBlockQuery blk)
nodeQuery_Listings blk = NodeQuery_Listings (toBlockQuery blk)
nodeQuery_Proposals blk = NodeQuery_Proposals (toBlockQuery blk)
nodeQuery_CurrentProposal blk = NodeQuery_CurrentProposal (toBlockQuery blk)
nodeQuery_CurrentQuorum blk = NodeQuery_CurrentQuorum (toBlockQuery blk)
nodeQuery_Block blk = NodeQuery_Block (toBlockQuery blk)
nodeQuery_BlockHeader blk = NodeQuery_BlockHeader (toBlockQuery blk)
nodeQuery_DelegateInfo blk = NodeQuery_DelegateInfo (toBlockQuery blk)
nodeQuery_ParticipationInfo blk = NodeQuery_ParticipationInfo (toBlockQuery blk)
nodeQuery_Balance blk = NodeQuery_Balance (toBlockQuery blk)
nodeQuery_Round blk = NodeQuery_Round (toBlockQuery blk)

data NodeQueryIx a where
  NodeQueryIx_BakingRights    :: BlockQuery -> Set RawLevel -> NodeQueryIx (Seq BakingRightsCrossCompat)
  NodeQueryIx_EndorsingRights :: BlockQuery -> Set RawLevel -> NodeQueryIx (Seq EndorsingRightsCrossCompat)
deriving instance Show (NodeQueryIx a)

nodeQueryIx_BakingRights    :: ToBlockQuery blk => blk -> Set RawLevel -> NodeQueryIx (Seq BakingRightsCrossCompat)
nodeQueryIx_EndorsingRights :: ToBlockQuery blk => blk -> Set RawLevel -> NodeQueryIx (Seq EndorsingRightsCrossCompat)
nodeQueryIx_BakingRights blk = NodeQueryIx_BakingRights (toBlockQuery blk)
nodeQueryIx_EndorsingRights blk = NodeQueryIx_EndorsingRights (toBlockQuery blk)

toCacheDelegateInfo :: PublicKeyHash -> DelegateInfoCrossCompat -> CacheDelegateInfo
toCacheDelegateInfo pkh di = CacheDelegateInfo
  { _cacheDelegateInfo_balance = di ^. delegateInfoCrossCompat_balance
  , _cacheDelegateInfo_frozenBalance = di ^. delegateInfoCrossCompat_frozenBalance
  , _cacheDelegateInfo_stakingBalance = di ^. delegateInfoCrossCompat_stakingBalance
  -- , _cacheDelegateInfo_delegatedContracts = _delegateInfo_delegatedContracts di
  , _cacheDelegateInfo_delegatedBalance = di ^. delegateInfoCrossCompat_delegatedBalance
  , _cacheDelegateInfo_deactivated = di ^. delegateInfoCrossCompat_deactivated
  , _cacheDelegateInfo_gracePeriod = di ^. delegateInfoCrossCompat_gracePeriod
  , _cacheDelegateInfo_activeConsensusKey = fromMaybe pkh $ di ^. delegateInfoCrossCompat_activeConsensusKey
  , _cacheDelegateInfo_pendingConsensusKey = mbClosestPendingPkh
  }

data RpcResult a = RpcResult
  { _rpcResult_raw :: LBS.ByteString
  , _rpcResult_value :: a
  } deriving (Functor)

-- | Cache that is used within @NodeQueryT@.
-- It stores occured errors and successful responses
data NodeQueryTCache = NodeQueryTCache
  { _nodeQueryTCache_errors :: DMap NodeQuery (Const KilnRpcError)
  , _nodeQueryTCache_responses :: DMap NodeQuery RpcResult
  }

emptyNodeQueryTCache :: NodeQueryTCache
emptyNodeQueryTCache = NodeQueryTCache DMap.empty DMap.empty

makeLenses 'NodeQueryTCache

data NodeDataSource = NodeDataSource
  { _nodeDataSource_chain :: ChainId
  , _nodeDataSource_httpMgr :: Http.Manager
  , _nodeDataSource_pool :: Pool Postgresql
  , _nodeDataSource_latestFinalHead :: TVar (Maybe BranchInfo)
  , _nodeDataSource_logger :: LoggingEnv
  , _nodeDataSource_ioQueue :: TQueue (IO ())
  , _nodeDataSource_ledgerIOQueue :: TQueue (LedgerQuery (LoggingT IO))
  , _nodeDataSource_kilnNodeUri :: URI
  , _nodeDataSource_nodeForQuery :: Maybe URI -- Override the node selection algo, and do RPC using this node
  } deriving (Typeable, Generic)
makeLenses 'NodeDataSource

class HasNodeDataSource a where
  nodeDataSource :: Lens' a NodeDataSource

instance HasNodeDataSource NodeDataSource where
  nodeDataSource = id

class MonadLogger m => MonadNodeQuery m where
  data AnswerM m :: * -> *
  asksNodeDataSource :: (NodeDataSource -> a) -> m a
  nqThrowError :: KilnRpcError -> m a
  nqCatchError :: m a -> (KilnRpcError -> m a) -> m a
  nqInDB :: (forall n. (MonadLogger n, PostgresRaw n) => n a) -> m a
  nqAtomically :: STM a -> m a
  default nqAtomically :: MonadIO m => STM a -> m a
  nqAtomically action = liftIO $ atomically action
  withFinishWith :: NodeDataSource -> (forall r. (Either KilnRpcError a -> STM r) -> STM (m r)) -> STM (m (AnswerM m a))
  nodeRPCOrBust :: (FromJSON a) => NodeQuery a -> m (RpcResult a)

askNodeDataSource :: MonadNodeQuery m => m NodeDataSource
askNodeDataSource = asksNodeDataSource id

nqLiftEither :: (MonadNodeQuery m) => Either KilnRpcError a -> m a
nqLiftEither = \case
  Left e -> nqThrowError e
  Right v -> pure v

nqTry :: (MonadNodeQuery m) => m a -> m (Either KilnRpcError a)
nqTry action = (Right <$> action) `nqCatchError` (pure . Left)

newtype NodeQueryQueued a = NodeQueryQueued { unNodeQueryQueued :: ExceptT KilnRpcError (ReaderT NodeDataSource IO) a }

runNodeQueryQueued :: (MonadReader s m, HasNodeDataSource s, MonadError e m, AsKilnRpcError e, MonadIO m) => NodeQueryQueued a -> m a
runNodeQueryQueued action = do
  nds <- view nodeDataSource
  (liftEither =<<) $ fmap (left (review asKilnRpcError)) $ liftIO $ flip runReaderT nds $ runExceptT $ unNodeQueryQueued action

deriving newtype instance Functor NodeQueryQueued
deriving newtype instance Applicative NodeQueryQueued
deriving newtype instance Monad NodeQueryQueued
deriving newtype instance MonadIO NodeQueryQueued
deriving newtype instance MonadBase IO NodeQueryQueued
deriving newtype instance MonadBaseControl IO NodeQueryQueued
deriving newtype instance MonadThrow NodeQueryQueued
deriving newtype instance MonadCatch NodeQueryQueued
deriving newtype instance MonadMask NodeQueryQueued
instance MonadLogger NodeQueryQueued where
  monadLoggerLog a b c d = do
    logger <- asksNodeDataSource _nodeDataSource_logger
    runLoggingEnv logger $ monadLoggerLog a b c d

instance MonadNodeQuery NodeQueryQueued where
  data AnswerM NodeQueryQueued a = NodeQueryQueuedAnswerM { unNodeQueryQueuedAnswerM :: ReaderT UTCTime STM (Maybe (Either KilnRpcError a)) }
  asksNodeDataSource = NodeQueryQueued . asks
  nqThrowError = NodeQueryQueued . throwError
  nqCatchError action handler = NodeQueryQueued $ catchError (unNodeQueryQueued action) (unNodeQueryQueued . handler)
  nqInDB action = do
    db <- asksNodeDataSource _nodeDataSource_pool
    logger <- asksNodeDataSource _nodeDataSource_logger
    NodeQueryQueued $ lift @(ExceptT KilnRpcError) $ runLoggingEnv logger $ runDb (Identity db) action
  withFinishWith nds cb = do
    -- A separate TVar for keeping the actual API result (outside the cache structure)
    apiResultVar :: TVar (Maybe (Either KilnRpcError a)) <- newTVar' Nothing
    action <- cb $ writeTVar' apiResultVar . Just
    let ioQueue = _nodeDataSource_ioQueue nds
    liftSTM $ writeTQueue ioQueue $ void $ flip runReaderT nds $ runExceptT $ unNodeQueryQueued action
    return $ return $ NodeQueryQueuedAnswerM $ readTVar' apiResultVar

  -- Here we examine the internal & external nodes before doing the query
  -- We keep hold of our candidate nodes right till the end in case we exhaust all of our options
  -- and need to give everything that we tried and what went wrong to the user in a KilnRpcError_NoSuitableNode
  -- error.
  nodeRPCOrBust q = do
    $(logDebug) [i|nodeRPCOrBust@NodeQueryQueued: ${tshow q}|]
    dsrc <- askNodeDataSource
    mNodesToTry <- case _nodeDataSource_nodeForQuery dsrc of
      -- If we have the nodeForQuery override set, just push it through assuming that the overrider is responsible for
      -- making sure that it's good and don't load any other candidates.
      Just n -> pure [n]
      Nothing -> do
        nodes <- nqInDB $ getActiveNodeDetails $ _nodeDataSource_kilnNodeUri dsrc
        pure $ map (view _1) nodes

    result <- case mNodesToTry of
      [] -> pure $ Left $ KilnRpcError_NoSuitableNode (tshow q) []
      nodesToTry -> do
        res <- foldM `flip` Left [] `flip` nodesToTry $ \case
            answer@(Right _) -> const $ pure answer -- short circuit if there is already an answer
            Left es -> \anyNode -> do
              let ctx = NodeRPCContext (_nodeDataSource_httpMgr dsrc) (Uri.render anyNode)
              r <- NodeQueryQueued $ liftIO $ nodeQueryDataSourceImpl (_nodeDataSource_chain dsrc) ctx (_nodeDataSource_logger dsrc) q
              pure $ first ((:es).(anyNode,)) r
        case res of
          Right r -> pure $ Right r
          Left failedNodes -> pure $ Left $ KilnRpcError_NoSuitableNode (tshow q) $
            fmap (second (UnsuitableNodeReason_QueryFailed . tshow)) failedNodes

    nqLiftEither result

newtype NodeQueryImmediate a = NodeQueryImmediate { unNodeQueryImmediate :: NodeQueryQueued a }

deriving newtype instance Functor NodeQueryImmediate
deriving newtype instance Applicative NodeQueryImmediate
deriving newtype instance Monad NodeQueryImmediate
deriving newtype instance MonadIO NodeQueryImmediate
deriving newtype instance MonadBase IO NodeQueryImmediate
deriving newtype instance MonadBaseControl IO NodeQueryImmediate
deriving newtype instance MonadThrow NodeQueryImmediate
deriving newtype instance MonadCatch NodeQueryImmediate
deriving newtype instance MonadMask NodeQueryImmediate
instance MonadLogger NodeQueryImmediate where
  monadLoggerLog a b c d = do
    logger <- asksNodeDataSource _nodeDataSource_logger
    runLoggingEnv logger $ monadLoggerLog a b c d
instance MonadNodeQuery NodeQueryImmediate where
  newtype AnswerM NodeQueryImmediate a = NodeQueryImmediateAnswerM { unNodeQueryImmediateAnswerM :: a }
  asksNodeDataSource = NodeQueryImmediate . asksNodeDataSource
  nqThrowError = NodeQueryImmediate . nqThrowError
  nqCatchError action handler = NodeQueryImmediate $ nqCatchError (unNodeQueryImmediate action) (unNodeQueryImmediate . handler)
  nqInDB action = NodeQueryImmediate $ nqInDB action
  withFinishWith _ cb = (fmap NodeQueryImmediateAnswerM . nqLiftEither =<<) <$> cb return
  nodeRPCOrBust q = NodeQueryImmediate $ nodeRPCOrBust q

data NodeQueryTResult a where
  NodeQueryTResult_Done :: a -> NodeQueryTResult a
  NodeQueryTResult_Query :: forall a b. (FromJSON a) => NodeQuery a -> NodeQueryTResult b

deriving instance Functor NodeQueryTResult

newtype NodeQueryT m a = NodeQueryT { unNodeQueryT :: NodeQueryTCache -> m (NodeQueryTResult a) }

instance (MonadIO m, MonadReader s m, HasNodeDataSource s, MonadError e m, AsKilnRpcError e, PostgresRaw m) => MonadNodeQuery (NodeQueryT m) where
  newtype AnswerM (NodeQueryT m) a = NodeQueryTAnswerM { unNodeQueryTAnswerM :: a }
  asksNodeDataSource = lift . views nodeDataSource
  nqThrowError e = lift $ throwError $ e ^. re asKilnRpcError
  nqCatchError action handler = NodeQueryT $ \cache -> catching asKilnRpcError (unNodeQueryT action cache) (flip unNodeQueryT cache . handler)
  nqInDB = id
  withFinishWith _ cb = (fmap NodeQueryTAnswerM . nqLiftEither =<<) <$> cb return
  nodeRPCOrBust q = NodeQueryT $ \cache ->
    case DMap.lookup q (cache ^. nodeQueryTCache_responses) of
      Nothing -> case DMap.lookup q (cache ^. nodeQueryTCache_errors) <&> getConst of
        Just e -> throwError $ e ^. re asKilnRpcError
        Nothing -> pure $ NodeQueryTResult_Query q
      Just res -> pure $ NodeQueryTResult_Done res

instance (MonadIO m, MonadNodeQuery (NodeQueryT m)) => MonadLogger (NodeQueryT m) where
  monadLoggerLog a b c d = do
    logger <- asksNodeDataSource _nodeDataSource_logger
    runLoggingEnv logger $ monadLoggerLog a b c d

instance Monad m => Monad (NodeQueryT m) where
  return = NodeQueryT . const . return . NodeQueryTResult_Done
  (NodeQueryT x) >>= f = NodeQueryT $ \cache -> x cache >>= \case
    NodeQueryTResult_Done v -> unNodeQueryT (f v) cache
    NodeQueryTResult_Query q -> pure $ NodeQueryTResult_Query q

instance Monad m => Applicative (NodeQueryT m) where
  (<*>) = ap
  pure = return

deriving instance Functor m => Functor (NodeQueryT m)

instance MonadTrans NodeQueryT where
  lift = NodeQueryT . const . fmap NodeQueryTResult_Done

instance MonadIO m => MonadIO (NodeQueryT m) where
  liftIO = lift . liftIO

instance MonadReader r m => MonadReader r (NodeQueryT m) where
  ask = lift ask
  local = mapNodeQueryT . local
  reader = lift . reader

instance MonadError e m => MonadError e (NodeQueryT m) where
  throwError = lift . throwError
  catchError (NodeQueryT m) h = NodeQueryT $ \bad -> catchError (m bad) (flip unNodeQueryT bad . h)

instance MonadThrow m => MonadThrow (NodeQueryT m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (NodeQueryT m) where
  catch (NodeQueryT m) h = NodeQueryT $ \bad -> catch (m bad) (flip unNodeQueryT bad . h)

-- mostly copied from the MonadMask instances for EitherT, ExceptT, and MaybeT
instance MonadMask m => MonadMask (NodeQueryT m) where
  mask f = NodeQueryT $ \bad -> mask $ \u -> flip unNodeQueryT bad $ f $ \(NodeQueryT b) -> NodeQueryT $ u . b
  uninterruptibleMask f = NodeQueryT $ \bad -> uninterruptibleMask $ \u -> flip unNodeQueryT bad $ f $ \(NodeQueryT b) -> NodeQueryT $ u . b
  generalBracket acquire release use = NodeQueryT $ \bad -> do
    (ranswer, rreleased) <- generalBracket
      (unNodeQueryT acquire bad)
      (\case
        NodeQueryTResult_Query q -> const $ return $ NodeQueryTResult_Query q -- query during acquire, nothing to release
        NodeQueryTResult_Done resource -> \case
          ExitCaseSuccess (NodeQueryTResult_Done answer) -> flip unNodeQueryT bad $ release resource $ ExitCaseSuccess answer
          ExitCaseSuccess (NodeQueryTResult_Query _) -> flip unNodeQueryT bad $ release resource ExitCaseAbort
            -- because things need to actually happen in the release handler.  The query will still get passed through
            -- on another channel.
          ExitCaseException e -> flip unNodeQueryT bad $ release resource $ ExitCaseException e
          ExitCaseAbort -> flip unNodeQueryT bad $ release resource ExitCaseAbort)
      (\case
        NodeQueryTResult_Query q -> return $ NodeQueryTResult_Query q
        NodeQueryTResult_Done resource -> flip unNodeQueryT bad $ use resource)
    return $ case ranswer of
      NodeQueryTResult_Query q -> NodeQueryTResult_Query q
        -- let the query from 'use' win even if both are queries, both
        -- because it is first, and because 'release' will be called
        -- with different arguments on the final retry.
      NodeQueryTResult_Done answer -> case rreleased of
        NodeQueryTResult_Query q -> NodeQueryTResult_Query q
        NodeQueryTResult_Done released -> NodeQueryTResult_Done (answer, released)

instance (Monad m, PostgresRaw m) => PostgresRaw (NodeQueryT m)

instance PersistBackend m => PersistBackend (NodeQueryT m) where
  type PhantomDb (NodeQueryT m) = PhantomDb m
  type TableAnalysis (NodeQueryT m) = TableAnalysis m
  insert = lift . insert
  insert_ = lift . insert_
  insertBy u v = lift $ insertBy u v
  insertByAll = lift . insertByAll
  replace k v = lift $ replace k v
  replaceBy u v = lift $ replaceBy u v
  select = lift . select
  selectAll = lift selectAll
  get = lift . get
  getBy = lift . getBy
  update us c = lift $ update us c
  delete = lift . delete
  deleteBy = lift . deleteBy
  deleteAll = lift . deleteAll
  count = lift . count
  countAll = lift . countAll
  project p o = lift $ project p o
  migrate u v = S.mapStateT lift $ migrate u v
  executeRaw c q p = lift $ executeRaw c q p
  queryRaw c q p f = NodeQueryT $ \k -> do
    queryRaw c q p $ \rp -> unNodeQueryT (f $ lift rp) k
  insertList = lift . insertList
  getList = lift . getList

class Monad m => HasPgConn m where
  askPgConn :: m PG.Connection
  --default askPgConn :: forall t a m'. (m ~ t m', MonadTrans t, HasPgConn m') => t m PG.Connection -> m PG.Connection
  --askPgConn = lift askPgConn

instance (HasPgConn m) => HasPgConn (ExceptT e m) where
  askPgConn = lift askPgConn

instance (HasPgConn m) => HasPgConn (ReaderT r m) where
  askPgConn = lift askPgConn

instance Monad m => HasPgConn (DbPersist Postgresql m) where
  askPgConn = DbPersist $ do
    (Postgresql conn) <- ask
    pure conn

instance HasPgConn Serializable where
  askPgConn = unsafeLiftDbPersist askPgConn


instance (MonadBase b m, Monad m) => MonadBase b (NodeQueryT m) where
  liftBase = liftBaseDefault

instance (MonadMask m, PostgresLargeObject m, HasPgConn m, MonadIO m) => PostgresLargeObject (ExceptT e m) where
  withLargeObject oid mode = bracket
    (lift $ genericLiftWithConn $ \conn -> PG.loOpen conn (toOid oid) mode)
    (\lofd -> lift $ genericLiftWithConn $ \conn -> PG.loClose conn lofd)
    where
      toOid :: LargeObjectId -> PG.Oid
      toOid (LargeObjectId n) = PG.Oid (fromIntegral n)

      genericLiftWithConn :: (PG.Connection -> IO a) -> m a
      genericLiftWithConn f = liftIO . f =<< askPgConn

-- This is required for queueEmail
-- Would much rather write this instance for any @PostgresLargeObject m@ but neither
-- 'PostgresLargeObject' nor 'PostgresRaw' have a way to get the @Connection@ directly
-- so it's impossible to write this instance without a more precise stack.
-- Mostly copied from https://github.com/obsidiansystems/rhyolite/blob/073dd9187765accaacd35292911fbecdf5b73f4c/backend-db/Rhyolite/Backend/DB/LargeObjects.hs#L99-L102
instance (MonadMask m, PostgresLargeObject m, HasPgConn m, MonadIO m) => PostgresLargeObject (NodeQueryT m) where
  withLargeObject oid mode = bracket
    (lift $ genericLiftWithConn $ \conn -> PG.loOpen conn (toOid oid) mode)
    (\lofd -> lift $ genericLiftWithConn $ \conn -> PG.loClose conn lofd)
    where
      toOid :: LargeObjectId -> PG.Oid
      toOid (LargeObjectId n) = PG.Oid (fromIntegral n)

      genericLiftWithConn :: (PG.Connection -> IO a) -> m a
      genericLiftWithConn f = liftIO . f =<< askPgConn

-- | Map the unwrapped computation using the given function.
--
-- * @'unNodeQueryT' ('mapNodeQueryT' f m) = f ('unNodeQueryT' m)@
mapNodeQueryT :: (m (NodeQueryTResult a) -> n (NodeQueryTResult b)) -> NodeQueryT m a -> NodeQueryT n b
mapNodeQueryT f m = NodeQueryT $ f . unNodeQueryT m

{- | Run a database transaction using information from the node RPC.
     If information is needed from the node and it is not already
     cached in the database (or memory), the transaction will be
     rolled back, the RPC query will be loaded into the cache, and
     then the transaction will be retried from the beginning.  This
     ensures that the transaction will see a consistent view of the
     world even if it had to be interrupted to query the node.
-}
runNodeQueryT
  :: forall a s e m.
    ( HasCallStack
    , MonadIO m, MonadBaseNoPureAborts IO m
    , MonadReader s m, HasNodeDataSource s
    , MonadLogger m
    , MonadLoggerIO m
    , Show e
    )
  => NodeQueryT (ExceptT e (ReaderT NodeDataSource Serializable)) a -> ExceptT e m a
runNodeQueryT f = ExceptT @e $ go 0 emptyNodeQueryTCache
  where
    go :: Int -> NodeQueryTCache -> m (Either e a)
    go n cache = do
      $(logDebug) [i|RPC monad attempt number ${n} starting: ${prettyCallStack callStack}|]
      tryNodeQueryTWithDb cache f >>= \case
        Left e -> do
          $(logDebug) [i|RPC monad attempt number ${n}: failed: ${tshow e}: ${prettyCallStack callStack}|]
          return $ Left e
        Right (NodeQueryTResult_Done v) -> do
          $(logDebug) [i|RPC monad attempt number ${n}: succeeded|]
          return $ Right v
        Right (NodeQueryTResult_Query q) -> do
          $(logDebug) [i|RPC monad attempt number ${n}: retrying for query: ${tshow q}|]
          -- just get it into cache
          runExceptT (nodeQueryDataSource' q) >>= \case
            Right res -> go (n + 1) $ cache & nodeQueryTCache_responses <>~ DMap.singleton q res
            Left e -> go (n + 1) $ cache & nodeQueryTCache_errors <>~ DMap.singleton q (Const e)

tryNodeQueryTWithDb
  :: forall a s e m.
    ( MonadIO m, MonadBaseNoPureAborts IO m
    , MonadReader s m, HasNodeDataSource s
    , MonadLogger m
    , MonadLoggerIO m
    )
  => NodeQueryTCache -> NodeQueryT (ExceptT e (ReaderT NodeDataSource Serializable)) a -> m (Either e (NodeQueryTResult a))
tryNodeQueryTWithDb cache f = do
  nds <- view nodeDataSource
  let db = _nodeDataSource_pool nds
      bail = unsafeMkSerializable $ ReaderT $ \conn -> liftIO $ PG.rollback conn *> PG.begin conn
  runDb (Identity db) $ runReaderT (runExceptT (unNodeQueryT f cache)) nds >>= \case
    e@(Left _) -> e <$ bail
    v@(Right (NodeQueryTResult_Done _)) -> return v
    q@(Right (NodeQueryTResult_Query _)) -> q <$ bail


tryNodeQueryT :: Functor m => NodeQueryT m a -> m (Maybe a)
tryNodeQueryT f = do
  unNodeQueryT f emptyNodeQueryTCache <&> \case
    NodeQueryTResult_Done a -> Just a
    NodeQueryTResult_Query{} -> Nothing

-- | Blocks until a new head is seen.
--
-- Returns most recently seen head.
waitForNewFinalHead :: (HasNodeDataSource nds) => nds -> IO VeryBlockLike
waitForNewFinalHead nds = do
  oldHead <- view hash <<$>> dataSourceFinalHead nds

  atomically $ do
    newHeadInfo <- maybe retry pure =<< readTVar (nds ^. nodeDataSource . nodeDataSource_latestFinalHead)
    when (oldHead == Just (newHeadInfo ^. hash)) retry
    pure $ newHeadInfo ^.  branchInfo_block . withProtocolHash_value

-- | Extracts the level of the latest seen head
dataSourceHeadLevel :: (HasNodeDataSource nds, MonadSTM m) => nds -> m (Maybe RawLevel)
dataSourceHeadLevel nds = ((+2) . view level) <<$>> readTVar' (nds ^. nodeDataSource . nodeDataSource_latestFinalHead)

-- | extracts the latest known final head
dataSourceFinalHead
  :: forall nds m. (HasNodeDataSource nds, MonadSTM m)
  => nds -> m (Maybe BranchInfo)
dataSourceFinalHead nds = readTVar' (nds ^. nodeDataSource . nodeDataSource_latestFinalHead)

roundChunkSize :: Num a => a
roundChunkSize = 64

-- | Caching query function simplified by blocking until we get a result.
nodeQueryDataSource
  :: forall a s e m.
    ( MonadIO m
    , MonadReader s m, HasNodeDataSource s
    , MonadError e m, AsKilnRpcError e
    , FromJSON a
    )
  => NodeQuery a -> m a
nodeQueryDataSource q = fmap _rpcResult_value $ nodeQueryDataSource' q

nodeQueryDataSource'
  :: forall a s e m.
    ( MonadIO m
    , MonadReader s m, HasNodeDataSource s
    , MonadError e m, AsKilnRpcError e
    , FromJSON a
    )
  => NodeQuery a -> m (RpcResult a)
nodeQueryDataSource' q = do
  view (nodeDataSource . nodeDataSource_logger) >>=
    flip runLoggingEnv ($(logDebug) [i|nodeQueryDataSource: ${tshow q}|])
  NodeQueryQueuedAnswerM getResult <- runNodeQueryQueued $ nodeQueryDataSourceRaw' q
  now <- liftIO getCurrentTime
  timeout' timeoutSeconds (atomically $ maybe retry pure =<< runReaderT getResult now) >>= \case
    Nothing -> throwError $ KilnRpcError_Timeout timeoutSeconds ^. re asKilnRpcError
    Just (Left e) -> throwError $ e ^. re asKilnRpcError
    Just (Right x) -> pure x
  where
    -- Base timeout
    timeoutSeconds = 60*5

-- | Query cached data "nonblockingly".  Which is to say it will block for the database, but won't
--   try to connect to the node.  Calling code can handle the condition where the data was not
--   cached, for instance by abandoning the transaction before attempting an RPC call.
nodeQueryDataSourceSafe
  :: forall a m.
    ( MonadNodeQuery (NodeQueryT m)
    , MonadMask m
    , FromJSON a
    )
  => NodeQuery a -> NodeQueryT m a
nodeQueryDataSourceSafe q = unNodeQueryTAnswerM <$> nodeQueryDataSourceRaw q

-- | Query cached data immediately, in this thread.  Only meant to be used in the implementation
--   of recursive queries, lest the dreaded deadlock heisenbunny return.
nodeQueryDataSourceImmediate
  :: forall a s e m.
    ( MonadIO m
    , MonadReader s m, HasNodeDataSource s
    , MonadError e m, AsKilnRpcError e
    , FromJSON a
    )
  => NodeQuery a -> m a
nodeQueryDataSourceImmediate q = runNodeQueryQueued $
  unNodeQueryImmediate $ unNodeQueryImmediateAnswerM <$> nodeQueryDataSourceRaw q

nodeQueryDataSourceRaw
  :: forall m a.
    ( MonadNodeQuery m
    , MonadMask m
    , FromJSON a
    )
  => NodeQuery a -> m (AnswerM m a)
nodeQueryDataSourceRaw q = do
  $(logDebug) [i|nodeQueryDataSourceRaw: ${tshow q}|]
  dsrc <- asksNodeDataSource id
  join $ nqAtomically $ nodeQueryDataSourceSTM _rpcResult_value dsrc q

-- | Query cached data immediately, in this thread.  Only meant to be used in the implementation
--   of recursive queries, lest the dreaded deadlock heisenbunny return.
nodeQueryDataSourceImmediate'
  :: forall a s e m.
    ( MonadIO m
    , MonadReader s m, HasNodeDataSource s
    , MonadError e m, AsKilnRpcError e
    , FromJSON a
    )
  => NodeQuery a -> m (RpcResult a)
nodeQueryDataSourceImmediate' q = runNodeQueryQueued $
  unNodeQueryImmediate $ unNodeQueryImmediateAnswerM <$> nodeQueryDataSourceRaw' q

-- | Query cached data "nonblockingly".  Which is to say it will block for the database, but won't
--   try to connect to the node.  Calling code can handle the condition where the data was not
--   cached, for instance by abandoning the transaction before attempting an RPC call.
nodeQueryDataSourceSafe'
  :: forall a m.
    ( MonadNodeQuery (NodeQueryT m)
    , MonadMask m
    , FromJSON a
    )
  => NodeQuery a -> NodeQueryT m (RpcResult a)
nodeQueryDataSourceSafe' q = unNodeQueryTAnswerM <$> nodeQueryDataSourceRaw' q

nodeQueryDataSourceRaw'
  :: forall m a.
    ( MonadNodeQuery m
    , MonadMask m
    , FromJSON a
    )
  => NodeQuery a -> m (AnswerM m (RpcResult a))
nodeQueryDataSourceRaw' q = do
  $(logDebug) [i|nodeQueryDataSourceRaw: ${tshow q}|]
  dsrc <- asksNodeDataSource id
  join $ nqAtomically $ nodeQueryDataSourceSTM id dsrc q

-- | Core primitive for running a 'NodeQuery' against the worker queue.
-- Returns an action that will wait for a new request to be finished.
nodeQueryDataSourceSTM
  :: forall n a b m nds. (HasNodeDataSource nds, MonadSTM m, MonadLogger n, MonadNodeQuery n, MonadMask n, FromJSON a)
  => (RpcResult a -> b) -> nds -> NodeQuery a -> m (n (AnswerM n b))
nodeQueryDataSourceSTM projectRpcResult nds q = do
  liftSTM $ withFinishWith @n dsrc $ \finishWith -> do
    let
      -- Communicates the result upstream.
      -- XXX Can't actually use this type signature since 'r' is not in scope...
      -- writeResult :: Either KilnRpcError (RpcResult a) -> m r
      writeResult a' = nqAtomically $ finishWith $ fmap projectRpcResult a'
    return $
      -- Try very hard to write *something* into the result TVar in case of exception.
      -- The catch handles synchronous/recoverable errors, and its result passes through,
      -- which is necessary in the immediate case and harmless in the worker queue case.
      -- The withException handles asynchronous/unrecoverable errors.  In the case of a
      -- worker queue, the calling thread can still recover because it's a different
      -- thread.  The result is thrown away meaning in the immediate case the caller
      -- cannot recover, but this is fine because that's what is supposed to happen for
      -- such an error.
      (writeResult =<< nqTry (nodeRPCOrBust q))
        `catch` \e ->
          nqAtomically (finishWith $ Left $ KilnRpcError_SomeException e)
        `withException` \x ->
          nqAtomically (finishWith $ Left $ KilnRpcError_SomeException x)

  where
    dsrc = nds ^. nodeDataSource

unliftEither :: MonadError e m => m a -> m (Either e a)
unliftEither action = (Right <$> action) `catchError` (pure . Left)

nodeQueryDataSourceImpl
  :: forall a.
     ChainId
  -> NodeRPCContext
  -> LoggingEnv
  -> NodeQuery a
  -> IO (Either KilnRpcError (RpcResult a))
nodeQueryDataSourceImpl = nodeQueryImpl myNodeRPC ChainTag_Hash
  where
    myNodeRPC (RpcQuery decoder body method resources) =
      nodeRPC (RpcQuery (keepOriginalInput decoder) body method resources)

nodeQueryImpl
  :: forall a chain repr.
   ( QueryBlock repr, QueryHistory repr, BlockType repr ~ BlockCrossCompat, BlockHeaderType repr ~ BlockHeader, ChainType repr ~ chain)
  => (forall c m s e.
       ( MonadIO m, MonadLogger m, MonadReader s m , HasNodeRPC s, MonadError e m , AsRpcError e, Aeson.FromJSON c)
     => repr c -> m (RpcResult c))
  -> (ChainId -> chain)
  -> ChainId
  -> NodeRPCContext
  -> LoggingEnv
  -> NodeQuery a
  -> IO (Either KilnRpcError (RpcResult a))
nodeQueryImpl doNodeRPC toChain chainId ctx logger q = runExceptT $ runLoggingEnv logger ( $(logDebugSH) ("nodeQueryImpl called" :: Text,q)) *> case q of
  NodeQuery_ProtocolConstants branch -> nodeRPC' $ rProtoConstants chainId branch
  NodeQuery_BakingRights branch targetLevel ->
    -- Now Kiln uses only baking rights with zero round, so it's not
    -- neccessary to have an ability to set an arbitrary value of this parameter.
    --
    -- In case it's needed, the caching logic in 'nodeQueryIx' should also be changed.
    nodeRPC' $ rBakingRightsFull (Right targetLevel) 0 chainId branch
  NodeQuery_EndorsingRights branch targetLevel ->
    nodeRPC' $ rEndorsingRights (Right targetLevel) chainId branch
  NodeQuery_Account branch contractId ->
    nodeRPC' $ rContract contractId (toChain chainId) branch
  NodeQuery_Ballots branch -> nodeRPC' $ rBallots chainId branch
  NodeQuery_Ballot branch pkh -> nodeRPC' $ rBallot chainId branch pkh
  NodeQuery_ProposalVote branch pkh -> nodeRPC' $ rProposalVote chainId branch pkh
  NodeQuery_Listings branch -> nodeRPC' $ rListings chainId branch
  NodeQuery_Proposals branch -> nodeRPC' $ rProposals chainId branch
  NodeQuery_CurrentProposal branch -> nodeRPC' $ rCurrentProposal chainId branch
  NodeQuery_CurrentQuorum branch -> nodeRPC' $ rCurrentQuorum chainId branch
  NodeQuery_Block branch -> nodeRPC' $ rBlock (toChain chainId) branch
  NodeQuery_BlockHeader branch -> nodeRPC' $ rBlockHeader (toChain chainId) branch
  NodeQuery_DelegateInfo branch _lvl pkh -> fmap (fmap $ toCacheDelegateInfo pkh) $ nodeRPC' $ rDelegateInfo pkh chainId branch
  NodeQuery_ParticipationInfo branch _lvl pkh -> nodeRPC' $ rParticipationInfo pkh chainId branch
  NodeQuery_Balance branch _lvl pkh -> nodeRPC' $ rBalance pkh chainId branch
  NodeQuery_Blocks branch length' -> do
    (RpcResult _ response) <- nodeRPC' $ rBlocks chainId length' (Set.singleton branch)
    let blocks = branch Seq.<| fromMaybe mempty (Map.lookup branch response)
    pure $ RpcResult (Aeson.encode blocks) blocks
  NodeQuery_Round branch -> nodeRPC' $ rRound chainId branch
  where
    nodeRPC' :: forall c. Aeson.FromJSON c => repr c -> ExceptT KilnRpcError IO (RpcResult c)
    nodeRPC' q' = runReaderT (runLoggingEnv logger $ doNodeRPC q') ctx
    {-# INLINE nodeRPC' #-}

keepOriginalInput :: (LBS.ByteString -> Either err a) -> LBS.ByteString -> Either err (RpcResult a)
keepOriginalInput f str = RpcResult str <$> f str

nodeQueryIx
  :: forall a m.
    ( MonadNodeQuery (NodeQueryT m)
    , MonadMask m
    , PostgresRaw m
    , PersistBackend m
    , Aeson.FromJSON a
    , Monoid a
    )
  => NodeQueryIx a -> NodeQueryT m a
nodeQueryIx q = $(logDebugSH) ("nodeQueryIx called" :: Text, q) *> case q of
  NodeQueryIx_BakingRights ctx queryLvls -> do
    (cachedLvls, cachedRights) <- fmap unzip $ checkBakingRightsCache queryLvls
    let cachedLvlsSet = Set.fromList cachedLvls
        lvlsToFetch = queryLvls `Set.difference` cachedLvlsSet
        chunkedRights = mconcat $ catMaybes cachedRights
    if null lvlsToFetch
      then pure chunkedRights
      else do
        let filteredNodeQuery = nodeQuery_BakingRights ctx lvlsToFetch
        result <- nodeQueryDataSourceSafe filteredNodeQuery
        addBakingRightsToCache queryLvls result
        pure $ result <> chunkedRights
  -- We don't cache endorsing rights
  NodeQueryIx_EndorsingRights ctx lvl -> nodeQueryDataSourceSafe $ nodeQuery_EndorsingRights ctx lvl
  where
    checkBakingRightsCache
      :: ( Monad f
         , PostgresRaw f
         , MonadLogger f
         )
      => Set RawLevel -> f [(RawLevel, Maybe (Seq BakingRightsCrossCompat))]
    checkBakingRightsCache lvls = do
      let minLvl = Set.findMin lvls
          maxLvl = Set.findMax lvls
      cachedRights <- [queryQ|
        SELECT "level", "delegate", "round", "estimatedTime" AT TIME ZONE 'UTC'
        FROM "CacheBakingRights"
        WHERE "level" BETWEEN ?minLvl AND ?maxLvl
      |]
      for cachedRights $ \(lvl, delegate, round, estimatedTime) ->
        let
          bakingRight = BakingRightsLima $ Lima.BakingRights
            { _bakingRights_level = lvl
            , _bakingRights_delegate = delegate
            , _bakingRights_round = round
            , _bakingRights_estimatedTime = estimatedTime
            }
        in pure (lvl, Just $ Seq.singleton bakingRight)

    addBakingRightsToCache
      :: ( Monad f
         , PostgresRaw f
         , MonadLogger f
         , PersistBackend f
         )
      => Set RawLevel -> Seq BakingRightsCrossCompat -> f ()
    addBakingRightsToCache queryLevels bakingRights = do
      let
        rightsToCache = flip mapMaybe (Set.toList queryLevels) $ \lvl ->
          flip find bakingRights $ \right ->
            right ^. bakingRightsCrossCompat_level == lvl

      void $ executeMany [sql|
        INSERT INTO "CacheBakingRights" ("level", "round", "delegate", "estimatedTime")
        VALUES (?, ?, ?, ?)
      |] $ rightsToCache <&> \br ->
        ( br ^. bakingRightsCrossCompat_level
        , br ^. bakingRightsCrossCompat_round
        , br ^. bakingRightsCrossCompat_delegate
        , br ^. bakingRightsCrossCompat_estimatedTime
        )

-- | Logs the RPC error if it's not caused by an endpoint restriction.
{-# INLINE logKilnRpcError #-}
logKilnRpcError :: MonadLogger m => Text -> KilnRpcError -> m ()
logKilnRpcError _ (KilnRpcError_RpcError (RpcError_RestrictedEndpoint _)) = pure ()
logKilnRpcError desc err = $(logError) $
  "Node Query failed for '" <> desc <> "' Reason: " <> prettyKilnRpcError err

prettyKilnRpcError :: KilnRpcError -> Text
prettyKilnRpcError = \case
  KilnRpcError_NoKnownHeads -> "No known blocks in kiln's internal memory. This should resolve a few seconds after a first bootstrapped node is added."
  KilnRpcError_NoSuitableNode q reasons -> noSuitableNodeLogMessage q reasons
  KilnRpcError_Timeout t -> "Timed out after " <> tshow t
  KilnRpcError_RpcError rpcErr -> case rpcErr of
    RpcError_UnexpectedStatus _url _ statusLine -> "RPC Unexpected Status (Indicates that the node is unhealthy): " <> T.decodeUtf8 statusLine
    RpcError_HttpException _url e -> "RPC Exception (The Node is unreachable) " <> tshow e
    RpcError_NonJSON _url e bytes -> "The RPC returned a response that kiln did not understand. JSON Parse Error: " <> T.pack e <> " Response: " <> T.decodeUtf8 (LBS.toStrict bytes)
    RpcError_RestrictedEndpoint url -> "Node endpoint " <> url <> " is restricted."
  KilnRpcError_SomeException e -> "Kiln Exception (this indicates a kiln bug): " <> tshow e
  KilnRpcError_UnrevealedPublicKey contractId -> "Unrevealed Public Key: " <> tshow contractId
  KilnRpcError_UnknownProtocol p -> "Node does not know protocol: " <> tshow p

noSuitableNodeLogMessage :: Text -> [(URI, UnsuitableNodeReason)] -> Text
noSuitableNodeLogMessage q reasons = "No suitable node was found for query `" <> q <> "`. Nodes are [" <> (T.intercalate "," . fmap prettyUnsuitableReason $ reasons) <> "]"
  where
    prettyUnsuitableReason (u, r) = (("(" <> Uri.render u <> ",") <>) $ case r of
      UnsuitableNodeReason_QueryFailed ce -> "Query Failed on node: " <> ce
      UnsuitableNodeReason_QueryBeforeSavepoint savepointLevel queryLevel -> "The level required to fulfill this query is " <> prettyLevel queryLevel <> " but the node savepoint is at " <> prettyLevel savepointLevel
      UnsuitableNodeReason_MissingBlockInfo -> "Kiln has not yet retrieved the latest block head for this node"
      UnsuitableNodeReason_MissingSavepoint -> "Kiln has not yet retrieved the information about whether this node is on a savepoint or not"

    prettyLevel = tshow . unRawLevel

getActiveNodeDetails
  :: (MonadLogger m, PostgresRaw m) => URI -> m [(URI, Maybe VeryBlockLike)]
getActiveNodeDetails kilnNodeUri = do
  int <- let runningState = ProcessState_Running in [queryQ|
      SELECT d."data#headLevel"
           , d."data#headBlockHash"
           , d."data#headBlockPred"
           , d."data#headBlockBakedAt" AT TIME ZONE 'UTC'
           , d."data#fitness"
        FROM "NodeInternal" n
        JOIN "NodeDetails" d ON d.id = n.id
        JOIN "ProcessData" p ON p.id = n."data#data"
      WHERE NOT n."data#deleted"
        AND p."state" = ?runningState
      |] <&> fmap (\(l, b, p, t, f) -> (kilnNodeUri, VeryBlockLike <$> b <*> p <*> f <*> l <*> t))
  ext <- [queryQ|
      SELECT n."data#data#address"
           , d."data#headLevel"
           , d."data#headBlockHash"
           , d."data#headBlockPred"
           , d."data#headBlockBakedAt" AT TIME ZONE 'UTC'
           , d."data#fitness"
        FROM "NodeExternal" n
        JOIN "NodeDetails" d ON d.id = n.id
      WHERE NOT n."data#deleted"
      |] <&> fmap (\(addr, l, b, p, t, f) -> (addr, VeryBlockLike <$> b <*> p <*> f <*> l <*> t))
  pure $ ext <> int

-- Protocol Constants

getProtocolConstants
  :: forall m
   . (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => Either BlockHash ProtocolHash -> NodeQueryT m ProtoInfo
getProtocolConstants ct = do
  protoHash <- case ct of
    Right p -> pure p
    Left h -> view protocolHash <$> nodeQueryDataSourceSafe (nodeQuery_BlockHeader h)
  chainId <- asksNodeDataSource _nodeDataSource_chain
  existingEntries :: [ProtocolIndex] <- select $
    ProtocolIndex_chainIdField ==. chainId &&. ProtocolIndex_hashField ==. protoHash

  _protocolIndex_constants <$> case headMay existingEntries of
    Just existing -> pure existing
    Nothing -> do
      hash' <- case ct of
        Right _ -> askNodeDataSource >>= nqAtomically . dataSourceFinalHead
          >>= maybe (nqThrowError KilnRpcError_NoKnownHeads) (pure . view hash)
        Left hash' -> pure hash'
      getProtocolIndex hash' protoHash


getProtocolIndex
  :: forall m
   . (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => BlockHash -> ProtocolHash -> NodeQueryT m ProtocolIndex
getProtocolIndex branch protoHash = do
  chainId <- asksNodeDataSource _nodeDataSource_chain
  existingEntries :: [ProtocolIndex] <- select $
    ProtocolIndex_chainIdField ==. chainId &&. ProtocolIndex_hashField ==. protoHash

  case headMay existingEntries of
    Just existing -> pure existing
    Nothing -> nqTry (buildProtocolIndex branch protoHash) >>= \case
      Right p' -> do
        pure p'
      Left e -> do
        -- as a last measure just fetch the protocol constants without building the index
        p <- fetchProtocolForBlock chainId branch
        if p ^. protocolIndex_hash == protoHash
          then pure p
          else nqThrowError e

buildProtocolIndex
  :: forall m
   . (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => BlockHash -> ProtocolHash -> NodeQueryT m ProtocolIndex
buildProtocolIndex branch protoHash = do
  chainId <- asksNodeDataSource _nodeDataSource_chain
  -- Search until we have the history up to the desired protocol.
  (protocolFirstBlocksHistory, protocolLastBlocksHistory) <- buildProtocolHistoryUntil
    ! #predicate (\blk -> blk ^. protocolHash == protoHash)
    ! #branch branch

  case NE.nonEmpty $ sortOn (Down . (^. _2 . level)) $ Map.toList protocolFirstBlocksHistory of
    Nothing -> nqThrowError KilnRpcError_NoKnownHeads
    Just orderedFirstBlocksWithProto -> do
      -- To increase likelihood that a node knows the answer, we will use the *last* block
      -- in a protocol to get it's constants (the most recent block possible). To do this we
      -- pair up the protocols with the block immediately *prior* to the first block in the
      -- next protocol. For the most recent protocol, we will use 'branch' as the query block.
      branchBlock <- nodeQueryDataSourceSafe $ nodeQuery_Block branch
      let initOrderedLastBlocks = flip NE.map orderedFirstBlocksWithProto $ \(proto, _) ->
            -- The only protocol for which we know the first but don't know the last block is the current protocol.
            -- For it we use current @branch@ as the last block
            fromMaybe branchBlock $ Map.lookup proto protocolLastBlocksHistory
          protocolQueryBlockMap = NE.zip (NE.map snd orderedFirstBlocksWithProto) initOrderedLastBlocks

      protoIndexes :: [ProtocolIndex] <- fmap (rights . toList) $
        for protocolQueryBlockMap $ \(firstBlock, queryBlock') -> nqTry $ do
          -- Before using the query block instead of 'firstBlock', make sure it's protocol really is
          -- the same. If not, fall back to 'firstBlock'.
          -- While this situation shouldn't happen, it's possible for protocols to be introduced
          -- apart from the amendment process. In this case we may actually skip one
          -- in the scan which would cause this logic to pair the wrong constants with a
          -- protocol hash--and that's just too scary to think about.
          let
            actualQueryBlockHash = case queryBlock' of
              queryBlock | queryBlock ^. protocolHash == firstBlock ^. protocolHash -> queryBlock ^. hash
              _ -> firstBlock ^. hash
          constants <- nodeQueryDataSourceSafe' $ nodeQuery_ProtocolConstants actualQueryBlockHash
          pure ProtocolIndex
            { _protocolIndex_chainId = chainId
            , _protocolIndex_hash = firstBlock ^. protocolHash
            , _protocolIndex_proto = firstBlock ^. blockHeaderFull . blockHeaderFull_proto
            , _protocolIndex_jsonConstants = case Aeson.eitherDecode' (_rpcResult_raw constants) of
                                               Left errorMsg -> error ("the 'impossible' happened: aeson parse error on _rpcResult_raw: " <> errorMsg)
                                               Right x -> x
            , _protocolIndex_constants = _rpcResult_value constants
            , _protocolIndex_firstBlockHash = Just $ firstBlock ^. hash
            , _protocolIndex_firstBlockPredecessor = Just $ firstBlock ^. predecessor
            , _protocolIndex_firstBlockLevel = Just $ firstBlock ^. level
            , _protocolIndex_firstBlockFitness = Just $ firstBlock ^. fitness
            , _protocolIndex_firstBlockTimestamp = Just $ firstBlock ^. timestamp
            , _protocolIndex_firstBlockCycle = Just $ firstBlock ^. blockMetadata . blockMetadata_levelInfo . levelInfo_cycle
            }

      for_ protoIndexes $ \protoIndex -> do
        mp :: Maybe (Maybe BlockHash) <- project1 ProtocolIndex_firstBlockHashField
          (( ProtocolIndex_hashField ==. protoIndex ^. protocolIndex_hash )
            &&. (ProtocolIndex_chainIdField ==. chainId)
          )
        when (join mp == Nothing) $ do
          insert protoIndex
          notifyDefault $ Id @ProtocolIndex (protoIndex ^. protocolIndex_chainId, protoIndex ^. protocolHash)

      maybe (nqThrowError KilnRpcError_NoKnownHeads) pure $
        find ((protoHash ==) . view protocolHash) protoIndexes

-- | For each found protocol in the blockchain history we return
-- its first and last blocks.
buildProtocolHistoryUntil
  :: forall m
   . (MonadNodeQuery (NodeQueryT m), MonadMask m)
  => "predicate" :! (BlockCrossCompat -> Bool)
  -> "branch" :! BlockHash
  -> NodeQueryT m (Map ProtocolHash BlockCrossCompat, Map ProtocolHash BlockCrossCompat)
  -- Turns this into table, ProtocolHash
buildProtocolHistoryUntil (Arg predicate) (Arg branch) = do
  branchBlock <- nodeQueryDataSourceSafe $ nodeQuery_Block branch
  go ! #currentBlock branchBlock
     ! #currentProtocol (branchBlock ^. protocolHash)
     ! #protocolFirstBlocksHistory mempty
     ! #protocolLastBlocksHistory mempty
  where
    levelsBefore :: BlockCrossCompat -> RawLevel -> NodeQueryT m BlockCrossCompat
    levelsBefore blk lvls = nodeQueryDataSourceSafe $ nodeQuery_Block $ (blk ^. hash) ~~ lvls

    votingPeriodPosition = blockMetadata . blockMetadata_votingPeriodInfo . votingPeriodInfo_position

    go :: "currentBlock" :! BlockCrossCompat
       -> "currentProtocol" :! ProtocolHash
       -> "protocolFirstBlocksHistory" :! Map ProtocolHash BlockCrossCompat
       -> "protocolLastBlocksHistory" :! Map ProtocolHash BlockCrossCompat
       -> NodeQueryT m (Map ProtocolHash BlockCrossCompat, Map ProtocolHash BlockCrossCompat)
    go (Arg currentBlock) (Arg currentProtocol) (Arg protocolFirstBlocksHistory) (Arg protocolLastBlocksHistory) =
      case currentBlock ^. level == 1 of
        True -> do
          $(logDebug) [i|Got to the root searching for protocol: ${currentProtocol}|]
          -- If 'currentBlock' is at the minimum level, we call it the beginning of 'currentProtocol'.
          pure (Map.insert currentProtocol currentBlock protocolFirstBlocksHistory, protocolLastBlocksHistory)
        False -> do
          lastBlockInPreviousVotingPeriod <- levelsBefore currentBlock (currentBlock ^. votingPeriodPosition + 1)
          case currentProtocol == lastBlockInPreviousVotingPeriod ^. protocolHash of
            True -> do
              $(logDebug) [i|Found another voting period with the same protocol ${currentProtocol} - ${lastBlockInPreviousVotingPeriod ^. hash}|]
              go ! #currentBlock lastBlockInPreviousVotingPeriod
                 ! #currentProtocol currentProtocol
                 ! #protocolFirstBlocksHistory protocolFirstBlocksHistory
                 ! #protocolLastBlocksHistory protocolLastBlocksHistory
            False -> do
              $(logDebug) [i|Found a transition for ${currentProtocol} at ${lastBlockInPreviousVotingPeriod ^. hash}|]
              firstBlockInVotingPeriod <- levelsBefore currentBlock (currentBlock ^. votingPeriodPosition)

              (lastBlockInPreviousProtocol, firstBlockInProtocol) <- case currentProtocol == firstBlockInVotingPeriod ^. protocolHash of
                True -> pure (lastBlockInPreviousVotingPeriod, firstBlockInVotingPeriod)
                False -> do
                  $(logError) [i|Couldn't build ProtocolIndex for the protocol ${currentProtocol}|]
                  nqThrowError $ KilnRpcError_UnknownProtocol currentProtocol

              let protocolFirstBlocksHistory' = Map.insert currentProtocol firstBlockInProtocol protocolFirstBlocksHistory
                  protocolLastBlocksHistory' = Map.insert (lastBlockInPreviousProtocol ^. protocolHash) lastBlockInPreviousProtocol protocolLastBlocksHistory
              case predicate firstBlockInProtocol of
                True -> pure (protocolFirstBlocksHistory', protocolLastBlocksHistory') -- We finished searching.
                False -> go ! #currentBlock lastBlockInPreviousProtocol
                            ! #currentProtocol (lastBlockInPreviousProtocol ^. protocolHash)
                            ! #protocolFirstBlocksHistory protocolFirstBlocksHistory'
                            ! #protocolLastBlocksHistory protocolLastBlocksHistory'

fetchProtocolForBlock
  :: forall m
   . (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => ChainId
  -> BlockHash
  -> NodeQueryT m ProtocolIndex
fetchProtocolForBlock chainId blkHash = do
  $(logDebug) [i|fetchProtocolForBlock: ${blkHash}|]
  protoInfo <- nodeQueryDataSourceSafe' $ nodeQuery_ProtocolConstants blkHash
  blockHeader <- nodeQueryDataSourceSafe $ nodeQuery_BlockHeader blkHash
  let p = ProtocolIndex
          { _protocolIndex_chainId = chainId
          , _protocolIndex_hash = blockHeader ^. protocolHash
          , _protocolIndex_jsonConstants = case Aeson.eitherDecode' (_rpcResult_raw protoInfo) of
                                             Left errorMsg -> error ("the 'impossible' happened: aeson parse error on _rpcResult_raw: " <> errorMsg)
                                             Right x -> x
          , _protocolIndex_constants = _rpcResult_value protoInfo
          , _protocolIndex_proto = blockHeader ^. blockHeader_proto
          , _protocolIndex_firstBlockHash = Nothing
          , _protocolIndex_firstBlockPredecessor = Nothing
          , _protocolIndex_firstBlockLevel = Nothing
          , _protocolIndex_firstBlockFitness = Nothing
          , _protocolIndex_firstBlockTimestamp = Nothing
          , _protocolIndex_firstBlockCycle = Nothing
          }
  insert p
  pure p

deriveGEq ''NodeQuery
deriveGCompare ''NodeQuery
deriveGShow ''NodeQuery
deriveJSONGADT ''NodeQuery

deriveGEq ''NodeQueryIx
deriveGCompare ''NodeQueryIx
deriveGShow ''NodeQueryIx
deriveJSONGADT ''NodeQueryIx
