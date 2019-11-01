{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
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

{-# OPTIONS_GHC -Wall -Werror -Wno-orphans #-}

-- TODO: move this to ~lib?
module Backend.CachedNodeRPC where

import Prelude hiding (cycle)
import Control.Arrow (left)
import Control.Concurrent.STM (
    STM,
    TQueue,
    TVar,
    atomically,
    readTVar,
    readTVarIO,
    retry,
    writeTQueue,
  )

import Control.Error (note)
import Control.Exception (throw)
import Control.Exception.Safe (Exception)
import Control.Exception.Safe (MonadMask, withException)
import Control.Exception.Safe (toException)
import Control.Lens (re)
import Control.Lens (review)
import Control.Lens.TH (makeLenses)
import Control.Monad (ap)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (ExitCase (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Catch (catch)
import Control.Monad.Catch (generalBracket, bracket)
import Control.Monad.Catch (mask)
import Control.Monad.Catch (throwM)
import Control.Monad.Catch (uninterruptibleMask)
import Control.Monad.Error.Lens (catching)
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT, throwError)
import Control.Monad.Except (catchError)
import Control.Monad.Except (liftEither)
import Control.Monad.Logger (MonadLogger, logDebug, logDebugSH, logWarnSH)
import Control.Monad.Logger (monadLoggerLog)
import Control.Monad.Reader (local)
import Control.Monad.Reader (reader)
import qualified Control.Monad.State as S
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Encoding (emptyObject_)
import Data.Bifunctor (bimap, first)
import Data.Aeson.GADT (deriveJSONGADT)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Either (partitionEithers)
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.LCA.Online.Polymorphic as LCA
import Data.List (genericTake, sortOn)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing, Down(..))
import Data.Pool (Pool)
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.String.Here.Interpolated (i)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Vector as V
import Database.Id.Class
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple.LargeObjects as PG
import qualified Database.PostgreSQL.Simple as PG
import Named
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Method as Http (methodGet)
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (runDb, project1)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject, withLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, queryQ, executeQ)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Rhyolite.Schema (Json (..), LargeObjectId (..))
import Safe (headMay, minimumMay)
import Safe.Foldable (maximumByMay)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.NodeRPC
import Tezos.Types hiding (Block)
import Tezos.Unsafe (unsafeAssumptionRightsContextLevel)

import Backend.Common (timeout')
import Backend.Schema
import Backend.STM (HasTimestamp, MonadSTM (liftSTM), atomicallyWith, atomicallyWithTime,
                    newTVar', readTVar', retry', writeTVar')
import qualified Backend.STM as Stm
import Common (unixEpoch)
import Common.Schema
import ExtraPrelude


-- This exception should be impossible, but that depends on the node
-- working correctly.  The information inside is just the arguments of
-- the request you would have made to end up with it.
data NoRightsException = NoRightsException BlockHash RawLevel Priority
  deriving (Eq, Ord, Show, Typeable)

instance Exception NoRightsException

data NodeQuery a where
  NodeQuery_ProtocolConstants :: !BlockHash -> NodeQuery ProtoInfo
  NodeQuery_ProtocolIndex   :: !ProtocolHash -> NodeQuery ProtocolIndex
  NodeQuery_BakingRights    :: BlockHash -> RawLevel -> NodeQuery (Seq BakingRights)
  NodeQuery_EndorsingRights :: BlockHash -> RawLevel -> NodeQuery (Seq EndorsingRights)
  NodeQuery_Account         :: BlockHash -> ContractId -> NodeQuery AccountCrossCompat
  NodeQuery_Ballots         :: BlockHash -> NodeQuery Ballots
  NodeQuery_Ballot          :: BlockHash -> PublicKeyHash -> NodeQuery (Maybe Ballot)
  NodeQuery_ProposalVote    :: BlockHash -> PublicKeyHash -> NodeQuery (Set ProtocolHash)
  NodeQuery_Listings        :: BlockHash -> NodeQuery (Seq VoterDelegate)
  NodeQuery_Proposals       :: BlockHash -> NodeQuery (Seq ProposalVotes)
  NodeQuery_CurrentProposal :: BlockHash -> NodeQuery (Maybe ProtocolHash)
  NodeQuery_CurrentQuorum   :: BlockHash -> NodeQuery Int
  NodeQuery_Block           :: BlockHash -> NodeQuery BlockCrossCompat
  NodeQuery_BlockHeader     :: BlockHash -> NodeQuery BlockHeader
  NodeQuery_DelegateInfo    :: BlockHash -> RawLevel -> PublicKeyHash -> NodeQuery CacheDelegateInfo
  NodeQuery_PublicKey       :: ContractId -> NodeQuery PublicKey
deriving instance Show (NodeQuery a)
deriving instance Typeable (NodeQuery a)

data NodeQueryIx a where
  NodeQueryIx_BakingRights    :: BlockHash -> RawLevel -> NodeQueryIx (Seq BakingRights)
  NodeQueryIx_EndorsingRights :: BlockHash -> RawLevel -> NodeQueryIx (Seq EndorsingRights)
deriving instance Show (NodeQueryIx a)

toCacheDelegateInfo :: DelegateInfo -> CacheDelegateInfo
toCacheDelegateInfo di = CacheDelegateInfo
  { _cacheDelegateInfo_balance = _delegateInfo_balance di
  , _cacheDelegateInfo_frozenBalance = _delegateInfo_frozenBalance di
  , _cacheDelegateInfo_frozenBalanceByCycle = _delegateInfo_frozenBalanceByCycle di
  , _cacheDelegateInfo_stakingBalance = _delegateInfo_stakingBalance di
  -- , _cacheDelegateInfo_delegatedContracts = _delegateInfo_delegatedContracts di
  , _cacheDelegateInfo_delegatedBalance = _delegateInfo_delegatedBalance di
  , _cacheDelegateInfo_deactivated = _delegateInfo_deactivated di
  , _cacheDelegateInfo_gracePeriod = _delegateInfo_gracePeriod di
  }



data CachedBlockInfo = CachedBlockInfo
  deriving (Eq, Ord, Show, Typeable)

type CachedHistory' = CachedHistory ()
type DirtyBit = Maybe (Id GenericCacheEntry)

data CacheLine a where
  CacheLine :: (ToJSON a, FromJSON a) =>
    { _cacheLine_value :: !a
    , _cacheLine_used :: !UTCTime
    , _cacheLine_dirty :: !DirtyBit -- is this entry already in the database?
    } -> CacheLine a

data NodeDataSource = NodeDataSource
  { _nodeDataSource_history :: !(TVar CachedHistory')
  , _nodeDataSource_cache :: !(TVar (DMap NodeQuery (Compose TVar CacheLine)))
  , _nodeDataSource_chain :: !ChainId
  , _nodeDataSource_httpMgr :: !Http.Manager
  , _nodeDataSource_pool :: !(Pool Postgresql)
  , _nodeDataSource_latestHead :: !(TVar (Maybe VeryBlockLike))
  , _nodeDataSource_logger :: !LoggingEnv
  , _nodeDataSource_ioQueue :: !(TQueue (IO ()))
  , _nodeDataSource_osPublicNode :: !(Maybe URI)
  , _nodeDataSource_kilnNodeUri :: !URI
  , _nodeDataSource_nodeForQuery :: !(Maybe URI) -- Override the node selection algo, and do RPC using this node
  } deriving (Typeable, Generic)
makeLenses 'NodeDataSource

class HasNodeDataSource a where
  nodeDataSource :: Lens' a NodeDataSource

instance HasNodeDataSource NodeDataSource where
  nodeDataSource = id

class MonadLogger m => MonadNodeQuery m where
  data AnswerM m :: * -> *
  asksNodeDataSource :: (NodeDataSource -> a) -> m a
  nqThrowError :: CacheError -> m a
  nqCatchError :: m a -> (CacheError -> m a) -> m a
  nqInDB :: (forall n. (MonadLogger n, PostgresRaw n) => n a) -> m a
  nqAtomically :: STM a -> m a
  default nqAtomically :: MonadIO m => STM a -> m a
  nqAtomically action = liftIO $ atomically action
  default nqAtomicallyWithTime :: MonadIO m => ReaderT UTCTime STM a -> m a
  nqAtomicallyWithTime :: ReaderT UTCTime STM a -> m a
  nqAtomicallyWithTime action = liftIO $ atomicallyWithTime action
  answerImmediate :: ReaderT UTCTime STM (Maybe (Either CacheError a)) -> STM (m (AnswerM m a))
  withFinishWith :: NodeDataSource -> (forall r. (Either CacheError a -> STM r) -> STM (m r)) -> STM (m (AnswerM m a))
  nodeRPCOrBust :: (ToJSON a, FromJSON a) => BlockHash -> NodeQuery a -> m a

askNodeDataSource :: MonadNodeQuery m => m NodeDataSource
askNodeDataSource = asksNodeDataSource id

nqLiftEither :: (MonadNodeQuery m) => Either CacheError a -> m a
nqLiftEither = \case
  Left e -> nqThrowError e
  Right v -> pure v

nqTry :: (MonadNodeQuery m) => m a -> m (Either CacheError a)
nqTry action = (Right <$> action) `nqCatchError` (pure . Left)

newtype NodeQueryQueued a = NodeQueryQueued { unNodeQueryQueued :: ExceptT CacheError (ReaderT NodeDataSource IO) a }

runNodeQueryQueued :: (MonadReader s m, HasNodeDataSource s, MonadError e m, AsCacheError e, MonadIO m) => NodeQueryQueued a -> m a
runNodeQueryQueued action = do
  nds <- view nodeDataSource
  (liftEither =<<) $ fmap (left (review asCacheError)) $ liftIO $ flip runReaderT nds $ runExceptT $ unNodeQueryQueued action

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
  data AnswerM NodeQueryQueued a = NodeQueryQueuedAnswerM { unNodeQueryQueuedAnswerM :: ReaderT UTCTime STM (Maybe (Either CacheError a)) }
  asksNodeDataSource = NodeQueryQueued . asks
  nqThrowError = NodeQueryQueued . throwError
  nqCatchError action handler = NodeQueryQueued $ catchError (unNodeQueryQueued action) (unNodeQueryQueued . handler)
  nqInDB action = do
    db <- asksNodeDataSource _nodeDataSource_pool
    logger <- asksNodeDataSource _nodeDataSource_logger
    NodeQueryQueued $ lift @(ExceptT CacheError) $ runLoggingEnv logger $ runDb (Identity db) action
  answerImmediate = return . return . NodeQueryQueuedAnswerM
  withFinishWith nds cb = do
    -- A separate TVar for keeping the actual API result (outside the cache structure)
    apiResultVar :: TVar (Maybe (Either CacheError a)) <- newTVar' Nothing
    action <- cb $ writeTVar' apiResultVar . Just
    let ioQueue = _nodeDataSource_ioQueue nds
    liftSTM $ writeTQueue ioQueue $ void $ flip runReaderT nds $ runExceptT $ unNodeQueryQueued action
    return $ return $ NodeQueryQueuedAnswerM $ readTVar' apiResultVar

  -- Here we examine the internal & external (but not public) nodes before doing the query
  -- We keep hold of our candidate nodes right till the end in case we exhaust all of our options
  -- and need to give everything that we tried and what went wrong to the user in a CacheError_NoSuitableNode
  -- error.
  nodeRPCOrBust qBranch q = do
    $(logDebug) [i|nodeRPCOrBust@NodeQueryQueued: Branch ${qBranch}: ${tshow q}|]
    dsrc <- askNodeDataSource
    (mNodesToTry, badCandidates) <- case _nodeDataSource_nodeForQuery dsrc of
      -- If we have the nodeForQuery override set, just push it through assuming that the overrider is responsible for
      -- making sure that it's good and don't load any other candidates.
      Just n -> pure ([n], [])
      Nothing -> do
        nodes <- nqInDB $ getActiveNodeDetails $ _nodeDataSource_kilnNodeUri dsrc
        NodeQueryQueued $ atomicallyWith $ do
          nodes' <- validNodes nodes q
          let (badCandidates', okCandidates) = partitionEithers $ (\(u,e) -> bimap (u,) (u,) e) <$> nodes'
          -- pickNodes will filter out any nodes where our query branch isn't on that node
          (okNodes, branchedNodes) <- pickNodes qBranch okCandidates
          -- So we want to combine the nodes ignored because of pickNodes and those ignored by validNodes
          pure (okNodes, badCandidates' <> branchedNodes)

    let

    result <- case mNodesToTry of
      -- The public node is the last resort
      [] -> case _nodeDataSource_osPublicNode dsrc of
        Nothing -> pure $ Left $ CacheError_NoSuitableNode (tshow q) badCandidates
        Just uri ->
          let
            ctx = NodeRPCContext (_nodeDataSource_httpMgr dsrc) (Uri.render uri)
          -- TODO: If the public node fails, we lose all history of the nodes that we found unsuitable. Probably bad.
          in NodeQueryQueued $ liftIO $ nodeQueryOsPubNodeImpl (_nodeDataSource_chain dsrc) qBranch ctx (_nodeDataSource_logger dsrc) q
      -- But if we have candidate nodes, try them until we succeed
      -- TODO: Why isn't there a public node call as the last attempt here?
      nodesToTry -> do
        res <- foldM `flip` Left [] `flip` nodesToTry $ \case
            answer@(Right _) -> const $ pure answer -- short circuit if there is already an answer
            Left es -> \anyNode -> do
              let ctx = NodeRPCContext (_nodeDataSource_httpMgr dsrc) (Uri.render anyNode)
              r <- NodeQueryQueued $ liftIO $ nodeQueryDataSourceImpl (_nodeDataSource_chain dsrc) qBranch ctx (_nodeDataSource_logger dsrc) q
              pure $ first ((:es).(anyNode,)) r
        pure $ first (CacheError_NoSuitableNode (tshow q) . fmap (second (UnsuitableNodeReason_QueryFailed . tshow))) res

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
  answerImmediate getResult = return $ fmap NodeQueryImmediateAnswerM $ (nqLiftEither =<<) $ nqAtomicallyWithTime (getResult >>= maybe retry' return)
  withFinishWith _ cb = (fmap NodeQueryImmediateAnswerM . nqLiftEither =<<) <$> cb return
  nodeRPCOrBust h q = NodeQueryImmediate $ nodeRPCOrBust h q

data NodeQueryTResult a where
  NodeQueryTResult_Done :: a -> NodeQueryTResult a
  NodeQueryTResult_Query :: forall a b. (ToJSON a, FromJSON a) => BlockHash -> NodeQuery a -> NodeQueryTResult b

deriving instance Functor NodeQueryTResult

newtype NodeQueryT m a = NodeQueryT { unNodeQueryT :: DMap NodeQuery (Const (Map BlockHash CacheError)) -> m (NodeQueryTResult a) }

instance (MonadIO m, MonadReader s m, HasNodeDataSource s, MonadError e m, AsCacheError e, PostgresRaw m) => MonadNodeQuery (NodeQueryT m) where
  newtype AnswerM (NodeQueryT m) a = NodeQueryTAnswerM { unNodeQueryTAnswerM :: a }
  asksNodeDataSource = lift . views nodeDataSource
  nqThrowError e = lift $ throwError $ e ^. re asCacheError
  nqCatchError action handler = NodeQueryT $ \bad -> catching asCacheError (unNodeQueryT action bad) (flip unNodeQueryT bad . handler)
  nqInDB = id
  answerImmediate getResult = return $ fmap NodeQueryTAnswerM $ (nqLiftEither =<<) $ nqAtomicallyWithTime (getResult >>= maybe retry' return)
  withFinishWith _ cb = (fmap NodeQueryTAnswerM . nqLiftEither =<<) <$> cb return
  nodeRPCOrBust h q = NodeQueryT $ \bad ->
    case DMap.lookup q bad >>= pure . getConst >>= Map.lookup h of
      Just e -> throwError $ e ^. re asCacheError
      Nothing -> pure $ NodeQueryTResult_Query h q

instance (MonadIO m, MonadNodeQuery (NodeQueryT m)) => MonadLogger (NodeQueryT m) where
  monadLoggerLog a b c d = do
    logger <- asksNodeDataSource _nodeDataSource_logger
    runLoggingEnv logger $ monadLoggerLog a b c d

instance Monad m => Monad (NodeQueryT m) where
  return = NodeQueryT . const . return . NodeQueryTResult_Done
  (NodeQueryT x) >>= f = NodeQueryT $ \bad -> x bad >>= \case
    NodeQueryTResult_Done v -> unNodeQueryT (f v) bad
    NodeQueryTResult_Query h q -> pure $ NodeQueryTResult_Query h q

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
        NodeQueryTResult_Query h q -> const $ return $ NodeQueryTResult_Query h q -- query during acquire, nothing to release
        NodeQueryTResult_Done resource -> \case
          ExitCaseSuccess (NodeQueryTResult_Done answer) -> flip unNodeQueryT bad $ release resource $ ExitCaseSuccess answer
          ExitCaseSuccess (NodeQueryTResult_Query _ _) -> flip unNodeQueryT bad $ release resource ExitCaseAbort
            -- because things need to actually happen in the release handler.  The query will still get passed through
            -- on another channel.
          ExitCaseException e -> flip unNodeQueryT bad $ release resource $ ExitCaseException e
          ExitCaseAbort -> flip unNodeQueryT bad $ release resource ExitCaseAbort)
      (\case
        NodeQueryTResult_Query h q -> return $ NodeQueryTResult_Query h q
        NodeQueryTResult_Done resource -> flip unNodeQueryT bad $ use resource)
    return $ case ranswer of
      NodeQueryTResult_Query h q -> NodeQueryTResult_Query h q
        -- let the query from 'use' win even if both are queries, both
        -- because it is first, and because 'release' will be called
        -- with different arguments on the final retry.
      NodeQueryTResult_Done answer -> case rreleased of
        NodeQueryTResult_Query h q -> NodeQueryTResult_Query h q
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
    , Show e
    )
  => NodeQueryT (ExceptT e (ReaderT NodeDataSource (DbPersist Postgresql m))) a -> ExceptT e m a
runNodeQueryT f = ExceptT @e $ go 0 DMap.empty
  where
    go :: Int -> DMap NodeQuery (Const (Map BlockHash CacheError)) -> m (Either e a)
    go n bad = do
      $(logDebug) [i|RPC monad attempt number ${n} starting: ${prettyCallStack callStack}|]
      tryNodeQueryTWithDb bad f >>= \case
        Left e -> do
          $(logDebug) [i|RPC monad attempt number ${n}: failed: ${tshow e}: ${prettyCallStack callStack}|]
          return $ Left e
        Right (NodeQueryTResult_Done v) -> do
          $(logDebug) [i|RPC monad attempt number ${n}: succeeded|]
          return $ Right v
        Right (NodeQueryTResult_Query h q) -> do
          $(logDebug) [i|RPC monad attempt number ${n}: retrying for query: ${tshow q}|]
          -- just get it into cache
          runExceptT (nodeQueryDataSource q) >>= \case
            Right _ -> go (n + 1) bad
            Left e -> go (n + 1) (bad <> DMap.singleton q (Const $ Map.singleton h e))

tryNodeQueryTWithDb
  :: forall a s e m.
    ( MonadIO m, MonadBaseNoPureAborts IO m
    , MonadReader s m, HasNodeDataSource s
    , MonadLogger m
    )
  => DMap NodeQuery (Const (Map BlockHash CacheError)) -> NodeQueryT (ExceptT e (ReaderT NodeDataSource (DbPersist Postgresql m))) a -> m (Either e (NodeQueryTResult a))
tryNodeQueryTWithDb bad f = do
  nds <- view nodeDataSource
  let db = _nodeDataSource_pool nds
      bail = DbPersist $ ReaderT $ \(Postgresql conn) -> liftIO $ PG.rollback conn *> PG.begin conn
  runDb (Identity db) $ runReaderT (runExceptT (unNodeQueryT f bad)) nds >>= \case
    e@(Left _) -> e <$ bail
    v@(Right (NodeQueryTResult_Done _)) -> return v
    q@(Right (NodeQueryTResult_Query _ _)) -> q <$ bail


tryNodeQueryT :: Functor m => NodeQueryT m a -> m (Maybe a)
tryNodeQueryT f = do
  unNodeQueryT f DMap.empty <&> \case
    NodeQueryTResult_Done a -> Just a
    NodeQueryTResult_Query{} -> Nothing

unpackCacheResult
  :: forall a r m. (MonadSTM m, MonadReader r m, HasTimestamp r)
  => Compose TVar CacheLine a -> m a
unpackCacheResult (Compose var) = do
  result <- readTVar' var
  now <- asks (^. Stm.timestamp)
  when (_cacheLine_used result < now) $
    writeTVar' var $ result{_cacheLine_used = now}
  pure $ _cacheLine_value result

-- get lca between two blocks
branchPoint
  :: forall r m. (HasNodeDataSource r, MonadSTM m, MonadReader r m)
  => BlockHash -> BlockHash -> m (Maybe VeryBlockLike)
branchPoint x y =
  fmap (branchPointPure x y) $ readTVar' =<< asks (^. nodeDataSource . nodeDataSource_history)

branchPointPure :: BlockHash -> BlockHash -> CachedHistory' -> Maybe VeryBlockLike
branchPointPure x y history =
  let
    xPath = Map.lookup x $ _cachedHistory_blocks history
    yPath = Map.lookup y $ _cachedHistory_blocks history
  in liftA2 LCA.lca xPath yPath >>= \v -> case LCA.view v of
      LCA.Root -> Nothing
      LCA.Node blockHash () path -> Just $ histToBlockLike (_cachedHistory_minLevel history) blockHash path


-- | enumerate the block hashes between lca(x, y) and (x,y), respectively, from newest to oldest
enumerateBranches
  :: ( MonadSTM m
     , MonadReader a m, HasNodeDataSource a
     )
  => BlockHash -> BlockHash -> m (Maybe ([BlockHash], [BlockHash]))
enumerateBranches x y = do
  dsrc <- asks (^. nodeDataSource)
  history <- readTVar' $ _nodeDataSource_history dsrc
  pure $ do
    xPath <- Map.lookup x $ _cachedHistory_blocks history
    yPath <- Map.lookup y $ _cachedHistory_blocks history
    let pathPrefix long = fmap fst $ take (LCA.length long - LCA.length (LCA.lca xPath yPath)) $ LCA.toList long
    pure (pathPrefix xPath, pathPrefix yPath)


lookupBlock
  :: forall nds m. (HasNodeDataSource nds, MonadSTM m)
  => nds -> BlockHash -> m (Maybe VeryBlockLike)
lookupBlock nds x = do
  let dsrc = nds ^. nodeDataSource
  history <- readTVar' $ _nodeDataSource_history dsrc
  let xPath = Map.lookup x $ _cachedHistory_blocks history
  pure $ xPath >>= \v -> case LCA.view v of
    LCA.Root -> Nothing
    LCA.Node blockHash () path -> Just $ histToBlockLike (_cachedHistory_minLevel history) blockHash path

{-

withNDSLogging :: (MonadReader r m, HasNodeDataSource r) => LoggingT m a -> m a
withNDSLogging x = flip runLoggingEnv x . _nodeDataSource_logger =<< asks (^. nodeDataSource)

-}

fittestHead
  :: (HasNodeDataSource nds, MonadReader nds m, MonadSTM m)
  => m (Maybe (WithProtocolHash VeryBlockLike))
fittestHead = do
  hist <- asks (^. nodeDataSource . nodeDataSource_history)
  fittestBranchInHistory <$> readTVar' hist

-- | Blocks until a new head is seen or the time between blocks has elapsed.
waitForNewHeadWithTimeout :: (HasNodeDataSource nds) => nds -> IO ()
waitForNewHeadWithTimeout nds = do
  -- First wait at most 'defaultTimeLimit' to get the fittest head.
  headBlock' <- timeout' defaultTimeLimit $ atomically $ maybe retry' pure =<< runReaderT fittestHead nds
  case headBlock' of
    Nothing -> pure () -- We've already waited for a while so return immediately.
    Just headBlock -> do
      params' <- runLoggingEnv (nds ^. nodeDataSource . nodeDataSource_logger) $ flip runReaderT (nds ^. nodeDataSource) $ runExceptT @CacheError $ runNodeQueryT $
        getProtocolConstants $ Right $ headBlock ^. protocolHash
      let timeLimit = either (const defaultTimeLimit) calcTimeBetweenBlocks params'
      void $ timeout' timeLimit $ waitForNewHead nds
  where
    defaultTimeLimit = 60

-- | Blocks until a new head is seen.
--
-- Returns most recently seen head.
waitForNewHead :: (HasNodeDataSource nds) => nds -> IO VeryBlockLike
waitForNewHead nds = do
  history <- readTVarIO $ nds ^. nodeDataSource . nodeDataSource_history
  let minLevel =  _cachedHistory_minLevel history
  oldHead <- readTVarIO $ nds ^. nodeDataSource . nodeDataSource_latestHead

  atomically $ do
    newHead <- maybe retry pure =<< readTVar (nds ^. nodeDataSource . nodeDataSource_latestHead)
    when (oldHead == Just newHead || newHead ^. level <= minLevel) retry
    pure newHead

-- turn the result of an LCA.view on the block history into a VeryBlockLike
histToBlockLike :: RawLevel -> BlockHash -> LCA.Path BlockHash () -> VeryBlockLike
histToBlockLike minLevel h path = VeryBlockLike h p mempty blkLevel unixEpoch
  where
    blkLevel = minLevel + fromIntegral (length path)
    p = maybe h (\(pp, _, _) -> pp) $ LCA.uncons path

fittestBranchInHistory :: CachedHistory a -> Maybe (WithProtocolHash VeryBlockLike)
fittestBranchInHistory hist =
  maximumByMay (comparing $ view fitness) (Map.elems $ _cachedHistory_branches hist)

-- | extracts the fittest known branch from cache
dataSourceHead
  :: forall nds m. (HasNodeDataSource nds, MonadSTM m)
  => nds -> m (Maybe (WithProtocolHash VeryBlockLike))
dataSourceHead nds =
  fittestBranchInHistory <$> readTVar' (nds ^. nodeDataSource . nodeDataSource_history)

{-
-- | extracts the fittest known node from cache
dataSourceNode
  :: forall nds m. (HasNodeDataSource nds, MonadSTM m)
  => nds -> m (Maybe NodeRPCContext)
dataSourceNode nds = do
  let dsrc = nds ^. nodeDataSource
  nodes <- readTVar' $ _nodeDataSource_nodes dsrc
  pure $ fmap (NodeRPCContext (_nodeDataSource_httpMgr dsrc) . Uri.render . fst) $
    maximumByMay (compare `on` snd) $ mapMaybe sequence $ Map.toList nodes
-}

levelAncestor :: CachedHistory' -> RawLevel -> BlockHash -> Maybe BlockHash
levelAncestor hist lvl ctx = fmap (view _1) $ LCA.uncons =<< LCA.keep (fromIntegral $ lvl - minLevel + 1) <$> branch
  where
    minLevel = _cachedHistory_minLevel hist
    branch = Map.lookup ctx $ _cachedHistory_blocks hist

-- | We want the first block in the cycle that sits PRESERVED_CYCLES before the
-- requested level, that is on the correct branch.
rightsContext :: ProtoInfo -> CachedHistory' -> BlockHash -> RawLevel -> (RawLevel, Maybe BlockHash)
rightsContext params hist ctx lvl = (ctxLvl, levelAncestor hist ctxLvl ctx)
  where ctxLvl = unsafeAssumptionRightsContextLevel params lvl

-- | Round the second argument to the next lower multiple of the first
floorBy :: Integral a => a -> a -> a
floorBy k n = n - n `mod` k

priorityChunkSize :: Num a => a
priorityChunkSize = 64

getContext :: forall m a. (MonadNodeQuery m) => NodeQuery a -> m BlockHash
getContext = \case
  NodeQuery_ProtocolConstants ctx -> pure ctx
  NodeQuery_ProtocolIndex _ctx -> getFittestBranch
  NodeQuery_BakingRights ctx _lvl -> pure ctx
  NodeQuery_EndorsingRights ctx _lvl -> pure ctx
  NodeQuery_Block ctx -> pure ctx
  NodeQuery_BlockHeader ctx -> pure ctx
  NodeQuery_Account ctx _contractId -> pure ctx
  NodeQuery_Ballots ctx -> pure ctx
  NodeQuery_Ballot ctx _pkh -> pure ctx
  NodeQuery_ProposalVote ctx _pkh -> pure ctx
  NodeQuery_Listings ctx -> pure ctx
  NodeQuery_Proposals ctx -> pure ctx
  NodeQuery_CurrentProposal ctx -> pure ctx
  NodeQuery_CurrentQuorum ctx -> pure ctx
  NodeQuery_DelegateInfo ctx _lvl _pkh -> pure ctx
  NodeQuery_PublicKey _ -> getFittestBranch

  where
    getFittestBranch :: m BlockHash
    getFittestBranch = do
      histVar <- asksNodeDataSource _nodeDataSource_history
      hist <- nqAtomically $ readTVar' histVar
      let branches = _cachedHistory_branches hist
          mHash = view hash <$> maximumByMay (comparing $ view fitness) (Map.elems branches)
      maybe (nqThrowError CacheError_NotEnoughHistory) pure mHash

-- | Caching query function simplified by blocking until we get a result.
nodeQueryDataSource
  :: forall a s e m.
    ( MonadIO m
    , MonadReader s m, HasNodeDataSource s
    , MonadError e m, AsCacheError e
    , FromJSON a, ToJSON a
    )
  => NodeQuery a -> m a
nodeQueryDataSource q = do
  view (nodeDataSource . nodeDataSource_logger) >>=
    flip runLoggingEnv ($(logDebug) [i|nodeQueryDataSource: ${tshow q}|])
  NodeQueryQueuedAnswerM getResult <- runNodeQueryQueued $ nodeQueryDataSourceRaw q
  now <- liftIO getCurrentTime
  timeout' timeoutSeconds (atomically $ maybe retry pure =<< runReaderT getResult now) >>= \case
    Nothing -> throwError $ CacheError_Timeout timeoutSeconds ^. re asCacheError
    Just (Left e) -> throwError $ e ^. re asCacheError
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
    , ToJSON (NodeQuery a)
    , FromJSON a, ToJSON a
    )
  => NodeQuery a -> NodeQueryT m a
nodeQueryDataSourceSafe q = unNodeQueryTAnswerM <$> nodeQueryDataSourceRaw q

-- | Query cached data immediately, in this thread.  Only meant to be used in the implementation
--   of recursive queries, lest the dreaded deadlock heisenbunny return.
nodeQueryDataSourceImmediate
  :: forall a s e m.
    ( MonadIO m
    , MonadReader s m, HasNodeDataSource s
    , MonadError e m, AsCacheError e
    , ToJSON (NodeQuery a)
    , FromJSON a, ToJSON a
    )
  => NodeQuery a -> m a
nodeQueryDataSourceImmediate q = runNodeQueryQueued $
  unNodeQueryImmediate $ unNodeQueryImmediateAnswerM <$> nodeQueryDataSourceRaw q

nodeQueryDataSourceRaw
  :: forall m a.
    ( MonadNodeQuery m
    , MonadMask m
    , ToJSON (NodeQuery a)
    , FromJSON a, ToJSON a
    )
  => NodeQuery a -> m (AnswerM m a)
nodeQueryDataSourceRaw q = do
  $(logDebug) [i|nodeQueryDataSourceRaw: ${tshow q}|]
  dsrc <- asksNodeDataSource id
  qBranch <- getContext q
  view _2 <=< nqAtomically $ nodeQueryDataSourceSTM dsrc qBranch q

-- | Core primitive for running a 'NodeQuery' against the cache / worker queue.
-- Returns the raw cache value (if found) and an action that will wait on the cache
-- regardless of whether it was found or required a new request to be queued.
nodeQueryDataSourceSTM
  :: forall n a m nds. (HasNodeDataSource nds, MonadSTM m, MonadNodeQuery n, MonadMask n, ToJSON (NodeQuery a), FromJSON a, ToJSON a)
  => nds -> BlockHash -> NodeQuery a -> m (Maybe (Compose TVar CacheLine a), n (AnswerM n a))
nodeQueryDataSourceSTM nds qBranch q = do
  cache <- readTVar' cacheVar
  liftSTM $ case DMap.lookup q cache of
    -- Cache Hit: Return an STM that reads the cache and updates the "access" timestamp
    Just avar -> fmap (Just avar,) $ answerImmediate $ Just . Right <$> unpackCacheResult avar

    -- Cache Miss: Queue the IO action to collect data and return an STM that reads the result.
    Nothing -> fmap (Nothing,) $ withFinishWith @n dsrc $ \finishWith -> do
      let
        -- Updates the cache key if the result is useful and communicates the result upstream.
        -- XXX Can't actually use this type signature since 'r' is not in scope...
        -- writeResult :: Either CacheError (a, DirtyBit) -> m r
        writeResult a' = nqAtomicallyWithTime $ do
          case a' of
            Right (a, dirty) -> populateKey q a dirty
            Left _ -> pure ()
          lift $ finishWith $ fmap fst a'

      return $
        -- Try very hard to write *something* into the result TVar in case of exception.
        -- The catch handles synchronous/recoverable errors, and its result passes through,
        -- which is necessary in the immediate case and harmless in the worker queue case.
        -- The withException handles asynchronous/unrecoverable errors.  In the case of a
        -- worker queue, the calling thread can still recover because it's a different
        -- thread.  The result is thrown away meaning in the immediate case the caller
        -- cannot recover, but this is fine because that's what is supposed to happen for
        -- such an error.
        (writeResult =<< makeRequestAndCache)
          `catch` \e ->
            nqAtomically (finishWith $ Left $ CacheError_SomeException e)
          `withException` \x ->
            nqAtomically (finishWith $ Left $ CacheError_SomeException x)

  where
    dsrc = nds ^. nodeDataSource
    cacheVar = _nodeDataSource_cache dsrc
    chainId = _nodeDataSource_chain dsrc

    populateKey q_ a dirty = do
      cache <- readTVar' cacheVar
      case DMap.lookup q_ cache of
        Just _ -> pure ()
        Nothing -> do
          now <- asks (^. Stm.timestamp)
          var <- newTVar' $ CacheLine a now dirty
          writeTVar' cacheVar $ DMap.insert q_ (Compose var) cache

    makeRequestAndCache :: n (Either CacheError (a, DirtyBit))
    makeRequestAndCache = nqTry $
      tryFetchFromCache chainId q >>= \case
        Just x -> pure $ fmap Just x :: n (a, DirtyBit)
        Nothing -> (,Nothing) <$> nodeRPCOrBust qBranch q :: n (a, DirtyBit)

unliftEither :: MonadError e m => m a -> m (Either e a)
unliftEither action = (Right <$> action) `catchError` (pure . Left)

-- Check the level of the query and determine the nodes which could service the queries
validNodes
  :: forall r m a . (HasNodeDataSource r, MonadSTM m, MonadReader r m)
  => [(URI, Maybe VeryBlockLike, Maybe RawLevel)]
  -> NodeQuery a
  -> m [(URI, Either UnsuitableNodeReason VeryBlockLike)]
validNodes nodes q = case q of
  NodeQuery_ProtocolConstants ctx -> findNodes <$> getLvl ctx
  NodeQuery_ProtocolIndex _ctx -> pure $ (\(u,_,_) -> (u, Left UnsuitableNodeReason_ProtocolIndex)) <$> nodes -- only OS public node can do this query
  NodeQuery_BakingRights _ctx lvl -> pure $ findNodes $ Just lvl
  NodeQuery_EndorsingRights _ctx lvl -> pure $ findNodes $ Just lvl
  NodeQuery_Block ctx -> findNodes <$> getLvl ctx
  NodeQuery_BlockHeader _ctx -> pure $ findNodes Nothing
  NodeQuery_Account ctx _contractId -> findNodes <$> getLvl ctx
  NodeQuery_Ballots ctx -> findNodes <$> getLvl ctx
  NodeQuery_Ballot ctx _pkh -> findNodes <$> getLvl ctx
  NodeQuery_ProposalVote ctx _pkh -> findNodes <$> getLvl ctx
  NodeQuery_Listings ctx -> findNodes <$> getLvl ctx
  NodeQuery_Proposals ctx -> findNodes <$> getLvl ctx
  NodeQuery_CurrentProposal ctx -> findNodes <$> getLvl ctx
  NodeQuery_CurrentQuorum ctx -> findNodes <$> getLvl ctx
  NodeQuery_DelegateInfo _ctx lvl _pkh -> pure $ findNodes $ Just lvl
  NodeQuery_PublicKey _ -> pure $ findNodes Nothing
  where
    getLvl :: BlockHash -> m (Maybe RawLevel)
    getLvl ctx = do
      dsrc <- asks (^. nodeDataSource)
      fmap (view level) <$> lookupBlock dsrc ctx

    findNodes :: Maybe RawLevel -> [(URI, Either UnsuitableNodeReason VeryBlockLike)]
    findNodes mLvl = do
      case mLvl of
        Nothing ->
          (\(nUri, mBlk, _) -> (nUri,note UnsuitableNodeReason_MissingBlockInfo mBlk)) <$> nodes
        Just lvl -> (\(nUri, mBlk, mSp) -> (nUri, suitableNodeBlock mBlk mSp)) <$> nodes
          where
            suitableNodeBlock mBlk mSp =
              -- If our node has a save point, check that the query that we are doing is not
              -- for a level prior to this savepoint.
              note UnsuitableNodeReason_MissingSavePoint mSp >>= \savepoint ->
                if savepoint <= lvl
                  then note UnsuitableNodeReason_MissingBlockInfo mBlk
                  else Left $ UnsuitableNodeReason_QueryBeforeSavepoint savepoint lvl

-- Sort candidate nodes into suitable and unsuitable buckets based on whether the supplied branch
-- is contained within the node.
pickNodes
  :: (HasNodeDataSource r, MonadSTM m, MonadReader r m)
  => BlockHash -> [(URI, VeryBlockLike)] -> m ([URI],[(URI, UnsuitableNodeReason)])
pickNodes branch =
  fmap partitionEithers
  . traverse (\(nodeUri, nodeHead) ->
    bool
      (Right (nodeUri, UnsuitableNodeReason_BranchNotContained branch))
      (Left (nodeUri))
      <$> containsBranch nodeHead)
  where
    containsBranch nodeHead = (Just branch ==) . (^? _Just . hash) <$> branchPoint (nodeHead ^. hash) branch

nodeQueryDataSourceImpl
  :: forall a.
     ChainId
  -> BlockHash
  -> NodeRPCContext
  -> LoggingEnv
  -> NodeQuery a
  -> IO (Either CacheError a)
nodeQueryDataSourceImpl = nodeQueryImpl nodeRPC ChainTag_Hash

nodeQueryImpl
  :: forall a chain repr.
   ( QueryBlock repr, QueryHistory repr, QueryProtocolIndex repr, BlockType repr ~ BlockCrossCompat, BlockHeaderType repr ~ BlockHeader, ChainType repr ~ chain)
  => (forall c m s e.
       ( MonadIO m, MonadLogger m, MonadReader s m , HasNodeRPC s, MonadError e m , AsRpcError e, Aeson.FromJSON c)
     => repr c -> m c)
  -> (ChainId -> chain)
  -> ChainId
  -> BlockHash
  -> NodeRPCContext
  -> LoggingEnv
  -> NodeQuery a
  -> IO (Either CacheError a)
nodeQueryImpl doNodeRPC toChain chainId qBranch ctx logger q = runExceptT $ runLoggingEnv logger ( $(logDebugSH) ("nodeQueryImpl called" :: Text,q)) *> case q of
  NodeQuery_ProtocolConstants branch -> nodeRPC' $ rProtoConstants chainId branch
  NodeQuery_ProtocolIndex protoHash -> nodeRPC' $ rProtocolIndex chainId protoHash
  NodeQuery_BakingRights branch targetLevel ->
    nodeRPC' $ rBakingRightsFull (Set.singleton $ Left targetLevel) priorityChunkSize chainId branch
  NodeQuery_EndorsingRights branch targetLevel ->
    nodeRPC' $ rEndorsingRights (Set.singleton $ Left targetLevel) chainId branch
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
  NodeQuery_DelegateInfo branch _lvl pkh -> fmap toCacheDelegateInfo $ nodeRPC' $ rDelegateInfo pkh chainId branch
  NodeQuery_PublicKey contractId -> do
    managerkeyResp <- nodeRPC' $ rManagerKey contractId chainId qBranch
    case view managerKey_key managerkeyResp of
      Nothing -> throwError $ CacheError_UnrevealedPublicKey contractId
      Just pk -> pure pk
  where
    nodeRPC' :: forall c. Aeson.FromJSON c => repr c -> ExceptT CacheError IO c
    nodeRPC' q' = runReaderT (runLoggingEnv logger $ doNodeRPC q') ctx
    {-# INLINE nodeRPC' #-}

nodeQueryOsPubNodeImpl
  :: forall a.
     ChainId
  -> BlockHash
  -> NodeRPCContext
  -> LoggingEnv
  -> NodeQuery a
  -> IO (Either CacheError a)
nodeQueryOsPubNodeImpl = nodeQueryImpl osPublicNodeRPC id

osPublicNodeRPC
  :: (MonadIO m, MonadLogger m, MonadReader s m , HasNodeRPC s, MonadError e m , AsRpcError e, Aeson.FromJSON a)
  => OsNodeQuery a -> m a
osPublicNodeRPC (OsNodeQuery route params) = nodeRPCImpl' Aeson.eitherDecode emptyObject_ Http.methodGet rpcSelector
  where
    rpcSelector = route <> paramsE
    paramsE = maybe "" (("?" <>) . mconcat . NE.toList . NE.intersperse "&" . fmap (\(k, v) -> k <> "=" <> v)) (nonEmpty params)

data OsNodeQuery a = OsNodeQuery
  { _osNodeQuery_route :: Text
  , _osNodeQuery_params :: [(Text, Text)]
  }

class QueryProtocolIndex (repr :: * -> *) where
  rProtocolIndex :: ChainId -> ProtocolHash -> repr ProtocolIndex

instance QueryProtocolIndex OsNodeQuery where
  rProtocolIndex = chainApi2 "/protocol-index" $ \protocol ->
    [("protocol", toBase58Text protocol)]

instance QueryProtocolIndex RpcQuery where
  rProtocolIndex = error "rProtocolIndex not possible for RpcQuery"

type instance ChainType OsNodeQuery = ChainId

instance QueryChain OsNodeQuery where
  rChain = OsNodeQuery "/v3/chain" []

instance QueryBlock OsNodeQuery where
  type BlockType OsNodeQuery = BlockCrossCompat
  type BlockHeaderType OsNodeQuery = BlockHeader
  rHead = chainApi1 "/head"
  rBlock = chainApi2 "/block-full" $ \h -> [("hash", toBase58Text h)]
  rBlockHeader = chainApi2 "/block-header" $ \h -> [("hash", toBase58Text h)]

instance QueryHistory OsNodeQuery where
  rBlocks = error "rBlocks NYI for OsNodeQuery"
  rBlockPred = error "rBlockPred NYI for OsNodeQuery"
  rProtoConstants = error "rProtoConstants NYI for OsNodeQuery"
  rBakingRights = error "rBakingRights NYI, use rBakingRightsFull"
  rRunOperation = error "rRunOperation for OsNodeQuery"

  rBallots = blockApi1 "/ballots"
  rContract contractId = case contractId of
    Implicit pkh -> chainApi2 "/account" $ \block ->
      [("block", toBase58Text block), ("pkh", toPublicKeyHashText pkh)]
    _ -> error "rContract only support Implicit"
  rListings = blockApi1 "/listings"
  rProposals = blockApi1 "/proposals"
  rCurrentProposal = blockApi1 "/current-proposal"
  rCurrentQuorum = blockApi1 "/current-quorum"
  rBallot = chainApi3 "/ballot" $ \block pkh ->
    [("block", toBase58Text block), ("pkh", toPublicKeyHashText pkh)]
  rProposalVote = chainApi3 "/proposal-vote" $ \block pkh ->
    [("block", toBase58Text block), ("pkh", toPublicKeyHashText pkh)]
  rManagerKey contractId = chainApi2 "/public-key" $ \_ ->
    [("contract-id", toContractIdText contractId)]

  rBakingRightsFull levelSet _ = chainApi2 "/baking-rights" (\branch ->
    [("branch", toBase58Text branch), ("level", tshow lvl)])
    where lvl = maybe (error "rBakingRights set empty")
            (either unRawLevel (error "rBakingRights cycle not handled")) $ headMay $ Set.toList levelSet
  rEndorsingRights levelSet = chainApi2 "/endorsing-rights" (\branch ->
    [("branch", toBase58Text branch), ("level", tshow lvl)])
    where lvl = maybe (error "rEndorsingRights set empty")
            (either unRawLevel (error "rEndorsingRights cycle not handled")) $ headMay $ Set.toList levelSet

  rDelegateInfo pkh = chainApi2 "/delegate-info" (\branch ->
    [("branch", toBase58Text branch), ("delegate", toPublicKeyHashText pkh)])

chainApi1 :: Text -> ChainId -> OsNodeQuery a
chainApi1 path chainId = chainApi2 path (const []) chainId ()

chainApi2 :: Text -> (b -> [(Text, Text)]) -> ChainId -> b -> OsNodeQuery a
chainApi2 path getParams chainId = chainApi3 path (const getParams) chainId ()

chainApi3 :: Text -> (b -> c  -> [(Text, Text)]) -> ChainId -> b -> c -> OsNodeQuery a
chainApi3 path getParams chainId b c = OsNodeQuery route (getParams b c)
  where route = "/v3/" <> toBase58Text chainId <> path

blockApi1 :: Text -> ChainId -> BlockHash -> OsNodeQuery a
blockApi1 path = chainApi2 path (\block -> [("block", toBase58Text block)])

nodeQueryIx
  :: forall a m.
    ( MonadNodeQuery (NodeQueryT m)
    , MonadMask m
    , PostgresRaw m
    , PersistBackend m
    , Aeson.FromJSON a, Aeson.ToJSON a
    )
  => NodeQueryIx a -> NodeQueryT m a
nodeQueryIx q = do
  $(logDebugSH) ("nodeQueryIx called" :: Text,q)
  dsrc <- askNodeDataSource
  protoInfo <- getProtocolConstants $ Left $ case q of
    NodeQueryIx_BakingRights ctx _lvl -> ctx
    NodeQueryIx_EndorsingRights ctx _lvl -> ctx
  hist <- do
      histVar <- asksNodeDataSource _nodeDataSource_history
      nqAtomically $ readTVar' histVar
  let
    getRightsContext ctx lvl = maybe (nqThrowError CacheError_NotEnoughHistory) pure mCtx
      where (_, mCtx) = rightsContext protoInfo hist ctx lvl
    getCheckpointContext ctx lvl = do
      let (ctxLvl, _) = rightsContext protoInfo hist ctx lvl
      nodes <- nqInDB $ getActiveNodeDetails $ _nodeDataSource_kilnNodeUri dsrc
      let
        fitNodes :: [(URI, Maybe VeryBlockLike, Maybe RawLevel)]
        fitNodes = filter (\v -> v ^? _2 . _Just . level >= Just ctxLvl) nodes
        mCtxCp = (\l -> levelAncestor hist l ctx) =<< fmap (max ctxLvl) (minimumMay $
          mapMaybe (view _3) fitNodes)
      pure $ fromMaybe ctx mCtxCp

  q1 <- modifyContext getRightsContext q
  mRes <- checkCacheDb q1
  case mRes of
    Just v -> pure v
    Nothing -> do
      q2 <- modifyContext getCheckpointContext q
      result <- nodeQueryDataSourceSafe $ getNodeQuery q2
      addToDb result q1
      pure result
  where
    modifyContext :: Functor f => (BlockHash -> RawLevel -> f BlockHash) -> NodeQueryIx a -> f (NodeQueryIx a)
    modifyContext f = \case
      NodeQueryIx_BakingRights ctx lvl -> (\ctx' -> NodeQueryIx_BakingRights ctx' lvl) <$> f ctx lvl
      NodeQueryIx_EndorsingRights ctx lvl -> (\ctx' -> NodeQueryIx_EndorsingRights ctx' lvl) <$> f ctx lvl

    getNodeQuery :: NodeQueryIx a -> NodeQuery a
    getNodeQuery = \case
      NodeQueryIx_BakingRights ctx lvl -> NodeQuery_BakingRights ctx lvl
      NodeQueryIx_EndorsingRights ctx lvl -> NodeQuery_EndorsingRights ctx lvl

    checkCacheDb
      :: ( Monad m1
      , PostgresRaw m1
      , MonadLogger m1)
      => NodeQueryIx a -> m1 (Maybe a)
    checkCacheDb = \case
      NodeQueryIx_BakingRights ctx lvl -> do
        res <- [queryQ|
          SELECT "result"
          FROM "CacheBakingRights"
          WHERE "context" = ?ctx AND "level" = ?lvl
        |] <&> stripOnly
        fmap join $ traverse getResult $ headMay res
      NodeQueryIx_EndorsingRights ctx lvl -> do
        res <- [queryQ|
          SELECT "result"
          FROM "CacheEndorsingRights"
          WHERE "context" = ?ctx AND "level" = ?lvl
        |] <&> stripOnly
        fmap join $ traverse getResult $ headMay res
      where
        getResult json = case Aeson.fromJSON (unJson json) of
          Aeson.Success v -> return $ Just v
          Aeson.Error bad -> do
            $(logWarnSH) $ "checkCacheDb failed to decode: " <> bad
            return Nothing

    addToDb :: (Monad m1, PostgresRaw m1) => a -> NodeQueryIx a -> m1 ()
    addToDb result' = \case
      NodeQueryIx_BakingRights ctx lvl -> void [executeQ|
        INSERT INTO "CacheBakingRights" ("context", "level", "result")
        values (?ctx, ?lvl, ?result)
      |]
      NodeQueryIx_EndorsingRights ctx lvl -> void [executeQ|
        INSERT INTO "CacheEndorsingRights" ("context", "level", "result")
        values (?ctx, ?lvl, ?result)
      |]
      where result = Json $ Aeson.toJSON result'


nodeQueryIxBakingRights1
  :: forall m.
    ( MonadNodeQuery (NodeQueryT m)
    , MonadMask m
    , PersistBackend m
    , PostgresRaw m
    )
  => BlockHash -> RawLevel -> Priority -> NodeQueryT m BakingRights
nodeQueryIxBakingRights1 ctx lvl prio = do
  allRights <- nodeQueryIx $ NodeQueryIx_BakingRights ctx lvl
  let
    chunked = fillChunk allRights

    fillChunk :: Seq BakingRights -> V.Vector BakingRights
    fillChunk = (makeBlanks V.//)
      . map (\x -> (fromIntegral $ _bakingRights_priority x - prio, x))
      . filter (\x -> _bakingRights_priority x >= prio)
      . toList

    makeBlanks :: V.Vector BakingRights
    makeBlanks = V.generate priorityChunkSize $ \i' ->
      throw $ NoRightsException ctx lvl $ prio + fromIntegral i'

  maybe (nqThrowError $ CacheError_SomeException $ toException $ NoRightsException ctx lvl prio) pure $ chunked V.!? fromIntegral (prio `mod` priorityChunkSize)

{-
calculateBakerStats ::
  ( TraversableWithIndex (PublicKeyHash, RawLevel) f
  , MonadReader r m, HasNodeDataSource r
  , MonadIO m
  )
  => f a
  -> m (f (First (Maybe (BakeEfficiency, Account)), a))
calculateBakerStats pkhs = do
  nds <- asks (^. nodeDataSource)
  liftIO (atomically (dataSourceHead nds)) >>= \case
    -- I think i should probably just ask for a `forall b. f b` to pass on the no heads case
    Nothing -> return $ fmap (First Nothing,) pkhs
    Just currentHead -> ifor pkhs $ \(pkh, lvl) a -> do
      result <- fmap (First . either (const Nothing) Just) $ runExceptT $ do
        efficiency <- calculateBakeEfficiency currentHead lvl pkh
        account <- nodeQueryDataSource $ NodeQuery_Account (currentHead ^. hash) (Implicit pkh)
        return (efficiency, account)
      return (result, a)


-}
-- produce (up to) n ancestor hashes (including the block itself)
ancestors ::
  ( MonadIO m
  , MonadReader s m , HasNodeDataSource s
  , MonadError CacheError m
  )
  => RawLevel -> BlockHash -> m [BlockHash]
ancestors (RawLevel n) branch = do
  hist <- liftIO . readTVarIO =<< asks (_nodeDataSource_history . view nodeDataSource)
  case Map.lookup branch (_cachedHistory_blocks hist) of
    Just branchPath -> return $ fmap fst $ genericTake n $ LCA.toList branchPath
    Nothing -> throwError $ RpcError_UnexpectedStatus 404 "NO BRANCH" ^. re asRpcError

{-
calculateBakeEfficiency ::
  ( MonadIO m
  , MonadReader s m , HasNodeDataSource s
  , MonadError CacheError m
  , BlockLike b
  )
  => b -> RawLevel -> PublicKeyHash -> m BakeEfficiency
calculateBakeEfficiency branch len baker = do
  withNDSLogging $ $(logDebugSH) ("bake efficiency requested" :: Text, branch ^. hash, len, baker)

  let
    branchLevel = branch ^. level
    branchHash = branch ^. hash
    levels = [branchLevel - len..branchLevel]
  branchHashes <- ancestors len branchHash

  rights <- (fmap.fmap) bakingRightsMap $ for levels $ nodeQueryDataSource . NodeQuery_BakingRights branchHash
  bakers <- for branchHashes $ fmap (^. block_metadata . blockMetadata_baker) . nodeQueryDataSource . NodeQuery_Block
  let result = fold $ efficiencyOfBlock <$> ZipList rights <*> ZipList bakers
  withNDSLogging $ $(logDebugSH) ("efficiency" :: Text, baker, result)
  return result
  where
    efficiencyOfBlock :: Map PublicKeyHash Priority -> PublicKeyHash -> BakeEfficiency
    efficiencyOfBlock rights blockBaker = BakeEfficiency
      { _bakeEfficiency_bakedBlocks = if blockBaker == baker then 1 else 0
      , _bakeEfficiency_bakingRights = case (Map.lookup blockBaker rights, Map.lookup baker rights) of
          (_, Nothing) -> 0
          (Just them, Just us) -> if us <= them then 1 else 0
          (Nothing, _) -> 0 -- error "Very wrong"
      }

    bakingRightsMap :: Foldable f => f BakingRights -> Map PublicKeyHash Priority -- map from baker to
    bakingRightsMap xs = Map.fromList
      [ (d, prio)
      | BakingRights _lvl d prio _ <- toList xs
      ]

-}
tryFetchFromCache
  :: forall m a. (MonadNodeQuery m, FromJSON a, ToJSON (NodeQuery a))
  => ChainId -> NodeQuery a -> m (Maybe (a, Id GenericCacheEntry))
tryFetchFromCache chainId q = do
  let
    qJson = Json $ Aeson.toJSON q
  -- although this is within the grasp of groundhog, this table is very hot,
  -- and the "IS NOT DISTINCT FROM" queries it generates are cataclysmically
  -- terrible:
  -- https://www.postgresql.org/message-id/17764.1405993868%40sss.pgh.pa.us
  resultM :: [(Id GenericCacheEntry, GenericCacheEntry)] <- nqInDB $ [queryQ|
    SELECT "id", "chainId", "key", "value"
    FROM "GenericCacheEntry"
    WHERE "chainId" = ?chainId
      AND "key" = ?qJson
    |] <&> fmap (\(id_, c, k, v) -> (id_, GenericCacheEntry c k v))
  case nonEmpty resultM of
    Nothing -> return Nothing
    Just ((rid, result) :| _) -> case Aeson.fromJSON (unJson $ _genericCacheEntry_value result) of
      Aeson.Success v -> return $ Just (v, rid)
      Aeson.Error bad -> do
        $(logWarnSH) $ "tryFetchFromCache failed to decode: " <> bad
        return Nothing

getActiveNodeDetails
  :: (MonadLogger m, PostgresRaw m) => URI -> m [(URI, Maybe VeryBlockLike, Maybe RawLevel)]
getActiveNodeDetails kilnNodeUri = do
  int <- let runningState = ProcessState_Running in [queryQ|
      SELECT d."data#headLevel"
           , d."data#headBlockHash"
           , d."data#headBlockPred"
           , d."data#headBlockBakedAt" AT TIME ZONE 'UTC'
           , d."data#fitness"
           , d."data#savePoint"
        FROM "NodeInternal" n
        JOIN "NodeDetails" d ON d.id = n.id
        JOIN "ProcessData" p ON p.id = n."data#data"
      WHERE NOT n."data#deleted"
        AND p."state" = ?runningState
      |] <&> fmap (\(l, b, p, t, f, s) -> (kilnNodeUri, VeryBlockLike <$> b <*> p <*> f <*> l <*> t, s))
  ext <- [queryQ|
      SELECT n."data#data#address"
           , d."data#headLevel"
           , d."data#headBlockHash"
           , d."data#headBlockPred"
           , d."data#headBlockBakedAt" AT TIME ZONE 'UTC'
           , d."data#fitness"
           , d."data#savePoint"
        FROM "NodeExternal" n
        JOIN "NodeDetails" d ON d.id = n.id
      WHERE NOT n."data#deleted"
      |] <&> fmap (\(addr, l, b, p, t, f, s) -> (addr, VeryBlockLike <$> b <*> p <*> f <*> l <*> t, s))
  pure $ ext <> int

-- Protocol Constants

getProtocolConstants
  :: forall m
   . (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => Either BlockHash ProtocolHash -> NodeQueryT m ProtoInfo
getProtocolConstants ct = do
  protoHash <- case ct of
    Right p -> pure p
    Left h -> view protocolHash <$> nodeQueryDataSourceSafe (NodeQuery_BlockHeader h)
  chainId <- asksNodeDataSource _nodeDataSource_chain
  existingEntries :: [ProtocolIndex] <- select $
    ProtocolIndex_chainIdField ==. chainId &&. ProtocolIndex_hashField ==. protoHash

  _protocolIndex_constants <$> case headMay existingEntries of
    Just existing -> pure existing
    Nothing -> do
      hash' <- case ct of
        Right _ -> askNodeDataSource >>= nqAtomically . dataSourceHead
          >>= maybe (nqThrowError CacheError_NotEnoughHistory) (pure . view hash)
        Left hash' -> pure hash'
      getProtocolIndex hash' protoHash

-- TODO: Pass history in
getProtocolIndex
  :: forall m
   . (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => BlockHash -> ProtocolHash -> NodeQueryT m ProtocolIndex
getProtocolIndex branch protoHash = do
  (chainId, historyVar) <- asksNodeDataSource (_nodeDataSource_chain &&& _nodeDataSource_history)
  existingEntries :: [ProtocolIndex] <- select $
    ProtocolIndex_chainIdField ==. chainId &&. ProtocolIndex_hashField ==. protoHash

  history <- nqAtomically $ readTVar' historyVar
  case headMay existingEntries of
    Just existing -> pure existing
    Nothing -> nqTry (nodeQueryDataSourceSafe $ NodeQuery_ProtocolIndex protoHash) >>= \case
      Right p' -> do
        insert p'
        pure p'
      Left _ -> nqTry (buildProtocolIndex branch protoHash history) >>= \case
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
  => BlockHash -> ProtocolHash -> CachedHistory' -> NodeQueryT m ProtocolIndex
buildProtocolIndex branch protoHash history = do
  chainId <- asksNodeDataSource _nodeDataSource_chain
  -- Search until we have the history up to the desired protocol.
  protocolHistory <- buildProtocolHistoryUntil
    ! #predicate (\blk -> blk ^. protocolHash == protoHash)
    ! #branch branch
    ! #history history

  case NE.nonEmpty $ sortOn (Down . (^. level)) $ toList protocolHistory of
    Nothing -> nqThrowError CacheError_NotEnoughHistory
    Just orderedFirstBlocks -> do
      -- To increase likelihood that a node knows the answer, we will use the *last* block
      -- in a protocol to get it's constants (the most recent block possible). To do this we
      -- pair up the protocols with the block immediately *prior* to the first block in the
      -- next protocol. For the most recent protocol, we will use 'branch' as the query block.
      let
        initOrderedLastBlockHashes = flip map (NE.init orderedFirstBlocks) $ \blk ->
          levelAncestor history (blk ^. level - 1) branch
        protocolQueryBlockMap = NE.zip orderedFirstBlocks (Just branch NE.:| initOrderedLastBlockHashes)

      protoIndexes :: [ProtocolIndex] <- fmap (mapMaybe (^? _Right) . toList) $
        for protocolQueryBlockMap $ \(firstBlock, queryBlockHash') -> nqTry $ do
          -- Before using the query block instead of 'firstBlock', make sure it's protocol really is
          -- the same. If not, fall back to 'firstBlock'.
          -- While this situation shouldn't happen, it's possible for protocols to be introduced
          -- apart from the amendment process. In this case we may actually skip one
          -- in the scan which would cause this logic to pair the wrong constants with a
          -- protocol hash--and that's just too scary to think about.
          queryBlock' <- for queryBlockHash' $ nodeQueryDataSourceSafe . NodeQuery_Block
          let
            actualQueryBlockHash = case queryBlock' of
              Just queryBlock | queryBlock ^. protocolHash == firstBlock ^. protocolHash -> queryBlock ^. hash
              _ -> firstBlock ^. hash
          constants <- nodeQueryDataSourceSafe $ NodeQuery_ProtocolConstants actualQueryBlockHash
          pure ProtocolIndex
            { _protocolIndex_chainId = chainId
            , _protocolIndex_hash = firstBlock ^. protocolHash
            , _protocolIndex_proto = firstBlock ^. blockHeaderFull . blockHeaderFull_proto
            , _protocolIndex_constants = constants
            , _protocolIndex_firstBlockHash = Just $ firstBlock ^. hash
            , _protocolIndex_firstBlockPredecessor = Just $ firstBlock ^. predecessor
            , _protocolIndex_firstBlockLevel = Just $ firstBlock ^. level
            , _protocolIndex_firstBlockFitness = Just $ firstBlock ^. fitness
            , _protocolIndex_firstBlockTimestamp = Just $ firstBlock ^. timestamp
            , _protocolIndex_firstBlockCycle = Just $ firstBlock ^. blockMetadata . blockMetadata_level . level_cycle
            }

      for_ protoIndexes $ \protoIndex -> do
        mp :: Maybe (Maybe BlockHash) <- project1 ProtocolIndex_firstBlockHashField
          (( ProtocolIndex_hashField ==. protoIndex ^. protocolIndex_hash )
            &&. (ProtocolIndex_chainIdField ==. chainId)
          )
        when (join mp == Nothing) $ do
          insert protoIndex
          notifyDefault $ Id @ProtocolIndex (protoIndex ^. protocolIndex_chainId, protoIndex ^. protocolHash)

      maybe (nqThrowError CacheError_NotEnoughHistory) pure $
        find ((protoHash ==) . view protocolHash) protoIndexes

buildProtocolHistoryUntil
  :: forall m
   . (MonadNodeQuery (NodeQueryT m), MonadMask m)
  => "predicate" :! (BlockCrossCompat -> Bool)
  -> "branch" :! BlockHash
  -> "history" :! CachedHistory'
  -> NodeQueryT m (Map ProtocolHash BlockCrossCompat)
  -- Turns this into table, ProtocolHash
buildProtocolHistoryUntil (Arg predicate) (Arg branch) (Arg history) = do
  branchBlock <- nodeQueryDataSourceSafe $ NodeQuery_Block branch
  go ! #currentBlock branchBlock
     ! #currentProtocol (branchBlock ^. protocolHash)
     ! #protocolHistory mempty
  where
    levelsBefore blk lvls = maybe (nqThrowError CacheError_NotEnoughHistory) pure $
      if blk ^. level - lvls < 0
      then Nothing
      else
        levelAncestor history (max (blk ^. level - lvls) (history ^. cachedHistory_minLevel)) (blk ^. hash)

    votingPeriodPosition = blockMetadata . blockMetadata_level . level_votingPeriodPosition

    go :: "currentBlock" :! BlockCrossCompat
       -> "currentProtocol" :! ProtocolHash
       -> "protocolHistory" :! Map ProtocolHash BlockCrossCompat
       -> NodeQueryT m (Map ProtocolHash BlockCrossCompat)
    go (Arg currentBlock) (Arg currentProtocol) (Arg protocolHistory) =
      case currentBlock ^. level == history ^. cachedHistory_minLevel of
        True -> do
          $(logDebug) [i|Got to the root searching for protocol: ${currentProtocol}|]
          -- If 'currentBlock' is at the minimum level, we call it the beginning of 'currentProtocol'.
          pure $ Map.insert currentProtocol currentBlock protocolHistory
        False -> do
          lastBlockHashInPreviousVotingPeriod <- levelsBefore currentBlock (currentBlock ^. votingPeriodPosition + 1)
          lastBlockInPreviousVotingPeriod <- nodeQueryDataSourceSafe $ NodeQuery_Block lastBlockHashInPreviousVotingPeriod -- HEADER ONLY?
          case currentProtocol == lastBlockInPreviousVotingPeriod ^. protocolHash of
            True -> do
              $(logDebug) [i|Found another voting period with the same protocol ${currentProtocol} - ${lastBlockInPreviousVotingPeriod ^. hash}|]
              go ! #currentBlock lastBlockInPreviousVotingPeriod
                 ! #currentProtocol currentProtocol
                 ! #protocolHistory protocolHistory
            False -> do
              $(logDebug) [i|Found a transition for ${currentProtocol} at ${lastBlockInPreviousVotingPeriod ^. hash}|]
              firstBlockHashInVotingPeriod <- levelsBefore currentBlock (currentBlock ^. votingPeriodPosition)
              firstBlockInVotingPeriod <- nodeQueryDataSourceSafe $ NodeQuery_Block firstBlockHashInVotingPeriod

              (lastBlockInPreviousProtocol, firstBlockInProtocol) <- case currentProtocol == firstBlockInVotingPeriod ^. protocolHash of
                True -> pure (lastBlockInPreviousVotingPeriod, firstBlockInVotingPeriod)
                False -> do
                  $(logDebug) [i|Entering binary search for ${currentProtocol}|]
                  maybe (nqThrowError $ CacheError_UnknownProtocol currentProtocol) pure =<<
                      binarySearch firstBlockInVotingPeriod currentBlock

              let protocolHistory' = Map.insert currentProtocol firstBlockInProtocol protocolHistory
              case predicate firstBlockInProtocol of
                True -> pure protocolHistory' -- We finished searching.
                False -> go ! #currentBlock lastBlockInPreviousProtocol
                            ! #currentProtocol (lastBlockInPreviousProtocol ^. protocolHash)
                            ! #protocolHistory protocolHistory'

    binarySearch :: BlockCrossCompat -> BlockCrossCompat -> NodeQueryT m (Maybe (BlockCrossCompat, BlockCrossCompat))
    binarySearch low high = do
      $(logDebug) [i|Protocol binary search between levels ${low ^. level} and ${high ^. level}|]
      binarySearch' low high

    binarySearch' :: BlockCrossCompat -> BlockCrossCompat -> NodeQueryT m (Maybe (BlockCrossCompat, BlockCrossCompat))
    binarySearch' low high
      | low ^. level >= high ^. level = pure Nothing
      | low ^. level == high ^. level - 1 =
          pure $ if low ^. protocolHash == high ^. protocolHash then Nothing else Just (low, high)
      | otherwise = do
        let halfwayLevel = high ^. level - ((high ^. level - low ^. level) `div` 2)
        halfway <- nodeQueryDataSourceSafe . NodeQuery_Block <=<
          maybe (nqThrowError CacheError_NotEnoughHistory) pure $
            levelAncestor history halfwayLevel branch
        case halfway of
          x | x ^. protocolHash == low ^. protocolHash -> binarySearch halfway high
            | x ^. protocolHash == high ^. protocolHash -> binarySearch low halfway
            | otherwise -> pure Nothing

fetchProtocolForBlock
  :: forall m
   . (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => ChainId
  -> BlockHash
  -> NodeQueryT m ProtocolIndex
fetchProtocolForBlock chainId blkHash = do
  $(logDebug) [i|fetchProtocolForBlock: ${blkHash}|]
  protoInfo <- nodeQueryDataSourceSafe $ NodeQuery_ProtocolConstants blkHash
  blockHeader <- nodeQueryDataSourceSafe $ NodeQuery_BlockHeader blkHash
  let p = ProtocolIndex
          { _protocolIndex_chainId = chainId
          , _protocolIndex_hash = blockHeader ^. protocolHash
          , _protocolIndex_constants = protoInfo
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

instance (ToJSON (NodeQuery a)) => Hashable (NodeQuery a) where
  hashWithSalt s = hashWithSalt s . Aeson.toJSON
