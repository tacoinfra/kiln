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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- for MonadError instance

{-# OPTIONS_GHC -Wall -Werror #-}

-- TODO: move this to ~lib?
module Backend.CachedNodeRPC where

import Prelude hiding (cycle)

import Control.Applicative (ZipList (..))
import Control.Arrow (left)
import Control.Concurrent.STM (STM, TQueue, TVar, atomically, newTQueueIO, newTVarIO, readTVar, readTVarIO,
                               retry, writeTQueue, writeTVar)
import Control.Exception (throw)
import Control.Exception.Safe (Exception)
import Control.Exception.Safe (MonadMask, withException)
import Control.Exception.Safe (toException)
-- import Control.Lens (TraversableWithIndex)
import Control.Lens (re)
import Control.Lens (review)
import Control.Lens.TH (makeLenses)
import Control.Monad (ap)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (ExitCase (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Catch (catch)
import Control.Monad.Catch (generalBracket)
import Control.Monad.Catch (mask)
import Control.Monad.Catch (throwM)
import Control.Monad.Catch (uninterruptibleMask)
import Control.Monad.Error.Lens (catching)
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT, throwError)
import Control.Monad.Except (catchError)
import Control.Monad.Except (liftEither)
import Control.Monad.Logger (LoggingT (..), MonadLogger, logDebugSH, logErrorSH, logInfo, logWarnSH)
import Control.Monad.Logger (monadLoggerLog)
import Control.Monad.Reader (local)
import Control.Monad.Reader (reader)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.Aeson as Aeson
import Data.Constraint (Dict (..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.LCA.Online.Polymorphic as LCA
import Data.List (genericTake)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
-- import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Pool (Pool)
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import qualified Data.Vector as V
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.HTTP.Client as Http (Manager)
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, queryQ)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Rhyolite.Request.Class (requestResponseFromJSON, requestToJSON)
import Rhyolite.Request.TH (makeRequestForData)
import Rhyolite.Schema (Json (..))
import Safe (headMay)
import Safe.Foldable (maximumByMay)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.History
import Tezos.NodeRPC.Class
import Tezos.NodeRPC.Network
import Tezos.NodeRPC.Sources
import Tezos.NodeRPC.Types
import Tezos.PublicKey
import Tezos.Types

import Backend.Common (timeout')
import Backend.Schema
import Backend.STM (HasTimestamp, MonadSTM (liftSTM), atomicallyWith, atomicallyWithTime, modifyTVar_',
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
  NodeQuery_BakingRights    :: BlockHash -> RawLevel -> NodeQuery (Seq BakingRights)
  NodeQuery_BakingRights1   :: BlockHash -> RawLevel -> Priority -> NodeQuery BakingRights
    -- Baking rights for a specific priority.
  NodeQuery_BakingRightsChunk :: BlockHash -> RawLevel -> Priority -> NodeQuery (V.Vector BakingRights)
    -- Baking rights for a chunk of 64 priorities including the indicated one.  You probably shouldn't use this directly.
  NodeQuery_EndorsingRights :: BlockHash -> RawLevel -> NodeQuery (Seq EndorsingRights)
  NodeQuery_Account         :: BlockHash -> ContractId -> NodeQuery Account
  NodeQuery_Ballots         :: BlockHash -> NodeQuery Ballots
  NodeQuery_Proposals       :: BlockHash -> NodeQuery (Seq ProposalVotes)
  NodeQuery_CurrentProposal :: BlockHash -> RawLevel -> NodeQuery (Maybe ProtocolHash)
  NodeQuery_Block           :: BlockHash -> NodeQuery Block
  NodeQuery_BlockBaker      :: BlockHash -> RawLevel -> NodeQuery BlockBaker
  NodeQuery_DelegateInfo    :: BlockHash -> RawLevel -> PublicKeyHash -> NodeQuery CacheDelegateInfo
  NodeQuery_PublicKey       :: ContractId -> NodeQuery PublicKey
deriving instance Show (NodeQuery a)

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

data CacheLine a = CacheLine
  { _cacheLine_value :: !a
  , _cacheLine_used :: !UTCTime
  , _cacheLine_dirty :: !DirtyBit -- is this entry already in the database?
  }

data NodeDataSource = NodeDataSource
  { _nodeDataSource_history :: !(TVar CachedHistory')
  , _nodeDataSource_nodes :: !(TVar (Map URI (Maybe VeryBlockLike)))
  , _nodeDataSource_cache :: !(TVar (DMap NodeQuery (Compose TVar CacheLine)))
  , _nodeDataSource_chain :: !ChainId
  , _nodeDataSource_parameters :: !(TVar (Maybe ProtoInfo))
  , _nodeDataSource_httpMgr :: !Http.Manager
  , _nodeDataSource_pool :: !(Pool Postgresql)
  , _nodeDataSource_latestHead :: !(TVar (Maybe VeryBlockLike))
  , _nodeDataSource_logger :: !LoggingEnv
  , _nodeDataSource_ioQueue :: TQueue (IO ())
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
  nodeRPCOrBust :: ProtoInfo -> BlockHash -> NodeQuery a -> m a

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
  (liftEither =<<) $ fmap (left (review asCacheError)) $ liftIO $ flip runReaderT nds $ runExceptT $ unNodeQueryQueued $ action

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
    NodeQueryQueued $ lift @(ExceptT CacheError) $ runLoggingEnv logger $ runDb (Identity db) $ action
  answerImmediate = return . return . NodeQueryQueuedAnswerM
  withFinishWith nds cb = do
    -- A separate TVar for keeping the actual API result (outside the cache structure)
    apiResultVar :: TVar (Maybe (Either CacheError a)) <- newTVar' Nothing
    action <- cb $ writeTVar' apiResultVar . Just
    let ioQueue = _nodeDataSource_ioQueue nds
    liftSTM $ writeTQueue ioQueue $ void $ flip runReaderT nds $ runExceptT $ unNodeQueryQueued $ action
    return $ return $ NodeQueryQueuedAnswerM $ readTVar' apiResultVar
  nodeRPCOrBust protoInfo qBranch q = do
    dsrc <- askNodeDataSource
    nodesToTry <- NodeQueryQueued $ atomicallyWith $ pickNode qBranch >>= \case
      Nothing -> fmap Map.keys $ readTVar' $ _nodeDataSource_nodes dsrc
      Just anyNode -> pure [anyNode]
    result <- foldM `flip` Left CacheError_NoSuitableNode `flip` nodesToTry $ \case
      answer@(Right _) -> const $ pure answer -- short circuit if there is already an answer
      Left _ -> \anyNode -> do
        let
          ctx = NodeRPCContext (_nodeDataSource_httpMgr dsrc) (Uri.render anyNode)

          nodeQueryViaCache :: forall b. NodeQuery b -> IO (Either CacheError b)
          nodeQueryViaCache qInner = runReaderT (runExceptT $ nodeQueryDataSourceImmediate qInner) dsrc

        NodeQueryQueued $ liftIO $ nodeQueryDataSourceImpl (_nodeDataSource_chain dsrc) qBranch protoInfo ctx (_nodeDataSource_logger dsrc) nodeQueryViaCache q
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
  nqInDB action = NodeQueryImmediate $ nqInDB $ action
  answerImmediate getResult = return $ fmap NodeQueryImmediateAnswerM $ (nqLiftEither =<<) $ nqAtomicallyWithTime (getResult >>= maybe retry' return)
  withFinishWith _ cb = (fmap NodeQueryImmediateAnswerM . nqLiftEither =<<) <$> cb return
  nodeRPCOrBust p h q = NodeQueryImmediate $ nodeRPCOrBust p h q

data NodeQueryTResult a where
  NodeQueryTResult_Done :: a -> NodeQueryTResult a
  NodeQueryTResult_Query :: forall a b. BlockHash -> NodeQuery a -> NodeQueryTResult b

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
  nodeRPCOrBust _ h q = NodeQueryT $ \bad -> case DMap.lookup q bad >>= pure . getConst >>= Map.lookup h of
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
    ( MonadIO m, MonadBaseNoPureAborts IO m
    , MonadReader s m, HasNodeDataSource s
    , MonadLogger m
    )
  => NodeQueryT (ExceptT e (ReaderT NodeDataSource (DbPersist Postgresql m))) a -> ExceptT e m a
runNodeQueryT f = ExceptT @e $ go 0 DMap.empty
  where
    go :: Int -> DMap NodeQuery (Const (Map BlockHash CacheError)) -> m (Either e a)
    go n bad = do
      $(logDebugSH) ("RPC monad attempt number" :: Text, n :: Int, "starting" :: Text)
      tryNodeQueryT bad f >>= \case
        Left e -> return $ Left e
        Right (NodeQueryTResult_Done v) -> do
          $(logDebugSH) ("RPC monad attempt number" :: Text, n, "succeeded" :: Text)
          return $ Right v
        Right (NodeQueryTResult_Query h q) -> do
          $(logDebugSH) ("RPC monad attempt number" :: Text, n, "retry for query" :: Text, q)
          -- just get it into cache
          runExceptT (nodeQueryDataSource q) >>= \case
            Right _ -> go (n + 1) bad
            Left e -> go (n + 1) (bad <> DMap.singleton q (Const $ Map.singleton h e))

tryNodeQueryT
  :: forall a s e m.
    ( MonadIO m, MonadBaseNoPureAborts IO m
    , MonadReader s m, HasNodeDataSource s
    , MonadLogger m
    )
  => DMap NodeQuery (Const (Map BlockHash CacheError)) -> NodeQueryT (ExceptT e (ReaderT NodeDataSource (DbPersist Postgresql m))) a -> m (Either e (NodeQueryTResult a))
tryNodeQueryT bad f = do
  nds <- view nodeDataSource
  let db = _nodeDataSource_pool nds
      bail = (DbPersist $ ReaderT $ \(Postgresql conn) -> liftIO $ PG.rollback conn *> PG.begin conn)
  runDb (Identity db) $ runReaderT (runExceptT (unNodeQueryT f bad)) nds >>= \case
    e@(Left _) -> e <$ bail
    v@(Right (NodeQueryTResult_Done _)) -> return v
    q@(Right (NodeQueryTResult_Query _ _)) -> q <$ bail

waitForParams :: (HasNodeDataSource r, MonadSTM m) => r -> m ProtoInfo
waitForParams r = maybe retry' pure =<< readTVar' (r ^. nodeDataSource . nodeDataSource_parameters)

withParams :: (HasNodeDataSource r, MonadIO m) => r -> (ProtoInfo -> m a) -> m a
withParams r act = liftIO (atomically (waitForParams r)) >>= act

unpackCacheResult
  :: forall a r m. (MonadSTM m, MonadReader r m, HasTimestamp r)
  => Compose TVar CacheLine a -> m a
unpackCacheResult (Compose var) = do
  result <- readTVar' var
  now <- asks (^. Stm.timestamp)
  writeTVar' var $ result{_cacheLine_used = now}
  pure $ _cacheLine_value result

-- get lca between two blocks
branchPoint
  :: forall r m. (HasNodeDataSource r, MonadSTM m, MonadReader r m)
  => BlockHash -> BlockHash -> m (Maybe VeryBlockLike)
branchPoint x y = do
  dsrc <- asks (^. nodeDataSource)
  history <- readTVar' $ _nodeDataSource_history dsrc
  let
    xPath = Map.lookup x $ _cachedHistory_blocks history
    yPath = Map.lookup y $ _cachedHistory_blocks history
  return $ fmap (histToBlockLike (_cachedHistory_minLevel history)) . LCA.uncons =<< LCA.lca <$> xPath <*> yPath

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
  return $ fmap (histToBlockLike (_cachedHistory_minLevel history)) . LCA.uncons =<< xPath

blankNodeDataSource :: Pool Postgresql -> ChainId -> Maybe ProtoInfo -> Http.Manager -> LoggingEnv -> IO NodeDataSource
blankNodeDataSource db chain protoInfo' mgr logger = do
  nodes <- newTVarIO mempty
  hist <- newTVarIO emptyCache
  cache <- newTVarIO mempty
  protoInfoVar <- newTVarIO protoInfo'
  latestHead <- newTVarIO Nothing
  ioQueue <- newTQueueIO

  return NodeDataSource
    { _nodeDataSource_history = hist
    , _nodeDataSource_nodes = nodes
    , _nodeDataSource_cache = cache
    , _nodeDataSource_chain = chain
    , _nodeDataSource_parameters = protoInfoVar
    , _nodeDataSource_httpMgr = mgr
    , _nodeDataSource_pool = db
    , _nodeDataSource_latestHead = latestHead
    , _nodeDataSource_logger = logger
    , _nodeDataSource_ioQueue = ioQueue
    }
{-

withNDSLogging :: (MonadReader r m, HasNodeDataSource r) => LoggingT m a -> m a
withNDSLogging x = flip runLoggingEnv x . _nodeDataSource_logger =<< asks (^. nodeDataSource)

-}
calcTimeBetweenBlocks :: ProtoInfo -> NominalDiffTime
calcTimeBetweenBlocks = fromIntegral . sum . take 1 . toList . _protoInfo_timeBetweenBlocks

-- | Blocks until a new head is seen or the time between blocks has elapsed.
waitForNewHeadWithTimeout :: NodeDataSource -> IO ()
waitForNewHeadWithTimeout nds = do
  -- TODO: This shouldn't be necessary once we have a way to know the parameters better. Foundation nodes should give us params.
  timeLimit <- maybe 60 calcTimeBetweenBlocks <$> readTVarIO (_nodeDataSource_parameters $ nds ^. nodeDataSource)
  void $ timeout' timeLimit $ waitForNewHead nds

-- | Blocks until a new head is seen.
--
-- Returns most recently seen head.
waitForNewHead :: NodeDataSource -> IO VeryBlockLike
waitForNewHead nds = do
  oldHead <- readTVarIO (_nodeDataSource_latestHead nds)
  atomically $ do
    newHead <- maybe retry pure =<< readTVar (_nodeDataSource_latestHead nds)
    when (oldHead == Just newHead) retry
    pure newHead

-- turn the result of an LCA.uncons on the block history into a VeryBlockLike
histToBlockLike :: RawLevel -> (BlockHash, (), LCA.Path BlockHash ()) -> VeryBlockLike
histToBlockLike minLevel (h, (), path) = VeryBlockLike h p mempty blkLevel unixEpoch
  where
    blkLevel = minLevel + fromIntegral (length path)
    p = maybe h (\(pp, _, _) -> pp) $ LCA.uncons path

updateNodeDataSource
  :: forall nds b m. (HasNodeDataSource nds, BlockLike b, MonadSTM m)
  => nds -> URI -> b -> m ()
updateNodeDataSource nds nodeAddr blk = do
  let nodesVar = nds ^. nodeDataSource . nodeDataSource_nodes
  modifyTVar_' nodesVar $ pure . Map.insert nodeAddr (Just $ mkVeryBlockLike blk)

-- Make sure that the protocol parameters have been loaded and the datasource initialized.
initParams :: Foldable f => NodeDataSource -> f (Maybe PublicNode, URI) -> IO Bool
initParams nds theseNodes = runLoggingEnv (_nodeDataSource_logger nds) $ do
  needParams <- fmap isNothing $ liftIO $ readTVarIO $ _nodeDataSource_parameters nds
  case needParams of
    False -> pure True
    True -> onChainNodes >>= \case
      Nothing -> $(logInfo) "Still no params" $> False
      Just params -> do
        void $ liftIO $ atomically $ writeTVar (_nodeDataSource_parameters nds) $ Just params
        insertParams params
        pure True

  where
    chainId = _nodeDataSource_chain nds

    step :: LoggingT IO (Maybe ProtoInfo) -> (Maybe PublicNode, URI) -> LoggingT IO (Maybe ProtoInfo)
    step l (pn, someNode) = l >>= \case
      Nothing -> do
        let ctx = PublicNodeContext (NodeRPCContext (_nodeDataSource_httpMgr nds) (Uri.render someNode)) pn
        runExceptT (runReaderT (getProtoConstants chainId) ctx) >>= \case
          Left (e :: PublicNodeError) -> $(logErrorSH) e $> Nothing
          Right params -> $(logDebugSH) params $> Just params
      l' -> return l'

    onChainNodes = foldl step (return Nothing) theseNodes

    insertParams params = runDb (Identity $ _nodeDataSource_pool nds) $ do
      have :: Maybe (Id Parameters) <- fmap toId . listToMaybe <$> project AutoKeyField (Parameters_chainField ==. chainId)
      case have of
        Just _entryId -> pure ()
        Nothing -> do
          let
            entry = Parameters
              { _parameters_protoInfo = params
              , _parameters_chain = chainId
              }
          notify NotifyTag_Parameters . (, entry) =<< insert' entry


-- | extrats the fittest known branch from cache
dataSourceHead
  :: forall nds m. (HasNodeDataSource nds, MonadSTM m)
  => nds -> m (Maybe VeryBlockLike)
dataSourceHead nds = withCache nds Nothing $ \_ -> do
  let dsrc = nds ^. nodeDataSource
  history <- readTVar' $ _nodeDataSource_history dsrc
  let branches = _cachedHistory_branches history
  pure $ maximumByMay (compare `on` view fitness) $ toList branches

{-
-- | extrats the fittest known node from cache
dataSourceNode
  :: forall nds m. (HasNodeDataSource nds, MonadSTM m)
  => nds -> m (Maybe NodeRPCContext)
dataSourceNode nds = do
  let dsrc = nds ^. nodeDataSource
  nodes <- readTVar' $ _nodeDataSource_nodes dsrc
  pure $ fmap (NodeRPCContext (_nodeDataSource_httpMgr dsrc) . Uri.render . fst) $
    maximumByMay (compare `on` snd) $ mapMaybe sequence $ Map.toList nodes
-}

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust [] = []
takeWhileJust (Just x: xs) = x:takeWhileJust xs
takeWhileJust (Nothing: _) = []


data RightsCycleInfo = RightsCycleInfo
  { _rightsCycleInfo_branch :: !BlockHash  -- the hash of the first block in some cycle
  , _rightsCycleInfo_cycle :: !Cycle
  , _rightsCycleInfo_minLevel :: !RawLevel -- the first level of _rightsCycleInfo_cycle
  , _rightsCycleInfo_maxLevel :: !RawLevel -- the last level of _rightsCycleInfo_cycle
  } deriving (Eq, Ord, Show, Generic, Typeable)

-- produce the list of the first blocks in the cycle for the previous 7 cycles ending on $blkHash$
cycleStartHashes
  :: forall nds m. (HasNodeDataSource nds, MonadReader nds m, MonadSTM m)
  => BlockHash -> m (Maybe [RightsCycleInfo]) -- Nothing when the branch is not in history.
cycleStartHashes blkHash = do
  dsrc <- asks (^. nodeDataSource)
  protoInfo <- maybe retry' pure =<< readTVar' (_nodeDataSource_parameters dsrc)
  history <- readTVar' $ _nodeDataSource_history dsrc
  return $ do
    branch <- blkHash `Map.lookup` (_cachedHistory_blocks history)
    let
      minLvl = _cachedHistory_minLevel history
      lvl = minLvl + RawLevel (fromIntegral $ length branch)
      cycle = levelToCycle protoInfo lvl
      preservedCycles = _protoInfo_preservedCycles protoInfo
      cycles = [max 0 (cycle - (1 + preservedCycles)) .. cycle - 1] -- ignore the unconfirmed "current" cycle.
      minLevels = firstLevelInCycle protoInfo <$> cycles
      maxLevels = pred . firstLevelInCycle protoInfo . succ <$> cycles
      branches = fmap (^. _1) $ takeWhileJust $ LCA.uncons . flip LCA.keep branch . fromIntegral . unRawLevel . subtract minLvl <$> minLevels
    return $ getZipList $ RightsCycleInfo
      <$> ZipList branches
      <*> ZipList cycles
      <*> ZipList minLevels
      <*> ZipList maxLevels




levelAncestor :: CachedHistory' -> RawLevel -> BlockHash -> Maybe BlockHash
levelAncestor hist lvl ctx = ctxBlockHash
  where
    minLevel = _cachedHistory_minLevel hist
    branch = Map.lookup ctx $ _cachedHistory_blocks hist
    ctxBlockHash = fmap (view _1) $ LCA.uncons =<< LCA.keep (fromIntegral $ lvl - minLevel + 1) <$> branch

-- | We want the first block in the cycle that sits PRESERVED_CYCLES before the
-- requested level, that is on the correct branch.
rightsContext :: ProtoInfo -> CachedHistory' -> BlockHash -> RawLevel -> Maybe BlockHash
rightsContext params hist ctx lvl = levelAncestor hist (rightsContextLevel params lvl) ctx

-- | Round the second argument to the next lower multiple of the first
floorBy :: Integral a => a -> a -> a
floorBy k n = n - n `mod` k

priorityChunkSize :: Num a => a
priorityChunkSize = 64

-- Recontextualize a query for maximum cache friendliness, and also return the least block
getKey :: ProtoInfo -> CachedHistory' -> NodeQuery a -> Maybe (BlockHash, NodeQuery a) -- , Set ClientAddress)
getKey params hist = \case
  NodeQuery_BakingRights ctx lvl -> (\ctx' -> (ctx' , NodeQuery_BakingRights ctx' lvl)) <$> rightsContext params hist ctx lvl
  NodeQuery_BakingRights1 ctx lvl prio -> (\ctx' -> (ctx' , NodeQuery_BakingRights1 ctx' lvl prio)) <$> rightsContext params hist ctx lvl
  NodeQuery_BakingRightsChunk ctx lvl prio -> (\ctx' -> (ctx' , NodeQuery_BakingRightsChunk ctx' lvl (floorBy priorityChunkSize prio))) <$> rightsContext params hist ctx lvl
  NodeQuery_EndorsingRights ctx lvl -> (\ctx' -> (ctx' , NodeQuery_EndorsingRights ctx' lvl)) <$> rightsContext params hist ctx lvl
  NodeQuery_Block ctx -> pure (ctx, NodeQuery_Block ctx)
  NodeQuery_Account ctx contractId -> pure (ctx, NodeQuery_Account ctx contractId)
  NodeQuery_Ballots ctx -> pure (ctx, NodeQuery_Ballots ctx)
  NodeQuery_Proposals ctx -> pure (ctx, NodeQuery_Proposals ctx)
  NodeQuery_CurrentProposal ctx lvl -> pure (ctx, NodeQuery_CurrentProposal ctx lvl)
  NodeQuery_BlockBaker ctx lvl -> (\ctx' -> (ctx' , NodeQuery_BlockBaker ctx' lvl)) <$> levelAncestor hist lvl ctx
  NodeQuery_DelegateInfo ctx lvl pkh -> (\ctx' -> (ctx' , NodeQuery_DelegateInfo ctx' lvl pkh)) <$> levelAncestor hist lvl ctx
  q@(NodeQuery_PublicKey _) -> do
    let branches = _cachedHistory_branches hist
    block <- maximumByMay (comparing $ view fitness) $ Map.elems branches
    pure (view hash block, q)

-- | Caching query function simplified by blocking until we get a result.
nodeQueryDataSource
  :: forall a s e m.
    ( MonadIO m
    , MonadReader s m, HasNodeDataSource s
    , MonadError e m, AsCacheError e
    )
  => NodeQuery a -> m a
nodeQueryDataSource q = do
  (view $ nodeDataSource . nodeDataSource_logger) >>= (flip runLoggingEnv $ $(logDebugSH) ("nodeQueryDataSource called" :: Text,q))
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
    )
  => NodeQuery a -> NodeQueryT m a
nodeQueryDataSourceSafe q = do
  $(logDebugSH) ("nodeQueryDataSourceSafe called" :: Text,q)
  unNodeQueryTAnswerM <$> nodeQueryDataSourceRaw q

-- | Query cached data immediately, in this thread.  Only meant to be used in the implementation
--   of recursive queries, lest the dreaded deadlock heisenbunny return.
nodeQueryDataSourceImmediate
  :: forall a s e m.
    ( MonadIO m
    , MonadReader s m, HasNodeDataSource s
    , MonadError e m, AsCacheError e
    )
  => NodeQuery a -> m a
nodeQueryDataSourceImmediate q = runNodeQueryQueued $ do
  $(logDebugSH) ("nodeQueryDataSourceImmediate called" :: Text,q)
  unNodeQueryImmediate $ unNodeQueryImmediateAnswerM <$> nodeQueryDataSourceRaw q

-- Foundational caching query function exposing a low-level API to the underlying 'STM' operations.
nodeQueryDataSourceRaw
  :: forall m a.
    ( MonadNodeQuery m
    , MonadMask m
    )
  => NodeQuery a -> m (AnswerM m a)
nodeQueryDataSourceRaw q' = do
  $(logDebugSH) ("nodeQueryDataSourceRaw called" :: Text,q')
  dsrc <- askNodeDataSource
  updateCache dsrc >>= nqLiftEither

  where
    updateCache :: NodeDataSource -> m (Either CacheError (AnswerM m a))
    updateCache dsrc = (sequence =<<) $ nqAtomically $ runExceptT @CacheError $ do
      protoInfo <- maybe retry' pure =<< readTVar' (_nodeDataSource_parameters dsrc)
      history <- readTVar' (_nodeDataSource_history dsrc)

      (qBranch, q) <- maybe (throwError CacheError_NotEnoughHistory) pure $ getKey protoInfo history q'

      cache <- readTVar' cacheVar
      lift $ case DMap.lookup q cache of
        -- Cache Hit: Return an STM that reads the cache and updates the "access" timestamp
        Just avar -> answerImmediate $
          Just . Right <$> unpackCacheResult avar

        -- Cache Miss: Queue the IO action to collect data and return an STM that reads the result.
        Nothing -> withFinishWith @m dsrc $ \finishWith -> do
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
            (writeResult =<< makeRequestAndCache protoInfo q qBranch)
              `catch` \e ->
                nqAtomically (finishWith $ Left $ CacheError_SomeException e)
              `withException` \x ->
                nqAtomically (finishWith $ Left $ CacheError_SomeException x)

      where
        cacheVar = _nodeDataSource_cache dsrc
        chainId = _nodeDataSource_chain dsrc

        populateKey q a dirty = do
          cache <- readTVar' cacheVar
          case DMap.lookup q cache of
            Just _ -> pure ()
            Nothing -> do
              now <- asks (^. Stm.timestamp)
              var <- newTVar' $ CacheLine a now dirty
              writeTVar' cacheVar $ DMap.insert q (Compose var) cache

        makeRequestAndCache :: ProtoInfo -> NodeQuery a -> BlockHash -> m (Either CacheError (a, DirtyBit))
        makeRequestAndCache protoInfo q qBranch = nqTry $
          tryFetchFromCache chainId q >>= \case
            Just x -> pure $ fmap Just x :: m (a, DirtyBit)
            Nothing -> (,Nothing) <$> nodeRPCOrBust protoInfo qBranch q :: m (a, DirtyBit)

unliftEither :: MonadError e m => m a -> m (Either e a)
unliftEither action = (Right <$> action) `catchError` (pure . Left)

pickNode
  :: (HasNodeDataSource r, MonadSTM m, MonadReader r m)
  => BlockHash -> m (Maybe URI)
pickNode branch = do
  dsrc <- asks (^. nodeDataSource)
  nodeHeads <- readTVar' $ _nodeDataSource_nodes dsrc
  fmap (headMay . catMaybes) $ for (Map.toList $ Map.mapMaybe id nodeHeads) $ \(nodeUri, nodeHead) ->
    containsBranch nodeHead >>= \isCandidate ->
      pure $ if isCandidate then Just nodeUri else Nothing
  where
    containsBranch nodeHead = (Just branch ==) . (^? _Just . hash) <$> branchPoint (nodeHead ^. hash) branch

nodeQueryDataSourceImpl
  :: forall a.
     ChainId
  -> BlockHash
  -> ProtoInfo
  -> NodeRPCContext
  -> LoggingEnv
  -> (forall b. NodeQuery b -> IO (Either CacheError b))
  -> NodeQuery a
  -> IO (Either CacheError a)
nodeQueryDataSourceImpl chainId qBranch _proto ctx logger self' q = runExceptT $ (runLoggingEnv logger $ $(logDebugSH) ("nodeQueryDataSourceImpl called" :: Text,q)) *> case q of
  NodeQuery_BakingRights branch targetLevel ->
    nodeRPC' $ rBakingRights (Set.singleton $ Left targetLevel) chainId branch
  NodeQuery_BakingRights1 branch targetLevel prio ->
    ExceptT $ fmap join $ runExceptT $ fmap (maybe (Left $ CacheError_SomeException $ toException $ NoRightsException branch targetLevel prio) Right . (V.!? fromIntegral (prio `mod` priorityChunkSize))) $ self $ NodeQuery_BakingRightsChunk branch targetLevel prio
  NodeQuery_BakingRightsChunk branch targetLevel prio ->
    fmap (fillChunk branch targetLevel prio) $ nodeRPC' $ rBakingRightsFull (Set.singleton $ Left targetLevel) (priorityChunkSize + fromIntegral prio) chainId branch
  NodeQuery_EndorsingRights branch targetLevel ->
    nodeRPC' $ rEndorsingRights (Set.singleton $ Left targetLevel) chainId branch
  NodeQuery_Account branch contractId ->
    nodeRPC' $ rContract contractId chainId branch
  NodeQuery_Ballots branch -> nodeRPC' $ rBallots chainId branch
  NodeQuery_Proposals branch -> nodeRPC' $ rProposals chainId branch
  NodeQuery_CurrentProposal branch _lvl -> nodeRPC' $ rCurrentProposal chainId branch
  NodeQuery_Block branch -> nodeRPC' $ rBlock chainId branch
  NodeQuery_BlockBaker branch _lvl -> fmap getBakerFromBlock $ self $ NodeQuery_Block branch
  NodeQuery_DelegateInfo branch _lvl pkh -> fmap toCacheDelegateInfo $ nodeRPC' $ rDelegateInfo pkh chainId branch
  NodeQuery_PublicKey contractId -> do
    managerkeyResp <- nodeRPC' $ rManagerKey contractId chainId qBranch
    case view managerKey_key managerkeyResp of
      Nothing -> throwError $ CacheError_UnrevealedPublicKey contractId
      Just pk -> pure $ pk
  where
    nodeRPC' :: forall c. (forall repr. (BlockType repr ~ Block, QueryNode repr, QueryHistory repr, QueryBlock repr) => repr c) -> ExceptT CacheError IO c
    nodeRPC' q' = runReaderT (runLoggingEnv logger $ nodeRPC q') ctx
    {-# INLINE nodeRPC' #-}

    self :: forall b. NodeQuery b -> ExceptT CacheError IO b
    self = ExceptT . self'

    fillChunk :: BlockHash -> RawLevel -> Priority -> Seq BakingRights -> V.Vector BakingRights
    fillChunk branch targetLevel prio
      = (makeBlanks branch targetLevel prio V.//)
      . map (\x -> (fromIntegral $ _bakingRights_priority x - prio, x))
      . filter (\x -> _bakingRights_priority x >= prio)
      . toList

    makeBlanks :: BlockHash -> RawLevel -> Priority -> V.Vector BakingRights
    makeBlanks branch targetLevel prio = V.generate priorityChunkSize $ \i -> throw $ NoRightsException branch targetLevel $ prio + fromIntegral i

withCache
  :: forall nds a m. (HasNodeDataSource nds, MonadSTM m)
  => nds -> a -> (ProtoInfo -> m a) -> m a
withCache nds dft action = do
  let dsrc = nds ^. nodeDataSource
  protoInfo <- readTVar' $ _nodeDataSource_parameters dsrc
  fromMaybe dft <$> traverse action protoInfo

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
  :: forall m a. MonadNodeQuery m
  => ChainId -> NodeQuery a -> m (Maybe (a, Id GenericCacheEntry))
tryFetchFromCache chainId q = do
  let
    qJson = Json $ requestToJSON q
  -- although this is within the grasp of groundhog, this table is very hot,
  -- and the "IS NOT DISTINCT FROM" queries it generates are cataclysmically
  -- terrible:
  -- https://www.postgresql.org/message-id/17764.1405993868%40sss.pgh.pa.us
  resultM :: [(Id GenericCacheEntry, GenericCacheEntry)] <- nqInDB $ [queryQ|
    SELECT "id", "chainId", "key", "value"
    FROM "GenericCacheEntry"
    WHERE "chainId" = ?chainId
      AND "key" = ?qJson
    |] <&> fmap (\(i, c, k, v) -> (i, GenericCacheEntry c k v))
  case nonEmpty $ resultM of
    Nothing -> return Nothing
    Just ((rid, result) :| _) -> case requestResponseFromJSON q of
      Dict -> case Aeson.fromJSON (unJson $ _genericCacheEntry_value result) of
        Aeson.Success v -> return $ Just (v, rid)
        Aeson.Error bad -> do
          $(logWarnSH) $ "tryFetchFromCache failed to decode: " <> bad
          return Nothing

deriveGEq ''NodeQuery
deriveGCompare ''NodeQuery
deriveGShow ''NodeQuery
makeRequestForData ''NodeQuery

-- TODO: Is this worth keeping?
instance Hashable (NodeQuery a) where
  hashWithSalt s = hashWithSalt s . requestToJSON
