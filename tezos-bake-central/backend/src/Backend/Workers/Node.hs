{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fmax-relevant-binds=32 #-}

module Backend.Workers.Node where

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (STM, atomically, readTVar, writeTVar)
import Control.Lens (ifor, ifor_, ix, to, view, (.~), (<&>), (^.), (^?), _Just, _Right)
import Control.Monad (when, (<=<))
import Control.Monad.Except (ExceptT (..), MonadError, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.State (execStateT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bifunctor (first, second)
import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (for_, toList)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import qualified Data.LCA.Online.Polymorphic as LCA
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup (Max (..), Sum (..), (<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Data.Traversable (for)
import Data.Tuple (swap)
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql, (=.), (==.))
import qualified Network.HTTP.Client as Http
import Rhyolite.Backend.DB (RunDb, getTime, openDb, runDb, selectMap)
import Rhyolite.Backend.DB.PsqlSimple (In (..), Only (..), PostgresRaw, Values (..), executeQ, queryQ)
import Rhyolite.Backend.Listen (NotificationType (..), insertAndNotify, insertAndNotify_, notifyEntityId,
                                updateAndNotify)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Backend.Schema.Class
import Rhyolite.Concurrent (supervise, worker)
import Rhyolite.Schema (Id (..), IdData, Json (..))
import Say (say, sayErr, sayShow)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.History (CachedHistory (..), accumHistory)
import Tezos.NodeRPC
import Tezos.NodeRPC.Sources (BlockscaleNode (..), DataSource (..), QDataSource, TzScanNode (..), querySource)
import Tezos.Types

import Backend.CachedNodeRPC
import Backend.Common (worker', workerWithDelay)
import Backend.Config (AppConfig (..), HasAppConfig, getAppConfig)
import Backend.Errors
import Backend.Schema
import Backend.Supervisor (withTermination)
import Common (tshow)
import Common.Schema

selectIds
  :: forall a (m :: * -> *) v (c :: (* -> *) -> *) t.
     ( ProjectionDb t (PhantomDb m)
     , ProjectionRestriction t (RestrictionHolder v c), DefaultKeyId v
     , Projection t v, EntityConstr v c
     , HasSelectOptions a (PhantomDb m) (RestrictionHolder v c)
     , PersistBackend m, Ord (IdData v), AutoKey v ~ DefaultKey v)
  => t -- ^ Constructor
  -> a -- ^ Select options
  -> m [(Id v, v)]
selectIds constr = fmap (fmap (first toId)) . project (AutoKeyField, constr)

-- We assume that the implicit nodeaddr is the same one we just learned the new
-- branch from, so we insist that we bootstrap from it (rather than using a
-- pool of nodes)

-- TODO: make this "configurable" implementation idea:  we could partition
-- history into horizontal level regions (say, every 10k levels) and require
-- each "slice" start on a boundary, and contain only the blocks within their
-- assigned slice.
minCachedBlockLevel :: RawLevel
minCachedBlockLevel = 1

nodeMonitorBranchProgess :: MonadIO m => BlockHash -> BlockHash -> Int -> Int -> m ()
nodeMonitorBranchProgess branch current i n = liftIO $ when (i `mod` 1000 == 0) $ sayShow ("catching up", branch, current, i, n)

nodeMonitor :: ChainId -> Http.Manager -> NodeDataSource -> AppConfig -> URI -> Id Node -> MonitorBlock -> IO ()
nodeMonitor chainId httpMgr nds appConfig nodeAddr nodeId headBlockInfo = do
  oldHead <- runReaderT dataSourceHead nds
  updateNodeDataSource nds nodeAddr headBlockInfo
  let cacheVar = _nodeDataSource_history nds
  let ctx = NodeRPCContext httpMgr (Uri.render nodeAddr)
  newBlock <- modifyMVar cacheVar $ \cache -> do
    let newBlock = Map.member (headBlockInfo ^. hash) (_cachedHistory_blocks cache)
    newStateRsp
      :: Either RpcError CachedHistory'
      <- runExceptT $ flip runReaderT ctx $ flip execStateT cache $ do
        acc <- accumHistory nodeMonitorBranchProgess chainId blockSummary headBlockInfo
        sayShow ("new block", nodeAddr, headBlockInfo, acc)
    case newStateRsp of
      Left bad -> sayShow bad $> (cache, False)
      Right good -> return (good, newBlock)

  when newBlock $ do
    say $ "new block from node at " <> Uri.render nodeAddr
    say $ T.pack $ show headBlockInfo

    when (Just (headBlockInfo ^. fitness) > oldHead ^? _Just . fitness) $
      updateLatestHead nds headBlockInfo

  let db = _nodeDataSource_pool nds
  runNoLoggingT $ runDb (Identity db) $ flip runReaderT appConfig $ do
    -- This isn't very nuanced: old, stale nodes, even if they are catching
    -- up, will churn a lot here.  Maybe we could improve this to filter
    -- out "new" blocks that are already on the branch of `oldHead`?
    when ((view hash <$> oldHead) /= (Just $ view hash headBlockInfo)) $ do
      let now = headBlockInfo ^. timestamp
      have :: Maybe (Id Parameters) <- listToMaybe . stripOnly <$> [queryQ|
        SELECT c."id"
        FROM "Parameters" c
        WHERE c."chain" = ?chainId |]
      case have of
        Just entryId ->
          updateAndNotify entryId
            [Parameters_headTimestampField =. now]
        Nothing -> do
          params <- liftIO $ readMVar $ _nodeDataSource_parameters nds
          insertAndNotify_ Parameters
            { _parameters_protoInfo = params
            , _parameters_chain = chainId
            , _parameters_headTimestamp = now
            }

    updateAndNotify nodeId
      [ Node_headLevelField =. Just (headBlockInfo ^. monitorBlock_level)
      , Node_headBlockHashField =. Just (headBlockInfo ^. monitorBlock_hash)
      , Node_headBlockBakedAtField =. Just (headBlockInfo ^. monitorBlock_timestamp)
      , Node_fitnessField =. Just (headBlockInfo ^. monitorBlock_fitness)
      , Node_lastHeartbeatField =. Just (headBlockInfo ^. monitorBlock_timestamp)
      ]

blockSummary :: BlockLike b => b -> BranchData a
blockSummary blk = BranchData
  { _branchData_info = Nothing
  , _branchData_timestamp = blk ^. timestamp
  , _branchData_level = blk ^. level
  , _branchData_fitness = blk ^. fitness
  }

updateNetworkStats :: AppConfig -> Http.Manager -> Pool Postgresql -> Id Node -> Node -> IO (Either RpcError ())
updateNetworkStats appConfig httpMgr db nid before = do
  after :: Either RpcError Node <- runExceptT $ flip runReaderT (NodeRPCContext httpMgr $ Uri.render nodeAddr) $ do
    connections <- nodeRPC rConnections
    networkStat <- nodeRPC rNetworkStat

    pure $ before
      { _node_peerCount = Just connections
      , _node_networkStat = networkStat
      }

  case after of
    Left err -> pure $ Left err
    Right after -> do
      -- We will rely on the block monitor to clear any inaccessible endpoint errors for this node.
      when (before /= after) $ inDb $
        updateAndNotify nid
          [ Node_peerCountField =. _node_peerCount after
          , Node_networkStatField =. _node_networkStat after
          ]
      pure $ Right ()

  where
    nodeAddr = _node_address before
    inDb = runNoLoggingT . runDb (Identity db)

nodeWorker
  :: NominalDiffTime -- delay between checking for updates, in microseconds
  -> NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
nodeWorker delay nds appConfig db = withTermination $ \addFinalizer -> do
  nodePool :: MVar (Map URI (IO ())) <- newMVar mempty
  let httpMgr = _nodeDataSource_httpMgr nds
  workerWithDelay (pure delay) $ const $ do
    say "Update node cycle."

    -- read the persistent list of nodes
    theseNodeRecords :: Map (Id Node) Node <- runNoLoggingT $ runDb (Identity db) $ do
      selectMap NodeConstructor (Node_deletedField ==. False)
    -- give them all a chance to

    ifor_ theseNodeRecords $ \nodeId node -> do
      updateNetworkStats appConfig httpMgr db nodeId node >>= \case
        Left _e -> reportNodeInaccessible $ _node_address node
        Right () -> pure () -- We'll rely on the block monitor to clear this error

    let theseNodes = Map.fromList $ fmap (\(i, n) -> (_node_address n, i)) $ Map.toList theseNodeRecords

    -- we may need to bootstrap our parameters.  if the cache.parameters var is empty, lets try to fill it with the nodes we currently have
    initParams nds (Map.keys theseNodes)

    thoseNodes <- readMVar nodePool
    let newNodes = theseNodes `Map.difference` thoseNodes
    let staleNodes = thoseNodes `Map.difference` theseNodes

    ifor_ staleNodes $ \nodeAddr killMonitor ->
      say ("stop monitor on " <> Uri.render nodeAddr) *> killMonitor

    let
      chainId = _nodeDataSource_chain nds

    ifor_ newNodes $ \nodeAddr nodeId -> do
      killMonitor <- workerWithDelay (pure 1) $ const $ do
        _ :: Either RpcError () <- runExceptT $ flip runReaderT (NodeRPCContext httpMgr $ Uri.render nodeAddr) $ do
          nodeRPC $ rMonitorHeads chainId $ \block -> do
            -- If we receive a new head, we can clear connectivity errors for this node.
            clearNodeInaccessible nodeAddr

            nodeMonitor chainId httpMgr nds appConfig nodeAddr nodeId block

        -- If the monitor stopped for any reason, we should report it as a connectivity error.
        reportNodeInaccessible nodeAddr

      let cleanup = killMonitor *> modifyMVar_ nodePool (pure . Map.delete nodeAddr)
      liftIO $ modifyMVar_ nodePool $ pure . Map.insert nodeAddr cleanup
      liftIO $ addFinalizer cleanup
      say $ "start monitor on " <> Uri.render nodeAddr

  where
    inDb = runNoLoggingT . runDb (Identity db)
    reportNodeInaccessible nodeAddr = inDb $ runReaderT (reportInaccessibleEndpointError EndpointType_Node nodeAddr) appConfig
    clearNodeInaccessible nodeAddr = inDb $ runReaderT (clearInaccessibleEndpointError EndpointType_Node nodeAddr) appConfig


publicNodesWorker
  :: NodeDataSource
  -> NamedChain
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
publicNodesWorker nds namedChain appConfig db =
  (*>)
    <$> workerForSource (DataSource_BlockscaleNode $ BlockscaleNode namedChain)
    <*> workerForSource (DataSource_TzScan $ TzScanNode namedChain)

  where
    workerForSource source = worker' $ updatePublicNodeInDb source *> waitForNewHead nds

    getHeadFromSource :: DataSource -> IO (Either RpcError VeryBlockLike)
    getHeadFromSource = \case
      DataSource_TzScan node -> second mkVeryBlockLike <$> getHeadFromNode node
      DataSource_BlockscaleNode node -> second mkVeryBlockLike <$> getHeadFromNode node
      DataSource_PlainNode node -> second mkVeryBlockLike <$> getHeadFromNode node

    getHeadFromNode :: QueryBlock (QDataSource node) => node -> IO (Either RpcError (BlockType (QDataSource node)))
    getHeadFromNode = runExceptT . querySource (rHead $ _nodeDataSource_chain nds) (_nodeDataSource_httpMgr nds)

    updatePublicNodeInDb :: DataSource -> IO ()
    updatePublicNodeInDb source = getHeadFromSource source >>= \case
      Left e -> sayErr (tshow e)
      Right b -> do
        updateLatestHead nds b
        let
          sourceJson = Json source
          bLevel = b ^. level
          bHash = b ^. hash
          bFitness = b ^. fitness
          bBakedAt = b ^. timestamp
        runNoLoggingT $ runDb (Identity db) $ do
          updatedRecord :: Maybe (Id PublicNodeHead) <- listToMaybe . stripOnly <$> [queryQ|
            INSERT INTO "PublicNodeHead"
              ("source", "headLevel", "headBlockHash", "headBlockFitness", "headBlockBakedAt", updated)
              VALUES (?sourceJson, ?bLevel, ?bHash, ?bFitness, ?bBakedAt, NOW())
            ON CONFLICT ("source") DO UPDATE SET
              "headLevel" = ?bLevel,
              "headBlockHash" = ?bHash,
              "headBlockFitness" = ?bFitness,
              "headBlockBakedAt" = ?bBakedAt,
              updated = NOW()
            RETURNING id
          |]
          for_ updatedRecord $ notifyEntityId NotificationType_Update

updateLatestHead :: (BlockLike blk, MonadIO m) => NodeDataSource -> blk -> m ()
updateLatestHead nds blk = liftIO $ do
  updatedLevel <- atomically $ do
    let latestHeadTVar = _nodeDataSource_latestHead nds
    latestHead <- readTVar latestHeadTVar
    if Just (blk ^. fitness) > latestHead ^? _Just . fitness then do
      writeTVar latestHeadTVar $ Just $ mkVeryBlockLike blk
      pure $ Just $ blk ^. level
    else
      pure Nothing

  for_ updatedLevel $ \lev -> say $ "Saw more recent head: " <> tshow (unRawLevel lev)
