{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Backend.Workers.Node where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Lens (ifor_, view, (^.), (^?), _Just)
import Control.Monad (when)
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.State (execStateT)
import Data.Bifunctor (first, second)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql, (&&.), (=.), (==.))
import qualified Network.HTTP.Client as Http
import Rhyolite.Backend.DB (runDb, selectMap)
import Rhyolite.Backend.DB.PsqlSimple (Only (..), queryQ)
import Rhyolite.Backend.Listen (NotificationType (..), insertAndNotify_, notifyEntityId, updateAndNotify)
import Rhyolite.Backend.Schema (toId)
import Rhyolite.Backend.Schema.Class
import Rhyolite.Schema (Id (..), IdData, Json (..))
import Say (say, sayErr, sayShow)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.History (CachedHistory (..), accumHistory)
import Tezos.NodeRPC (NodeRPCContext (..), PlainNodeStream, RpcError, RpcQuery, rChain, rConnections,
                      rMonitorHeads, rNetworkStat)
import Tezos.NodeRPC.Network (nodeRPC, nodeRPCChunked)
import Tezos.NodeRPC.Sources (AsPublicNodeError, HasPublicNodeContext, PublicNode (..),
                              PublicNodeContext (..), PublicNodeError (..), getCurrentHead, getPublicNodeUri)
import Tezos.Types

import Backend.Alerts (clearBadNodeHeadError, clearInaccessibleEndpointError, clearNodeWrongChainError,
                       reportBadNodeHeadError, reportInaccessibleEndpointError, reportNodeWrongChainError)
import Backend.CachedNodeRPC
import Backend.Common (unsupervisedWorkerWithDelay, worker', workerWithDelay)
import Backend.Config (AppConfig (..))
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

haveNewHead :: BlockLike blk => NodeDataSource -> Maybe PublicNode -> URI -> blk -> IO ()
haveNewHead nds pn nodeAddr headBlockInfo = do
  let httpMgr = _nodeDataSource_httpMgr nds
  let chainId = _nodeDataSource_chain nds
  let cacheVar = _nodeDataSource_history nds
  oldHead <- runReaderT dataSourceHead nds
  newBlock <- modifyMVar cacheVar $ \cache -> do
    let newBlock = Map.member (headBlockInfo ^. hash) (_cachedHistory_blocks cache)
    newStateRsp :: Either PublicNodeError CachedHistory' <- runExceptT $ flip runReaderT (PublicNodeContext (NodeRPCContext httpMgr $ Uri.render nodeAddr) pn) $ flip execStateT cache $ do
      acc <- accumHistory nodeMonitorBranchProgess chainId blockSummary headBlockInfo
      sayShow ("new block", Uri.render nodeAddr, mkVeryBlockLike headBlockInfo, acc)
    case newStateRsp of
      Left e -> sayShow e $> (cache, Left e)
      Right good -> return (good, Right newBlock)

  when ((newBlock == Right True) && (Just (headBlockInfo ^. fitness) > oldHead ^? _Just . fitness)) $ do
    -- say $ "new block from node at " <> Uri.render nodeAddr
    -- say $ T.pack $ show headBlockInfo
    updatedLevel <- atomically $ do
      let latestHeadTVar = _nodeDataSource_latestHead nds
      latestHead <- readTVar latestHeadTVar
      if Just (headBlockInfo ^. fitness) > latestHead ^? _Just . fitness then do
        writeTVar latestHeadTVar $ Just $ mkVeryBlockLike headBlockInfo
        pure $ Just $ headBlockInfo ^. level
      else
        pure Nothing
    for_ updatedLevel $ \lev -> say $ "Saw more recent head: " <> tshow (unRawLevel lev)

nodeMonitor :: ChainId -> NodeDataSource -> AppConfig -> URI -> Id Node -> MonitorBlock -> IO ()
nodeMonitor chainId nds appConfig nodeAddr nodeId headBlockInfo = do
  let httpMgr = _nodeDataSource_httpMgr nds
  oldHead <- runReaderT dataSourceHead nds
  updateNodeDataSource nds nodeAddr headBlockInfo
  haveNewHead nds Nothing nodeAddr headBlockInfo

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

updateNetworkStats :: Http.Manager -> Pool Postgresql -> Id Node -> Node -> IO (Either RpcError ())
updateNetworkStats httpMgr db nid before = do
  after' :: Either RpcError Node <- runExceptT $ flip runReaderT (NodeRPCContext httpMgr $ Uri.render nodeAddr) $ do
    connections <- nodeRPC rConnections
    networkStat <- nodeRPC rNetworkStat

    pure $ before
      { _node_peerCount = Just connections
      , _node_networkStat = networkStat
      }

  case after' of
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
    theseNodeRecords :: Map (Id Node) Node <- runNoLoggingT $ runDb (Identity db) $
      selectMap NodeConstructor (Node_deletedField ==. False)
    -- give them all a chance to

    ifor_ theseNodeRecords $ \nodeId node ->
      updateNetworkStats httpMgr db nodeId node >>= \case
        Left _e -> inDb $ reportNodeInaccessible $ _node_address node
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
      let reconnectDelay = 5
      killMonitor <- unsupervisedWorkerWithDelay reconnectDelay $ do
        let
          nodeQuery :: RpcQuery a -> IO (Either RpcError a)
          nodeQuery f = runExceptT $ runReaderT (nodeRPC f) $ NodeRPCContext httpMgr $ Uri.render nodeAddr
          chunkedNodeQuery :: PlainNodeStream a -> (a -> IO ()) -> IO (Either RpcError ()) --(Either RpcError a)
          chunkedNodeQuery f k = runExceptT $ runReaderT (nodeRPCChunked f k) $ NodeRPCContext httpMgr $ Uri.render nodeAddr

        chunkedNodeQuery (rMonitorHeads chainId) $ \block -> do
          -- Since we receive a new head, we can clear connectivity and wrong-chain errors for this node.
          inDb $ do
            clearNodeInaccessible nodeAddr
            clearNodeWrongChainError nodeAddr

          nodeMonitor chainId nds appConfig nodeAddr nodeId block

        nodeQuery rChain >>= inDb . \case
          Left _e -> reportNodeInaccessible nodeAddr -- We have clear evidence that there are connectivity issues.
          Right actualChainId
            | actualChainId == chainId -> do
                -- Monitor stopped even though we're on the right chain, so we'll assume there was a connectivity issue.
                clearNodeWrongChainError nodeAddr
                reportNodeInaccessible nodeAddr
            | otherwise -> reportNodeWrongChainError nodeAddr chainId actualChainId

      let cleanup = killMonitor *> modifyMVar_ nodePool (pure . Map.delete nodeAddr)
      liftIO $ modifyMVar_ nodePool $ pure . Map.insert nodeAddr cleanup
      liftIO $ addFinalizer cleanup
      say $ "start monitor on " <> Uri.render nodeAddr

  where
    inDb = runNoLoggingT . runDb (Identity db) . flip runReaderT appConfig
    reportNodeInaccessible = reportInaccessibleEndpointError EndpointType_Node
    clearNodeInaccessible = clearInaccessibleEndpointError EndpointType_Node
type DataSource = (PublicNode, Either NamedChain ChainId, URI)

publicNodesWorker
  :: NodeDataSource
  -> NamedChain
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
publicNodesWorker nds namedChain appConfig db =

  foldMap workerForSource [ minBound .. maxBound ]

  where
    chain = _nodeDataSource_chain nds

    queryPublicNode :: forall a. (forall e r m.
      ( MonadIO m
      , MonadError e m , AsPublicNodeError e
      , MonadReader r m, HasPublicNodeContext r
      ) => m a) -> DataSource -> IO (Either PublicNodeError a)
    queryPublicNode k (pn, nc, uri) = runExceptT $ runReaderT k $ PublicNodeContext (NodeRPCContext (_nodeDataSource_httpMgr nds) (Uri.render uri)) (Just pn)

    workerForSource :: PublicNode -> IO (IO ())
    workerForSource source = worker' $ updatePublicNodeInDb source' *> waitForNewHeadWithTimeout nds
      where
        source' = (source, Left namedChain, getPublicNodeUri source namedChain)

    getHeadFromSource :: DataSource -> IO (Either PublicNodeError VeryBlockLike)
    getHeadFromSource = queryPublicNode $ getCurrentHead chain

    updatePublicNodeInDb :: DataSource -> IO ()
    updatePublicNodeInDb dsrc@(source, chain, uri) = getHeadFromSource dsrc >>= \case
      Left e -> sayErr (tshow e)
      Right b -> do
        haveNewHead nds (Just source) uri b
        let
          bChain = NamedChainOrChainId chain
          bLevel = b ^. level
          bHash = b ^. hash
          bFitness = b ^. fitness
          bBakedAt = b ^. timestamp
        runNoLoggingT $ runDb (Identity db) $ do
          updatedRecord :: Maybe (Id PublicNodeHead) <- listToMaybe . stripOnly <$> [queryQ|
            INSERT INTO "PublicNodeHead"
              ("source", "chain", "headLevel", "headBlockHash", "headBlockFitness", "headBlockBakedAt", updated)
              VALUES (?source, ?bChain, ?bLevel, ?bHash, ?bFitness, ?bBakedAt, NOW())
            ON CONFLICT ("source", "chain") DO UPDATE SET
              "headLevel" = ?bLevel,
              "headBlockHash" = ?bHash,
              "headBlockFitness" = ?bFitness,
              "headBlockBakedAt" = ?bBakedAt,
              updated = NOW()
            RETURNING id
          |]
          for_ updatedRecord $ notifyEntityId NotificationType_Update

nodeAlertWorker
  :: NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
nodeAlertWorker nds appConfig db = worker' $ waitForNewHead nds >>= \latestHead -> do
  nodes <- readMVar (_nodeDataSource_nodes nds)
  ifor_ nodes $ \nodeUri nodeHead' -> for_ nodeHead' $ \nodeHead -> do
    lcaBlock' <- flip runReaderT nds $ branchPoint (nodeHead ^. hash) (latestHead ^. hash)

    runNoLoggingT $ runDb (Identity db) $ flip runReaderT appConfig $ do
      nodeId' <- fmap toId . listToMaybe <$> project AutoKeyField ((Node_addressField ==. nodeUri &&. Node_deletedField ==. False) `limitTo` 1)
      case nodeId' of
        Nothing -> sayErr $ "Node must have been deleted at address " <> Uri.render nodeUri
        Just nodeId -> case lcaBlock' of
          Nothing -> reportBadNodeHeadError nodeId latestHead nodeHead (Nothing :: Maybe VeryBlockLike)
          Just lcaBlock -> do
            let
              -- Two cases to consider:
              --   * Node is behind, so the LCA block and node block will be the same
              --   * Node is branched, so the LCA block will be behind both the node *and* the latest
              levelsBehindHead = latestHead ^. level - lcaBlock ^. level

            if levelsBehindHead > 1
              then reportBadNodeHeadError nodeId latestHead nodeHead (Just lcaBlock)
              else clearBadNodeHeadError nodeId


updateLatestHead :: (BlockLike blk, MonadIO m) => NodeDataSource -> blk -> m ()
updateLatestHead nds blk = liftIO $ do
  latestBlock' <- atomically $ do
    let latestHeadTVar = _nodeDataSource_latestHead nds
    latestHead <- readTVar latestHeadTVar
    if Just (blk ^. fitness) > latestHead ^? _Just . fitness then do
      writeTVar latestHeadTVar $ Just $ mkVeryBlockLike blk
      pure $ Just $ mkVeryBlockLike blk
    else
      pure Nothing

  for_ latestBlock' $ \latestBlock ->
    say $ "Saw more recent head: " <> tshow (unRawLevel $ latestBlock ^. level)
