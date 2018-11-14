{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Workers.Node where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebug, logErrorSH, logInfo, logInfoSH, logWarnSH)
import Control.Monad.State (execStateT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Pool (Pool)
import Data.Time (NominalDiffTime)
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql, isFieldNothing, (&&.), (=.), (==.))
import qualified Network.HTTP.Client as Http
import Rhyolite.Backend.DB (getTime, runDb, selectMap)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Rhyolite.Backend.Schema (toId)
import Rhyolite.Schema (Id (..))
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.History (CachedHistory (..), accumHistory)
import Tezos.NodeRPC (NodeRPCContext (..), PlainNodeStream, RpcError, RpcQuery, rChain, rConnections,
                      rMonitorHeads, rNetworkStat)
import Tezos.NodeRPC.Network (PublicNodeContext (..), getCurrentHead, nodeRPC, nodeRPCChunked)
import Tezos.NodeRPC.Sources (PublicNode (..), PublicNodeError (..))
import Tezos.Types

import Backend.Alerts (clearBadNodeHeadError, clearInaccessibleNodeError, clearNodeWrongChainError,
                       reportBadNodeHeadError, reportInaccessibleNodeError, reportNodeWrongChainError)
import Backend.CachedNodeRPC
import Backend.Common (unsupervisedWorkerWithDelay, worker', workerWithDelay)
import Backend.Config (AppConfig (..))
import Backend.Schema
import Backend.Supervisor (withTermination)
import Common.Schema
import ExtraPrelude

-- We assume that the implicit nodeaddr is the same one we just learned the new
-- branch from, so we insist that we bootstrap from it (rather than using a
-- pool of nodes)

-- TODO: make this "configurable" implementation idea:  we could partition
-- history into horizontal level regions (say, every 10k levels) and require
-- each "slice" start on a boundary, and contain only the blocks within their
-- assigned slice.
minCachedBlockLevel :: RawLevel
minCachedBlockLevel = 1

nodeMonitorBranchProgess :: (MonadLogger m) => BlockHash -> BlockHash -> Int -> Int -> m ()
nodeMonitorBranchProgess branch current i n = when (i `mod` 1000 == 0) $ $(logInfoSH) ("catching up" :: Text, branch, current, i, n)

haveNewHead :: (MonadIO m, BlockLike blk) => NodeDataSource -> Maybe PublicNode -> URI -> blk -> m ()
haveNewHead nds pn nodeAddr headBlockInfo = runLoggingEnv (_nodeDataSource_logger nds) $ do
  let httpMgr = _nodeDataSource_httpMgr nds
  let chainId = _nodeDataSource_chain nds
  let cacheVar = _nodeDataSource_history nds
  oldHead <- runReaderT dataSourceHead nds
  newBlock <- liftIO $ modifyMVar cacheVar $ \cache -> runLoggingEnv (_nodeDataSource_logger nds) $ do
    let newBlock = not $ Map.member (headBlockInfo ^. hash) (_cachedHistory_blocks cache)
    newStateRsp :: Either PublicNodeError CachedHistory' <- runExceptT $
      flip runReaderT (PublicNodeContext (NodeRPCContext httpMgr $ Uri.render nodeAddr) pn) $
        flip execStateT cache $ do
          _ <- accumHistory nodeMonitorBranchProgess chainId (const ()) headBlockInfo
          $(logInfoSH) (if newBlock then "new block" else "known block" :: Text, pn, Uri.render nodeAddr, mkVeryBlockLike headBlockInfo)
    case newStateRsp of
      Left e -> $(logWarnSH) e $> (cache, Left e)
      Right good -> return (good, Right newBlock)

  when ((newBlock == Right True) && (Just (headBlockInfo ^. fitness) > oldHead ^? _Just . fitness)) $ do
    updatedLevel <- liftIO $ atomically $ do
      let latestHeadTVar = _nodeDataSource_latestHead nds
      latestHead <- readTVar latestHeadTVar
      if Just (headBlockInfo ^. fitness) > latestHead ^? _Just . fitness then do
        writeTVar latestHeadTVar $ Just $ mkVeryBlockLike headBlockInfo
        pure $ Just $ headBlockInfo ^. level
      else
        pure Nothing
    for_ updatedLevel $ \lev -> $(logDebug) $ "Saw more recent head: " <> tshow (unRawLevel lev)

nodeMonitor :: NodeDataSource -> AppConfig -> URI -> Id Node -> MonitorBlock -> IO ()
nodeMonitor nds appConfig nodeAddr nodeId headBlockInfo = do
  updateNodeDataSource nds nodeAddr headBlockInfo
  haveNewHead nds Nothing nodeAddr headBlockInfo

  let db = _nodeDataSource_pool nds
  runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $ flip runReaderT appConfig $ do
    -- This isn't very nuanced: old, stale nodes, even if they are catching
    -- up, will churn a lot here.  Maybe we could improve this to filter
    -- out "new" blocks that are already on the branch of `oldHead`?
    now <- getTime
    updateId nodeId
      [ Node_headLevelField =. Just (headBlockInfo ^. monitorBlock_level)
      , Node_headBlockHashField =. Just (headBlockInfo ^. monitorBlock_hash)
      , Node_headBlockBakedAtField =. Just (headBlockInfo ^. monitorBlock_timestamp)
      , Node_fitnessField =. Just (headBlockInfo ^. monitorBlock_fitness)
      , Node_updatedField =. Just now
      , Node_headBlockPredField =. Just (headBlockInfo ^. monitorBlock_predecessor)
      ]
    getId nodeId >>= traverse_ (notify . Notify_Node nodeId)

updateNetworkStats :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => Http.Manager -> Pool Postgresql -> Id Node -> Node -> m (Either RpcError ())
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
      when (before /= after) $ inDb $ do
        updateId nid
          [ Node_peerCountField =. _node_peerCount after
          , Node_networkStatField =. _node_networkStat after
          ]
        getId nid >>= traverse_ (notify . Notify_Node nid)
      pure $ Right ()

  where
    nodeAddr = _node_address before
    inDb = runDb (Identity db)

nodeWorker
  :: NominalDiffTime -- delay between checking for updates, in microseconds
  -> NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
nodeWorker delay nds appConfig db = runLoggingEnv (_nodeDataSource_logger nds) $ withTermination $ \addFinalizer -> do
  nodePool :: MVar (Map URI (IO ())) <- newMVar mempty
  let httpMgr = _nodeDataSource_httpMgr nds
  -- IO
  workerWithDelay (pure delay) $ const $ (runLoggingEnv :: LoggingEnv -> LoggingT IO () -> IO ()) (_nodeDataSource_logger nds) $ do
    $(logDebug) "Update node cycle."

    -- read the persistent list of nodes
    theseNodeRecords :: Map (Id Node) Node <- runDb (Identity db) $
      selectMap NodeConstructor (Node_deletedField ==. False)
    -- give them all a chance to

    ifor_ theseNodeRecords $ \nodeId node ->
      updateNetworkStats httpMgr db nodeId node >>= \case
        Left _e -> inDb $ reportInaccessibleNodeError nodeId
        Right () -> pure () -- We'll rely on the block monitor to clear this error

    let theseNodes = Map.fromList $ fmap (\(i, n) -> (_node_address n, (i, _node_alias n))) $ Map.toList theseNodeRecords

    -- we may need to bootstrap our parameters.  if the cache.parameters var is empty, lets try to fill it with the nodes we currently have
    _ <- liftIO $ initParams nds $ (,) <$> pure Nothing <*> Map.keys theseNodes

    thoseNodes <- liftIO $ readMVar nodePool
    let newNodes = theseNodes `Map.difference` thoseNodes
    let staleNodes = thoseNodes `Map.difference` theseNodes

    ifor_ staleNodes $ \nodeAddr killMonitor ->
      $(logInfo) ("stop monitor on " <> Uri.render nodeAddr) *> liftIO killMonitor

    let
      chainId = _nodeDataSource_chain nds

    ifor_ newNodes $ \nodeAddr (nodeId, _nodeAlias) -> do
      let reconnectDelay = 5
      killMonitor <- unsupervisedWorkerWithDelay reconnectDelay $ runLoggingEnv (_nodeDataSource_logger nds) $ do
        let
          nodeQuery :: RpcQuery a -> IO (Either RpcError a)
          nodeQuery f = runLoggingEnv (_nodeDataSource_logger nds) $ runExceptT $ runReaderT (nodeRPC f) $ NodeRPCContext httpMgr $ Uri.render nodeAddr
          chunkedNodeQuery :: PlainNodeStream a -> (a -> IO ()) -> IO (Either RpcError ()) --(Either RpcError a)
          chunkedNodeQuery f k = runLoggingEnv (_nodeDataSource_logger nds) $ runExceptT $ runReaderT (nodeRPCChunked f k) $ NodeRPCContext httpMgr $ Uri.render nodeAddr

        _ <- liftIO $ chunkedNodeQuery (rMonitorHeads chainId) $ \block -> do
          -- Since we receive a new head, we can clear connectivity and wrong-chain errors for this node.
          runLoggingEnv (_nodeDataSource_logger nds) $ inDb $ do
            clearInaccessibleNodeError nodeId
            clearNodeWrongChainError nodeId

          nodeMonitor nds appConfig nodeAddr nodeId block

        liftIO (nodeQuery rChain) >>= inDb . \case
          Left _e -> reportInaccessibleNodeError nodeId -- We have clear evidence that there are connectivity issues.
          Right actualChainId
            | actualChainId == chainId -> do
                -- Monitor stopped even though we're on the right chain, so we'll assume there was a connectivity issue.
                clearNodeWrongChainError nodeId
                reportInaccessibleNodeError nodeId
            | otherwise -> reportNodeWrongChainError nodeId chainId actualChainId

      let cleanup = killMonitor *> modifyMVar_ nodePool (pure . Map.delete nodeAddr)
      liftIO $ modifyMVar_ nodePool $ pure . Map.insert nodeAddr cleanup
      liftIO $ addFinalizer cleanup
      $(logInfo) $ "start monitor on " <> Uri.render nodeAddr

  where
    inDb = runDb (Identity db) . flip runReaderT appConfig

type DataSource = (PublicNode, Either NamedChain ChainId, URI)

publicNodesWorker
  :: NodeDataSource
  -> [DataSource]
  -> IO (IO ())
publicNodesWorker nds = foldMap workerForSource
  where
    workerForSource :: DataSource -> IO (IO ())
    workerForSource source = worker' $ updateDataSource nds source *> waitForNewHeadWithTimeout nds

updateDataSource
  :: forall m. (MonadIO m, MonadBaseControl IO m)
  => NodeDataSource -> DataSource -> m ()
updateDataSource nds (pn, chain, uri) = do
  enabled <- publicNodeEnabled
  -- TODO: prefer to get this from the database, or from private nodes before
  when enabled $ do
    _ <- liftIO $ initParams nds (Identity (Just pn, uri))
    updatePublicNodeInDb

  where
    db = _nodeDataSource_pool nds
    chainId = _nodeDataSource_chain nds

    queryPublicNode
      :: forall a. ReaderT PublicNodeContext (ExceptT PublicNodeError m) a
      -> m (Either PublicNodeError a)
    queryPublicNode k = runExceptT $
      runReaderT k $
        PublicNodeContext (NodeRPCContext (_nodeDataSource_httpMgr nds) (Uri.render uri)) (Just pn)

    getHeadFromSource :: m (Either PublicNodeError VeryBlockLike)
    getHeadFromSource = queryPublicNode $ runLoggingEnv (_nodeDataSource_logger nds) $ getCurrentHead chainId

    publicNodeEnabled :: m Bool
    publicNodeEnabled = fmap (fromMaybe False . listToMaybe) $
      runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $
        project PublicNodeConfig_enabledField (PublicNodeConfig_sourceField ==. pn)

    updatePublicNodeInDb :: m ()
    updatePublicNodeInDb = getHeadFromSource >>= runLoggingEnv (_nodeDataSource_logger nds) . \case
      Left e -> $(logErrorSH) e
      Right b -> do
        haveNewHead nds (Just pn) uri b
        runDb (Identity db) $ do
          let chainField = NamedChainOrChainId chain
          now <- getTime
          eid' :: Maybe (Id PublicNodeHead) <-
            fmap toId . listToMaybe <$> project AutoKeyField (PublicNodeHead_chainField ==. chainField &&. PublicNodeHead_sourceField ==. pn)
          case eid' of
            Nothing -> do
              let
                pnh = PublicNodeHead
                  { _publicNodeHead_source = pn
                  , _publicNodeHead_chain = chainField
                  , _publicNodeHead_headBlock = b
                  , _publicNodeHead_updated = now
                  }
              notify . flip Notify_PublicNodeHead (Just pnh) =<< insert' pnh
            Just eid -> do
              updateId eid
                [ PublicNodeHead_headBlockField =. b
                , PublicNodeHead_updatedField =. now
                ]
              notify . Notify_PublicNodeHead eid =<< getId eid

nodeAlertWorker
  :: NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
nodeAlertWorker nds appConfig db = worker' $ waitForNewHead nds >>= \latestHead -> do
  nodeHeadHashes <- fmap (Map.mapMaybe _node_headBlockHash) $ runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $
    selectMap NodeConstructor (Node_deletedField ==. False &&. Not (isFieldNothing Node_headBlockHashField))

  ifor_ nodeHeadHashes $ \nodeId nodeHeadHash -> do
    nodeHeadAndLca :: Either RpcError (Block, Maybe VeryBlockLike)
      <- flip runReaderT nds $ runExceptT $ do
        nodeHead <- nodeQueryDataSource (NodeQuery_Block nodeHeadHash)
        bp <- branchPoint (nodeHead ^. hash) (latestHead ^. hash)
        pure (nodeHead, bp)

    for_ nodeHeadAndLca $ \(nodeHead, lcaBlock') -> runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $ flip runReaderT appConfig $
      case lcaBlock' of
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
updateLatestHead nds blk = runLoggingEnv (_nodeDataSource_logger nds) $ do
  latestBlock' <- liftIO $ atomically $ do
    let latestHeadTVar = _nodeDataSource_latestHead nds
    latestHead <- readTVar latestHeadTVar
    if Just (blk ^. fitness) > latestHead ^? _Just . fitness then do
      writeTVar latestHeadTVar $ Just $ mkVeryBlockLike blk
      pure $ Just $ mkVeryBlockLike blk
    else
      pure Nothing

  for_ latestBlock' $ \latestBlock ->
    $(logInfo) $ "Saw more recent head: " <> tshow (unRawLevel $ latestBlock ^. level)
