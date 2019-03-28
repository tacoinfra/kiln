{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Workers.Node where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (atomically, readTVar, readTVarIO, writeTQueue, writeTVar)
import Control.Monad.Except (ExceptT, runExceptT, forM_, forM)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebug, logErrorSH, logInfo, logInfoSH, logWarnSH)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
import Data.Align
import Data.Functor.Apply
import qualified Data.LCA.Online.Polymorphic as LCA
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Pool (Pool)
import Data.These
import Data.Time (NominalDiffTime, diffUTCTime)
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql, in_, isFieldNothing, (&&.), (=.), (==.))
import qualified Network.HTTP.Client as Http
import Reflex.Class (fmapMaybe)
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (getTime, runDb, selectMap, project1)
import Rhyolite.Backend.DB.PsqlSimple (executeQ)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Rhyolite.Backend.Schema (toId, fromId)
import Rhyolite.Schema (Id (..))
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.History (AccumHistoryContext (..), CachedHistory (..), accumHistory)
import Tezos.NodeRPC (NodeRPCContext (..), PlainNodeStream, RpcError, RpcQuery, rChain, rConnections,
                      rMonitorHeads, rNetworkStat)
import Tezos.NodeRPC.Network (PublicNodeContext (..), getCurrentHead, nodeRPC, nodeRPCChunked)
import Tezos.NodeRPC.Sources (PublicNode (..), PublicNodeError (..))
import Tezos.Types

import Backend.Alerts (clearBadNodeHeadError, clearInaccessibleNodeError, clearNodeWrongChainError,
                       reportBadNodeHeadError, reportInaccessibleNodeError, reportNodeWrongChainError,
                       reportNodeInvalidPeerCountError, clearNodeInvalidPeerCountError)
import Backend.CachedNodeRPC
import Backend.Common (unsupervisedWorkerWithDelay, threadDelay', worker', workerWithDelay, timeout')
import Backend.Config (AppConfig (..), kilnNodeURI)
import Backend.Schema
import Backend.Supervisor (withTermination)
import Backend.STM (atomicallyWith)
import Common.Schema
import ExtraPrelude

-- We assume that the implicit nodeaddr is the same one we just learned the new
-- branch from, so we insist that we bootstrap from it (rather than using a
-- pool of nodes)

haveNewHead :: (MonadIO m, BlockLike blk) => NodeDataSource -> Maybe PublicNode -> URI -> blk -> m ()
haveNewHead nds pn nodeAddr headBlockInfo = runLoggingEnv (_nodeDataSource_logger nds) $ do
  let
    httpMgr = _nodeDataSource_httpMgr nds
    chainId = _nodeDataSource_chain nds
    historyVar = _nodeDataSource_history nds
  (oldHead, history) <- liftIO $ atomically $ liftA2 (,) (dataSourceHead nds) (readTVar historyVar)
  newBlock <- do
    let newBlock = not $ Map.member (headBlockInfo ^. hash) (_cachedHistory_blocks history)
    newStateRsp :: Either PublicNodeError () <- runExceptT $
      flip runReaderT (AccumHistoryContext historyVar $ PublicNodeContext (NodeRPCContext httpMgr $ Uri.render nodeAddr) pn) $ do
        accumHistory chainId (const ()) headBlockInfo
        $(logInfoSH) (if newBlock then "new block" else "known block" :: Text, pn, Uri.render nodeAddr, mkVeryBlockLike headBlockInfo)

    case newStateRsp of
      Left e -> $(logWarnSH) e $> Left e
      Right () -> pure $ Right newBlock

  when ((newBlock == Right True) && (Just (headBlockInfo ^. fitness) > oldHead ^? _Just . fitness)) $ do
    updatedLevel <- liftIO $ atomically $ do
      let latestHeadTVar = _nodeDataSource_latestHead nds
      latestHead <- readTVar latestHeadTVar
      if Just (headBlockInfo ^. fitness) > latestHead ^? _Just . fitness
        then do
          writeTVar latestHeadTVar $ Just $ mkVeryBlockLike headBlockInfo
          pure $ Just $ headBlockInfo ^. level
        else
          pure Nothing
    for_ updatedLevel $ \lev -> $(logDebug) $ "Saw more recent head: " <> tshow (unRawLevel lev)

nodeMonitor :: NodeDataSource -> AppConfig -> URI -> Id Node -> MonitorBlock -> IO ()
nodeMonitor nds appConfig nodeAddr nodeId headBlockInfo = do
  atomically $ do
    updateNodeDataSource nds nodeAddr headBlockInfo
    writeTQueue (_nodeDataSource_ioQueue nds) $ haveNewHead nds Nothing nodeAddr headBlockInfo

  let db = _nodeDataSource_pool nds
  runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $ flip runReaderT appConfig $ do
    -- This isn't very nuanced: old, stale nodes, even if they are catching
    -- up, will churn a lot here.  Maybe we could improve this to filter
    -- out "new" blocks that are already on the branch of `oldHead`?
    now <- getTime
    let newHash = headBlockInfo ^. hash
        newLevel = headBlockInfo ^. level
        chainId = _nodeDataSource_chain nds
     in void $ [executeQ|
          insert into "BlockTodo" (hash, level, chain, "claimedBy", "claimedAt", "parsedParent", "parsedAccusations")
          values (?newHash, ?newLevel, ?chainId, null, null, false, false)
          on conflict do nothing
          |]
    let p = (NodeDetails_dataField ~>)
    project NodeDetails_idField (NodeDetails_idField `in_` [nodeId]) >>= \case
      [] -> insert $ NodeDetails
        { _nodeDetails_id = nodeId
        , _nodeDetails_data = mkNodeDetails
          { _nodeDetailsData_headLevel = Just (headBlockInfo ^. monitorBlock_level)
          , _nodeDetailsData_headBlockHash = Just (headBlockInfo ^. monitorBlock_hash)
          , _nodeDetailsData_headBlockBakedAt = Just (headBlockInfo ^. monitorBlock_timestamp)
          , _nodeDetailsData_fitness = Just (headBlockInfo ^. monitorBlock_fitness)
          , _nodeDetailsData_updated = Just now
          , _nodeDetailsData_headBlockPred = Just (headBlockInfo ^. monitorBlock_predecessor)
          }
        }
      (_:_) -> update
        [ p NodeDetailsData_headLevelSelector =. Just (headBlockInfo ^. monitorBlock_level)
        , p NodeDetailsData_headBlockHashSelector =. Just (headBlockInfo ^. monitorBlock_hash)
        , p NodeDetailsData_headBlockBakedAtSelector =. Just (headBlockInfo ^. monitorBlock_timestamp)
        , p NodeDetailsData_fitnessSelector =. Just (headBlockInfo ^. monitorBlock_fitness)
        , p NodeDetailsData_updatedSelector =. Just now
        , p NodeDetailsData_headBlockPredSelector =. Just (headBlockInfo ^. monitorBlock_predecessor)
        ]
        (NodeDetails_idField `in_` [nodeId])
    newNodeDetails <- project NodeDetails_dataField $ (NodeDetails_idField ==. nodeId) `limitTo` 1
    traverse_ (notify NotifyTag_NodeDetails . (nodeId,) . Just) newNodeDetails

updateNetworkStats
  :: (MonadIO m, MonadLogger m, MonadBaseNoPureAborts IO m)
  => AppConfig
  -> Http.Manager
  -> Pool Postgresql
  -> Id Node
  -> NodeData
  -> NodeDetailsData
  -> m (Either RpcError ())
updateNetworkStats appConfig httpMgr db nid node before = runExceptT $ do
  after :: NodeDetailsData <- flip runReaderT (NodeRPCContext httpMgr $ Uri.render (nodeData_address appConfig node)) $ do
    connections <- nodeRPC rConnections
    networkStat <- nodeRPC rNetworkStat
    pure $ before
      { _nodeDetailsData_peerCount = Just connections
      , _nodeDetailsData_networkStat = networkStat
      }

  let
    inDb = runDb (Identity db)

  -- We will rely on the block monitor to clear any inaccessible endpoint errors
  -- for this node.m
  when (before /= after) $ lift $ inDb $ do
    let
      minPeerCount = nodeData_minPeerConnections node
    for_ (_nodeDetailsData_peerCount after) $ \peerCount -> do
      flip runReaderT appConfig $
        if (peerCount < fromIntegral minPeerCount)
          then reportNodeInvalidPeerCountError nid minPeerCount peerCount
          else clearNodeInvalidPeerCountError nid

    let p = (NodeDetails_dataField ~>)
    update
      [ p NodeDetailsData_peerCountSelector =. _nodeDetailsData_peerCount after
      , p NodeDetailsData_networkStatSelector =. _nodeDetailsData_networkStat after
      ]
      (NodeDetails_idField ==. nid)
    project NodeDetails_dataField (NodeDetails_idField ==. nid) >>= traverse_ (notify NotifyTag_NodeDetails . (nid,) . Just)
  pure ()

type NodeData = Either (Id ProcessData) NodeExternalData
nodeData_address :: AppConfig -> NodeData -> URI
nodeData_address appConfig = either (const $ kilnNodeURI appConfig) _nodeExternalData_address

nodeData_minPeerConnections :: NodeData -> Int
nodeData_minPeerConnections = either (const 0) (fromMaybe 0 . _nodeExternalData_minPeerConnections)

nodeData_alias :: NodeData -> Maybe Text
nodeData_alias = either (const $ Just "Kiln managed node") _nodeExternalData_alias


-- | Select from all the tables that have to do with join.
--
-- TODO do join in database, not Haskell. Also don't get all the data
getNodes
  :: ( MonadIO m, MonadLogger m, MonadBaseNoPureAborts IO m
     , HasSelectOptions cond Postgresql (RestrictionHolder NodeDetails NodeDetailsConstructor)
     )
  => Pool Postgresql
  -> cond
  -> m (Map (Id Node) (Node, NodeData, NodeDetailsData))
getNodes db constraints = do
  (nodeIds, nodeEs, nodeIs, nodeDs) :: ( Map (Id Node) Node
                               , Map (Id Node) NodeExternalData
                               , Map (Id Node) (Id ProcessData)
                               , Map (Id Node) NodeDetailsData
                               )
    <- runDb (Identity db) $ (,,,)
      <$> (selectMap NodeConstructor CondEmpty)
      <*> (Map.fromList <$> project
            ( NodeExternal_idField
            , NodeExternal_dataField ~> DeletableRow_dataSelector)
            (NodeExternal_dataField ~> DeletableRow_deletedSelector ==. False))
      <*> (Map.fromList <$> project
            ( NodeInternal_idField
            , NodeInternal_dataField ~> DeletableRow_dataSelector)
            (NodeInternal_dataField ~> DeletableRow_deletedSelector ==. False))
      <*> (Map.fromList <$> project
            (NodeDetails_idField, NodeDetails_dataField)
            constraints)

  let nodeIEs :: Map (Id Node) NodeData = fmap Left nodeIs `Map.union` fmap Right nodeEs

  pure $ fmapMaybe id $ alignWith
    (these (Just . ($ mkNodeDetails)) (const Nothing) (\f a -> Just $ f a))
    ((,,) <$> nodeIds <.> nodeIEs) nodeDs

nodeWorker
  :: NominalDiffTime -- delay between checking for updates, in microseconds
  -> NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
nodeWorker delay nds appConfig db = runLoggingEnv (_nodeDataSource_logger nds) $ withTermination $ \addFinalizer -> do
  nodePool :: MVar (Map URI (IO ())) <- newMVar mempty
  let httpMgr = _nodeDataSource_httpMgr nds
  workerWithDelay (pure delay) $ const $ (runLoggingEnv :: LoggingEnv -> LoggingT IO () -> IO ()) (_nodeDataSource_logger nds) $ do
    $(logDebug) "Update node cycle."

    -- read the persistent list of nodes
    theseNodeRecords <- getNodes db CondEmpty

    -- give them all a chance to
    ifor_ theseNodeRecords $ \nodeId (Node, node, nodeDetails) ->
      updateNetworkStats appConfig httpMgr db nodeId node nodeDetails >>= \case
        Left _e -> inDb $ reportInaccessibleNodeError nodeId
        Right () -> pure () -- We'll rely on the block monitor to clear this error

    let theseNodes = Map.fromList $ fmap (\(i, (_, nE, _)) -> (nodeData_address appConfig nE, (i, nodeData_alias nE))) $ Map.toList theseNodeRecords

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
    workerForSource source = worker' $ do
      let (pn, chain, _) = source
      updateDataSource nds source
      (now, mLastBlock) <- runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity $ _nodeDataSource_pool nds) $ do
        now <- getTime
        mLastBlock <- project1 PublicNodeHead_headBlockField $
          PublicNodeHead_sourceField ==. pn &&. PublicNodeHead_chainField ==. NamedChainOrChainId chain
        pure (now, mLastBlock)
      timeBetweenBlocks <- maybe 60 calcTimeBetweenBlocks <$> readTVarIO (_nodeDataSource_parameters $ nds ^. nodeDataSource)
      let secsSinceLastBlock = maybe 0 (\v -> _veryBlockLike_timestamp v `diffUTCTime` now) mLastBlock
          secsTillNextBlock = case secsSinceLastBlock + timeBetweenBlocks of
              -- if the next expected block is in the past, the node is probably quite laggy and we give it a little more delay
            x | x <= 0 -> timeBetweenBlocks / 2
              | otherwise -> x
      _ <- timeout' secsTillNextBlock (waitForNewHead nds)
      threadDelay' 5 -- always give a little extra delay to make it more likely the public node reports the new block

updateDataSource
  :: forall m. (MonadIO m, MonadBaseNoPureAborts IO m)
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
      Left e -> $(logErrorSH) ("updatePublicNodeInDb"::Text,(pn,chain,Uri.render uri),e)
      Right b -> do
        haveNewHead nds (Just pn) uri b
        runDb (Identity db) $ do
          let newHash = b ^. hash
              newLevel = b ^. level
           in void $ [executeQ|
                insert into "BlockTodo" (hash, level, chain, "claimedBy", "claimedAt", "parsedParent", "parsedAccusations")
                values (?newHash, ?newLevel, ?chainId, null, null, false, false)
                on conflict do nothing
                |]
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
              notify NotifyTag_PublicNodeHead . (, Just pnh) =<< insert' pnh
            Just eid -> do
              updateId eid
                [ PublicNodeHead_headBlockField =. b
                , PublicNodeHead_updatedField =. now
                ]
              notify NotifyTag_PublicNodeHead . (eid,) =<< getId eid

{- Send a 'bad branch' alert if either:
 - 1) last common ancestor is at least 3 levels old (on either branch)
 - 2) last common ancestor is 2 levels old (on the best branch) and the node is still on it
 - 3) last common ancestor is 2 levels old (on the best branch) and the best branch's parent was better than what the other branch had on the same level
 -}
nodeAlertWorker
  :: NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
nodeAlertWorker nds appConfig db = worker' $ waitForNewHead nds >>= \latestHead -> do
  nodeHeadHashes <- fmap (Map.mapMaybe $ view $ _3 . nodeDetailsData_headBlockHash) $ runLoggingEnv (_nodeDataSource_logger nds) $ getNodes db (Not (isFieldNothing (NodeDetails_dataField ~> NodeDetailsData_headBlockHashSelector)))

  ifor_ nodeHeadHashes $ \nodeId nodeHeadHash -> do
    action' <- flip runReaderT nds $ runExceptT @CacheError $ do
      nodeHead <- nodeQueryDataSource (NodeQuery_Block nodeHeadHash)
      lcaBlock' <- atomicallyWith $ branchPoint (nodeHead ^. hash) (latestHead ^. hash)
      let bad = reportBadNodeHeadError nodeId latestHead nodeHead lcaBlock'
          good = clearBadNodeHeadError nodeId
      case lcaBlock' of
        Nothing -> return bad
        Just lcaBlock -> do
          let
            -- Two cases to consider:
            --   * Node is behind, so the LCA block and node block will be the same
            --   * Node is branched, so the LCA block will be behind both the node *and* the latest
            levelsBehindHead = latestHead ^. level - lcaBlock ^. level
            levelsBehindNode = nodeHead ^. level - lcaBlock ^. level
          if | max levelsBehindHead levelsBehindNode > 2 -> return bad
             | levelsBehindHead < 2 -> return good
             | levelsBehindNode == 0 -> return bad
             | otherwise -> do
                 history <- liftIO $ readTVarIO $ _nodeDataSource_history nds
                 let parentHash = view _1 $ fromMaybe (error "latest hash should have a parent because it has a grandparent") $ LCA.uncons $ LCA.drop 1 $ fromMaybe (error "latest hash was already looked up once") $ Map.lookup (latestHead ^. hash) $ _cachedHistory_blocks history
                     uncleHash = view _1 $ fromMaybe (error "node hash should have an ancestor at the level above the branch point") $ LCA.uncons $ LCA.drop (fromIntegral $ levelsBehindNode - 1) $ fromMaybe (error "node head hash was already looked up once") $ Map.lookup (nodeHead ^. hash) $ _cachedHistory_blocks history
                 latestParent <- nodeQueryDataSource (NodeQuery_Block parentHash)
                 latestUncle <- nodeQueryDataSource (NodeQuery_Block uncleHash)
                 if latestParent ^. fitness > latestUncle ^. fitness then return bad else return good
    for_ action' $ \action -> runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $ runReaderT action appConfig

updateLatestHead :: (BlockLike blk, MonadIO m) => NodeDataSource -> blk -> m ()
updateLatestHead nds blk = runLoggingEnv (_nodeDataSource_logger nds) $ do
  latestBlock' <- liftIO $ atomically $ do
    let latestHeadTVar = _nodeDataSource_latestHead nds
    latestHead <- readTVar latestHeadTVar
    if Just (blk ^. fitness) > latestHead ^? _Just . fitness
      then do
        writeTVar latestHeadTVar $ Just $ mkVeryBlockLike blk
        pure $ Just $ mkVeryBlockLike blk
      else
        pure Nothing

  for_ latestBlock' $ \latestBlock ->
    $(logInfo) $ "Saw more recent head: " <> tshow (unRawLevel $ latestBlock ^. level)

-- Monitors changes in protocol, and restarts the baker daemon if running
protocolMonitorWorker
  :: NodeDataSource
  -> Pool Postgresql
  -> IO (IO ())
protocolMonitorWorker nds db = worker' $ waitForNewHead nds >>= \latestHead -> do
  protoInfo <- liftIO $ atomically $ waitForParams nds
  let
    getNextProtocol = getNextProtocol' >>= \case
      Right p -> return p
      Left e -> do
        putStrLn $ "DIVAM: debug: no nextProto: " <> show e
        threadDelay' 1
        getNextProtocol
    getNextProtocol' = flip runReaderT nds $ runExceptT @CacheError $ do
      blk <- nodeQueryDataSource (NodeQuery_Block $ latestHead ^. hash)
      return $ _blockMetadata_nextProtocol $ _block_metadata blk

  nextProto <- getNextProtocol
  liftIO $ putStrLn $ "DIVAM: debug nextProto:" <> (show nextProto)
  let
    inDb :: DbPersist Postgresql (LoggingT IO) a -> IO a
    inDb m = runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) m
  processes <- inDb $ do
    let ds = BakerDaemonInternal_dataField ~> DeletableRow_dataSelector
    project1 ds CondEmpty >>= \case
      Nothing -> return Nothing
      Just (BakerDaemonInternalData _ _ _ mp bpid epid) -> (join <$>) $ forM mp $ \proto -> if proto == nextProto
        then return Nothing
        else do
          update [ds ~> BakerDaemonInternalData_protocolSelector =. Just nextProto] CondEmpty
          let processes = map fromId [bpid, epid]
          r <- project1 ProcessData_runningField $ AutoKeyField `in_` processes
          if r == Just True
            then do
              update [ProcessData_runningField =. False] $ AutoKeyField `in_` processes
              return $ Just processes
            else return Nothing
  forM_ processes $ mapM $ \p -> do
    let
      loop = do
        s <- inDb $ project1 ProcessData_stateField $ AutoKeyField ==. p
        if s == Just ProcessState_Stopped
          then inDb $ update [ProcessData_runningField =. True] $ AutoKeyField ==. p
          else threadDelay' 1 >> loop
    loop

  -- Wait till the end of this cycle
  let
    currentLvl = latestHead ^. level
    nextCycle = 1 + levelToCycle protoInfo currentLvl
    -- resume the worker during the last block of this cycle
    nextCheckLvl = firstLevelInCycle protoInfo nextCycle - 1
    delay = fromInteger $ toInteger (nextCheckLvl - currentLvl) * toInteger oneBlockTime
    oneBlockTime :: TezosWord64
    oneBlockTime = NonEmpty.head $ unPeriodSequence $ _protoInfo_timeBetweenBlocks protoInfo
  liftIO $ putStrLn $ "DIVAM: debug current:" <> (show currentLvl) <> " nextCheckLvl: " <> (show nextCheckLvl)
  liftIO $ putStrLn $ "DIVAM: debug delay:" <> (show delay) <> " oneBlockTime: " <> (show oneBlockTime)
  threadDelay' delay
  return ()
