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
import Control.Concurrent.STM (atomically, readTVar, readTVarIO, writeTQueue, writeTVar, retry)
import Control.Monad.Except (ExceptT, runExceptT, unless)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebug, logDebugSH, logErrorSH, logInfo, logInfoSH, logWarnSH)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
import Data.Align
import Data.Foldable (foldl', length)
import Data.Functor.Apply
import qualified Data.LCA.Online.Polymorphic as LCA
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Pool (Pool)
import qualified Data.Set as S
import Data.These
import Data.Time (NominalDiffTime, diffUTCTime)
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql, in_, isFieldNothing, (&&.), (=.), (==.))
import qualified Network.HTTP.Client as Http
import Reflex.Class (fmapMaybe)
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (getTime, runDb, selectMap, project1)
import Rhyolite.Backend.DB.PsqlSimple (executeQ, In(..), sql, returning, queryQ)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Rhyolite.Backend.Schema (toId, fromId)
import Rhyolite.Schema (Id (..))
import Safe.Foldable (maximumMay, maximumByMay)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.History (AccumHistoryContext (..), CachedHistory (..), accumHistory)
import Tezos.NodeRPC (NodeRPCContext (..), PlainNodeStream, RpcError(..), RpcQuery, rChain, rConnections,
                      rMonitorHeads, rNetworkStat, rCheckpoint)
import Tezos.NodeRPC.Network (PublicNodeContext (..), getCurrentHead, nodeRPC, nodeRPCChunked)
import Tezos.NodeRPC.Sources (PublicNode (..), PublicNodeError (..))
import Tezos.Types
import qualified Tezos.TestChainStatus as Tezos

import Backend.Alerts (clearBadNodeHeadError, clearInaccessibleNodeError, clearNodeWrongChainError,
                       reportBadNodeHeadError, reportInaccessibleNodeError, reportNodeWrongChainError,
                       reportNodeInvalidPeerCountError, clearNodeInvalidPeerCountError,
                       clearPastVotingPeriodErrors, reportVotingReminderError)
import Backend.CachedNodeRPC
import Backend.Common (unsupervisedWorkerWithDelay, threadDelay', worker', workerWithDelay, timeout')
import Backend.Config (AppConfig (..), kilnNodeRpcURI)
import Backend.Schema
import Backend.Supervisor (withTermination)
import Backend.STM (atomicallyWith)
import Backend.ViewSelectorHandler (getProposals)
import Common.App (getEndTimeForPeriod)
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

nodeMonitor :: NodeDataSource -> AppConfig -> URI -> Id Node -> MonitorBlock -> Maybe RawLevel -> IO ()
nodeMonitor nds appConfig nodeAddr nodeId headBlockInfo mSp = do
  atomically $ do
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
     in void [executeQ|
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
          , _nodeDetailsData_savePoint = mSp
          }
        }
      (_:_) -> update
        ([ p NodeDetailsData_headLevelSelector =. Just (headBlockInfo ^. monitorBlock_level)
        , p NodeDetailsData_headBlockHashSelector =. Just (headBlockInfo ^. monitorBlock_hash)
        , p NodeDetailsData_headBlockBakedAtSelector =. Just (headBlockInfo ^. monitorBlock_timestamp)
        , p NodeDetailsData_fitnessSelector =. Just (headBlockInfo ^. monitorBlock_fitness)
        , p NodeDetailsData_updatedSelector =. Just now
        , p NodeDetailsData_headBlockPredSelector =. Just (headBlockInfo ^. monitorBlock_predecessor)
        ] <> maybe [] (\sp -> [p NodeDetailsData_savePointSelector =. Just sp]) mSp)
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
        if peerCount < fromIntegral minPeerCount
          then reportNodeInvalidPeerCountError nid minPeerCount peerCount
          else clearNodeInvalidPeerCountError nid

    let p = (NodeDetails_dataField ~>)
    update
      [ p NodeDetailsData_peerCountSelector =. _nodeDetailsData_peerCount after
      , p NodeDetailsData_networkStatSelector =. _nodeDetailsData_networkStat after
      ]
      (NodeDetails_idField ==. nid)
    project NodeDetails_dataField (NodeDetails_idField ==. nid) >>= traverse_ (notify NotifyTag_NodeDetails . (nid,) . Just)

type NodeData = Either (Id ProcessData) NodeExternalData
nodeData_address :: AppConfig -> NodeData -> URI
nodeData_address appConfig = either (const $ kilnNodeRpcURI appConfig) _nodeExternalData_address

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
      <$> selectMap NodeConstructor CondEmpty
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
      let
        reconnectDelay = 5

        nodeQuery :: RpcQuery a -> IO (Either RpcError a)
        nodeQuery f = runLoggingEnv (_nodeDataSource_logger nds) $ runExceptT $ runReaderT (nodeRPC f) $ NodeRPCContext httpMgr $ Uri.render nodeAddr

        chunkedNodeQuery :: PlainNodeStream a -> (a -> IO ()) -> IO (Either RpcError ())
        chunkedNodeQuery f k = runLoggingEnv (_nodeDataSource_logger nds) $ runExceptT $ runReaderT (nodeRPCChunked f k) $ NodeRPCContext httpMgr $ Uri.render nodeAddr

        updateCheckpoint :: (BlockLike blk, MonadIO m, MonadLogger m) => blk -> Maybe RawLevel -> m (Maybe RawLevel)
        updateCheckpoint blk mSp = do
          mParams <- liftIO $ readTVarIO $ _nodeDataSource_parameters nds
          let
            shouldUpdate = maybe True checkCycle mParams
            checkCycle protoInfo = mSp == Nothing || thisCycle /= predCycle
              where
                thisCycle = levelToCycle protoInfo (blk ^. level)
                predCycle = levelToCycle protoInfo $ pred (blk ^. level)

          if not shouldUpdate
            then pure Nothing
            else do
              $(logDebugSH) ("nodeWorker: fetching checkpoint for Node: "::Text, nodeAddr)
              liftIO (nodeQuery $ rCheckpoint chainId) >>= \case
                Left e ->
                  case e of
                    RpcError_UnexpectedStatus 404 _ -> pure $ Just 0
                    _ -> Nothing <$ $(logErrorSH) ("nodeWorker: could not fetch checkpoint for Node: "::Text, nodeAddr, e)
                Right checkpoint ->
                  pure $ Just $ _checkpoint_savePoint checkpoint

      killMonitor <- unsupervisedWorkerWithDelay reconnectDelay $ runLoggingEnv (_nodeDataSource_logger nds) $ do
        _ <- liftIO $ chunkedNodeQuery (rMonitorHeads chainId) $ \block -> do
          -- Since we receive a new head, we can clear connectivity and wrong-chain errors for this node.
          mNewSp <- runLoggingEnv (_nodeDataSource_logger nds) $ do
            mOldSp <- inDb $ do
              clearInaccessibleNodeError nodeId
              clearNodeWrongChainError nodeId
              project1 (NodeDetails_dataField ~> NodeDetailsData_savePointSelector) (NodeDetails_idField `in_` [nodeId])
            updateCheckpoint block $ join mOldSp

          nodeMonitor nds appConfig nodeAddr nodeId block mNewSp

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
    inDb :: (MonadIO m, MonadBaseNoPureAborts IO m, MonadLogger m) => ReaderT AppConfig (DbPersist Postgresql m) a -> m a
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
           in void [executeQ|
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
      nodeHead <- (,) nodeHeadHash <$> nodeQueryDataSource (NodeQuery_BlockHeader nodeHeadHash)
      lcaBlock' <- atomicallyWith $ branchPoint nodeHeadHash (latestHead ^. hash)
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
                 latestParent <- (,) parentHash <$> nodeQueryDataSource (NodeQuery_BlockHeader parentHash)
                 latestUncle <- (,) uncleHash <$> nodeQueryDataSource (NodeQuery_BlockHeader uncleHash)
                 if latestParent ^. fitness > latestUncle ^. fitness then return bad else return good
    for_ action' $ \action -> runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $ runReaderT action appConfig

safePred :: (Eq a, Enum a, Bounded a) => a -> a
safePred a = if a /= minBound then pred a else minBound

{-
Interpretation of a baker's voting state as a function of:
- the current head state
- the head state seen when the baker last attempted a vote which is recognized by the current head

This encoding doesn't really enable more code reuse (quite the opposite, at least right now)
but it does provide a centralized point of reasoning about the pathways voting can take
and allows the determination of the state to be handled separately from the acting on the state

We treat errors as specific to a single period. If a period starts in an error state,
a new error will be issued, and all old ones cleared.
Likewise, transitioning from hasn't-voted to has-voted states also triggers a new error.

Note we could also look at all previous recognized vote attempts rather than just the last
i.e. consider a vote choice up-to-date if all current proposals
are contained in the union of seen proposal sets.
We look only at the last attempt because it is both simpler and less opinionated,
since a baker might now want to vote for a proposal they previously didn't vote for
(e.g. because they were hoping better alternatives came along).

We assume votes in an operation are either all accepted or all rejected.
Kiln enforces this by only allowing the baker to vote once per operation, but that is not the case for tezos-client.
-}

data BakerVotingState
  = BakerVotingState_Proposal ProposalVoteState
  | BakerVotingState_Exploration Bool -- whether baker previously voted
  | BakerVotingState_Testing -- no voting takes place
  | BakerVotingState_Promotion Bool -- whether baker previously voted
  deriving (Eq, Ord, Show)

data ProposalVoteState
  = ProposalVotingState_SilentRange -- no alerts during this range
  | ProposalVotingState_OutOfUpvotes -- no alerts since they're not actionable anyway
  | ProposalVotingState_CaughtUp -- has voted, and there are no new proposals since
  | ProposalVotingState_NoPreviousVote -- no votes from this baker are included in current head
  | ProposalVotingState_OutdatedVote -- some proposals in the current block were not visible at the time of last vote
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Monitors the amendment process
amendmentProcessWorker
  :: AppConfig
  -> NodeDataSource
  -> Pool Postgresql
  -> IO (IO ())
amendmentProcessWorker appConfig nds db = worker' $ waitForNewHead nds >>= \latestHead -> runLoggingEnv (_nodeDataSource_logger nds) $ do
  (history, protoInfo) <- liftIO $ atomically $ liftA2 (,) (readTVar $ _nodeDataSource_history nds) (waitForParams nds)
  latestBlock <- throwing $ getBlock (latestHead ^. hash)
  let
    blocksPerVotingPeriod = protoInfo ^. protoInfo_blocksPerVotingPeriod
    chainId = _nodeDataSource_chain nds
    minLevel = _cachedHistory_minLevel history

    -- The RPCs under /votes/ return the information for the *next block*, not the current block.
    -- So we might have a voting_period_position of blocks_per_voting_period-1 in a given block
    -- (the last block of the period), but /votes/current_period_kind for that block will return
    -- the *next* period kind.
    votingPeriod = latestBlock ^. block_metadata . blockMetadata_level . level_votingPeriod
    currentVotingPosition = latestBlock ^. block_metadata . blockMetadata_level . level_votingPeriodPosition
    isLastBlockOfPeriod blk = blocksPerVotingPeriod == succ (blk ^. block_metadata . blockMetadata_level . level_votingPeriodPosition)
    -- The period of the *current* block, not the next one
    currentPeriodKind = (if isLastBlockOfPeriod latestBlock then safePred else id)
      $ latestBlock ^. block_metadata . blockMetadata_votingPeriodKind
    periodFraction = fromIntegral currentVotingPosition / fromIntegral blocksPerVotingPeriod :: Double

    singleVotePeriod pkh periodKindOffset mkVotingState = do
      let blk = latestHead ^.hash
      mBallot <- runMaybe $ nodeQueryDataSource $ NodeQuery_Ballot blk pkh
      -- The voting period of the last proposal period
      let amendmentPeriod = latestBlock ^. block_metadata . blockMetadata_level . level_votingPeriod - periodKindOffset

      runDb (Identity db) $ case mBallot of
        Nothing -> do
          deleteAll' @BakerVote Proxy
          notify NotifyTag_BakerVote Nothing
        Just ballot -> do
          pps <- [queryQ|
            UPDATE "BakerVote" SET included = ?blk
            FROM "PeriodProposal" pp
            WHERE pp.id = proposal AND pp."chainId" = ?chainId AND pp."votingPeriod" = ?amendmentPeriod AND ballot = ?ballot AND pkh = ?pkh
            RETURNING proposal, attempted
          |]
          for_ pps $ \(proposal, attempted) -> notify NotifyTag_BakerVote $ Just $ BakerVote
            { _bakerVote_pkh = pkh
            , _bakerVote_proposal = proposal
            , _bakerVote_ballot = ballot
            , _bakerVote_included = Just blk
            , _bakerVote_attempted = attempted
            }

      pure $ mkVotingState $ isJust mBallot

  -- Update baker votes
  mPkh <- runDb (Identity db) $ join <$> project1
    (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector ~> BakerDaemonInternalData_publicKeyHashSelector)
    (BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector ==. False)
  for_ mPkh $ \pkh -> do
    votingState <- case currentPeriodKind of
      VotingPeriodKind_Proposal -> throwing $ runNodeQueryT $ do
        let blk = latestHead ^. hash
        bakerProposals <- nodeQueryDataSourceSafe $ NodeQuery_ProposalVote blk pkh

        pps <- let inBakerProposals = In $ S.toList bakerProposals in [queryQ|
          UPDATE "BakerProposal" SET included = ?blk
          FROM "PeriodProposal" pp
          WHERE pp.id = proposal AND pp.hash IN ?inBakerProposals
            AND pp."chainId" = ?chainId
            AND pp."votingPeriod" = ?votingPeriod
          RETURNING pp.id, pp.hash, pp."chainId", pp."votingPeriod", pp.votes, attempted
        |]
        for_ pps $ \(pid, phash, chain, vp, votes, _ :: Maybe BlockHash) ->
          notify NotifyTag_Proposals (pid, Just (PeriodProposal phash chain vp votes, Just True))

        fmap BakerVotingState_Proposal $
          if periodFraction < 0.5
          then pure ProposalVotingState_SilentRange
          else fmap NE.nonEmpty getProposals >>= \case
            Nothing -> pure ProposalVotingState_CaughtUp
            Just proposals
              | length (NE.filter (isJust . snd . snd) proposals) >= maxProposalUpvotes ->
                pure ProposalVotingState_OutOfUpvotes
              | otherwise -> case maximumMay $ fmapMaybe (\(_,_,_,_,_,attempted) -> attempted) pps of
                Nothing -> pure ProposalVotingState_NoPreviousVote
                Just lastAttempt -> do
                  proposalVotesWhenLastVoting <- nodeQueryDataSourceSafe $ NodeQuery_Proposals lastAttempt
                  let
                    proposalHashes = S.fromList $ toList $ (^. _2 . _1 . periodProposal_hash) <$> proposals
                    proposalHashesWhenLastVoting = S.fromList $ toList $ fst . unProposalVotes <$> proposalVotesWhenLastVoting
                    unseenProposalHashes = proposalHashes S.\\ proposalHashesWhenLastVoting
                  pure $ if null unseenProposalHashes then ProposalVotingState_CaughtUp else ProposalVotingState_OutdatedVote

      VotingPeriodKind_Testing -> pure BakerVotingState_Testing
      VotingPeriodKind_TestingVote -> singleVotePeriod pkh 1 BakerVotingState_Exploration
      VotingPeriodKind_PromotionVote -> singleVotePeriod pkh 3 BakerVotingState_Promotion

    runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $ flip runReaderT appConfig $ do

      as <- select $ Amendment_chainIdField ==. _nodeDataSource_chain nds
      let as' = Map.fromList $ flip fmap as $ _amendment_period &&& id
      for_ (maximumByMay (comparing _amendment_period) as') $ \a -> do
        let
          endTime = fst $ getEndTimeForPeriod currentPeriodKind a as' protoInfo

          -- Calculate which range we're in.
          rangeMax = case periodFraction of
            x | x < 0.5 -> 50 -- 0 to 50
              | x < 0.9 -> 90 -- 50 to 90
              | otherwise -> 100 -- 90 to 100

          singleVotePhase = bool (reportError False) clearAllErrors
          clearAllErrors = clearPastVotingPeriodErrors chainId (Id pkh) Nothing rangeMax
          reportError previouslyVoted = do
            clearPastVotingPeriodErrors chainId (Id pkh) (Just previouslyVoted) rangeMax
            reportVotingReminderError chainId (Id pkh) votingPeriod currentPeriodKind previouslyVoted rangeMax endTime

        case votingState of
          BakerVotingState_Proposal pvs -> case pvs of
            ProposalVotingState_SilentRange -> clearAllErrors
            ProposalVotingState_OutOfUpvotes -> clearAllErrors
            ProposalVotingState_CaughtUp -> clearAllErrors
            ProposalVotingState_NoPreviousVote -> reportError False
            ProposalVotingState_OutdatedVote -> reportError True
          BakerVotingState_Exploration previouslyVoted -> singleVotePhase previouslyVoted
          BakerVotingState_Testing -> pure ()
          BakerVotingState_Promotion previouslyVoted -> singleVotePhase previouslyVoted

  -- Any *lesser* periods should be updated to the values at the block level of the end of the given period.
  -- Current period should be updated to the values of the latest block.
  -- Any *greater* periods should be blanked out.

  for_ [minBound..maxBound] $ \p -> case compare p currentPeriodKind of
    LT -> do
      let periodDiff = fromIntegral $ fromEnum currentPeriodKind - fromEnum p
          startBlockLevel = max minLevel $ latestBlock ^. level - currentVotingPosition - periodDiff * blocksPerVotingPeriod
          endBlockLevel = startBlockLevel + blocksPerVotingPeriod - 1

      -- Ignore the update if the block cannot be obtained from current set of nodes
      mEndBlock <- flip runReaderT nds . runExceptT @CacheError $ getBlock $ fromMaybe (error "amendmentProcessWorker: can't get end block") $
        -- Calc the blockLevel at the start of the current voting period, move
        -- back by periodDiff voting periods, and move to the end of that period
        levelAncestor history endBlockLevel (latestBlock ^. hash)
      for_ mEndBlock $ \endBlock -> do
        (periodStartBlock, periodEndBlockPred) <- throwing $ do
          startBlock <- getBlockHeader $ fromMaybe (error "amendmentProcessWorker: can't get start block") $
            levelAncestor history startBlockLevel (latestBlock ^. hash)
          predBlock <- getBlockHeader $ endBlock ^. predecessor
          pure (startBlock, predBlock)
        updateTo periodStartBlock periodEndBlockPred endBlock p
    EQ -> do
      let startBlockLevel = max minLevel $ latestBlock ^. level - currentVotingPosition
      startBlock <- throwing $ getBlockHeader $ fromMaybe (error "amendmentProcessWorker: can't get start block for current period") $
        levelAncestor history startBlockLevel (latestBlock ^. hash)
      predOrLatest <-
        if isLastBlockOfPeriod latestBlock
        then throwing $ getBlockHeader $ latestBlock ^. predecessor -- For some queries we need to use the predecessor block
        else pure (latestBlock ^. hash, _block_header latestBlock)
      updateTo startBlock predOrLatest latestBlock p
    GT -> runDb (Identity db) $ do
      wipe p
      notify NotifyTag_Amendment (p, Nothing)
      case p of
        VotingPeriodKind_Proposal -> pure () -- can never happen
        VotingPeriodKind_TestingVote -> notify NotifyTag_PeriodTestingVote Nothing
        VotingPeriodKind_Testing -> notify NotifyTag_PeriodTesting Nothing
        VotingPeriodKind_PromotionVote -> notify NotifyTag_PeriodPromotionVote Nothing

  where

    getBlock hash' = nodeQueryDataSource $ NodeQuery_Block hash'
    getBlockHeader hash' = nodeQueryDataSource (NodeQuery_BlockHeader hash') >>= pure . (hash',)

    throwing :: Functor m => ExceptT CacheError (ReaderT NodeDataSource m) a -> m a
    throwing = fmap (either (error . show) id) . flip runReaderT nds . runExceptT @CacheError

    runMaybe :: Functor m => ExceptT CacheError (ReaderT NodeDataSource m) (Maybe a) -> m (Maybe a)
    runMaybe = fmap (either (const Nothing) id) . flip runReaderT nds . runExceptT

    wipe p = do
      delete $ Amendment_periodField ==. p
      case p of
        VotingPeriodKind_Proposal -> pure ()
        VotingPeriodKind_TestingVote -> deleteAll' @PeriodTestingVote Proxy
        VotingPeriodKind_Testing -> deleteAll' @PeriodTesting Proxy
        VotingPeriodKind_PromotionVote -> deleteAll' @PeriodPromotionVote Proxy
    updateTo startBlock predBlk blk p = do
      let position' = blk ^. block_metadata . blockMetadata_level . level_votingPeriodPosition
          votingPeriod = blk ^. block_metadata . blockMetadata_level . level_votingPeriod
          chainId = _nodeDataSource_chain nds
          amendment = Amendment
            { _amendment_period = p
            , _amendment_chainId = chainId
            , _amendment_votingPeriod = votingPeriod
            , _amendment_start = startBlock ^. timestamp
            , _amendment_startLevel = startBlock ^. level
            , _amendment_position = position'
            }
      runDb (Identity db) $ do
        wipe p
        insert_ amendment
        notify NotifyTag_Amendment (p, Just amendment)
      case p of
        VotingPeriodKind_Proposal -> do
          proposals <- throwing $ nodeQueryDataSource $ NodeQuery_Proposals (predBlk ^. hash)
          runDb (Identity db) $ do
            deletedIds <- [queryQ|
              DELETE FROM "BakerProposal"
              WHERE proposal IN (SELECT id FROM "PeriodProposal" WHERE "votingPeriod" < ?votingPeriod);
              DELETE FROM "PeriodProposal"
              WHERE "votingPeriod" < ?votingPeriod
              RETURNING id
            |]
            for_ deletedIds $ \(Only pid) -> notify NotifyTag_Proposals (pid, Nothing)
            inserted <- returning [sql|
              INSERT INTO "PeriodProposal" (hash, "chainId", "votingPeriod", votes)
              VALUES (?, ?, ?, ?)
              ON CONFLICT (hash, "chainId", "votingPeriod") DO UPDATE SET votes = EXCLUDED.votes
              RETURNING id, hash, "chainId", "votingPeriod", votes, (SELECT bp.pkh FROM "BakerProposal" bp WHERE bp.proposal = id), (SELECT bp.included FROM "BakerProposal" bp WHERE bp.proposal = id)
            |] $ (\(ProposalVotes (phash, votes)) -> (phash, chainId, votingPeriod, votes)) <$> toList proposals
            for_ inserted $ \(pid, phash, chain, vp, votes, includedPkh :: Maybe PublicKeyHash, includedBlock :: Maybe BlockHash) ->
              notify NotifyTag_Proposals (pid, Just (PeriodProposal phash chain vp votes, fmap (\_ -> isJust includedBlock) includedPkh))
        VotingPeriodKind_Testing -> do
          mProposal <- runMaybe $ nodeQueryDataSource $ NodeQuery_CurrentProposal (predBlk ^. hash)
          for_ mProposal $ \proposal -> do
            let (status, testChainId, startBlockHash) = case blk ^. block_metadata . blockMetadata_testChainStatus of
                  Tezos.TestChainStatus_NotRunning -> (TestChainStatus_NotRunning, Nothing, Nothing)
                  Tezos.TestChainStatus_Forking {} -> (TestChainStatus_Forking, Nothing, Nothing)
                  Tezos.TestChainStatus_Running
                    { Tezos._testChainStatusRunning_chainId = c
                    , Tezos._testChainStatusRunning_genesis = b
                    } -> (TestChainStatus_Running, Just c, Just b)
            tcStartBlock <- fmap join $ traverse (liftIO . atomically . lookupBlock nds) startBlockHash
            runDb (Identity db) $ do
              let startingLevel = (^. level) <$> tcStartBlock
              ts <- [queryQ|
                INSERT INTO "PeriodTesting" (proposal, "testChainId", "startingLevel", status)
                (SELECT p.id, ?testChainId, ?startingLevel, ?status FROM "PeriodProposal" p WHERE p.hash = ?proposal)
                RETURNING proposal, "testChainId", "startingLevel", status
              |]
              for_ ts $ \(ph,t,l,s) -> notify NotifyTag_PeriodTesting $ Just PeriodTesting
                { _periodTesting_proposal = ph
                , _periodTesting_testChainId = t
                , _periodTesting_startingLevel = l
                , _periodTesting_status = s
                }
        VotingPeriodKind_TestingVote -> handleVotingPeriod predBlk PeriodTestingVote NotifyTag_PeriodTestingVote
        VotingPeriodKind_PromotionVote -> handleVotingPeriod predBlk PeriodPromotionVote NotifyTag_PeriodPromotionVote

    handleVotingPeriod :: (PersistEntity a, BlockLike blk) => blk -> (Id PeriodProposal -> PeriodVote -> a) -> NotifyTag (Maybe a) -> LoggingT IO ()
    handleVotingPeriod blk f n = do
      mpv <- runMaybe $ do
        mProposal <- nodeQueryDataSource $ NodeQuery_CurrentProposal (blk ^. hash)
        ballots <- nodeQueryDataSource $ NodeQuery_Ballots (blk ^. hash)
        quorum <- nodeQueryDataSource $ NodeQuery_CurrentQuorum (blk ^. hash)
        totalRolls <- foldl' (\x d -> _voterDelegate_rolls d + x) 0 <$> nodeQueryDataSource (NodeQuery_Listings (blk ^. hash))
        pure $ flip fmap mProposal $ \proposal -> (proposal, ballots, quorum, totalRolls)

      for_ mpv $ \(proposal, ballots, quorum, totalRolls) -> runDb (Identity db) $ do
        mPid <- (fmap . fmap) toId $ project1 AutoKeyField $ PeriodProposal_hashField ==. proposal
        for_ mPid $ \pid -> do
          let pv = PeriodVote
                { _periodVote_ballots = ballots
                , _periodVote_quorum = quorum
                , _periodVote_totalRolls = totalRolls
                }
          insert_ $ f pid pv
          notify n $ Just $ f pid pv

-- Monitors changes in protocol/voting period, and manages the baker daemon if running
protocolMonitorWorker
  :: NodeDataSource
  -> Pool Postgresql
  -> IO (IO ())
protocolMonitorWorker nds db = worker' $ waitForNewHead nds >>= \latestHead -> runLoggingEnv (_nodeDataSource_logger nds) $ do
  $(logDebugSH) ("protocolMonitorWorker: Started"::Text,())
  let
    getProtocol = getProtocol' >>= \case
      Right p -> return p
      Left e -> do
        $(logWarnSH) ("protocolMonitorWorker: cannot fetch protocol"::Text, e)
        threadDelay' 1
        getProtocol
    getProtocol' = flip runReaderT nds $ runExceptT @CacheError $ do
      blk <- nodeQueryDataSource $ NodeQuery_Block (latestHead ^. hash)
      let vp = blk ^. block_metadata . blockMetadata_votingPeriodKind
      tp <- if vp == VotingPeriodKind_PromotionVote
        then nodeQueryDataSource $ NodeQuery_CurrentProposal (latestHead ^. hash)
        else return Nothing
      return (blk ^. block_metadata . blockMetadata_protocol, tp)

  (mainProto, altProto) <- getProtocol

  let
    inDb :: DbPersist Postgresql (LoggingT IO) a -> LoggingT IO a
    inDb = runDb (Identity db)
    setControl c ps = update [ProcessData_controlField =. c] (AutoKeyField `in_` map fromId ps)

  $(logDebugSH) ("protocolMonitorWorker: setting protocol"::Text, mainProto, altProto)
  let ds = BakerDaemonInternal_dataField ~> DeletableRow_dataSelector
  inDb $ project1 ds CondEmpty >>= \case
    Nothing -> return ()
    Just bdid -> do
      let
        mp = _bakerDaemonInternalData_protocol bdid
        tp = _bakerDaemonInternalData_altProtocol bdid
        bpid = _bakerDaemonInternalData_bakerProcessData bdid
        epid = _bakerDaemonInternalData_endorserProcessData bdid
        tbpid = _bakerDaemonInternalData_altBakerProcessData bdid
        tepid = _bakerDaemonInternalData_altEndorserProcessData bdid
      isRunning <- (/= Just ProcessControl_Stop) <$> project1 ProcessData_controlField (AutoKeyField ==. fromId bpid)
      let
        setMainProto = unless (mp == mainProto) $ do
          update [ds ~> BakerDaemonInternalData_protocolSelector =. mainProto] CondEmpty
          when isRunning $ setControl ProcessControl_Restart [bpid, epid]
        setAltProto p = do
          isAltRunning <- (/= Just ProcessControl_Stop) <$> project1 ProcessData_controlField (AutoKeyField ==. fromId tbpid)
          if tp == Just p
            then when (isRunning && not isAltRunning) $ setControl ProcessControl_Restart [tbpid, tepid]
            else do
              update [ds ~> BakerDaemonInternalData_altProtocolSelector =. Just p] CondEmpty
              when isRunning $ setControl ProcessControl_Restart [tbpid, tepid]
        stopMain = do
          $(logDebugSH) ("protocolMonitorWorker: stopping main protocol baker/endorser"::Text, mainProto)
          setControl ProcessControl_Stop [bpid, epid]
        stopAlt = do
          $(logDebugSH) ("protocolMonitorWorker: stopping alternate protocol baker/endorser"::Text, mainProto)
          setControl ProcessControl_Stop [tbpid, tepid]
        -- stop main and swap pids
        altToMain = do
          $(logDebugSH) ("protocolMonitorWorker: swapping processes"::Text, mainProto)
          stopMain
          update [ ds ~> BakerDaemonInternalData_protocolSelector =. mainProto
                 , ds ~> BakerDaemonInternalData_bakerProcessDataSelector =. tbpid
                 , ds ~> BakerDaemonInternalData_endorserProcessDataSelector =. tepid
                 , ds ~> BakerDaemonInternalData_altBakerProcessDataSelector =. bpid
                 , ds ~> BakerDaemonInternalData_altEndorserProcessDataSelector =. epid
                 ] CondEmpty
      unless isRunning stopAlt
      case altProto of
        Just tp' -> setMainProto >> setAltProto tp'
        Nothing -> if
          | mp == mainProto -> stopAlt
          | tp == Just mainProto -> stopMain >> altToMain
          | otherwise -> stopAlt >> setMainProto

  -- Wait till the end of this cycle
  $(logDebugSH) ("protocolMonitorWorker: waiting for next cycle"::Text)
  waitTillNextCycle nds

waitTillNextCycle :: MonadIO m => NodeDataSource -> m ()
waitTillNextCycle nds = liftIO $ do
  v <- atomically $ (liftA2 . liftA2) (,)
    (readTVar $ _nodeDataSource_latestHead nds)
    (readTVar $ _nodeDataSource_parameters nds)
  for_ v $ \(blk, protoInfo) -> do
    let
      nextCycle = 1 + levelToCycle protoInfo (blk ^. level)
      nextCycleLvl = firstLevelInCycle protoInfo nextCycle
    atomically $ do
      newHead <- maybe retry pure =<< readTVar (_nodeDataSource_latestHead nds)
      when (newHead ^. level < pred nextCycleLvl) retry
