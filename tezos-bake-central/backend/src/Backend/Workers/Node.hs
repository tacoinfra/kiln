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
{-# LANGUAGE DoAndIfThenElse #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Workers.Node where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (atomically, readTVar, readTVarIO, writeTQueue, writeTVar, retry)
import Control.Exception.Safe (try)
import Control.Lens (set)
import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.Except (ExceptT, runExceptT, unless)
import Control.Monad.Logger (LoggingT, MonadLoggerIO, MonadLogger, logDebug, logDebugSH, logInfo)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson (decode')
import Data.Align
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (foldl', length)
import Data.Functor.Apply
import Data.List (dropWhileEnd)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Pool (Pool)
import qualified Data.Set as S
import Data.String.Here.Interpolated (i)
import Data.These
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Text as T
import Data.Word
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql(..), in_, (=.), (==.))
import Database.Id.Class
import Database.Id.Groundhog
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http
-- import qualified Network.HTTP.Types.Method as Http (methodGet)
import Reflex.Class (fmapMaybe)
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (getTime, runDb, selectMap, project1)
import Rhyolite.Backend.DB.PsqlSimple (In(..), sql, returning, queryQ)
import Rhyolite.Backend.DB.Serializable
import Rhyolite.Backend.Logging (runLoggingEnv)
import Safe (headMay)
import Safe.Foldable (maximumMay, maximumByMay)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.NodeRPC hiding (getBlock)
import Tezos.Types hiding (TestChainStatus(..), toBlockHeader)
import qualified Tezos.Unsafe as Unsafe

import Backend.Alerts (clearBadNodeHeadError, clearInaccessibleNodeError, clearNodeWrongChainError,
                       reportBadNodeHeadError, reportInaccessibleNodeError, reportNodeWrongChainError,
                       reportNodeInsufficientPeersError, reportNodeInvalidPeerCountError,
                       clearNodeInsufficientPeersError, clearNodeInvalidPeerCountError,
                       clearPastVotingPeriodErrors, reportVotingReminderError)
import Backend.Common (AppSerializable, threadDelay', unsupervisedWorkerWithDelay, worker', workerWithDelay)
import Backend.Config (AppConfig (..), kilnNodeRpcURI)
import Backend.IndexQueries
import Backend.NodeRPC
import Backend.Schema
import Backend.Supervisor (withTermination)
import Backend.ViewSelectorHandler (getProposals)
import Common.App (getEndTimeForPeriod)
import Common.Schema
import ExtraPrelude

-- We assume that the implicit nodeaddr is the same one we just learned the new
-- branch from, so we insist that we bootstrap from it (rather than using a
-- pool of nodes)

haveNewHead
  :: (MonadIO m, BlockLike blk)
  => NodeDataSource
  -> URI
  -> blk
  -> m ()
haveNewHead nds nodeAddr headBlockInfo = runLoggingEnv (_nodeDataSource_logger nds) $ do
  $(logInfo) [i|Node reported new head: ${headBlockInfo ^. hash}, level ${headBlockInfo ^. level}|]
  -- We'd like to avoid handling blocks when node is bootstrapping since the data from the current
  -- head will be ovewritten nearly instantly by the next bootstrapped block.
  -- In order to achieve that we're going to handle only blocks that are recent, specifically
  -- their timestamps aren't later than 'maxTimeDiff' from the current time.
  now <- liftIO getCurrentTime
  let maxTimeDiff = 600 -- 10 minutes
      blockTimeDiff = diffUTCTime now (headBlockInfo ^. timestamp)
  when (blockTimeDiff < maxTimeDiff) $ do
    oldHead <- liftIO $ atomically (dataSourceHead nds)
    when (Just (headBlockInfo ^. fitness) > oldHead ^? _Just . fitness) $ do
      newStateRsp :: Either KilnRpcError BlockCrossCompat <- runExceptT $ do
          flip runReaderT nds { _nodeDataSource_nodeForQuery = Just nodeAddr } $ do
            nodeQueryDataSourceImmediate $ NodeQuery_Block $ headBlockInfo ^. hash
      case newStateRsp of
        Left e -> logKilnRpcError "Handle new node head" e
        Right headBlock -> do
        updatedLevel <- liftIO $ atomically $ do
          let latestHeadTVar = _nodeDataSource_latestHead nds
          latestHead <- readTVar latestHeadTVar
          if Just (headBlock ^. fitness) > latestHead ^? _Just . fitness
            then do
              let levelInfo = headBlock ^. blockMetadata . blockMetadata_levelInfo
                  newHead = BranchInfo (WithProtocolHash (mkVeryBlockLike headBlock) (headBlock ^. protocolHash))
                    (levelInfo ^. levelInfo_cycle) (levelInfo ^. levelInfo_cyclePosition)
              writeTVar latestHeadTVar $ Just newHead
              pure $ Just $ headBlock ^. level
            else
              pure Nothing
        for_ updatedLevel $ \lev -> $(logDebug) $ "Saw more recent head: " <> tshow (unRawLevel lev)

nodeMonitor :: NodeDataSource -> AppConfig -> URI -> Id Node -> MonitorBlock -> IO ()
nodeMonitor nds appConfig nodeAddr nodeId headBlockInfo = do
  let db = _nodeDataSource_pool nds
  runLoggingEnv (_nodeDataSource_logger nds) . runDb (Identity db) . flip runReaderT appConfig $ do
    $(logDebug) $ fold
      [ "Updating node "
      , tshow nodeId
      , " details "
      , Uri.render nodeAddr
      , " at block "
      , tshow headBlockInfo
      ]

    now <- getTime
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

  atomically $ do
    writeTQueue (_nodeDataSource_ioQueue nds) $ haveNewHead nds nodeAddr headBlockInfo


nodeVersionMonitor :: NodeDataSource -> URI -> Id Node -> IO ()
nodeVersionMonitor nds nodeAddr nodeId = do
  let db = _nodeDataSource_pool nds
      httpMgr = _nodeDataSource_httpMgr nds
  runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $ do
    $(logDebug) $ fold
        [ "Discovering node's version "
        , "at uri address: "
        , Uri.render nodeAddr
        ]
    version <- liftIO $ versionWorker httpMgr (T.unpack $ Uri.render nodeAddr)
    notify NotifyTag_NodeVersion (nodeId, version)

versionWorker :: MonadIO m => Http.Manager -> String -> m (Maybe TezosVersion)
versionWorker httpMgr baseUrl = do
    let versionUrl = ensure baseUrl "version"

    versionResp' :: Either Http.HttpException (Http.Response LB.ByteString) <-
        liftIO $ try $ Http.httpLBS =<< (Http.setRequestManager httpMgr <$> Http.parseRequest versionUrl)

    either (const doCommit) (maybe doCommit return . decode' . Http.getResponseBody) versionResp'

  where
    ensure :: String -> String -> String
    ensure base path = dropWhileEnd (== '/') base <> "/" <> path

    doCommit = do
        let commitUrl = ensure baseUrl "monitor/commit_hash"

        commitResp' :: Either Http.HttpException (Http.Response LB.ByteString) <-
            liftIO $ try $ Http.httpLBS =<< (Http.setRequestManager httpMgr <$> Http.parseRequest commitUrl)

        return $ either (const Nothing) (decode' . Http.getResponseBody) commitResp'

updateNetworkStats
  :: (MonadIO m, MonadLoggerIO m, MonadLogger m, MonadBaseNoPureAborts IO m)
  => AppConfig
  -> Http.Manager
  -> Pool Postgresql
  -> Id Node
  -> NodeData
  -> NodeDetailsData
  -> m (Either RpcError ())
updateNetworkStats appConfig httpMgr db nid node before = do
    eConnections :: Either RpcError Word64 <- runNodeRPC (nodeRPC rConnections)
    eNetworkStat :: Either RpcError NetworkStat <- runNodeRPC (nodeRPC rNetworkStat)
    eNodeConfig :: Either RpcError NodeConfig <- runNodeRPC (nodeRPC rConfig)

    let
      defaultSyncThreshold = 4
      eSyncThreshold = eNodeConfig
        <&> \v -> Just v
          >>= _nodeConfig_shell
          >>= _shell_chainValidator
          >>= _chainValidator_synchronisationThreshold
          & fromMaybe defaultSyncThreshold

    let after = before
                & update' nodeDetailsData_peerCount (Just <$> eConnections)
                & update' nodeDetailsData_networkStat eNetworkStat
                & update' nodeDetailsData_synchronisationThreshold  eSyncThreshold

    runExceptT $ do
        let inDb = runDb (Identity db)

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
            , p NodeDetailsData_synchronisationThresholdSelector =. _nodeDetailsData_synchronisationThreshold after
            ]
            (NodeDetails_idField ==. nid)
          project NodeDetails_dataField (NodeDetails_idField ==. nid) >>= traverse_ (notify NotifyTag_NodeDetails . (nid,) . Just)


  where

    update' lens e b = case e of
        Left _ -> b
        Right r -> set lens r b

    runNodeRPC :: ReaderT NodeRPCContext (ExceptT RpcError m) a -> m (Either RpcError a)
    runNodeRPC = runExceptT . flip runReaderT (NodeRPCContext httpMgr $ Uri.render (nodeData_address appConfig node))


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
  :: ( MonadIO m, MonadLoggerIO m, MonadLogger m, MonadBaseNoPureAborts IO m
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
  workerWithDelay "nodeWorker" (pure delay) $ const $ runLoggingEnv (_nodeDataSource_logger nds) $ do
    $(logDebug) "Update node cycle."

    -- read the persistent list of nodes
    theseNodeRecords <- getNodes db CondEmpty

    -- give them all a chance to
    ifor_ theseNodeRecords $ \nodeId (Node, node, nodeDetails) -> updateNetworkStats appConfig httpMgr db nodeId node nodeDetails
      {- >>= \case
        Left _e -> do
            -- Touching headBlock frequently may get us rate limited,
            -- commitHash may be less controversial
            eCommitHash :: Either RpcError Text <-
                runExceptT $ flip runReaderT (NodeRPCContext httpMgr $ Uri.render (nodeData_address appConfig node))
                           $ nodeRPC $ plainNodeRequest Http.methodGet "/monitor/commit_hash"

            unless (isRight eCommitHash) (inDb $ reportInaccessibleNodeError nodeId)
        Right () -> pure () -- We'll rely on the block monitor to clear this error
        -}

    let theseNodes = Map.fromList $ fmap (\(id_, (_, nE, _)) -> (nodeData_address appConfig nE, (id_, nodeData_alias nE))) $ Map.toList theseNodeRecords

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

        -- 2 minutes
        chunkedQueryTimeout :: Maybe NominalDiffTime
        chunkedQueryTimeout = Just 120

        nodeQuery :: RpcQuery a -> IO (Either RpcError a)
        nodeQuery f = runLoggingEnv (_nodeDataSource_logger nds) $ runExceptT $ runReaderT (nodeRPC f) $ NodeRPCContext httpMgr $ Uri.render nodeAddr

        chunkedNodeQuery :: PlainNodeStream a -> (a -> IO ()) -> IO (Either RpcError ())
        chunkedNodeQuery f k = runLoggingEnv (_nodeDataSource_logger nds) $ runExceptT $ runReaderT (nodeRPCChunked f k chunkedQueryTimeout) $
          NodeRPCContext httpMgr $ Uri.render nodeAddr

      killMonitor <- unsupervisedWorkerWithDelay reconnectDelay $ runLoggingEnv (_nodeDataSource_logger nds) $ do
        _ <- liftIO $ chunkedNodeQuery (rMonitorHeads chainId) $ \block -> do
          runLoggingEnv (_nodeDataSource_logger nds) $ inDb $ do
            clearInaccessibleNodeError nodeId
            clearNodeWrongChainError nodeId

          nodeMonitor nds appConfig nodeAddr nodeId block
          nodeAlertMonitor nds appConfig db nodeAddr nodeId block

          nodeVersionMonitor nds nodeAddr nodeId
        liftIO (nodeQuery rChain) >>= inDb . \case
          Left (RpcError_RestrictedEndpoint _) -> do
            let p = (NodeDetails_dataField ~>)
                nwStat = NetworkStat 0 0 0 0
            update
              [ p NodeDetailsData_headLevelSelector =. (Nothing :: Maybe RawLevel)
              , p NodeDetailsData_headBlockHashSelector =. (Nothing :: Maybe BlockHash)
              , p NodeDetailsData_headBlockPredSelector =. (Nothing :: Maybe BlockHash)
              , p NodeDetailsData_headBlockBakedAtSelector =. (Nothing :: Maybe UTCTime)
              , p NodeDetailsData_peerCountSelector =. (Nothing :: Maybe Word64)
              , p NodeDetailsData_networkStatSelector =. nwStat
              , p NodeDetailsData_fitnessSelector =. (Nothing :: Maybe Fitness)
              ] (NodeDetails_idField ==. nodeId)
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
    inDb :: (MonadIO m, MonadBaseNoPureAborts IO m, MonadLoggerIO m, MonadLogger m) => AppSerializable a -> m a
    inDb = runDb (Identity db) . flip runReaderT appConfig

-- Send a 'bad branch' alert if 'is_bootstrapped' response isn't bootstrapped and synced.
nodeAlertMonitor
  :: BlockLike blk
  => NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> URI
  -> Id Node
  -> blk
  -> IO ()
nodeAlertMonitor nds appConfig db nodeAddr nodeId nodeHead = runLoggingEnv (_nodeDataSource_logger nds) $ do
  let httpMgr = nds ^. nodeDataSource_httpMgr
      chainId = nds ^. nodeDataSource_chain
  mbNode <- headMay . toList <$> getNodes db (NodeDetails_idField ==. nodeId)
  latestHead <- liftIO $ readTVarIO $ nds ^. nodeDataSource_latestHead
  for_ mbNode $ \(Node, _, nodeDetails) -> do
    isBootstrapped :: Either RpcError IsBootstrapped <-
      runExceptT $ flip runReaderT (NodeRPCContext httpMgr $ Uri.render nodeAddr) $ nodeRPC $ rIsBootstrapped chainId
    action' :: Either KilnRpcError (ReaderT AppConfig Serializable ()) <- flip runReaderT nds $ runExceptT @KilnRpcError $ do
      case isBootstrapped of
        Left _ -> pure $ when (Just (nodeHead ^. level) < fmap (^. level) latestHead) $
          reportBadNodeHeadError nodeId latestHead nodeHead False SyncState_Unsynced
        Right (IsBootstrapped bootstrapped chainStatus) -> do
          let bad  = reportBadNodeHeadError nodeId latestHead nodeHead bootstrapped chainStatus
              good = clearBadNodeHeadError nodeId

              sufficientPeers :: ReaderT AppConfig Serializable ()
              sufficientPeers = clearNodeInsufficientPeersError nodeId

          Only isNodeAlive : _ <- runDb (Identity db)
            [queryQ|
              select count(el.id) = 0 from "ErrorLogInaccessibleNode" ein
              join "ErrorLog" el on el.id = ein.log
              where ein.node = ?nodeId and el.stopped is null
            |]

          let curPeerCount  = fromMaybe maxBound $ _nodeDetailsData_peerCount nodeDetails
              syncThreshold = _nodeDetailsData_synchronisationThreshold nodeDetails
              syncThreshold64 = fromIntegral @Word8 @Word64 syncThreshold

          if curPeerCount < syncThreshold64 && isNodeAlive then return $ do
            when (Just (nodeHead ^. level) < fmap (^. level) latestHead || not bootstrapped) $
              void bad
            reportNodeInsufficientPeersError nodeId syncThreshold curPeerCount
          else return $ do
            sufficientPeers
            case (bootstrapped, chainStatus) of
              (True, SyncState_Synced) -> good
              _ -> bad

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
  | BakerVotingState_Adoption  -- no voting takes place
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
amendmentProcessWorker appConfig nds db = worker' "amendmentProcessWorker" $ waitForNewHead nds >>= \latestHead -> runLoggingEnv (_nodeDataSource_logger nds) $ do
  (latestBlock, protoInfo) <- throwing $ runNodeQueryT $ liftA2 (,)
    (nodeQueryDataSourceSafe $ NodeQuery_Block (latestHead ^. hash))
    (getProtocolConstants $ Left $ latestHead ^. hash)
  let
    blocksPerVotingPeriod = _protoInfo_blocksPerVotingPeriod protoInfo
    chainId = _nodeDataSource_chain nds

    -- The RPCs under /votes/ return the information for the *next block*, not the current block.
    -- So we might have a voting_period_position of blocks_per_voting_period-1 in a given block
    -- (the last block of the period), but /votes/current_period_kind for that block will return
    -- the *next* period kind.
    votingPeriod = latestBlock ^. blockMetadata . blockMetadata_votingPeriodInfo . votingPeriodInfo_votingPeriod . votingPeriod_index
    currentVotingPosition = latestBlock ^. blockMetadata . blockMetadata_votingPeriodInfo . votingPeriodInfo_position
    isLastBlockOfPeriod blk = blocksPerVotingPeriod == succ (blk ^. blockMetadata . blockMetadata_votingPeriodInfo . votingPeriodInfo_position)
    -- The period of the *current* block
    currentPeriodKind = latestBlock ^. blockMetadata . blockMetadata_votingPeriodInfo . votingPeriodInfo_votingPeriod . votingPeriod_kind
    periodFraction = fromIntegral currentVotingPosition / fromIntegral blocksPerVotingPeriod :: Double

    singleVotePeriod pkh periodKindOffset mkVotingState = do
      let blk = latestHead ^.hash
      mBallot <- runMaybe $ nodeQueryDataSource $ NodeQuery_Ballot blk pkh
      -- The voting period of the last proposal period
      let amendmentPeriod = latestBlock ^. blockMetadata . blockMetadata_votingPeriodInfo .
            votingPeriodInfo_votingPeriod . votingPeriod_index - periodKindOffset

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

      VotingPeriodKind_Exploration -> singleVotePeriod pkh 1 BakerVotingState_Exploration
      VotingPeriodKind_Cooldown -> pure BakerVotingState_Testing
      VotingPeriodKind_Promotion -> singleVotePeriod pkh 3 BakerVotingState_Promotion
      VotingPeriodKind_Adoption -> pure BakerVotingState_Adoption

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
          BakerVotingState_Testing -> clearAllErrors
          BakerVotingState_Promotion previouslyVoted -> singleVotePhase previouslyVoted
          BakerVotingState_Adoption -> clearAllErrors -- TODO: Make
          -- sure this makes sense!

  -- Any *lesser* periods should be updated to the values at the block level of the end of the given period.
  -- Current period should be updated to the values of the latest block.
  -- Any *greater* periods should be blanked out.

  for_ [minBound..maxBound] $ \p -> case compare p currentPeriodKind of
    LT -> do
      let periodDiff = fromIntegral $ fromEnum currentPeriodKind - fromEnum p
          startBlockLevel = latestBlock ^. level - currentVotingPosition - periodDiff * blocksPerVotingPeriod
          endBlockLevel = startBlockLevel + blocksPerVotingPeriod - 1

      -- Ignore the update if the block cannot be obtained from current set of nodes
      -- Calc the blockLevel at the start of the current voting period, move
      -- back by periodDiff voting periods, and move to the end of that period
      mEndBlock <- flip runReaderT nds . runExceptT @KilnRpcError $ getBlockLevelAncestor (latestBlock ^. level - endBlockLevel) (latestBlock ^. hash)
      for_ mEndBlock $ \endBlock -> do
        (startBlockTimestamp, periodEndBlockPred) <- do
          startBlock <- flip runReaderT nds . runExceptT @KilnRpcError $
            fmap toBlockHeaderCrossCompat $ getBlockLevelAncestor (latestBlock ^. level - startBlockLevel) (latestBlock ^. hash)
          let startBlockTimestamp = case startBlock of
                Right block -> block ^. timestamp
                Left _ -> Unsafe.unsafeEstimatePastTimestamp protoInfo startBlockLevel latestBlock
          predBlock <- throwing $ getBlockHeader $ endBlock ^. predecessor
          pure (startBlockTimestamp, predBlock)
        updateTo startBlockLevel startBlockTimestamp periodEndBlockPred endBlock p
    EQ -> do
      let startBlockLevel = latestBlock ^. level - currentVotingPosition
      startBlock <- flip runReaderT nds . runExceptT @KilnRpcError $
        fmap toBlockHeaderCrossCompat $ getBlockLevelAncestor currentVotingPosition (latestBlock ^. hash)
      let startBlockTimestamp = case startBlock of
            Right block -> block ^. timestamp
            Left _ -> Unsafe.unsafeEstimatePastTimestamp protoInfo startBlockLevel latestBlock
      predOrLatest <-
        if isLastBlockOfPeriod latestBlock
        then throwing $ getBlockHeader $ latestBlock ^. predecessor -- For some queries we need to use the predecessor block
        else pure (toBlockHeaderCrossCompat latestBlock)
      updateTo startBlockLevel startBlockTimestamp predOrLatest latestBlock p
    GT -> runDb (Identity db) $ do
      wipe p
      notify NotifyTag_Amendment (p, Nothing)
      case p of
        VotingPeriodKind_Proposal -> pure () -- can never happen
        VotingPeriodKind_Exploration -> notify NotifyTag_PeriodTestingVote Nothing
        VotingPeriodKind_Cooldown -> notify NotifyTag_PeriodTesting Nothing
        VotingPeriodKind_Promotion -> notify NotifyTag_PeriodPromotionVote Nothing
        VotingPeriodKind_Adoption -> notify NotifyTag_PeriodAdoption Nothing

  where
    getBlockHeader hash' = nodeQueryDataSource $ NodeQuery_BlockHeader hash'
    getBlockLevelAncestor lvl hash' = nodeQueryDataSource $ NodeQuery_BlockPred hash' lvl

    throwing :: (Monad m, MonadLogger m) => ExceptT KilnRpcError (ReaderT NodeDataSource m) a -> m a
    throwing = (>>= either logThenThrow pure) . flip runReaderT nds . runExceptT @KilnRpcError
      where

        logThenThrow e' = do
            logKilnRpcError "amendmentProcessWorker" e'
            error $ case e' of
              KilnRpcError_RpcError (RpcError_NonJSON url e'' _bytes) -> mconcat
                                  ["Node Query failed for 'amendmentProcessWorker' Reason at url ("
                                  , T.unpack url
                                  , "): "
                                  , "The RPC returned a response that kiln did not understand. JSON Parse Error: "
                                  , e''
                                  , ". The response can be in found in the logs in namespace kiln-debugging."
                                  ]
              _ -> T.unpack $ "Node Query failed for 'amendmentProcessWorker' Reason: " <> prettyKilnRpcError e'

    catchUnsuitableNode :: (Monad m, MonadLogger m, MonadThrow m) => ExceptT KilnRpcError (ReaderT NodeDataSource m) a -> m (Maybe a)
    catchUnsuitableNode action = do
      res <- flip runReaderT nds $ runExceptT @KilnRpcError action
      case res of
        Right r -> pure $ Just r
        Left (KilnRpcError_NoSuitableNode _ _) -> pure Nothing
        Left e -> throwM e

    runMaybe :: Functor m => ExceptT KilnRpcError (ReaderT NodeDataSource m) (Maybe a) -> m (Maybe a)
    runMaybe = fmap (either (const Nothing) id) . flip runReaderT nds . runExceptT

    wipe p = do
      delete $ Amendment_periodField ==. p
      case p of
        VotingPeriodKind_Proposal -> pure ()
        VotingPeriodKind_Exploration -> deleteAll' @PeriodTestingVote Proxy
        VotingPeriodKind_Cooldown -> deleteAll' @PeriodTesting Proxy
        VotingPeriodKind_Promotion -> deleteAll' @PeriodPromotionVote Proxy
        VotingPeriodKind_Adoption -> deleteAll' @PeriodAdoption Proxy

    updateTo startLevel startTimestamp predBlk blk p = do
      let position' = blk ^. blockMetadata . blockMetadata_votingPeriodInfo . votingPeriodInfo_position
          votingPeriod = blk ^. blockMetadata . blockMetadata_votingPeriodInfo . votingPeriodInfo_votingPeriod . votingPeriod_index
          chainId = _nodeDataSource_chain nds
          amendment = Amendment
            { _amendment_period = p
            , _amendment_chainId = chainId
            , _amendment_votingPeriod = votingPeriod
            , _amendment_start = startTimestamp
            , _amendment_startLevel = startLevel
            , _amendment_position = position'
            }
      runDb (Identity db) $ do
        wipe p
        insert_ amendment
        notify NotifyTag_Amendment (p, Just amendment)
      case p of
        VotingPeriodKind_Proposal -> do
          mbProposals <- catchUnsuitableNode $ nodeQueryDataSource $ NodeQuery_Proposals (predBlk ^. hash)
          whenJust mbProposals $ \proposals -> do
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
        VotingPeriodKind_Exploration -> handleVotingPeriod predBlk PeriodTestingVote NotifyTag_PeriodTestingVote
        VotingPeriodKind_Cooldown -> do
          mProposal <- runMaybe $ nodeQueryDataSource $ NodeQuery_CurrentProposal (predBlk ^. hash)
          for_ mProposal $ \proposal -> do
            runDb (Identity db) $ do
              ts <- (fmap . fmap) fromOnly [queryQ|
                INSERT INTO "PeriodTesting"
                (SELECT p.id FROM "PeriodProposal" p WHERE p.hash = ?proposal)
                RETURNING proposal
              |]
              for_ ts $ \ph -> notify NotifyTag_PeriodTesting $ Just PeriodTesting
                { _periodTesting_proposal = ph }
        VotingPeriodKind_Promotion -> handleVotingPeriod predBlk PeriodPromotionVote NotifyTag_PeriodPromotionVote
        VotingPeriodKind_Adoption -> handleVotingPeriod predBlk PeriodAdoption NotifyTag_PeriodAdoption

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
protocolMonitorWorker nds db = worker' "protocolMonitorWorker" $ waitForNewHead nds >>= \latestHead -> runLoggingEnv (_nodeDataSource_logger nds) $ do
  $(logDebugSH) ("protocolMonitorWorker: Started"::Text,())
  let
    getProtocol = getProtocol' >>= \case
      Right p -> return p
      Left e -> do
        logKilnRpcError "protocolMonitorWorker: fetch protocol" e
        threadDelay' 1
        getProtocol

    -- There is a hardfork for babylon where if our node sees BABY5H promoted it'll get upgraded to BabyM1.
    -- So if we see BABY5H about to be promoted, we actually want to start the PsBabyM1 alt baker instead.
    babyHax :: ProtocolHash -> ProtocolHash
    babyHax "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU" = "PsBabyM1eUXZseaJdmXFApDSBqj8YBfwELoxZHHW77EMcAbbwAS"
    babyHax ph = ph

    -- Yet another hardfork happened on hangzhou, so we should follow it as well
    hangzhouHax :: ProtocolHash -> ProtocolHash
    hangzhouHax "PtHangzHogokSuiMHemCuowEavgYTP8J5qQ9fQS793MHYFpCY3r" = "PtHangz2aRngywmSRGGvrcTyMbbdpWdpFKuS4uMWxg2RaH9i1qx"
    hangzhouHax ph = ph

    getProtocol' = flip runReaderT nds $ runExceptT @KilnRpcError $ do
      blk <- nodeQueryDataSource $ NodeQuery_Block (latestHead ^. hash)
      let vp = blk ^. blockMetadata . blockMetadata_votingPeriodInfo . votingPeriodInfo_votingPeriod . votingPeriod_kind
          currentProtocol = blk ^. blockMetadata . blockMetadata_protocol
      remainingBlocksInVotingPeriod <- case blk ^. blockMetadata . blockMetadata_votingPeriodInfo . votingPeriodInfo_remaining of
        Just remaining -> return remaining
        Nothing -> do
          let votingPeriodPosition = blk ^. blockMetadata . blockMetadata_votingPeriodInfo . votingPeriodInfo_position
          protoConstants <- runNodeQueryT $ getProtocolConstants $ Right currentProtocol
          return $ protoConstants ^. protoInfo_blocksPerVotingPeriod - votingPeriodPosition - 1
      -- This will trigger daemons for the upcoming protocol to start, so we start them
      -- on the last voting period 100 blocks prior to the protocol upgrade.
      tp <- if vp == VotingPeriodKind_Adoption && remainingBlocksInVotingPeriod < 100
        then fmap (hangzhouHax . babyHax) <$> nodeQueryDataSource (NodeQuery_CurrentProposal (latestHead ^. hash))
        else return Nothing
      return (blk ^. blockMetadata . blockMetadata_protocol, tp)

  (mainProto, altProto) <- getProtocol

  let
    inDb :: Serializable a -> LoggingT IO a
    inDb = runDb (Identity db)
    setControl c ps = update
      [ ProcessData_controlField =. c
      , ProcessData_errorLogField =. (Nothing :: Maybe Text)
      ] (AutoKeyField `in_` map fromId ps)

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
  waitTillEndOfCycle nds latestHead

waitTillEndOfCycle
  :: ( MonadIO m
     , MonadLoggerIO m
     , MonadLogger m
     , BlockLike blk
     , MonadBaseNoPureAborts IO m
     , MonadMask m
     )
  => NodeDataSource -> blk -> m ()
waitTillEndOfCycle nds blk = do
  lastLevel :: Either KilnRpcError RawLevel <- flip runReaderT nds $ runExceptT $ runNodeQueryT $ do
    c <- levelToCycle (blk ^. hash, blk ^. level) (blk ^. level)
    lastLevelInCycle (blk ^. hash) c
  for_ lastLevel $ \lvl -> do
    liftIO $ atomically $ do
      newHead <- maybe retry pure =<< readTVar (_nodeDataSource_latestHead nds)
      when (newHead ^. level < lvl) retry
