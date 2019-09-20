{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
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
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT, runExceptT, unless)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebug, logDebugSH, logError, logErrorSH, logInfo, logWarn, logWarnSH)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
import Data.Align
import Data.Foldable (foldl', length)
import Data.Functor.Apply
import Data.Function (fix)
import Data.Int (Int32)
import qualified Data.LCA.Online.Polymorphic as LCA
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Pool (Pool)
import qualified Data.Set as S
import Data.String.Here.Interpolated (i)
import Data.These
import Data.Time (NominalDiffTime, diffUTCTime)
import Data.Word (Word8)
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql(..), in_, isFieldNothing, (&&.), (=.), (==.))
import qualified Network.HTTP.Client as Http
import Reflex.Class (fmapMaybe)
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (getTime, runDb, selectMap, project1)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, executeQ, In(..), sql, returning, queryQ, PGArray(..))
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Backend.Schema (toId, fromId)
import Rhyolite.Schema (Id (..))
import Safe.Foldable (maximumMay, maximumByMay)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.Block (toBlockHeader)
import Tezos.History (CachedHistory (..), Prehistory(..), addHeadBlock, accumPrehistory, initializeBlocks)
import Tezos.NodeRPC (NodeRPCContext (..), PlainNodeStream, RpcError(..), RpcQuery, rChain, rConnections,
                      rMonitorHeads, rNetworkStat, rCheckpoint)
import Tezos.NodeRPC.Network (PublicNodeContext (..), getCurrentHead, nodeRPC, nodeRPCChunked, getHistory)
import Tezos.NodeRPC.Sources (PublicNode (..), PublicNodeError (..))
import qualified Tezos.ProtocolConstants as ProtocolConstants
import Tezos.Types
import qualified Tezos.TestChainStatus as Tezos

import Backend.Alerts (clearBadNodeHeadError, clearInaccessibleNodeError, clearNodeWrongChainError,
                       reportBadNodeHeadError, reportInaccessibleNodeError, reportNodeWrongChainError,
                       reportNodeInvalidPeerCountError, clearNodeInvalidPeerCountError,
                       clearPastVotingPeriodErrors, reportVotingReminderError)
import Backend.CachedNodeRPC
import Backend.Common (unsupervisedWorkerWithDelay, threadDelay', worker', oneShot, workerWithDelay, timeout')
import Backend.Config (AppConfig (..), kilnNodeRpcURI)
import Backend.IndexQueries
import Backend.Schema
import Backend.Supervisor (withTermination)
import Backend.STM (atomicallyWith)
import Backend.ViewSelectorHandler (getProposals)
import Common (unixEpoch)
import Common.App (getEndTimeForPeriod)
import Common.Schema
import ExtraPrelude
import qualified Data.Sequence as Seq
import qualified Database.PostgreSQL.Simple.ToField as PG

import Tezos.Base58Check (HashedValue(..))
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short  as BS
import Data.ByteString.Builder (shortByteString, byteString)


-- We assume that the implicit nodeaddr is the same one we just learned the new
-- branch from, so we insist that we bootstrap from it (rather than using a
-- pool of nodes)

haveNewHead
  :: (MonadIO m, MonadBaseNoPureAborts IO m, BlockLike blk)
  => NodeDataSource
  -> Maybe PublicNode
  -> URI
  -> blk
  -> m ()
haveNewHead nds pn nodeAddr headBlockInfo = runLoggingEnv (_nodeDataSource_logger nds) $ do
  $(logInfo) [i|Node reported new head: ${headBlockInfo ^. hash}, level ${headBlockInfo ^. level}|]
  res <- runExceptT $ do
    flip runReaderT (nds { _nodeDataSource_nodeForQuery = Just nodeAddr }) $ do
      nodeQueryDataSourceImmediate $ NodeQuery_BlockHeader $ headBlockInfo ^. hash
  case res of
    Left (e :: CacheError) -> do
      $(logWarn) [i|Failed to handle new node head: ${e}|]
    Right headBlockHeader -> do
      haveNewHead' nds pn nodeAddr headBlockHeader

haveNewHead'
  :: (MonadIO m, MonadBaseNoPureAborts IO m, MonadLogger m)
  => NodeDataSource
  -> Maybe PublicNode
  -> URI
  -> BlockHeader
  -> m ()
haveNewHead' nds pn nodeAddr headBlock = do
  let
    httpMgr = _nodeDataSource_httpMgr nds
    chainId = _nodeDataSource_chain nds
    historyVar = _nodeDataSource_history nds
    publicNodeContext = PublicNodeContext (NodeRPCContext httpMgr $ Uri.render nodeAddr) pn

  let
    upsertHeadBlock = upsertBlockHeader chainId headBlock

  let
    updateMemCache xs = do
      mHistory' <- liftIO . atomically $ do
        history <- readTVar historyVar
        let mHistory' = addHeadBlock xs headBlock history
        writeTVar historyVar $ fromMaybe history mHistory'
        return mHistory'
      case mHistory' of
        Nothing -> fail "the 'impossible' happened: updateMemCache: invariants violated"
        Just !_ -> return ()

  let
    updatePgCache lvl xs = do
      let blockHashPgArray = PGArray xs
      void [executeQ|
        INSERT INTO "BlockShellIndex"
             ( hash   , predecessor , "chainId" , level     )
       (SELECT x[i+1] , x[i]        , ?chainId  , ?lvl + i
          FROM (VALUES (?blockHashPgArray::_bytea)) a(x)
          JOIN LATERAL generate_series(1,array_length(x,1)::integer-1) i
            ON TRUE) ON CONFLICT DO NOTHING
      |]

  let
    getBlockShellAncestors blkHash = do
      -- FIXME: make the number of predecessors to fetch configurable for QA
      map fromOnly <$> [queryQ|
        SELECT predecessor FROM "blockShellAncestors"(?blkHash, 21)
      |]

  let
    getHistoryFromPg knownBlocks blkHash = do
      getBlockShellAncestors blkHash >>= \case
        [] -> return []
        hashes -> go (reverse hashes)
      where
        failMsg = fail "the 'impossible' happened: getHistoryFromPg: invariant(s) violated"
        go [] = failMsg
        go hashes@(blkHash' : _) = do
          if Map.member blkHash' knownBlocks
          then return hashes
          else do
            hashes' <- getBlockShellAncestors blkHash'
            when (null hashes') failMsg
            go (foldl' (flip (:)) hashes hashes')

  let
    getHistoryFromNodeRPC oldHead knownBlocks = do
      let blkLevel    = headBlock ^. level
          -- FIXME: make the number of additional levels to fetch configurable for QA
          fetchLevels = 11 + max 0 (blkLevel - (oldHead ^. level))
      xs <- getHistory chainId headBlock fetchLevels (S.singleton (oldHead ^. hash))
      let len = length xs
      when (len < 2) failMsg
      let lvl = blkLevel - fromIntegral len
      go lvl (foldl' (flip (:)) [] xs)
     where
      failMsg = fail "the 'impossible' happened: getHistoryFromNodeRPC: invariant(s) violated"
      go lvl blks@(predHash:blkHash:_) = do
        if Map.member predHash knownBlocks
        then do
          updatePgCache lvl blks
          return blks
        else do
          pgBlks <- getHistoryFromPg knownBlocks predHash
          if not (null pgBlks)
          then do
            updatePgCache lvl blks
            return (pgBlks ++ blks)
          else do
            let bs = BlockSpine {
                       _blockSpine_hash = blkHash
                     , _blockSpine_predecessor = predHash
                     , _blockSpine_level = lvl
                     }
            xs <- getHistory chainId bs 11 mempty
            let len = length xs
            when (len < 2) failMsg
            let lvl' = lvl - fromIntegral len + 1
            go lvl' (foldl' (flip (:)) blks (Seq.drop 1 xs))
      go _ _ = failMsg

  let
    getHistoryImpl oldHead knownBlocks = do
      rsp <- runExceptT . flip runReaderT publicNodeContext $ do
        xs <- getHistoryFromPg knownBlocks (headBlock ^. predecessor)
        if null xs
        then getHistoryFromNodeRPC oldHead knownBlocks
        else return xs
      case rsp of
        Left (e :: PublicNodeError) -> do
          $(logWarn) [i|Failed to handle new node head: ${e}|]
          return []  -- FIXME?  What should we do here?
        Right h -> do
          return h

  let
    updateLatestHead oldHead = do
      let blkFitness = headBlock ^. fitness
      when (blkFitness > oldHead ^. fitness) $ do
        updatedLevel <- liftIO $ atomically $ do
          let latestHeadTVar = _nodeDataSource_latestHead nds
          latestHead <- readTVar latestHeadTVar
          if Just blkFitness > latestHead ^? _Just . fitness
            then do
              writeTVar latestHeadTVar $ Just $ mkVeryBlockLike headBlock
              pure $ Just $ headBlock ^. level
            else
              pure Nothing
        for_ updatedLevel $ \lev -> $(logDebug) $ "Saw more recent head: " <> tshow (unRawLevel lev)

  let
    blkHash = headBlock ^. hash
    predHash = headBlock ^. predecessor
    db = _nodeDataSource_pool nds
    failMsg = fail "the 'impossible' happened: haveNewHead: invariant(s) violated"

  fix $ \tryAgain -> do
    (mOldHead, history) <- liftIO $ atomically $ liftA2 (,) (dataSourceHead nds) (readTVar historyVar)

    let knownBlocks = _cachedHistory_blocks history

    if Map.member blkHash knownBlocks
    then do
      runDb (Identity db) upsertHeadBlock
    else if Map.member predHash knownBlocks
      then do
        runDb (Identity db) upsertHeadBlock
        updateMemCache []
        -- having a nonempty cache implies that mOldHead is Just
        oldHead <- maybe failMsg return mOldHead
        updateLatestHead oldHead
      else if Map.null knownBlocks
        then do
          restoreCachedHistory nds $ Just (headBlock, publicNodeContext)
          tryAgain
        else do
          oldHead <- maybe failMsg return mOldHead
          -- note the use of 'join' to correctly delimit the scope of the transaction
          join . runDb (Identity db) $ do
            xs <- getHistoryImpl oldHead knownBlocks
            if null xs
            then do
              -- here, we are in the transaction
              return $ do
                -- here, we are outside the transaction, but this code path has nothing to do.
                return ()
            else do
              -- we upsertHeadBlock while we are still in the transaction
              upsertHeadBlock
              return $ do
                -- then, after this transaction successfully completes,  we update the in-memory LCA cache
                updateMemCache xs
                updateLatestHead oldHead

nodeMonitor :: NodeDataSource -> AppConfig -> URI -> Id Node -> MonitorBlock -> (Maybe RawLevel, Maybe Cycle) -> IO ()
nodeMonitor nds appConfig nodeAddr nodeId headBlockInfo mSpData = do
  let db = _nodeDataSource_pool nds
  runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $ flip runReaderT appConfig $ do
    -- This isn't very nuanced: old, stale nodes, even if they are catching
    -- up, will churn a lot here.  Maybe we could improve this to filter
    -- out "new" blocks that are already on the branch of `oldHead`?
    now <- getTime
    let newHash = headBlockInfo ^. hash
        newLevel = headBlockInfo ^. level
        chainId = _nodeDataSource_chain nds
     in do
      void $ [executeQ|
        insert into "BlockTodo" (hash, level, chain, "claimedBy", "claimedAt", "parsedParent", "parsedAccusations")
        values (?newHash, ?newLevel, ?chainId, null, null, false, false)
        on conflict do nothing
        |]
      void $ [executeQ|
        insert into "IxBlockTodo" (hash, level, chain, "claimedBy", "claimedAt", "parsedParent")
        values (?newHash, ?newLevel, ?chainId, null, null, false)
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
          , _nodeDetailsData_savePoint = fst mSpData
          , _nodeDetailsData_savePointUpdated = snd mSpData
          }
        }
      (_:_) -> update
        ([ p NodeDetailsData_headLevelSelector =. Just (headBlockInfo ^. monitorBlock_level)
        , p NodeDetailsData_headBlockHashSelector =. Just (headBlockInfo ^. monitorBlock_hash)
        , p NodeDetailsData_headBlockBakedAtSelector =. Just (headBlockInfo ^. monitorBlock_timestamp)
        , p NodeDetailsData_fitnessSelector =. Just (headBlockInfo ^. monitorBlock_fitness)
        , p NodeDetailsData_updatedSelector =. Just now
        , p NodeDetailsData_headBlockPredSelector =. Just (headBlockInfo ^. monitorBlock_predecessor)
        ] <> maybe [] (\sp -> [ p NodeDetailsData_savePointSelector =. Just sp
                              , p NodeDetailsData_savePointUpdatedSelector =. snd mSpData
                              ]) (fst mSpData)
        )
        (NodeDetails_idField `in_` [nodeId])
    newNodeDetails <- project NodeDetails_dataField $ (NodeDetails_idField ==. nodeId) `limitTo` 1
    traverse_ (notify NotifyTag_NodeDetails . (nodeId,) . Just) newNodeDetails

  atomically $ do
    writeTQueue (_nodeDataSource_ioQueue nds) $ haveNewHead nds Nothing nodeAddr headBlockInfo

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
  workerWithDelay (pure delay) $ const $ runLoggingEnv (_nodeDataSource_logger nds) $ do
    $(logDebug) "Update node cycle."

    -- read the persistent list of nodes
    theseNodeRecords <- getNodes db CondEmpty

    -- give them all a chance to
    ifor_ theseNodeRecords $ \nodeId (Node, node, nodeDetails) ->
      updateNetworkStats appConfig httpMgr db nodeId node nodeDetails >>= \case
        Left _e -> inDb $ reportInaccessibleNodeError nodeId
        Right () -> pure () -- We'll rely on the block monitor to clear this error

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

        nodeQuery :: RpcQuery a -> IO (Either RpcError a)
        nodeQuery f = runLoggingEnv (_nodeDataSource_logger nds) $ runExceptT $ runReaderT (nodeRPC f) $ NodeRPCContext httpMgr $ Uri.render nodeAddr

        chunkedNodeQuery :: PlainNodeStream a -> (a -> IO ()) -> IO (Either RpcError ())
        chunkedNodeQuery f k = runLoggingEnv (_nodeDataSource_logger nds) $ runExceptT $ runReaderT (nodeRPCChunked f k) $ NodeRPCContext httpMgr $ Uri.render nodeAddr

        updateCheckpoint :: (MonadIO m, MonadLogger m)
          => MonitorBlock
          -> (Maybe (Maybe RawLevel, Maybe Cycle), Maybe ProtoInfo)
          -> m (Maybe RawLevel, Maybe Cycle)
        updateCheckpoint blk (mSavePointData, mProtoInfo) = do
          let
            mCurrentCycle = fmap (\protoInfo -> ProtocolConstants.unsafeAssumptionLevelToCycle protoInfo (blk ^. level)) mProtoInfo
            mLastCycle = snd =<< mSavePointData
            mSavePoint = fst =<< mSavePointData
            skipUpdate = isJust mSavePoint && isJust mCurrentCycle && mCurrentCycle == mLastCycle

          if skipUpdate
            then pure (Nothing, Nothing)
            else do
              $(logInfo) [i|nodeWorker: fetching checkpoint for Node: ${nodeAddr}|]
              liftIO (nodeQuery $ rCheckpoint chainId) >>= \case
                Left (RpcError_UnexpectedStatus 404 _) -> pure (Just 0, mCurrentCycle)
                Right checkpoint -> pure (Just $ _checkpoint_savePoint checkpoint, mCurrentCycle)
                _ -> (Nothing, Nothing) <$ $(logError) [i|nodeWorker: could not fetch checkpoint for Node: ${nodeAddr}|]

      killMonitor <- unsupervisedWorkerWithDelay reconnectDelay $ runLoggingEnv (_nodeDataSource_logger nds) $ do
        _ <- liftIO $ chunkedNodeQuery (rMonitorHeads chainId) $ \block -> do
          -- Since we receive a new head, we can clear connectivity and wrong-chain errors for this node.
          mNewSp <- runLoggingEnv (_nodeDataSource_logger nds) $ do
            mSavePointAndProto <- inDb $ do
              clearInaccessibleNodeError nodeId
              clearNodeWrongChainError nodeId
              mSavePoint <- project1 (NodeDetails_dataField ~> NodeDetailsData_savePointSelector, NodeDetails_dataField ~> NodeDetailsData_savePointUpdatedSelector) (NodeDetails_idField `in_` [nodeId])
              mProto <- project1 ProtocolIndex_constantsField (ProtocolIndex_protoField ==. _monitorBlock_proto block &&. ProtocolIndex_chainIdField ==. chainId)
              pure (mSavePoint, mProto)
            updateCheckpoint block mSavePointAndProto

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

      secsTillNextBlock :: Either CacheError (Maybe NominalDiffTime) <-
        runLoggingEnv (_nodeDataSource_logger nds) $ flip runReaderT nds $ runExceptT $ runNodeQueryT $ do
          mLastBlock <- project1 PublicNodeHead_headBlockField $
            PublicNodeHead_sourceField ==. pn &&. PublicNodeHead_chainField ==. NamedChainOrChainId chain
          for mLastBlock $ \lastBlock -> do
            protoConstants <- getProtocolConstants $ Left $ lastBlock ^. hash
            now <- getTime
            let
              timeBetweenBlocks = calcTimeBetweenBlocks protoConstants

              secsSinceLastBlock = _veryBlockLike_timestamp lastBlock `diffUTCTime` now
              secsTillNextBlock = case secsSinceLastBlock + timeBetweenBlocks of
                    -- if the next expected block is in the past, the node is probably quite laggy and we give it a little more delay
                  x | x <= 0 -> timeBetweenBlocks / 2
                    | otherwise -> x
            pure secsTillNextBlock

      _ <- timeout' (fromMaybe 5 $ secsTillNextBlock ^? _Right . _Just) (waitForNewHead nds)
      threadDelay' 5 -- always give a little extra delay to make it more likely the public node reports the new block

updateDataSource
  :: forall m. (MonadIO m, MonadBaseNoPureAborts IO m)
  => NodeDataSource -> DataSource -> m ()
updateDataSource nds (pn, chain, uri) = do
  enabled <- publicNodeEnabled
  when enabled updatePublicNodeInDb

  where
    db = _nodeDataSource_pool nds
    chainId = _nodeDataSource_chain nds

    queryPublicNode
      :: forall a. ReaderT PublicNodeContext (ExceptT PublicNodeError m) a
      -> m (Either PublicNodeError a)
    queryPublicNode k = runExceptT $
      runReaderT k $
        PublicNodeContext (NodeRPCContext (_nodeDataSource_httpMgr nds) (Uri.render uri)) (Just pn)

    getHeadFromSource :: m (Either PublicNodeError (WithProtocolHash VeryBlockLike))
    getHeadFromSource = queryPublicNode $ runLoggingEnv (_nodeDataSource_logger nds) $ getCurrentHead chainId

    publicNodeEnabled :: m Bool
    publicNodeEnabled = fmap (fromMaybe False . listToMaybe) $
      runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $
        project PublicNodeConfig_enabledField (PublicNodeConfig_sourceField ==. pn)

    updatePublicNodeInDb :: m ()
    updatePublicNodeInDb = getHeadFromSource >>= runLoggingEnv (_nodeDataSource_logger nds) . \case
      Left e -> $(logErrorSH) ("updatePublicNodeInDb"::Text,(pn,chain,Uri.render uri),e)
      Right b -> do
        liftIO $ atomically $
          writeTQueue (_nodeDataSource_ioQueue nds) $ haveNewHead nds (Just pn) uri b
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
                  , _publicNodeHead_headBlock = b ^. withProtocolHash_value
                  , _publicNodeHead_protocolHash = b ^. protocolHash
                  , _publicNodeHead_updated = now
                  }
              notify NotifyTag_PublicNodeHead . (, Just pnh) =<< insert' pnh
            Just eid -> do
              updateId eid
                [ PublicNodeHead_headBlockField =. b ^. withProtocolHash_value
                , PublicNodeHead_protocolHashField =. b ^. protocolHash
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
      nodeHead <- nodeQueryDataSource (NodeQuery_BlockHeader nodeHeadHash)
      lcaBlock' <- atomicallyWith $ branchPoint nodeHeadHash (latestHead ^. hash)
      let bad = reportBadNodeHeadError nodeId latestHead nodeHead (fudgeVeryBlockLike <$> lcaBlock')
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
                 latestParent <- nodeQueryDataSource (NodeQuery_BlockHeader parentHash)
                 latestUncle <- nodeQueryDataSource (NodeQuery_BlockHeader uncleHash)
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
  (latestBlock, protoInfo) <- throwing $ runNodeQueryT $ liftA2 (,)
    (nodeQueryDataSourceSafe $ NodeQuery_Block (latestHead ^. hash))
    (getProtocolConstants $ Left $ latestHead ^. hash)
  history <- liftIO $ readTVarIO $ _nodeDataSource_history nds
  let
    blocksPerVotingPeriod = _protoInfo_blocksPerVotingPeriod protoInfo
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
          BakerVotingState_Testing -> clearAllErrors
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
        else pure (toBlockHeader latestBlock)
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
    getBlockHeader hash' = nodeQueryDataSource $ NodeQuery_BlockHeader hash'

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
  waitTillEndOfCycle nds latestHead

waitTillEndOfCycle
  :: ( MonadIO m
     , MonadLogger m
     , BlockLike blk
     , MonadBaseNoPureAborts IO m
     , MonadMask m
     )
  => NodeDataSource -> blk -> m ()
waitTillEndOfCycle nds blk = do
  lastLevel :: Either CacheError RawLevel <- flip runReaderT nds $ runExceptT $ runNodeQueryT $ do
    c <- levelToCycle $ blk ^. level
    lastLevelInCycle (blk ^. hash) c
  for_ lastLevel $ \lvl -> do
    liftIO $ atomically $ do
      newHead <- maybe retry pure =<< readTVar (_nodeDataSource_latestHead nds)
      when (newHead ^. level < lvl) retry

restoreCachedHistoryWorker :: NodeDataSource -> IO (IO ())
restoreCachedHistoryWorker nds =
  oneShot $ runLoggingEnv (_nodeDataSource_logger nds) $ restoreCachedHistory nds Nothing

restoreCachedHistory ::
  ( MonadIO m
  , MonadBaseNoPureAborts IO m
  , MonadLogger m
  ) => NodeDataSource -> Maybe (BlockHeader, PublicNodeContext) -> m ()
restoreCachedHistory nds mHeadCtx = do
  -- Honestly, using a join here feels funny.  I'd rather get rid of it,  and
  -- simply replace `c` with `$1`, and send `chainId` in a binary
  -- protocol-level parameter
  let db  =_nodeDataSource_pool nds
  $(logDebug) [i| restoreCachedHistory worker started, checking postgres table "BlockShellIndex" for cached data |]
  runDb (Identity db) [queryQ|
    SELECT DISTINCT "BlockShellIndex".*
      FROM (VALUES (?chainId::bytea)) a(c)
      JOIN "BlockShellIndex"
        ON "chainId" = c AND level IN
         ( (SELECT MIN(level) FROM "BlockShellIndex" WHERE "chainId" = c)
           UNION
           (SELECT MAX(level) FROM "BlockShellIndex" WHERE "chainId" = c)
         )
     ORDER BY level
  |] >>= \case
    [] -> do
      for_ mHeadCtx $ \headCtx@(hblk, _) -> do
        $(logInfo) [i| restoreCachedHistory doing backfill via RPC |]
        backfillBlockShellIndex nds (Left headCtx)
        next BlockShellIndex
          { _blockShellIndex_hash        = hblk ^. hash
          , _blockShellIndex_predecessor = hblk ^. predecessor
          , _blockShellIndex_chainId     = chainId
          , _blockShellIndex_level       = hblk ^. level
          , _blockShellIndex_fitness     = Just $! (hblk ^. fitness  )
          , _blockShellIndex_timestamp   = Just $! (hblk ^. timestamp)
          , _blockShellIndex_proto       = Just $! _blockHeader_proto hblk
          }
    [ blk ] -> do
      $(logInfo) [i| restoreCachedHistory proceeding from single block ${blk} |]
      backfillBlockShellIndex nds (Right blk)
      next blk
    ( blkA : blkB : _ ) -> do
      $(logInfo) [i| restoreCachedHistory proceeding with blocks ${blkA} and ${blkB} |]
      if (blkA ^. level) >= (blkB ^. level)
        -- FIXME: throw a proper exception here
      then fail "the 'impossible' happened: postgres is double rooted"
      else do
        backfillBlockShellIndex nds (Right blkA)
        next blkB
  where
    chainId = _nodeDataSource_chain nds
    historyVar = _nodeDataSource_history nds

    next blkMax = do
      let blkMaxHash = blkMax ^. hash
          minBranchLvl = blkMax ^. level - 100 -- FIXME? configurable constant?

      -- FIXME: we could eliminate all intermediate lists (and thus also the need
      -- to reverse the list) if the conversion from postgresql-libpq
      -- postgresql-simple wasn't blindingly stupid in this particular case.
      let db = _nodeDataSource_pool nds
      (spine, uncles) <- runDb (Identity db) $ do
        spine <- reverse . map fromOnly <$>
          [queryQ| SELECT hash FROM "blockShellAncestors"(?blkMaxHash) |]

        uncles <-  -- Fetch some of the recent uncles
          [queryQ| SELECT * FROM "BlockShellIndex"
                    WHERE "chainId" = ?chainId AND level >= ?minBranchLvl
                    ORDER BY level
          |]
        return (spine, uncles)


      let      -- our cache's "genesis" block need not be at level 0
        !levelZero = (blkMax ^. level) - fromIntegral (length spine) + 1
        !blocks0 = initializeBlocks spine
        !prehist0 = Prehistory (Map.singleton blkMaxHash blkMax) blocks0
        (Prehistory branches blocks)  = foldl' (flip accumPrehistory) prehist0 uncles
        toInt32 :: Word8 -> Int32 = fromIntegral
        protos = In . S.toList . foldl' delta S.empty $ branches
          where delta acc blk = maybe acc (flip S.insert acc . toInt32) (_blockShellIndex_proto blk)

      protoMap <- runDb (Identity db) $ do
         [queryQ| SELECT proto, hash FROM "ProtocolIndex" WHERE "chainId" = ?chainId AND proto IN ?protos |]

      let
        fudge blk = WithProtocolHash blk' protoHash
          where
            blk' = VeryBlockLike
              { _veryBlockLike_hash        = blk ^. hash
              , _veryBlockLike_predecessor = blk ^. predecessor
              , _veryBlockLike_level       = blk ^. level
              -- Theoretically, the next three "Nothing" cases shouldn't happen
              , _veryBlockLike_fitness     = fromMaybe mempty (_blockShellIndex_fitness   blk)
              , _veryBlockLike_timestamp   = fromMaybe unixEpoch  (_blockShellIndex_timestamp blk)
              }
            protoHash = fromMaybe emptyHash (flip lookup protoMap . toInt32 =<< _blockShellIndex_proto blk)
            emptyHash = HashedValue ""

      let
        !fudgedBranches = fudge <$> branches

      liftIO $ atomically $ do
        history <- readTVar historyVar
        when (Map.null (_cachedHistory_blocks history)) $ do
          writeTVar historyVar $! CachedHistory
            { _cachedHistory_blocks = blocks
            , _cachedHistory_branches = fudgedBranches
            , _cachedHistory_minLevel = _cachedHistory_minLevel history
            , _cachedHistory_levelZero = levelZero
            }

-- Assumption: the spine we are passed is stored in Postgres in the
-- "BlockShellIndex" table at the lowest level for our current chainId
backfillBlockShellIndex ::
  ( MonadIO m
  , MonadBaseNoPureAborts IO m
  , MonadLogger m
  ) => NodeDataSource -> Either (BlockHeader, PublicNodeContext) BlockShellIndex -> m ()
backfillBlockShellIndex nds = \case
  Right _blk -> do
    -- let ctx = error "FIXME: resume BlockShellIndex backfills"
    -- loop ctx Nothing (mkBlockSpine blk)
    return ()
  Left (blk, ctx) -> do
    loop ctx (Just blk) (mkBlockSpine blk)
 where
  db = _nodeDataSource_pool nds
  chainId = _nodeDataSource_chain nds

  loop ctx mHead !blk = do
    let
      lvl = blk ^. level
      -- TODO: make these numbers config params for QA & Dev purposes
      backfillLevel    = 0
      threshholdLevels = 12000
      defaultLevels    = 8000
      fetchLevels = let x = lvl - backfillLevel in
                     if x > threshholdLevels then defaultLevels else x + 1
      blkLevel = blk ^. level
      warnPartiallyComplete = do
        $(logWarn) [i| backfill only partially complete, stopped at level ${blkLevel} |]
    if fetchLevels < 2   -- we need at least two to form a link in the spine
    then return ()
    else do
      $(logDebug) [i| fetching the block spine from level ${blkLevel} down to level ${blkLevel - fetchLevels + 1} from ${formatPublicNodeContext ctx} |]
      runExceptT (runReaderT (getHistory chainId blk fetchLevels mempty) ctx) >>= \case
        Left (err :: PublicNodeError) -> do
          $(logError) [i| fetch failed: ${err} |]
          -- We only need to warn about a partial backfill when the table is nonempty.
          -- The table is non-empty when mHead is @Nothing@
          maybe warnPartiallyComplete (const $ pure ()) mHead
        Right blockHashes -> do
          let
            n = length blockHashes
            lvl' = lvl - fromIntegral n + 1
          if n < 2
          then do
            -- What should we do if the node doesn't return enough blocks?
            -- Currently, we consider the backfill complete, though a restart of
            -- Kiln will cause another attempt at a deeper backfill.
            -- (Err... should cause another attempt:  see resume backfills FIXME above)
            warnPartiallyComplete
          else do
            $(logDebug) [i| inserting block spines from level ${blkLevel} down to level ${lvl'} into postgres table "BlockShellIndex" |]
            let blockHashPgArray = toArrayAction blockHashes
            runDb (Identity db) $ do
              maybe (pure ()) (upsertBlockHeader chainId) mHead
              void [executeQ|
                INSERT INTO "BlockShellIndex"
                     ( hash , predecessor , "chainId" , level    )
               (SELECT x[i] , x[i+1]      , ?chainId  , ?lvl - i
                  FROM (VALUES (?blockHashPgArray::_bytea)) a(x)
                  JOIN LATERAL generate_series(1,array_length(x,1)::integer-1) i
                       ON TRUE)
              |]

            loop ctx Nothing $ BlockSpine {
                _blockSpine_hash        = blockHashes `Seq.index` (n - 2)
              , _blockSpine_predecessor = blockHashes `Seq.index` (n - 1)
              , _blockSpine_level       = lvl'
              }

toArrayAction :: Seq.Seq BlockHash -> PG.Action
toArrayAction hs = PG.Plain res
  where
    res = case Seq.viewl hs of
           Seq.EmptyL -> empty
           (h Seq.:< hs') -> left <> base16 h <> foldr delta right hs'

    delta h rest = comma <> base16 h <> rest

    empty = shortByteString "'{}'"
    left  = shortByteString "'{\"\\\\x"
    right = shortByteString "\"}'"
    comma = shortByteString "\",\"\\\\x"

    base16 (HashedValue h) = byteString . Base16.encode . BS.fromShort $ h

formatPublicNodeContext :: PublicNodeContext -> String
formatPublicNodeContext ctx =
  case _publicNodeContext_api ctx of
    Nothing -> show_node
    Just api  -> show api ++ " " ++ show_node
  where show_node = show . _nodeRPCContext_node . _publicNodeContext_nodeCtx $ ctx

upsertBlockHeader ::
  ( Functor m
  , PostgresRaw m
  ) => ChainId -> BlockHeader -> m ()
upsertBlockHeader chainId blk = do
  let hashH        = blk ^. hash
      predecessorH = blk ^. predecessor
      fitnessH     = blk ^. fitness
      levelH       = blk ^. level
      timestampH   = blk ^. timestamp
      protoH       = _blockHeader_proto blk
  void [executeQ|
    INSERT INTO "BlockShellIndex"
           ( "hash", "predecessor", "fitness", "chainId", "level", "timestamp", "proto" )
    VALUES ( ?hashH, ?predecessorH, ?fitnessH, ?chainId , ?levelH, ?timestampH, ?protoH )
    ON CONFLICT (hash) DO UPDATE
            SET fitness = COALESCE ( "BlockShellIndex".fitness, EXCLUDED.fitness )
              , timestamp = COALESCE ( "BlockShellIndex".timestamp, EXCLUDED.timestamp )
              , proto = COALESCE ( "BlockShellIndex".proto, EXCLUDED.proto )
          WHERE "BlockShellIndex".fitness IS NULL
             OR "BlockShellIndex".timestamp IS NULL
             OR "BlockShellIndex".proto IS NULL
  |]
