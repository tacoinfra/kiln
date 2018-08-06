{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Workers.Node where

import Database.Groundhog.Core
import Control.Applicative
import Control.Concurrent.MVar
import Control.Lens (ifor, ifor_, ix, to, (.~), (<&>), (^.), (^?), _Just, _Right)
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT, throwError, catchError)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.State
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bifunctor (first)
import Data.Either.Combinators
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Semigroup((<>), Max(..))
import Data.Text(Text)
import Data.Traversable (for)
import Data.Tuple(swap)
import Database.Groundhog.Postgresql
import Say (say, sayErr, sayShow)
import qualified Data.LCA.Online.Polymorphic as LCA
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.HTTP.Client as Http (Manager)

import Backend.CachedNodeRPC
import Backend.Supervisor

import Rhyolite.Backend.Listen (NotificationType (..), insertAndNotify, insertAndNotify_, notifyEntityId,
                                updateAndNotify)
import Rhyolite.Backend.DB (RunDb, getTime, openDb, runDb, selectMap)
import Rhyolite.Backend.DB.PsqlSimple (In (..), Only (..), PostgresRaw, Values (..), executeQ, queryQ)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Concurrent (worker)
import Rhyolite.Schema (Id (..), Json (..))

import Tezos.NodeRPC
import Tezos.Types

import Common.Schema
import Backend.Errors
import Backend.Schema
import Backend.Config (AppConfig (..), HasAppConfig, getAppConfig)
import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.Backend.Schema
import Rhyolite.Backend.Schema.Class
import Rhyolite.Schema
import Tezos.History

import qualified Data.LCA.Online.Polymorphic as LCA

-- cacheChainCycle :: b -> m (Id CachedChainCycle)
-- cacheChainCycle = error "TODO"
-- 
-- cacheOneBlock
--   ::( MonadLogger m
--     , MonadBaseControl IO m
--     , MonadIO m
--     , MonadError Text m
--     )
--   => NodeRPCContext
--   -> NodeDataSource
--   -- -> ChainId
--   -> Id CachedChainCycle
--   -> RawLevel -- cycle Position!
--   -> BlockHash
--   -> DbPersist Postgresql m (Id CachedBlock)
-- cacheOneBlock ctx nds chainCycleId cyclePosition blockHash = do
--   details <- nodeDataSource $ NodeQuery_Block blockHash
--   let baker = _blockMetadata_baker $ _block_metadata details
--   let endorsers = Json mempty -- TODO: this requres operations parsing
--   let blkPred = _blockHeader_predecessor $ _block_header details
--   let cb :: CachedBlock = CachedBlock chainCycleId baker endorsers cyclePosition blkHash blkPred
--   say $ T.concat
--     [ "\n\t"
--     , toBase58Text $ _cachedBlock_hash cb
--     , " -> "
--     , toBase58Text $ _cachedBlock_predecessor cb
--     , "(\\x"
--     , toBase58Text $ _cachedBlock_hash cb
--     , ")"
--     ]
--   toId <$> insertAndNotify cb

-- doThing :: ChainId -> Pool Postgresql -> Http.Manager -> Text -> MonitorBlock -> IO ()
-- doThing chain db mgr nodeUrl newBlock = timeit "doThing" say $ runNoLoggingT $ runDb (Identity db) $ do
--   let ctx = NodeRPCContext mgr nodeUrl
--   let blockHash = _monitorBlock_hash newBlock
--   let blockPredecessor = _monitorBlock_predecessor newBlock
--   -- if non empty, we're done!
--   sayShow (T.pack "NEW BLOCK", nodeUrl, blockHash)
--   fmap listToMaybe (select (CachedBlock_hashField ==. blockHash)) >>= \case
--     Just _ -> return () -- sayShow (T.pack "have block, DONE", blockHash)
--     Nothing -> do
--       fmap listToMaybe [queryQ|
--           SELECT cb.chain, cb."cyclePosition"
--           FROM "CachedBlock" cb
--           JOIN "CachedChainCycle" ccc
--             ON cb.chain = ccc.id
--           JOIN "CachedProtocolConstants" cpc
--             ON ccc.constants = cpc.id
--           WHERE cb.hash = ?blockPredecessor
--             AND cb."cyclePosition" < (cpc."blocksPerCycle" - 1)
--         |] >>= \case
--         Just (chainCycleId, predCyclePosition) -> do
--           -- sayShow (T.pack "Have predecessor in chain", blockPredecessor)
--           cacheOneBlock ctx chain chainCycleId (predCyclePosition + 1) blockHash
--           return ()
--           -- insert_ $ CachedBlock chainCycleId Nothing (Json mempty) (predCyclePosition + 1) (_monitorBlock_hash newBlock) (_monitorBlock_predecessor newBlock)
--         -- we're not caught up yet :(
--         Nothing -> do
--           -- sayShow (T.pack "need chain history", blockHash)
--           block <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RBlock $ blockHashId' chain blockHash) ctx
--           let protoHash = _blockMetadata_protocol $ _block_metadata block
--           let cycle = _level_cycle $ _blockMetadata_level $ _block_metadata block
--           let cyclePosition = _level_cyclePosition $ _blockMetadata_level $ _block_metadata block
--           -- if we're at position n we need a result of length n + 1 to include the first block of the current cycle.
--           cycleInitBlock <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RBlock $ blockHashIdPred' chain blockHash $ fromIntegral cyclePosition) ctx
--           let cycleInitHash :: BlockHash = _block_hash cycleInitBlock
--           --sayShow cycleInitBlock
--           -- we now have enough information to get our metadata in sync
-- 
--           (chainCycleId, preservedCycles) :: (Id CachedChainCycle, Cycle) <- listToMaybe <$> [queryQ|
--               SELECT ccc.id, cpc."preservedCycles"
--               FROM "CachedChainCycle" ccc
--               JOIN "CachedProtocolConstants" cpc
--                 ON ccc.constants = cpc.id
--               WHERE hash = ?cycleInitHash
--             |] >>= \case
--             Nothing -> do
--               -- sayShow (T.pack "need chain metadata")
--               (protoId, proto) :: (Id CachedProtocolConstants, CachedProtocolConstants) <- listToMaybe <$> [queryQ|
--                   SELECT "id", "protocol", "blocksPerCycle", "preservedCycles"
--                   FROM "CachedProtocolConstants"
--                   WHERE "protocol" = ?protoHash
--                 |] >>= \case
--                 Nothing -> do
--                   proto <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RProtoConstants $ blockHashId' chain blockHash) ctx
--                   let
--                     proto' = CachedProtocolConstants
--                       { _cachedProtocolConstants_protocol = protoHash
--                       , _cachedProtocolConstants_blocksPerCycle = _protoInfo_blocksPerCycle proto
--                       , _cachedProtocolConstants_preservedCycles = _protoInfo_preservedCycles proto
--                       }
--                   protoId' <- insert proto'
--                   return (toId protoId', proto')
--                 Just (protoId', p, bpc, pc) -> return (protoId', CachedProtocolConstants p bpc pc)
-- 
--               previousCycleHash <- fmap _block_hash . onRpcError <=< flip runReaderT ctx $ runExceptT $ nodeRPC $
--                 RBlock (blockHashIdPred' chain cycleInitHash $ fromIntegral $ _cachedProtocolConstants_blocksPerCycle proto)
--               -- protocol version data is now in sync
--               cid' <- toId <$> insert CachedChainCycle
--                 { _cachedChainCycle_chainId = chain
--                 , _cachedChainCycle_constants = protoId
--                 , _cachedChainCycle_cycle = cycle
--                 , _cachedChainCycle_hash = cycleInitHash
--                 , _cachedChainCycle_predecessor = previousCycleHash
--                 }
--               return (cid', _cachedProtocolConstants_preservedCycles proto)
--             Just (cid', pc) -> do
--               -- sayShow ("What actually happened?")
--               return (cid', pc)
--           -- chain/cycle is now in sync
--           -- which blocks are still missing?
--           ancestorMap <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RBlocks (DynamicParamChainId_ChainId chain) (1 + cyclePosition) $ Set.singleton blockHash) ctx
--           ancestors <- maybe (throwError $ "bad heads response from node, missing hash:" <> toBase58Text blockHash) return $ Map.lookup blockHash ancestorMap
--           -- sayShow ("foundAncestors:", Seq.length ancestors, Seq.take 3 ancestors)
--           let inAncestors = In $ toList ancestors
--           maxGoodAncestor :: RawLevel <- head . stripOnly <$> [queryQ|
--               SELECT COALESCE(MAX("cyclePosition"), -1)
--               FROM "CachedBlock"
--               WHERE hash in ?inAncestors
--             |]
--           -- sayShow ("haveAncestors:", maxGoodAncestor)
--           -- let needAncestors = Seq.take (Seq.length ancestors - maxGoodAncestor) ancestors
--           -- sayShow ("needAncestors:", Seq.length needAncestors, Seq.take 3 needAncestors)
-- 
--           let blocks = cacheOneBlock ctx chain chainCycleId
--                 <$> ZipList [cyclePosition,cyclePosition-1..maxGoodAncestor+1]
--                 <*> ZipList (blockHash : toList ancestors)
--           sequence_ blocks
--           -- TODO: it makes sense to insertAndNotify if we actually inserted the MonitorBlock we just recieved...
--           -- we now have our history... all that's left is rights.
--           -- in context $cycleInitBlock, we can find rights for cycles in range [$cycle .. ($cycle - $preservedCycles - 1))]
--           -- but really, the cycle rights that are *determined* by $cycle is just $cycle + $preservedCycles
--           -- except for cycles [0 .. $preservedCycles], which are all determined by the genesis block and not too important anyway.
--           knownRights :: Maybe (Id CachedBlockRights) <- listToMaybe . stripOnly <$> [queryQ|
--               SELECT id
--               FROM "CachedBlockRights"
--               WHERE cycle = ?chainCycleId
--               LIMIT 1
--               |]
--           case knownRights of
--             Just _ -> return ()
--             Nothing -> do
--               -- TODO:  if cycle == 0, request [0..preservedCycles]
--               bRights <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RBakingRights (blockHashId' chain blockHash) $ Set.singleton $ Right . Cycle . fromIntegral $ cycle + preservedCycles) ctx
--               eRrights <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ REndorsingRights (blockHashId' chain blockHash) $ Set.singleton $ Right . Cycle . fromIntegral $ cycle + preservedCycles) ctx
--               let cachedRights = CachedBlockRights chainCycleId (fromIntegral $ cycle + preservedCycles) (Json bRights) (Json eRrights)
--               insertAndNotify_ cachedRights

    -- cache <- newIORef mempty
    -- runReaderT (runExceptT @ _ $ nodeRPC $ RBlock headId) nodeCtx >>= \case
    --   Right blockInfo -> do
    --     let chain = _block_chainId blockInfo
    --     void $ flip runReaderT nodeCtx $ runExceptT $ nodeRPC $ RMonitorHeads
    --       (\case
    --         Left e -> sayErr $ "Bad monitor block: " <> tshow e
    --         Right monitorBlock -> do
    --           let pkh = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
    --           eff <- flip runReaderT nodeCtx $ runExceptT $
    --             calculateBakeEfficiency cache chain (_monitorBlock_hash monitorBlock) pkh
    --           sayShow eff
    --       )
    --       --(either sayShow $ doThing chain db httpMgr "http://127.0.0.1:18731")
    --       (DynamicParamChainId_ChainId chain)
    --   Left bad -> sayShow bad


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
--
-- TODO: uh.. also incorporate CachedBlock
-- bootstrapHistory' ::
--   ( MonadIO m
--   , MonadBaseControl IO m
--   , MonadReader a m, HasNodeRPC a
--   , MonadError e m, AsRpcError e
--   )
--   => Pool Postgresql -> ChainId -> MonitorBlock -> m [(BlockHash, BranchData CachedBlockInfo)]
-- bootstrapHistory' db chainId blk = do
--   blocks :: [CachedBlock] <- runNoLoggingT $ runDb (Identity db) $ do
--     select $ CondEmpty `orderBy` [Asc CachedBlock_levelField]
-- 
--   error "stop"
--   sayShow ("need history", blk)
--   ((fmap . fmap) (, mempty) . bootstrapHistory chainId 1) blk

-- TODO: make this "configurable"
minCachedBlockLevel = 1

nodeMonitor :: ChainId -> Http.Manager  -> NodeDataSource -> AppConfig -> Pool Postgresql -> ClientAddress -> Id Node -> RpcResponse MonitorBlock -> IO ()
nodeMonitor chainId httpMgr nds appConfig db nodeAddr nodeId = \case
  Left bad -> error "sulk"
  Right headBlockInfo -> do
      let cacheVar = _nodeDataSource_history nds
      let ctx = NodeRPCContext httpMgr nodeAddr
      newBlock <- modifyMVar cacheVar $ \cache -> do
        let newBlock = Map.member (headBlockInfo ^. hash) (_cachedHistory_blocks cache)
        newStateRsp
          :: Either RpcError CachedHistory'
          <- runExceptT $ flip runReaderT ctx $ flip execStateT cache $ do
            accumHistory chainId minCachedBlockLevel blockSummary headBlockInfo -- (bootstrapHistory' db chainId) blockSummary headBlockInfo
            sayShow ("new block", nodeAddr, headBlockInfo)
        case newStateRsp of
          Left bad -> say "asdf" *> sayShow bad *> return (cache, False)
          Right good -> say "horray" *> return (good, newBlock)

      asdf <- readMVar cacheVar
      sayShow ("lookout", fmap (fmap fst . LCA.toList) $ Map.lookup (headBlockInfo ^. hash) $ _cachedHistory_blocks asdf )

      when newBlock $ do
        say $ "new block from node at " <> nodeAddr
        say $ T.pack $ show headBlockInfo
      runNoLoggingT $ runDb (Identity db) $ flip runReaderT appConfig $ do
        clearInaccessibleEndpointError EndpointType_Node nodeAddr
        updateAndNotify nodeId
          [ Node_headLevelField =. Just (headBlockInfo ^. monitorBlock_level)
          , Node_headBlockHashField =. Just (headBlockInfo ^. monitorBlock_hash)
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

-- Make sure that the protocol parameters have been loaded and the datasource initialzied.
initParams :: Foldable f => NodeDataSource -> f Text -> IO Bool
initParams nds theseNodes = do
  needParams <- isEmptyMVar $ _nodeDataSource_parameters nds
  when needParams $ do
    let chainId = _nodeDataSource_chain nds
    foundParams :: Either ProtoInfo () <- runExceptT $ for_ theseNodes $ \someNode -> do
      let ctx = NodeRPCContext (_nodeDataSource_httpMgr nds) someNode
      runExceptT (runReaderT (nodeRPC $ RProtoConstants $ headId' chainId) ctx) >>= \case
        Left (_ :: RpcError) -> return ()
        Right params -> throwError params
    case foundParams of
      Left params -> do
        void $ liftIO $ tryPutMVar (_nodeDataSource_parameters nds) params
      Right _ -> say "Still no params"
  fmap not $ isEmptyMVar $ _nodeDataSource_parameters nds

updateNetworkStats :: Http.Manager -> Pool Postgresql -> Id Node -> Node -> IO ()
updateNetworkStats httpMgr db nid before = flip runReaderT (NodeRPCContext httpMgr $ _node_address before) $ do
  let
    onErr :: forall m a. Functor m => ExceptT RpcError m a -> m (Maybe a)
    onErr = fmap rightToMaybe . runExceptT
  connections <- onErr $ nodeRPC RConnections
  networkStat <- onErr $ nodeRPC RNetworkStat
  let
    after = before
      { _node_peerCount = connections -- intentionally not coalescing.
      , _node_networkStat = fromMaybe (_node_networkStat before) networkStat
      }

  when (before /= after) $ runNoLoggingT $ runDb (Identity db) $ do
    updateAndNotify nid
      [ Node_peerCountField =. _node_peerCount after
      , Node_networkStatField =. _node_networkStat after
      ]

nodeWorker
  :: Int -- delay between checking for updates, in microseconds
  -> NodeDataSource
  -> AppConfig
  -> Http.Manager
  -> Pool Postgresql
  -> IO (IO ())
nodeWorker delay nds appConfig httpMgr db = supervise $ \addFinalizer -> do
  nodePool :: MVar (Map ClientAddress (IO ())) <- newMVar mempty
  worker delay $ do
    say "Update node cycle."

    -- read the persistent list of nodes
    theseNodeRecords :: Map (Id Node) Node <- runNoLoggingT $ runDb (Identity db) $ do
      selectMap NodeConstructor (Node_deletedField ==. False)
    -- give them all a chance to 
    ifor_ theseNodeRecords $ updateNetworkStats httpMgr db

    let theseNodes = Map.fromList $ fmap (\(i, n) -> (_node_address n, i)) $ Map.toList theseNodeRecords
    --
    -- we may need to bootstrap our parameters.  if the cache.parameters var is empty, lets try to fill it with the nodes we currently have
    initParams nds (Map.keys theseNodes)

    thoseNodes <- readMVar nodePool
    let newNodes = theseNodes `Map.difference` thoseNodes
    let staleNodes = thoseNodes `Map.difference` theseNodes
    sayShow ("new nodes", Map.keys newNodes, "deleted nodes", Map.keys staleNodes)

    ifor_ staleNodes $ \nodeAddr killMonitor -> say ("stop monitor on " <> nodeAddr) *> killMonitor

    let chainId = _nodeDataSource_chain nds

    let nodeError :: ClientAddress -> RpcError -> ExceptT RpcError IO ()
        nodeError nodeAddr _ = ExceptT ( fmap Right ( runNoLoggingT ( runDb (Identity db) ( flip runReaderT appConfig ( reportInaccessibleEndpointError EndpointType_Node nodeAddr )))))
    for_ (Map.toList newNodes) $ \(nodeAddr, nodeId :: Id Node) -> do
      runExceptT $ flip catchError (nodeError nodeAddr) $ flip runReaderT (NodeRPCContext httpMgr nodeAddr) $ do
          killMonitor <- nodeRPC $ RMonitorHeads (nodeMonitor chainId httpMgr nds appConfig db nodeAddr nodeId) $ DynamicParamChainId_ChainId chainId
          liftIO $ modifyMVar_ nodePool $ return . Map.insert nodeAddr killMonitor
          liftIO $ addFinalizer killMonitor
          say ("start monitor on " <> nodeAddr)

