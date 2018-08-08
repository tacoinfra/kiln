{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- TODO: move this to ~lib?
module Backend.CachedNodeRPC where

import qualified Data.Aeson as Aeson
import Data.Time (UTCTime)
import Common.Schema
import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar -- (MVar, modifyMVar)
import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.AppendMap(AppendMap)
import Data.Dependent.Map (DMap)
import Data.Foldable (fold, foldl', for_, toList, traverse_)
import Data.Function (on)
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.Map (Map)
import Data.Maybe(catMaybes, listToMaybe)
import Data.Semigroup
import Data.Sequence (Seq)
import Data.Set(Set)
import Data.Traversable (for)
import Data.Typeable
import Safe.Foldable (maximumByMay, maximumMay)
import Say (say, sayErr, sayShow)
import qualified Data.Dependent.Map as DMap
import qualified Data.LCA.Online.Polymorphic as LCA
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.HTTP.Client as Http (Manager)

import Tezos.Lenses
import Tezos.NodeRPC
import Tezos.Types
import Tezos.History

data NodeQuery a where
  -- TODO: the "real" cache key should be a blockHash at lvl 2, the actual
  -- parameters are stored on a genesis protocol block at lvl 1, in a format
  -- understood and commited at lvl 2,  that requires parsing the binary data
  -- on the dictated block.
  NodeQuery_GenesisParameters   :: NodeQuery ProtoInfo
  NodeQuery_BakingRights        :: BlockHash -> RawLevel -> NodeQuery (Seq BakingRights)
  NodeQuery_Baker               :: BlockHash -> RawLevel -> NodeQuery (PublicKeyHash, Priority)
  NodeQuery_Block               :: BlockHash -> NodeQuery Block

data BranchData a = BranchData
  { _branchData_fitness :: !Fitness
  , _branchData_level :: !RawLevel
  , _branchData_timestamp :: !UTCTime
  , _branchData_info :: !(Maybe a)
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- not 100% sure i have the lawful combination of these.
instance Applicative BranchData where
  f <*> x = BranchData (_branchData_fitness fx) (_branchData_level fx) (_branchData_timestamp fx) (_branchData_info f <*> _branchData_info x)
    where
      fx = void f <> void x
  pure a = mempty {_branchData_info = pure a}

instance Alternative BranchData where
  (<|>) = (<>)
  empty = mempty

instance Semigroup (BranchData a) where
  x <> y = BranchData (_branchData_fitness xy) (_branchData_level xy) (_branchData_timestamp xy) (_branchData_info xy)
    where
      xy | void x > void y = x
         | otherwise = y

instance Monoid (BranchData a) where
  mempty = BranchData
    { _branchData_fitness = toFitness mempty
    , _branchData_level = 0
    , _branchData_timestamp = maybe (error "impossible") id $ Aeson.decode "\"0000-01-01T00:00:00.000Z\""
    , _branchData_info = Nothing
    }
  mappend = (<>)

data CachedBlockInfo = CachedBlockInfo
  deriving (Eq, Ord, Show, Typeable)

type CachedHistory' = (CachedHistory (BranchData CachedBlockInfo))

-- TODO: age in cache?
newtype CachedResult a = CacheResult { unCacheResult :: MVar (Either ({- Map ClientAddress -} RpcError) a) }
unpackCacheResult
  :: ( MonadIO m , MonadError e m, AsRpcError e)
  => CachedResult a -> m a
unpackCacheResult = (liftIO . readMVar . unCacheResult) >=> \case
  Left bad -> do
    sayShow ("cached error:", bad)
    throwError $ (^. re asRpcError) {- $ maybe (RpcError_HttpException "no suitible node") snd $ listToMaybe $ Map.toList -} bad
  Right a -> pure a

-- get lca between two blocks
branchPoint ::
  ( MonadIO m
  , MonadReader a m, HasNodeDataSource a
  )
  => BlockHash -> BlockHash -> m (Maybe VeryBlockLike)
branchPoint x y = do
  dsrc <- asks $ (^. nodeDataSource)
  history <- liftIO $ readMVar $ _nodeDataSource_history dsrc
  let
    xPath = Map.lookup x $ _cachedHistory_blocks history
    yPath = Map.lookup y $ _cachedHistory_blocks history
  return $ fmap histToBlockLike . LCA.uncons =<< LCA.lca <$> xPath <*> yPath

data NodeDataSource = NodeDataSource
  { _nodeDataSource_history :: !(MVar CachedHistory')
  , _nodeDataSource_nodes :: !(MVar (Map ClientAddress (Maybe VeryBlockLike)))
  , _nodeDataSource_cache :: !(MVar (DMap NodeQuery CachedResult)) -- we add elements to the cache to ind
  , _nodeDataSource_chain :: !ChainId
  , _nodeDataSource_parameters :: !(MVar ProtoInfo)
  , _nodeDataSource_httpMgr :: !Http.Manager
  }
blankNodeDataSource :: ChainId -> Http.Manager -> IO NodeDataSource
blankNodeDataSource chain mgr = do
  nodes <- newMVar mempty
  hist <- newEmptyMVar
  cache <- newEmptyMVar
  protoInfo <- newEmptyMVar
  forkIO $ do
    -- wait for someone else to put something in protoInfo, then fill the rest of the MVars.
    _ <- readMVar protoInfo
    putMVar hist emptyCache
    putMVar cache mempty
    say "Cache ready!"
  return $ NodeDataSource
    { _nodeDataSource_history = hist
    , _nodeDataSource_nodes = nodes
    , _nodeDataSource_cache = cache
    , _nodeDataSource_chain = chain
    , _nodeDataSource_parameters = protoInfo
    , _nodeDataSource_httpMgr = mgr
    }

class HasNodeDataSource a where
  nodeDataSource :: Lens' a NodeDataSource

instance HasNodeDataSource NodeDataSource where
  nodeDataSource = id

-- turn the result of an LCA.uncons on the block history into a VeryBlockLike
histToBlockLike :: (BlockHash, BranchData CachedBlockInfo, LCA.Path BlockHash (BranchData CachedBlockInfo)) -> VeryBlockLike
histToBlockLike (h, BranchData f l t _, path) = VeryBlockLike h p f l t
      where
        p = maybe h (\(pp, _, _) -> pp) $ LCA.uncons $ path



-- | extrats the fittest known branch from cache
dataSourceHead
  :: ( MonadIO m , MonadReader s m, HasNodeDataSource s)
  => m (Maybe VeryBlockLike)
dataSourceHead = do
  dsrc <- asks $ (^. nodeDataSource)
  history <- liftIO $ readMVar $ _nodeDataSource_history dsrc
  let branches = _cachedHistory_blocks history `Map.intersection` Map.fromSet (const ()) (_cachedHistory_branches history)
  pure $ fmap histToBlockLike $ (>>= LCA.uncons) $ maximumByMay (compare `on` LCA.measure) $ toList branches

-- | extrats the fittest known node from cache
dataSourceNode ::
  ( MonadIO m
  , MonadReader s m, HasNodeDataSource s
  )
  => m (Maybe (NodeRPCContext))
dataSourceNode = do
  dsrc <- asks $ (^. nodeDataSource)
  nodes <- liftIO $ readMVar $ _nodeDataSource_nodes dsrc
  pure $ fmap (NodeRPCContext (_nodeDataSource_httpMgr dsrc) . fst) $ maximumByMay (on compare $ snd) $ catMaybes $ fmap sequence $ Map.toList nodes

-- Recontextualize a query for maximum cache friendliness, and also return the list of nodes that should know the answer.
getKey :: CachedHistory' -> Maybe BlockHash -> NodeQuery a -> NodeQuery a -- , Set ClientAddress)
getKey hist ctx = error "TODO"

pickNode :: MVar (Map ClientAddress a) -> IO ClientAddress
pickNode = error "TODO"

nodeQueryDataSource ::
  ( MonadIO m
  , MonadReader s m
  , HasNodeDataSource s
  , MonadError e m, AsRpcError e
  )
  => NodeQuery a -> m a
nodeQueryDataSource q' = do
  dsrc <- asks $ view nodeDataSource
  nodes <- liftIO $ readMVar $ _nodeDataSource_nodes dsrc
  sayShow ("time to query go!", Map.keys nodes)
  history <- liftIO $ readMVar $ _nodeDataSource_history dsrc
  let q = getKey history Nothing q

  resultM <- liftIO $ modifyMVar (_nodeDataSource_cache dsrc) $ \cache ->
    case DMap.lookup q cache of
      Just avar -> do
        sayShow ("cache hit!")
        pure (cache, avar)
      Nothing -> do
        sayShow ("cache miss!")
        newVar <- liftIO $ newEmptyMVar
        liftIO $ forkIO $ do
          anyNode <- pickNode $ _nodeDataSource_nodes dsrc
          let
            ctx = NodeRPCContext (_nodeDataSource_httpMgr dsrc) anyNode
            unliftDataSrc :: NodeQuery a -> IO (Either RpcError a)
            unliftDataSrc = flip runReaderT dsrc . runExceptT . nodeQueryDataSource
          res <- (nodeQueryDataSourceImpl ctx unliftDataSrc q)
          putMVar newVar res
        pure (DMap.insert q (CacheResult newVar) cache, CacheResult newVar)

  unpackCacheResult resultM

nodeQueryDataSourceImpl ::
  NodeRPCContext -> (forall a. NodeQuery a -> IO (Either RpcError a)) -> NodeQuery a -> IO (Either RpcError a)
nodeQueryDataSourceImpl ctx self' q = runExceptT $ do
  let 
    self :: NodeQuery b -> ExceptT RpcError IO b
    self = (>>= (either throwError pure)) . liftIO . self'
    nodeRPC' :: NodeRPCRequest b -> ExceptT RpcError IO b
    nodeRPC' q' = runReaderT (nodeRPC q') ctx
  case q of
    NodeQuery_GenesisParameters -> do
      currentHead <- nodeRPC' $ RBlock $ headId
      let headLevel = currentHead ^. block_header . blockHeader_level
      --TODO: if headLevel == 0 then error "error"
      nodeRPC' $ RProtoConstants $ blockHashIdPred (_block_hash currentHead) (headLevel - 1)

    NodeQuery_BakingRights branch targetLevel -> do
      proto <- self $ NodeQuery_GenesisParameters
      let
        cycleForLevel n = Cycle $ unRawLevel $ n `div` _protoInfo_blocksPerCycle proto
        cycleDeterminingRightsForLevel n = max 0 $ cycleForLevel n - _protoInfo_preservedCycles proto

        cycleToQuery = Set.singleton $ Right $ cycleDeterminingRightsForLevel targetLevel

      branchBlock <- nodeRPC' $ RBlock $ blockHashId branch
      let
        blockLevel = branchBlock ^. block_metadata . blockMetadata_level
        cyclesAgo = blockLevel ^. level_cycle - cycleDeterminingRightsForLevel targetLevel
        levelsAgo = RawLevel (unCycle cyclesAgo) * _protoInfo_blocksPerCycle proto - blockLevel ^. level_cyclePosition

      if
        | levelsAgo < 0 -> error "request for future stake"
        | levelsAgo == 0 ->
          nodeRPC' $ RBakingRights (blockHashId branch) cycleToQuery
        | otherwise -> do
          targetBlock <- nodeRPC' $ RBlock $ blockHashIdPred branch levelsAgo
          self $ NodeQuery_BakingRights
            (targetBlock ^. block_hash)
            (targetLevel `div` _protoInfo_blocksPerCycle proto * _protoInfo_blocksPerCycle proto)

    NodeQuery_Baker branch rawLevel -> do
      branchBlock <- nodeRPC' $ RBlock $ blockHashId branch
      let levelsAgo = branchBlock ^. block_header . blockHeader_level - rawLevel
      targetBlock <- nodeRPC' $ RBlock $ blockHashIdPred branch levelsAgo
      pure ( targetBlock ^. block_metadata . blockMetadata_baker
           , targetBlock ^. block_header . blockHeader_priority
           )

calculateBakeEfficiency ::
  ( MonadIO m
  , MonadReader s m , HasNodeDataSource s
  , MonadError RpcError m
  , BlockLike b
  )
  => b -> RawLevel -> PublicKeyHash -> m BakeEfficiency
calculateBakeEfficiency branch length delegate = do
  sayShow ("bake efficiency requested", branch ^. hash, length, delegate)
  let
    branchLevel = branch ^. level
    branchHash = branch ^. hash
    levels = [branchLevel - length..branchLevel]

  rights <- (fmap.fmap) bakingRightsMap $ for levels $ nodeQueryDataSource . NodeQuery_BakingRights branchHash
  bakers <- for levels $ fmap fst . nodeQueryDataSource . NodeQuery_Baker branchHash
  pure $ fold $ efficiencyOfBlock <$> ZipList rights <*> ZipList bakers

  where
    efficiencyOfBlock :: Map PublicKeyHash Priority -> PublicKeyHash -> BakeEfficiency
    efficiencyOfBlock rights baker = BakeEfficiency
      { _bakeEfficiency_bakedBlocks = if baker == delegate then 1 else 0
      , _bakeEfficiency_bakingRights = case (Map.lookup baker rights, Map.lookup delegate rights) of
          (_, Nothing) -> 0
          (Just them, Just us) -> if us <= them then 1 else 0
          (Nothing, _) -> error "Very wrong"
      }

    bakingRightsMap :: Foldable f => f BakingRights -> Map PublicKeyHash Priority -- map from delegate to
    bakingRightsMap xs = Map.fromList
      [ (delegate, prio)
      | BakingRights _lvl delegate prio _ <- toList xs
      ]

deriveGEq ''NodeQuery
deriveGCompare ''NodeQuery
