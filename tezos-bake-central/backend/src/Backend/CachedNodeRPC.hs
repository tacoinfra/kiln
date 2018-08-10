{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fmax-relevant-binds=20 #-}

-- TODO: move this to ~lib?
module Backend.CachedNodeRPC where

import Common.Schema
import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.AppendMap (AppendMap)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Foldable (fold, foldl', for_, toList, traverse_)
import Data.Function (on)
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import qualified Data.LCA.Online.Polymorphic as LCA
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Semigroup
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (UTCTime)
import Data.Traversable (for)
import Data.Typeable
import Generics.Deriving.TH
import qualified Network.HTTP.Client as Http (Manager)
import Safe.Foldable (maximumByMay, maximumMay)
import Say (say, sayErr, sayShow)

import Tezos.History
import Tezos.Lenses
import Tezos.NodeRPC
import Tezos.Types

data NodeQuery a where
  -- TODO: the "real" cache key should be a blockHash at lvl 2, the actual
  -- parameters are stored on a genesis protocol block at lvl 1, in a format
  -- understood and commited at lvl 2,  that requires parsing the binary data
  -- on the dictated block.
  -- NodeQuery_GenesisParameters   :: NodeQuery ProtoInfo
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
  dsrc <- asks (^. nodeDataSource)
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
        p = maybe h (\(pp, _, _) -> pp) $ LCA.uncons path

updateNodeDataSource :: BlockLike b => NodeDataSource -> ClientAddress -> b -> IO ()
updateNodeDataSource nds nodeAddr blk =
  modifyMVar_ (_nodeDataSource_nodes nds) $ return . Map.insert nodeAddr (Just $ mkVeryBlockLike blk)

-- Make sure that the protocol parameters have been loaded and the datasource initialzied.
initParams :: Foldable f => NodeDataSource -> f ClientAddress -> IO Bool
initParams nds theseNodes = do
  needParams <- isEmptyMVar $ _nodeDataSource_parameters nds

  let
    chainId = _nodeDataSource_chain nds
    step :: MonadIO m => ClientAddress -> m (Map ClientAddress ProtoInfo)
    step someNode = do
      let ctx = NodeRPCContext (_nodeDataSource_httpMgr nds) someNode
      let protoConstantsAtHead = do
            headBlockHash <- _block_hash <$> nodeRPC (rHead chainId)
            nodeRPC $ rProtoConstants chainId headBlockHash
      runExceptT (runReaderT protoConstantsAtHead ctx) >>= \case
        Left (_ :: RpcError) -> pure mempty
        Right params -> pure $ Map.singleton someNode params
  onChainNodes :: Map ClientAddress ProtoInfo <- fold <$> traverse step (toList $ Set.fromList $ toList theseNodes)

  when needParams $ case fmap fst $ uncons $ toList onChainNodes of
      Just params -> do
        void $ liftIO $ tryPutMVar (_nodeDataSource_parameters nds) params
      Nothing -> say "Still no params"

  fmap not $ isEmptyMVar $ _nodeDataSource_parameters nds


-- | extrats the fittest known branch from cache
dataSourceHead
  :: ( MonadIO m , MonadReader s m, HasNodeDataSource s)
  => m (Maybe VeryBlockLike)
dataSourceHead = withCache Nothing $ \_ -> do
  dsrc <- asks (^. nodeDataSource)
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

levelAncestor :: CachedHistory' -> RawLevel -> BlockHash -> Maybe BlockHash
levelAncestor hist lvl ctx = ctxBlockHash
  where
    minLevel = _cachedHistory_minLevel hist
    branch = Map.lookup ctx $ _cachedHistory_blocks hist
    ctxBlockHash = fmap (view _1) $ LCA.uncons =<< LCA.keep (fromIntegral $ lvl - minLevel) <$> branch

-- Recontextualize a query for maximum cache friendliness, and also return the least block
getKey :: ProtoInfo -> CachedHistory' -> NodeQuery a -> Maybe (BlockHash, NodeQuery a) -- , Set ClientAddress)
getKey params hist = \case
  NodeQuery_BakingRights ctx lvl -> (\ctx' -> (ctx' , NodeQuery_BakingRights ctx' lvl)) <$> ctxBlockHash
    -- for this case, we want the first block in the cycle that sits
    -- $PRESERVED_CYCLES before the requested level, that is on the correct
    -- branch.
    where
      minLevel = _cachedHistory_minLevel hist
      branch = Map.lookup ctx $ _cachedHistory_blocks hist
      branchLvl = minLevel + (fromIntegral $ length branch)
      reqCycle :: Cycle = max 0 $ fromIntegral $ (lvl - 1) `div` _protoInfo_blocksPerCycle params
      ctxCycle = max 0 (reqCycle - _protoInfo_preservedCycles params)
      ctxLvl :: RawLevel = 1 + fromIntegral ctxCycle * _protoInfo_blocksPerCycle params
      ctxBlockHash = levelAncestor hist ctxLvl ctx
  NodeQuery_Baker ctx lvl -> (\ctx' -> (ctx', NodeQuery_Baker ctx' lvl)) <$> ctxBlockHash
    where
      ctxBlockHash = levelAncestor hist lvl ctx
  NodeQuery_Block ctx -> pure $ (ctx, NodeQuery_Block ctx)

nodeQueryDataSource ::
  ( MonadIO m
  , MonadReader s m
  , HasNodeDataSource s
  , MonadError e m, AsRpcError e
  )
  => NodeQuery a -> m a
nodeQueryDataSource q' = do
  dsrc <- asks $ view nodeDataSource
  protoInfo <- liftIO $ readMVar $ _nodeDataSource_parameters dsrc
  nodes <- liftIO $ readMVar $ _nodeDataSource_nodes dsrc
  sayShow ("time to query go!", Map.keys nodes, q')
  history <- liftIO $ readMVar $ _nodeDataSource_history dsrc

  (qBranch, q) <- maybe (throwError $ (RpcError_HttpException "NOT ENOUGH HISTORY") ^. re asRpcError) pure $ getKey protoInfo history q'

  resultM <- liftIO $ modifyMVar (_nodeDataSource_cache dsrc) $ \cache ->
    case DMap.lookup q cache of
      Just avar -> do
        sayShow "cache hit!"
        pure (cache, avar)
      Nothing -> do
        sayShow ("cache miss!", q', qBranch, q)
        allNodes <- readMVar $ _nodeDataSource_nodes dsrc
        sayShow ("all nodes:", allNodes)
        pickNode qBranch (_nodeDataSource_nodes dsrc) >>= \case
          Nothing -> do
            newVar <- newMVar $ Left $ RpcError_HttpException "No suitable node"
            pure (cache, CacheResult newVar)
          Just anyNode -> do
            let
              ctx = NodeRPCContext (_nodeDataSource_httpMgr dsrc) anyNode
            newVar <- liftIO newEmptyMVar
            liftIO $ forkIO $ do
              let
                unliftDataSrc :: NodeQuery a -> IO (Either RpcError a)
                unliftDataSrc = flip runReaderT dsrc . runExceptT . nodeQueryDataSource
              res <- nodeQueryDataSourceImpl (_nodeDataSource_chain dsrc) protoInfo ctx unliftDataSrc q
              putMVar newVar res
            pure (DMap.insert q (CacheResult newVar) cache, CacheResult newVar)

  unpackCacheResult resultM

pickNode :: BlockLike b => BlockHash -> MVar (Map ClientAddress (Maybe b)) -> IO (Maybe ClientAddress)
pickNode branch = readMVar >=> pure . fmap fst . maximumByMay (compare `on` view fitness . snd) . catMaybes . fmap sequence . Map.toList

nodeQueryDataSourceImpl
  :: forall a.
     ChainId
  -> ProtoInfo
  -> NodeRPCContext
  -> (forall b. NodeQuery b -> IO (Either RpcError b))
  -> NodeQuery a
  -> IO (Either RpcError a)
nodeQueryDataSourceImpl chainId proto ctx self' q = runExceptT $ do
  let
    self :: NodeQuery b -> ExceptT RpcError IO b
    self = (>>= either throwError pure) . liftIO . self'
    nodeRPC' :: forall c. (forall repr. (BlockType repr ~ Block, QueryNode repr, QueryHistory repr, QueryBlock repr) => repr c) -> ExceptT RpcError IO c
    nodeRPC' q' = runReaderT (nodeRPC q') ctx
  case q of
    -- NodeQuery_GenesisParameters -> do
    --   currentHead <- nodeRPC' $ RBlock $ headId
    --   let headLevel = currentHead ^. block_header . blockHeader_level
    --   --TODO: if headLevel == 0 then error "error"
    --   nodeRPC' $ RProtoConstants $ blockHashIdPred (_block_hash currentHead) (headLevel - 1)

    NodeQuery_BakingRights branch targetLevel ->
      nodeRPC' $ rBakingRights chainId branch $ Set.singleton $ Left targetLevel

    NodeQuery_Baker branch rawLevel -> do
      branchBlock <- nodeRPC' $ rBlock chainId branch
      let levelsAgo = branchBlock ^. block_header . blockHeader_level - rawLevel
      targetBlock <- nodeRPC' $ rBlockPred chainId branch levelsAgo
      pure ( targetBlock ^. block_metadata . blockMetadata_baker
           , targetBlock ^. block_header . blockHeader_priority
           )

withCache ::
  ( MonadReader r m , HasNodeDataSource r
  , MonadIO m
  )
  => a -> (ProtoInfo -> m a) -> m a
withCache dft action = do
  dsrc <- asks $ (^. nodeDataSource)
  protoInfo <- liftIO $ tryReadMVar $ _nodeDataSource_parameters dsrc
  maybe dft id <$> traverse action protoInfo


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
  result <- pure $ fold $ efficiencyOfBlock <$> ZipList rights <*> ZipList bakers
  sayShow ("efficiency", delegate, result)
  return result
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
deriveGShow ''NodeQuery
deriving instance Show (NodeQuery a)
