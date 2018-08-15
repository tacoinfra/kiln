{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fmax-relevant-binds=20 -Wall #-}

-- TODO: move this to ~lib?
module Backend.CachedNodeRPC where

import Prelude hiding (length)

import Backend.Schema (Field (..))
import Common.Schema
import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Lens (Lens', TraversableWithIndex, (^.), re, uncons, view, _1, ifor)
import Control.Monad.Except
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.Constraint (Dict (..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Foldable (fold, toList)
import Data.Function (on)
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import qualified Data.LCA.Online.Polymorphic as LCA
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup
import Data.Sequence (Seq)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Traversable (for)
import Data.Typeable
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Client as Http (Manager)
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Request.Class
import Rhyolite.Schema (Json (..))
import Safe.Foldable (maximumByMay)
import Say (say, sayErr, sayShow)

import Rhyolite.Request.TH (makeRequestForData)

import Tezos.History
import Tezos.NodeRPC
import Tezos.Types

data NodeQuery a where
  NodeQuery_BakingRights        :: BlockHash -> RawLevel -> NodeQuery (Seq BakingRights)
  NodeQuery_EndorsingRights     :: BlockHash -> RawLevel -> NodeQuery (Seq EndorsingRights)
  NodeQuery_Account             :: BlockHash -> ContractId -> NodeQuery Account
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

data CacheLine a = CacheLine
  { _cacheLine_value :: !a
  , _cacheLine_used :: !UTCTime
  }
newtype CachedResult a = CachedResult { unCacheResult :: MVar (Either ({- Map ClientAddress -} RpcError) (CacheLine a)) }

unpackCacheResult
  :: forall m e a . ( MonadIO m , MonadError e m, AsRpcError e)
  => CachedResult a -> m a
unpackCacheResult (CachedResult var) = join $ liftIO $ modifyMVar var $ either onErr onSuccess
  where
    onErr :: RpcError -> IO (Either RpcError b, m a)
    onErr bad = pure (Left bad, throwError (bad ^. re asRpcError))
    onSuccess :: CacheLine a -> IO (Either RpcError (CacheLine a), m a)
    onSuccess result = do
      now <- getCurrentTime
      return (Right result {_cacheLine_used = now}, return $ _cacheLine_value result)

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
  , _nodeDataSource_pool :: !(Pool Postgresql)
  }
blankNodeDataSource :: Pool Postgresql -> ChainId -> Http.Manager -> IO NodeDataSource
blankNodeDataSource db chain mgr = do
  nodes <- newMVar mempty
  hist <- newEmptyMVar
  cache <- newEmptyMVar
  protoInfo <- newEmptyMVar
  _ <- forkIO $ do
    -- wait for someone else to put something in protoInfo, then fill the rest of the MVars.
    _ <- readMVar protoInfo
    putMVar hist emptyCache
    putMVar cache mempty
    say "Cache ready!"
  return NodeDataSource
    { _nodeDataSource_history = hist
    , _nodeDataSource_nodes = nodes
    , _nodeDataSource_cache = cache
    , _nodeDataSource_chain = chain
    , _nodeDataSource_parameters = protoInfo
    , _nodeDataSource_httpMgr = mgr
    , _nodeDataSource_pool = db
    }

class HasNodeDataSource a where
  nodeDataSource :: Lens' a NodeDataSource

instance HasNodeDataSource NodeDataSource where
  nodeDataSource = id

readTimeBetweenBlocks :: HasNodeDataSource nds => nds -> IO NominalDiffTime
readTimeBetweenBlocks nds = fromIntegral . sum . take 1 . toList . _protoInfo_timeBetweenBlocks
  <$> readMVar (_nodeDataSource_parameters $ nds ^. nodeDataSource)


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
  => m (Maybe NodeRPCContext)
dataSourceNode = do
  dsrc <- asks (^. nodeDataSource)
  nodes <- liftIO $ readMVar $ _nodeDataSource_nodes dsrc
  pure $ fmap (NodeRPCContext (_nodeDataSource_httpMgr dsrc) . fst) $ maximumByMay (on compare snd) $ catMaybes $ fmap sequence $ Map.toList nodes

levelAncestor :: CachedHistory' -> RawLevel -> BlockHash -> Maybe BlockHash
levelAncestor hist lvl ctx = ctxBlockHash
  where
    minLevel = _cachedHistory_minLevel hist
    branch = Map.lookup ctx $ _cachedHistory_blocks hist
    ctxBlockHash = fmap (view _1) $ LCA.uncons =<< LCA.keep (fromIntegral $ lvl - minLevel) <$> branch


rightsContext :: ProtoInfo -> CachedHistory' -> BlockHash -> RawLevel -> Maybe BlockHash
rightsContext params hist ctx lvl = ctxBlockHash
    -- for this case, we want the first block in the cycle that sits
    -- $PRESERVED_CYCLES before the requested level, that is on the correct
    -- branch.
    where
      reqCycle :: Cycle = max 0 $ fromIntegral $ (lvl - 1) `div` _protoInfo_blocksPerCycle params
      ctxCycle = max 0 (reqCycle - _protoInfo_preservedCycles params)
      ctxLvl :: RawLevel = 1 + fromIntegral ctxCycle * _protoInfo_blocksPerCycle params
      ctxBlockHash = levelAncestor hist ctxLvl ctx

-- Recontextualize a query for maximum cache friendliness, and also return the least block
getKey :: ProtoInfo -> CachedHistory' -> NodeQuery a -> Maybe (BlockHash, NodeQuery a) -- , Set ClientAddress)
getKey params hist = \case
  NodeQuery_BakingRights ctx lvl -> (\ctx' -> (ctx' , NodeQuery_BakingRights ctx' lvl)) <$> rightsContext params hist ctx lvl
  NodeQuery_EndorsingRights ctx lvl -> (\ctx' -> (ctx' , NodeQuery_EndorsingRights ctx' lvl)) <$> rightsContext params hist ctx lvl
  NodeQuery_Block ctx -> pure (ctx, NodeQuery_Block ctx)
  NodeQuery_Account ctx contractId -> pure (ctx, NodeQuery_Account ctx contractId)

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
  history <- liftIO $ readMVar $ _nodeDataSource_history dsrc

  (qBranch, q) <- maybe (throwError $ RpcError_HttpException "NOT ENOUGH HISTORY" ^. re asRpcError) pure $ getKey protoInfo history q'

  resultM <- liftIO $ modifyMVar (_nodeDataSource_cache dsrc) $ \cache ->
    case DMap.lookup q cache of
      Just avar -> do
        -- sayShow "cache hit!"
        pure (cache, avar)
      Nothing -> do
        -- sayShow ("cache miss!", q', qBranch, q)
        newVar <- liftIO newEmptyMVar
        let
          mkResult now v = CacheLine
            { _cacheLine_value = v
            , _cacheLine_used = now
            }
        _ <- liftIO $ forkIO $ do
          fromDB <- tryFetchFromCache (_nodeDataSource_pool dsrc) q
          case fromDB of
            Just x -> do
              -- sayShow ("found in db", q)
              now <- getCurrentTime
              putMVar newVar $ Right $ mkResult now x
            Nothing -> do
              -- sayShow ("all nodes:", allNodes)
              pickNode qBranch (_nodeDataSource_nodes dsrc) >>= \case
                Nothing -> do
                  putMVar newVar $ Left $ RpcError_HttpException "No suitable node"
                Just anyNode -> do
                  let
                    ctx = NodeRPCContext (_nodeDataSource_httpMgr dsrc) anyNode
                  do
                    let
                      unliftDataSrc :: NodeQuery a -> IO (Either RpcError a)
                      unliftDataSrc = flip runReaderT dsrc . runExceptT . nodeQueryDataSource
                    res' <- nodeQueryDataSourceImpl (_nodeDataSource_chain dsrc) protoInfo ctx unliftDataSrc q
                    now <- getCurrentTime
                    putMVar newVar $ mkResult now <$> res'
        pure (DMap.insert q (CachedResult newVar) cache, CachedResult newVar)

  unpackCacheResult resultM


pickNode :: BlockLike b => BlockHash -> MVar (Map ClientAddress (Maybe b)) -> IO (Maybe ClientAddress)
pickNode _branch = readMVar >=> pure . fmap fst . maximumByMay (compare `on` view fitness . snd) . catMaybes . fmap sequence . Map.toList

nodeQueryDataSourceImpl
  :: forall a.
     ChainId
  -> ProtoInfo
  -> NodeRPCContext
  -> (forall b. NodeQuery b -> IO (Either RpcError b))
  -> NodeQuery a
  -> IO (Either RpcError a)
nodeQueryDataSourceImpl chainId _proto ctx _self q = runExceptT $ do
  let
    -- self :: NodeQuery b -> ExceptT RpcError IO b
    -- self = (>>= either throwError pure) . liftIO . self'
    nodeRPC' :: forall c. (forall repr. (BlockType repr ~ Block, QueryNode repr, QueryHistory repr, QueryBlock repr) => repr c) -> ExceptT RpcError IO c
    nodeRPC' q' = runReaderT (nodeRPC q') ctx
  case q of

    NodeQuery_BakingRights branch targetLevel ->
      nodeRPC' $ rBakingRights chainId branch $ Set.singleton $ Left targetLevel
    NodeQuery_EndorsingRights branch targetLevel ->
      nodeRPC' $ rEndorsingRights chainId branch $ Set.singleton $ Left targetLevel
    NodeQuery_Account branch contractId ->
      nodeRPC' $ rContract chainId branch contractId

    NodeQuery_Block branch -> nodeRPC' $ rBlock chainId branch

withCache ::
  ( MonadReader r m , HasNodeDataSource r
  , MonadIO m
  )
  => a -> (ProtoInfo -> m a) -> m a
withCache dft action = do
  dsrc <- asks (^. nodeDataSource)
  protoInfo <- liftIO $ tryReadMVar $ _nodeDataSource_parameters dsrc
  maybe dft id <$> traverse action protoInfo

calculateDelegateStats ::
  ( TraversableWithIndex (PublicKeyHash, RawLevel) f
  , MonadReader r m, HasNodeDataSource r
  , MonadIO m
  )
  => f a
  -> m (f (First (Maybe (BakeEfficiency, Account)), a))
calculateDelegateStats pkhs = do
  dataSourceHead >>= \case
    -- I think i should probably just ask for a `forall b. f b` to pass on the no heads case
    Nothing -> return $ fmap (First Nothing,) pkhs
    Just currentHead -> ifor pkhs $ \(pkh, lvl) a -> do
      result <- fmap (First . either (const Nothing) Just) $ runExceptT $ do
        efficiency <- calculateBakeEfficiency currentHead lvl pkh
        account <- nodeQueryDataSource $ NodeQuery_Account (currentHead ^. hash) (Implicit pkh)
        return (efficiency, account)
      return $ (result, a)

-- produce (up to) n ancestor hashes (including the block itself)
ancestors ::
  ( MonadIO m
  , MonadReader s m , HasNodeDataSource s
  , MonadError RpcError m
  )
  => RawLevel -> BlockHash -> m [BlockHash]
ancestors (RawLevel n) branch = do
  -- it's a bit redundant, but how else can we be "sure" that we have the branch path
  _ <- nodeQueryDataSource $ NodeQuery_Block branch
  hist <- liftIO . readMVar =<< asks (_nodeDataSource_history . view nodeDataSource)
  case Map.lookup branch (_cachedHistory_blocks hist) of
    Just branchPath -> return $ fmap fst $ take n $ LCA.toList branchPath
    Nothing -> throwError $ (RpcError_UnexpectedStatus 404 "NO BRANCH") ^. re asRpcError

calculateBakeEfficiency ::
  ( MonadIO m
  , MonadReader s m , HasNodeDataSource s
  , MonadError RpcError m
  , BlockLike b
  )
  => b -> RawLevel -> PublicKeyHash -> m BakeEfficiency
calculateBakeEfficiency branch length delegate = do
  sayShow (T.pack "bake efficiency requested", branch ^. hash, length, delegate)

  let
    branchLevel = branch ^. level
    branchHash = branch ^. hash
    levels = [branchLevel - length..branchLevel]
  branchHashes <- ancestors length branchHash

  rights <- (fmap.fmap) bakingRightsMap $ for levels $ nodeQueryDataSource . NodeQuery_BakingRights branchHash
  bakers <- for branchHashes $ fmap (^. block_metadata . blockMetadata_baker) . nodeQueryDataSource . NodeQuery_Block
  result <- pure $ fold $ efficiencyOfBlock <$> ZipList rights <*> ZipList bakers
  sayShow (T.pack "efficiency", delegate, result)
  return result
  where
    efficiencyOfBlock :: Map PublicKeyHash Priority -> PublicKeyHash -> BakeEfficiency
    efficiencyOfBlock rights baker = BakeEfficiency
      { _bakeEfficiency_bakedBlocks = if baker == delegate then 1 else 0
      , _bakeEfficiency_bakingRights = case (Map.lookup baker rights, Map.lookup delegate rights) of
          (_, Nothing) -> 0
          (Just them, Just us) -> if us <= them then 1 else 0
          (Nothing, _) -> 0 -- error "Very wrong"
      }

    bakingRightsMap :: Foldable f => f BakingRights -> Map PublicKeyHash Priority -- map from delegate to
    bakingRightsMap xs = Map.fromList
      [ (d, prio)
      | BakingRights _lvl d prio _ <- toList xs
      ]

tryFetchFromCache :: Pool Postgresql -> NodeQuery a -> IO (Maybe a)
tryFetchFromCache db q = do
  let
    qJson = Json $ requestToJSON q
  resultM <- fmap listToMaybe $ runNoLoggingT $ runDb (Identity db) $ select $ GenericCacheEntry_keyField ==. qJson
  case resultM of
    Nothing -> return Nothing
    Just result -> case requestResponseFromJSON q of
      Dict -> case Aeson.fromJSON (unJson $ _genericCacheEntry_value result) of
        Aeson.Success v -> return $ Just v
        Aeson.Error bad -> do
          sayErr $ T.pack $ show (T.pack "tryFetchFromCache failed to decode:", bad)
          return Nothing

deriveGEq ''NodeQuery
deriveGCompare ''NodeQuery
deriveGShow ''NodeQuery
deriving instance Show (NodeQuery a)

makeRequestForData ''NodeQuery
