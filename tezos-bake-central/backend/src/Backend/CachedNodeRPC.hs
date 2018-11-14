{-# LANGUAGE DeriveTraversable #-}
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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: move this to ~lib?
module Backend.CachedNodeRPC where

import Prelude hiding (length)

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, retry)
import Control.Lens (Lens', TraversableWithIndex, ifor, re, view, (<&>), (^.), (^?), _1, _Just)
import Control.Lens.TH (makeLenses)
import Control.Monad.Except
import Control.Monad.Logger (LoggingT (..), logDebugSH, logInfo, logWarnSH)
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.Constraint (Dict (..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Foldable (fold, length, toList)
import Data.Function (on)
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import qualified Data.LCA.Online.Polymorphic as LCA
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup (First (..))
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Client as Http (Manager)
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Request.Class
import Rhyolite.Request.TH (makeRequestForData)
import Rhyolite.Schema (Json (..))
import Safe (headMay)
import Safe.Foldable (maximumByMay)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.History
import Tezos.NodeRPC.Class
import Tezos.NodeRPC.Network
import Tezos.NodeRPC.Sources
import Tezos.NodeRPC.Types
import Tezos.Types

import Backend.Common (timeout')
import Backend.Schema
import Common.Schema
import Rhyolite.Backend.Logging
import Common (unixEpoch)


data NodeQuery a where
  NodeQuery_BakingRights    :: BlockHash -> RawLevel -> NodeQuery (Seq BakingRights)
  NodeQuery_EndorsingRights :: BlockHash -> RawLevel -> NodeQuery (Seq EndorsingRights)
  NodeQuery_Account         :: BlockHash -> ContractId -> NodeQuery Account
  NodeQuery_Block           :: BlockHash -> NodeQuery Block
deriving instance Show (NodeQuery a)


data CachedBlockInfo = CachedBlockInfo
  deriving (Eq, Ord, Show, Typeable)

type CachedHistory' = CachedHistory ()

data CacheLine a = CacheLine
  { _cacheLine_value :: !a
  , _cacheLine_used :: !UTCTime
  }
newtype CachedResult a = CachedResult { unCacheResult :: MVar (Either RpcError (CacheLine a)) }

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
branchPoint
  :: ( MonadIO m
     , MonadReader a m, HasNodeDataSource a
     )
  => BlockHash -> BlockHash -> m (Maybe VeryBlockLike)
branchPoint x y = do
  dsrc <- asks (^. nodeDataSource)
  history <- liftIO $ readMVar $ _nodeDataSource_history dsrc
  let
    xPath = Map.lookup x $ _cachedHistory_blocks history
    yPath = Map.lookup y $ _cachedHistory_blocks history
  return $ fmap (histToBlockLike (_cachedHistory_minLevel history)) . LCA.uncons =<< LCA.lca <$> xPath <*> yPath

lookupBlock ::
  ( MonadIO m
  , MonadReader a m, HasNodeDataSource a
  )
  => BlockHash -> m (Maybe VeryBlockLike)
lookupBlock x = do
  dsrc <- asks (^. nodeDataSource)
  history <- liftIO $ readMVar $ _nodeDataSource_history dsrc
  let
    xPath = Map.lookup x $ _cachedHistory_blocks history
    f :: LCA.Path BlockHash () -> VeryBlockLike
    f p = histToBlockLike (_cachedHistory_minLevel history) (x, LCA.measure p, p)
  return $ fmap f xPath

data NodeDataSource = NodeDataSource
  { _nodeDataSource_history :: !(MVar CachedHistory')
  , _nodeDataSource_nodes :: !(MVar (Map URI (Maybe VeryBlockLike)))
  , _nodeDataSource_cache :: !(MVar (DMap NodeQuery CachedResult)) -- we add elements to the cache to ind
  , _nodeDataSource_chain :: !ChainId
  , _nodeDataSource_parameters :: !(MVar ProtoInfo)
  , _nodeDataSource_httpMgr :: !Http.Manager
  , _nodeDataSource_pool :: !(Pool Postgresql)
  , _nodeDataSource_latestHead :: !(TVar (Maybe VeryBlockLike))
  , _nodeDataSource_logger :: !LoggingEnv
  }

blankNodeDataSource :: Pool Postgresql -> ChainId -> Maybe ProtoInfo -> Http.Manager -> LoggingEnv -> IO NodeDataSource
blankNodeDataSource db chain protoInfo' mgr logger = do
  nodes <- newMVar mempty
  hist <- newEmptyMVar
  cache <- newEmptyMVar
  protoInfoVar <- newEmptyMVar
  latestHead <- newTVarIO Nothing

  let
    fill = do
      putMVar hist emptyCache
      putMVar cache mempty

  case protoInfo' of
    Just protoInfo -> do
      putMVar protoInfoVar protoInfo
      fill
      runLoggingEnv logger $ $(logInfo) "Cache pre-initialized"

    Nothing ->
      void $ forkIO $ do
        -- wait for someone else to put something in protoInfo, then fill the rest of the MVars.
        _ <- readMVar protoInfoVar
        fill
        runLoggingEnv logger $ $(logInfo) "Cache ready!"

  return NodeDataSource
    { _nodeDataSource_history = hist
    , _nodeDataSource_nodes = nodes
    , _nodeDataSource_cache = cache
    , _nodeDataSource_chain = chain
    , _nodeDataSource_parameters = protoInfoVar
    , _nodeDataSource_httpMgr = mgr
    , _nodeDataSource_pool = db
    , _nodeDataSource_latestHead = latestHead
    , _nodeDataSource_logger = logger
    }

class HasNodeDataSource a where
  nodeDataSource :: Lens' a NodeDataSource

instance HasNodeDataSource NodeDataSource where
  nodeDataSource = id

withNDSLogging :: (MonadReader r m, HasNodeDataSource r) => LoggingT m a -> m a
withNDSLogging x = flip runLoggingEnv x . _nodeDataSource_logger =<< asks (^. nodeDataSource)

calcTimeBetweenBlocks :: ProtoInfo -> NominalDiffTime
calcTimeBetweenBlocks = fromIntegral . sum . take 1 . toList . _protoInfo_timeBetweenBlocks

-- | Blocks until a new head is seen or the time between blocks has elapsed.
--
-- Returns most recently seen head.
waitForNewHeadWithTimeout :: NodeDataSource -> IO ()
waitForNewHeadWithTimeout nds = do
  -- TODO: This shouldn't be necessary once we have a way to know the parameters better. Foundation nodes should give us params.
  timeLimit <- maybe 60 calcTimeBetweenBlocks <$> tryReadMVar (_nodeDataSource_parameters $ nds ^. nodeDataSource)
  void $ timeout' timeLimit $ waitForNewHead nds

waitForNewHead :: NodeDataSource -> IO VeryBlockLike
waitForNewHead nds = do
  oldHead <- readTVarIO (_nodeDataSource_latestHead nds)
  atomically $ do
    newHead <- maybe retry pure =<< readTVar (_nodeDataSource_latestHead nds)
    when (oldHead == Just newHead) retry
    pure newHead

-- turn the result of an LCA.uncons on the block history into a VeryBlockLike
histToBlockLike :: RawLevel -> (BlockHash, (), LCA.Path BlockHash ()) -> VeryBlockLike
histToBlockLike minLevel (h, (), path) = VeryBlockLike h p mempty blkLevel unixEpoch
  where
    blkLevel = minLevel + fromIntegral (length path) + 1
    p = maybe h (\(pp, _, _) -> pp) $ LCA.uncons path

updateNodeDataSource :: BlockLike b => NodeDataSource -> URI -> b -> IO ()
updateNodeDataSource nds nodeAddr blk =
  modifyMVar_ (_nodeDataSource_nodes nds) $ return . Map.insert nodeAddr (Just $ mkVeryBlockLike blk)

-- Make sure that the protocol parameters have been loaded and the datasource initialzied.
initParams :: Foldable f => NodeDataSource -> f (Maybe PublicNode, URI) -> IO Bool
initParams nds theseNodes = runLoggingEnv (_nodeDataSource_logger nds) $ do
  needParams <- liftIO $ isEmptyMVar $ _nodeDataSource_parameters nds

  let
    step :: LoggingT IO (Maybe ProtoInfo) -> (Maybe PublicNode, URI) -> LoggingT IO (Maybe ProtoInfo)
    step l (pn, someNode) = l >>= \case
      Nothing -> do
        let ctx = PublicNodeContext (NodeRPCContext (_nodeDataSource_httpMgr nds) (Uri.render someNode)) pn
        runExceptT (runReaderT (getProtoConstants chainId) ctx) <&> \case
          Left (_ :: PublicNodeError) -> Nothing
          Right params -> Just params
      l' -> return l'
    onChainNodes = foldl step (return Nothing) theseNodes

  when needParams $ onChainNodes >>= \case
    Nothing -> $(logInfo) "Still no params"
    Just params -> do
      void $ liftIO $ tryPutMVar (_nodeDataSource_parameters nds) params
      insertParams params

  liftIO $ fmap not $ isEmptyMVar $ _nodeDataSource_parameters nds
  where
    chainId = _nodeDataSource_chain nds
    insertParams params = runDb (Identity $ _nodeDataSource_pool nds) $ do
      have :: Maybe (Id Parameters) <- fmap toId . listToMaybe <$> project AutoKeyField (Parameters_chainField ==. chainId)
      case have of
        Just _entryId -> pure ()
        Nothing -> do
          let
            entry = Parameters
              { _parameters_protoInfo = params
              , _parameters_chain = chainId
              }
          notify . flip Notify_Parameters entry =<< insert' entry


-- | extrats the fittest known branch from cache
dataSourceHead
  :: ( MonadIO m , MonadReader s m, HasNodeDataSource s)
  => m (Maybe VeryBlockLike)
dataSourceHead = withCache Nothing $ \_ -> do
  dsrc <- asks (^. nodeDataSource)
  history <- liftIO $ readMVar $ _nodeDataSource_history dsrc
  let branches = _cachedHistory_branches history
  pure $ maximumByMay (compare `on` view fitness) $ toList branches

-- | extrats the fittest known node from cache
dataSourceNode ::
  ( MonadIO m
  , MonadReader s m, HasNodeDataSource s
  )
  => m (Maybe NodeRPCContext)
dataSourceNode = do
  dsrc <- asks (^. nodeDataSource)
  nodes <- liftIO $ readMVar $ _nodeDataSource_nodes dsrc
  pure $ fmap (NodeRPCContext (_nodeDataSource_httpMgr dsrc) . Uri.render . fst) $
    maximumByMay (compare `on` snd) $ catMaybes $ fmap sequence $ Map.toList nodes

levelAncestor :: CachedHistory' -> RawLevel -> BlockHash -> Maybe BlockHash
levelAncestor hist lvl ctx = ctxBlockHash
  where
    minLevel = _cachedHistory_minLevel hist
    branch = Map.lookup ctx $ _cachedHistory_blocks hist
    ctxBlockHash = fmap (view _1) $ LCA.uncons =<< LCA.keep (fromIntegral $ lvl - minLevel) <$> branch


rightsContext :: ProtoInfo -> CachedHistory' -> BlockHash -> RawLevel -> Maybe BlockHash
rightsContext params hist ctx lvl = ctxBlockHash
    -- for this case, we want the first block in the cycle that sits
    -- PRESERVED_CYCLES before the requested level, that is on the correct
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
  let logger = _nodeDataSource_logger dsrc
  protoInfo <- liftIO $ readMVar $ _nodeDataSource_parameters dsrc
  history <- liftIO $ readMVar $ _nodeDataSource_history dsrc

  (qBranch, q) <- maybe (throwError $ RpcError_HttpException "NOT ENOUGH HISTORY" ^. re asRpcError) pure $ getKey protoInfo history q'

  resultM <- liftIO $ modifyMVar (_nodeDataSource_cache dsrc) $ \cache -> case DMap.lookup q cache of
    Just avar -> pure (cache, avar) -- cache hit
    Nothing -> do
      newVar <- liftIO newEmptyMVar
      let
        mkResult now v = CacheLine
          { _cacheLine_value = v
          , _cacheLine_used = now
          }
      _ <- liftIO $ forkIO $ do
        fromDB <- runLoggingEnv logger $ tryFetchFromCache (_nodeDataSource_pool dsrc) q
        case fromDB of
          Just x -> do
            now <- getCurrentTime
            putMVar newVar $ Right $ mkResult now x
          Nothing -> runReaderT (pickNode qBranch) dsrc >>= \case
            Nothing -> do
              putMVar newVar $ Left $ RpcError_HttpException "No suitable node"
            Just anyNode -> do
              let
                ctx = NodeRPCContext (_nodeDataSource_httpMgr dsrc) (Uri.render anyNode)

                unliftDataSrc :: NodeQuery a -> IO (Either RpcError a)
                unliftDataSrc = flip runReaderT dsrc . runExceptT . nodeQueryDataSource
              res' <- nodeQueryDataSourceImpl (_nodeDataSource_chain dsrc) protoInfo ctx logger unliftDataSrc q
              now <- getCurrentTime
              putMVar newVar $ mkResult now <$> res'
      pure (DMap.insert q (CachedResult newVar) cache, CachedResult newVar)

  unpackCacheResult resultM

pickNode
  :: forall m a.
  ( MonadIO m
  , MonadReader a m, HasNodeDataSource a
  )
  => BlockHash -> m (Maybe URI)
pickNode branch = do
  dsrc <- asks (^. nodeDataSource)
  nodeHeads <- liftIO $ readMVar $ _nodeDataSource_nodes dsrc
  fmap (headMay . catMaybes) $ for (Map.toList $ Map.mapMaybe id nodeHeads) $ \(nodeUri, nodeHead) ->
    containsBranch nodeHead >>= \isCanditate ->
      pure $ if isCanditate then Just nodeUri else Nothing
  where
    containsBranch nodeHead = (Just branch ==) . (^? _Just . hash) <$> branchPoint (nodeHead ^. hash) branch

nodeQueryDataSourceImpl
  :: forall a.
     ChainId
  -> ProtoInfo
  -> NodeRPCContext
  -> LoggingEnv
  -> (forall b. NodeQuery b -> IO (Either RpcError b))
  -> NodeQuery a
  -> IO (Either RpcError a)
nodeQueryDataSourceImpl chainId _proto ctx logger _self q = runExceptT $ case q of
  NodeQuery_BakingRights branch targetLevel ->
    nodeRPC' $ rBakingRights chainId branch $ Set.singleton $ Left targetLevel
  NodeQuery_EndorsingRights branch targetLevel ->
    nodeRPC' $ rEndorsingRights chainId branch $ Set.singleton $ Left targetLevel
  NodeQuery_Account branch contractId ->
    nodeRPC' $ rContract chainId branch contractId
  NodeQuery_Block branch -> nodeRPC' $ rBlock chainId branch
  where
    nodeRPC' :: forall c. (forall repr. (BlockType repr ~ Block, QueryNode repr, QueryHistory repr, QueryBlock repr) => repr c) -> ExceptT RpcError IO c
    nodeRPC' q' = runReaderT (runLoggingEnv logger $ nodeRPC q') ctx


withCache ::
  ( MonadReader r m , HasNodeDataSource r
  , MonadIO m
  )
  => a -> (ProtoInfo -> m a) -> m a
withCache dft action = do
  dsrc <- asks (^. nodeDataSource)
  protoInfo <- liftIO $ tryReadMVar $ _nodeDataSource_parameters dsrc
  fromMaybe dft <$> traverse action protoInfo

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
      return (result, a)


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
    Nothing -> throwError $ RpcError_UnexpectedStatus 404 "NO BRANCH" ^. re asRpcError


calculateBakeEfficiency ::
  ( MonadIO m
  , MonadReader s m , HasNodeDataSource s
  , MonadError RpcError m
  , BlockLike b
  )
  => b -> RawLevel -> PublicKeyHash -> m BakeEfficiency
calculateBakeEfficiency branch len delegate = do
  withNDSLogging $ $(logDebugSH) ("bake efficiency requested" :: Text, branch ^. hash, len, delegate)

  let
    branchLevel = branch ^. level
    branchHash = branch ^. hash
    levels = [branchLevel - len..branchLevel]
  branchHashes <- ancestors len branchHash

  rights <- (fmap.fmap) bakingRightsMap $ for levels $ nodeQueryDataSource . NodeQuery_BakingRights branchHash
  bakers <- for branchHashes $ fmap (^. block_metadata . blockMetadata_baker) . nodeQueryDataSource . NodeQuery_Block
  let result = fold $ efficiencyOfBlock <$> ZipList rights <*> ZipList bakers
  withNDSLogging $ $(logDebugSH) ("efficiency" :: Text, delegate, result)
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

tryFetchFromCache :: Pool Postgresql -> NodeQuery a -> LoggingT IO (Maybe a)
tryFetchFromCache db q = do
  let
    qJson = Json $ requestToJSON q
  resultM <- fmap listToMaybe $ runDb (Identity db) $ select $ GenericCacheEntry_keyField ==. qJson
  case resultM of
    Nothing -> return Nothing
    Just result -> case requestResponseFromJSON q of
      Dict -> case Aeson.fromJSON (unJson $ _genericCacheEntry_value result) of
        Aeson.Success v -> return $ Just v
        Aeson.Error bad -> do
          $(logWarnSH) (T.pack "tryFetchFromCache failed to decode:", bad)
          return Nothing

deriveGEq ''NodeQuery
deriveGCompare ''NodeQuery
deriveGShow ''NodeQuery
makeRequestForData ''NodeQuery
makeLenses 'NodeDataSource
