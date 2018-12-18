{-# LANGUAGE DeriveGeneric #-}
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

{-# OPTIONS_GHC -Wall -Werror #-}

-- TODO: move this to ~lib?
module Backend.CachedNodeRPC where

import Prelude hiding (cycle)

import Control.Applicative (ZipList (..))
import Control.Concurrent.STM (STM, TQueue, TVar, atomically, newTQueueIO, newTVarIO, readTVar, readTVarIO,
                               retry, writeTQueue, writeTVar)
import Control.Exception.Safe (withException)
import Control.Lens (TraversableWithIndex, re)
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT, throwError)
import Control.Monad.Logger (LoggingT (..), MonadLogger, logDebugSH, logErrorSH, logInfo, logWarnSH)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as Aeson
import Data.Constraint (Dict (..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.LCA.Online.Polymorphic as LCA
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Pool (Pool)
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Client as Http (Manager)
import Rhyolite.Backend.DB (runDb, selectMap)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Rhyolite.Request.Class (requestResponseFromJSON, requestToJSON)
import Rhyolite.Request.TH (makeRequestForData)
import Rhyolite.Schema (Json (..))
import Safe (headMay)
import Safe.Foldable (maximumByMay)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.History
import Tezos.Json (deriveTezosJson)
import Tezos.NodeRPC.Class
import Tezos.NodeRPC.Network
import Tezos.NodeRPC.Sources
import Tezos.NodeRPC.Types
import Tezos.Types

import Backend.Common (timeout')
import Backend.Schema
import Backend.STM (HasTimestamp, MonadSTM (liftSTM), atomicallyWith, atomicallyWithTime, modifyTVar_',
                    newTVar', readTVar', retry', writeTVar')
import qualified Backend.STM as Stm
import Common (unixEpoch)
import Common.Schema
import ExtraPrelude

data NodeQuery a where
  NodeQuery_BakingRights    :: BlockHash -> RawLevel -> NodeQuery (Seq BakingRights)
  NodeQuery_EndorsingRights :: BlockHash -> RawLevel -> NodeQuery (Seq EndorsingRights)
  NodeQuery_Account         :: BlockHash -> ContractId -> NodeQuery Account
  NodeQuery_Block           :: BlockHash -> NodeQuery Block
  NodeQuery_BlockBaker      :: BlockHash -> RawLevel -> NodeQuery BlockBaker
  NodeQuery_DelegateInfo    :: BlockHash -> RawLevel -> PublicKeyHash -> NodeQuery CacheDelegateInfo
deriving instance Show (NodeQuery a)


-- delegatedContracts isn't interesting to kiln at this time.  Even if it were,
-- we'd probably want to cache it seperately  (it changes way slower anyhow)
data CacheDelegateInfo = CacheDelegateInfo
  { _cacheDelegateInfo_balance :: !Tez
  , _cacheDelegateInfo_frozenBalance :: !Tez
  , _cacheDelegateInfo_frozenBalanceByCycle :: !(Seq FrozenBalanceByCycle)
  , _cacheDelegateInfo_stakingBalance :: !Tez
  -- , _cacheDelegateInfo_delegatedContracts :: !(Seq.Seq ContractId)
  , _cacheDelegateInfo_delegatedBalance :: !Tez
  , _cacheDelegateInfo_deactivated :: !Bool
  , _cacheDelegateInfo_gracePeriod :: !Cycle
  }

toCacheDelegateInfo :: DelegateInfo -> CacheDelegateInfo
toCacheDelegateInfo di = CacheDelegateInfo
  { _cacheDelegateInfo_balance = _delegateInfo_balance di
  , _cacheDelegateInfo_frozenBalance = _delegateInfo_frozenBalance di
  , _cacheDelegateInfo_frozenBalanceByCycle = _delegateInfo_frozenBalanceByCycle di
  , _cacheDelegateInfo_stakingBalance = _delegateInfo_stakingBalance di
  -- , _cacheDelegateInfo_delegatedContracts = _delegateInfo_delegatedContracts di
  , _cacheDelegateInfo_delegatedBalance = _delegateInfo_delegatedBalance di
  , _cacheDelegateInfo_deactivated = _delegateInfo_deactivated di
  , _cacheDelegateInfo_gracePeriod = _delegateInfo_gracePeriod di
  }



data CachedBlockInfo = CachedBlockInfo
  deriving (Eq, Ord, Show, Typeable)

type CachedHistory' = CachedHistory ()
type DirtyBit = Maybe (Id GenericCacheEntry)

data CacheLine a = CacheLine
  { _cacheLine_value :: !a
  , _cacheLine_used :: !UTCTime
  , _cacheLine_dirty :: !DirtyBit -- is this entry already in the database?
  }

data NodeDataSource = NodeDataSource
  { _nodeDataSource_history :: !(TVar CachedHistory')
  , _nodeDataSource_nodes :: !(TVar (Map URI (Maybe VeryBlockLike)))
  , _nodeDataSource_cache :: !(TVar (DMap NodeQuery (Compose TVar CacheLine)))
  , _nodeDataSource_chain :: !ChainId
  , _nodeDataSource_parameters :: !(TVar (Maybe ProtoInfo))
  , _nodeDataSource_httpMgr :: !Http.Manager
  , _nodeDataSource_pool :: !(Pool Postgresql)
  , _nodeDataSource_latestHead :: !(TVar (Maybe VeryBlockLike))
  , _nodeDataSource_logger :: !LoggingEnv
  , _nodeDataSource_ioQueue :: TQueue (IO ())
  } deriving (Typeable, Generic)
makeLenses 'NodeDataSource

class HasNodeDataSource a where
  nodeDataSource :: Lens' a NodeDataSource

instance HasNodeDataSource NodeDataSource where
  nodeDataSource = id

waitForParams :: (HasNodeDataSource r, MonadSTM m) => r -> m ProtoInfo
waitForParams r = maybe retry' pure =<< readTVar' (r ^. nodeDataSource . nodeDataSource_parameters)

withParams :: (HasNodeDataSource r, MonadIO m) => r -> (ProtoInfo -> m a) -> m a
withParams r act = liftIO (atomically (waitForParams r)) >>= act

unpackCacheResult
  :: forall a r m. (MonadSTM m, MonadReader r m, HasTimestamp r)
  => Compose TVar CacheLine a -> m a
unpackCacheResult (Compose var) = do
  result <- readTVar' var
  now <- asks (^. Stm.timestamp)
  writeTVar' var $ result{_cacheLine_used = now}
  pure $ _cacheLine_value result

-- get lca between two blocks
branchPoint
  :: forall r m. (HasNodeDataSource r, MonadSTM m, MonadReader r m)
  => BlockHash -> BlockHash -> m (Maybe VeryBlockLike)
branchPoint x y = do
  dsrc <- asks (^. nodeDataSource)
  history <- readTVar' $ _nodeDataSource_history dsrc
  let
    xPath = Map.lookup x $ _cachedHistory_blocks history
    yPath = Map.lookup y $ _cachedHistory_blocks history
  return $ fmap (histToBlockLike (_cachedHistory_minLevel history)) . LCA.uncons =<< LCA.lca <$> xPath <*> yPath

-- | enumerate the block hashes between lca(x, y) and (x,y), respectively, from newest to oldest
enumerateBranches
  :: ( MonadSTM m
     , MonadReader a m, HasNodeDataSource a
     )
  => BlockHash -> BlockHash -> m (Maybe ([BlockHash], [BlockHash]))
enumerateBranches x y = do
  dsrc <- asks (^. nodeDataSource)
  history <- readTVar' $ _nodeDataSource_history dsrc
  pure $ do
    xPath <- Map.lookup x $ _cachedHistory_blocks history
    yPath <- Map.lookup y $ _cachedHistory_blocks history
    let pathPrefix long = fmap fst $ take (LCA.length long - LCA.length (LCA.lca xPath yPath)) $ LCA.toList long
    pure (pathPrefix xPath, pathPrefix yPath)


lookupBlock
  :: forall nds m. (HasNodeDataSource nds, MonadSTM m)
  => nds -> BlockHash -> m (Maybe VeryBlockLike)
lookupBlock nds x = do
  let dsrc = nds ^. nodeDataSource
  history <- readTVar' $ _nodeDataSource_history dsrc
  let
    xPath = Map.lookup x $ _cachedHistory_blocks history
    f :: LCA.Path BlockHash () -> VeryBlockLike
    f p = histToBlockLike (_cachedHistory_minLevel history) (x, LCA.measure p, p)
  return $ fmap f xPath

blankNodeDataSource :: Pool Postgresql -> ChainId -> Maybe ProtoInfo -> Http.Manager -> LoggingEnv -> IO NodeDataSource
blankNodeDataSource db chain protoInfo' mgr logger = do
  nodes <- newTVarIO mempty
  hist <- newTVarIO emptyCache
  cache <- newTVarIO mempty
  protoInfoVar <- newTVarIO protoInfo'
  latestHead <- newTVarIO Nothing
  ioQueue <- newTQueueIO

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
    , _nodeDataSource_ioQueue = ioQueue
    }

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
  timeLimit <- maybe 60 calcTimeBetweenBlocks <$> readTVarIO (_nodeDataSource_parameters $ nds ^. nodeDataSource)
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

updateNodeDataSource
  :: forall nds b m. (HasNodeDataSource nds, BlockLike b, MonadSTM m)
  => nds -> URI -> b -> m ()
updateNodeDataSource nds nodeAddr blk = do
  let nodesVar = nds ^. nodeDataSource . nodeDataSource_nodes
  modifyTVar_' nodesVar $ pure . Map.insert nodeAddr (Just $ mkVeryBlockLike blk)

-- Make sure that the protocol parameters have been loaded and the datasource initialized.
initParams :: Foldable f => NodeDataSource -> f (Maybe PublicNode, URI) -> IO Bool
initParams nds theseNodes = runLoggingEnv (_nodeDataSource_logger nds) $ do
  needParams <- fmap isNothing $ liftIO $ readTVarIO $ _nodeDataSource_parameters nds
  case needParams of
    False -> pure True
    True -> onChainNodes >>= \case
      Nothing -> $(logInfo) "Still no params" $> False
      Just params -> do
        void $ liftIO $ atomically $ writeTVar (_nodeDataSource_parameters nds) $ Just params
        insertParams params
        pure True

  where
    chainId = _nodeDataSource_chain nds

    step :: LoggingT IO (Maybe ProtoInfo) -> (Maybe PublicNode, URI) -> LoggingT IO (Maybe ProtoInfo)
    step l (pn, someNode) = l >>= \case
      Nothing -> do
        let ctx = PublicNodeContext (NodeRPCContext (_nodeDataSource_httpMgr nds) (Uri.render someNode)) pn
        runExceptT (runReaderT (getProtoConstants chainId) ctx) >>= \case
          Left (e :: PublicNodeError) -> $(logErrorSH) e $> Nothing
          Right params -> $(logDebugSH) params $> Just params
      l' -> return l'

    onChainNodes = foldl step (return Nothing) theseNodes

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
  :: forall nds m. (HasNodeDataSource nds, MonadSTM m)
  => nds -> m (Maybe VeryBlockLike)
dataSourceHead nds = withCache nds Nothing $ \_ -> do
  let dsrc = nds ^. nodeDataSource
  history <- readTVar' $ _nodeDataSource_history dsrc
  let branches = _cachedHistory_branches history
  pure $ maximumByMay (compare `on` view fitness) $ toList branches

-- | extrats the fittest known node from cache
dataSourceNode
  :: forall nds m. (HasNodeDataSource nds, MonadSTM m)
  => nds -> m (Maybe NodeRPCContext)
dataSourceNode nds = do
  let dsrc = nds ^. nodeDataSource
  nodes <- readTVar' $ _nodeDataSource_nodes dsrc
  pure $ fmap (NodeRPCContext (_nodeDataSource_httpMgr dsrc) . Uri.render . fst) $
    maximumByMay (compare `on` snd) $ mapMaybe sequence $ Map.toList nodes

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust [] = []
takeWhileJust (Just x: xs) = x:takeWhileJust xs
takeWhileJust (Nothing: _) = []


data RightsCycleInfo = RightsCycleInfo
  { _rightsCycleInfo_branch :: !BlockHash  -- the hash of the first block in the cycle that confers rights
  , _rightsCycleInfo_cycle :: !Cycle       -- the cycle in which rights are confered
  , _rightsCycleInfo_minLevel :: !RawLevel -- the first level in that cycle
  , _rightsCycleInfo_maxLevel :: !RawLevel -- the last level in that cycle
  } deriving (Eq, Ord, Show, Generic, Typeable)

-- produce the list of the first blocks in the cycle for the previous 7 cycles ending on $blkHash$
cycleStartHashes
  :: forall nds m. (HasNodeDataSource nds, MonadReader nds m, MonadSTM m)
  => BlockHash -> m (Maybe [RightsCycleInfo]) -- Nothing when the branch is not in history.
cycleStartHashes blkHash = do
  dsrc <- asks (^. nodeDataSource)
  protoInfo <- maybe retry' pure =<< readTVar' (_nodeDataSource_parameters dsrc)
  history <- readTVar' $ _nodeDataSource_history dsrc
  return $ do
    branch <- blkHash `Map.lookup` (_cachedHistory_blocks history)
    let
      minLvl = _cachedHistory_minLevel history
      lvl = minLvl + RawLevel (length branch)
      cycle = levelToCycle protoInfo lvl
      preservedCycles = _protoInfo_preservedCycles protoInfo
      cycles = [max 0 (cycle - (2 + preservedCycles)) .. cycle]
      minLevels = firstLevelInCycle protoInfo <$> cycles
      maxLevels = pred . firstLevelInCycle protoInfo . succ <$> cycles
      branches = fmap (^. _1) $ takeWhileJust $ LCA.uncons . flip LCA.keep branch . unRawLevel . subtract minLvl <$> minLevels
    return $ getZipList $ RightsCycleInfo
      <$> ZipList branches
      <*> ZipList cycles
      <*> ZipList minLevels
      <*> ZipList maxLevels




levelAncestor :: CachedHistory' -> RawLevel -> BlockHash -> Maybe BlockHash
levelAncestor hist lvl ctx = ctxBlockHash
  where
    minLevel = _cachedHistory_minLevel hist
    branch = Map.lookup ctx $ _cachedHistory_blocks hist
    ctxBlockHash = fmap (view _1) $ LCA.uncons =<< LCA.keep (fromIntegral $ lvl - minLevel) <$> branch

-- | We want the first block in the cycle that sits PRESERVED_CYCLES before the
-- requested level, that is on the correct branch.
rightsContext :: ProtoInfo -> CachedHistory' -> BlockHash -> RawLevel -> Maybe BlockHash
rightsContext params hist ctx lvl = levelAncestor hist (rightsContextLevel params lvl) ctx

-- Recontextualize a query for maximum cache friendliness, and also return the least block
getKey :: ProtoInfo -> CachedHistory' -> NodeQuery a -> Maybe (BlockHash, NodeQuery a) -- , Set ClientAddress)
getKey params hist = \case
  NodeQuery_BakingRights ctx lvl -> (\ctx' -> (ctx' , NodeQuery_BakingRights ctx' lvl)) <$> rightsContext params hist ctx lvl
  NodeQuery_EndorsingRights ctx lvl -> (\ctx' -> (ctx' , NodeQuery_EndorsingRights ctx' lvl)) <$> rightsContext params hist ctx lvl
  NodeQuery_Block ctx -> pure (ctx, NodeQuery_Block ctx)
  NodeQuery_Account ctx contractId -> pure (ctx, NodeQuery_Account ctx contractId)
  NodeQuery_BlockBaker ctx lvl -> (\ctx' -> (ctx' , NodeQuery_BlockBaker ctx' lvl)) <$> levelAncestor hist lvl ctx
  NodeQuery_DelegateInfo ctx lvl pkh -> (\ctx' -> (ctx' , NodeQuery_DelegateInfo ctx' lvl pkh)) <$> levelAncestor hist lvl ctx

-- | Caching query function simplified by blocking until we get a result.
nodeQueryDataSource
  :: forall a s e m.
    ( MonadIO m
    , MonadReader s m, HasNodeDataSource s
    , MonadError e m, AsCacheError e
    )
  => NodeQuery a -> m a
nodeQueryDataSource q = do
  getResult <- nodeQueryDataSourceRaw q
  now <- liftIO getCurrentTime
  timeout' timeoutSeconds (atomically $ maybe retry pure =<< getResult now) >>= \case
    Nothing -> throwError $ CacheError_Timeout timeoutSeconds ^. re asCacheError
    Just (Left e) -> throwError $ e ^. re asCacheError
    Just (Right x) -> pure x
  where
    -- Base timeout
    timeoutSeconds = 60*5

-- Foundational caching query function exposing a low-level API to the underlying 'STM' operations.
nodeQueryDataSourceRaw
  :: forall a s e m.
    ( MonadIO m
    , MonadReader s m, HasNodeDataSource s
    , MonadError e m, AsCacheError e
    )
  => NodeQuery a -> m (UTCTime -> STM (Maybe (Either CacheError a)))
nodeQueryDataSourceRaw q' = do
  dsrc <- asks $ view nodeDataSource
  updateCache dsrc >>= \case
    Left e -> throwError $ e ^. re asCacheError
    Right getResult -> pure getResult

  where
    updateCache :: NodeDataSource -> m (Either CacheError (UTCTime -> STM (Maybe (Either CacheError a))))
    updateCache dsrc = liftIO $ atomically $ runExceptT $ do
      protoInfo <- maybe retry' pure =<< readTVar' (_nodeDataSource_parameters dsrc)
      history <- readTVar' (_nodeDataSource_history dsrc)

      (qBranch, q) <- maybe (throwError $ CacheError_NotEnoughHistory ^. re asCacheError) pure $ getKey protoInfo history q'

      cache <- readTVar' cacheVar
      case DMap.lookup q cache of
        -- Cache Hit: Return an STM that reads the cache and updates the "access" timestamp
        Just avar -> pure $ \time -> flip runReaderT time $
          Just . Right <$> unpackCacheResult avar

        -- Cache Miss: Queue the IO action to collect data and return an STM that reads the result.
        Nothing -> do
          -- A separate TVar for keeping the actual API result (outside the cache structure)
          apiResultVar :: TVar (Maybe (Either CacheError a)) <- newTVar' Nothing
          let
            -- Updates the cache key if the result is useful and sets the result 'TVar'.
            writeResult :: Either CacheError (a, DirtyBit) -> IO ()
            writeResult a' = liftIO $ atomicallyWithTime $ do
              case a' of
                Right (a, dirty) -> populateKey q a dirty
                Left _ -> pure ()
              writeTVar' apiResultVar $ Just $ fmap fst a'

          liftSTM $ writeTQueue ioQueue $
            -- Try very hard to write *something* into the result TVar in case of exception.
            (writeResult =<< makeRequestAndCache protoInfo q qBranch)
              `withException` \e ->
                atomically (writeTVar' apiResultVar $ Just $ Left $ CacheError_SomeException e)

          pure $ \_ -> readTVar apiResultVar

      where
        logger = _nodeDataSource_logger dsrc
        ioQueue = _nodeDataSource_ioQueue dsrc
        cacheVar = _nodeDataSource_cache dsrc
        chainId = _nodeDataSource_chain dsrc

        populateKey q a dirty = do
          cache <- readTVar' cacheVar
          case DMap.lookup q cache of
            Just _ -> pure ()
            Nothing -> do
              now <- asks (^. Stm.timestamp)
              var <- newTVar' $ CacheLine a now dirty
              writeTVar' cacheVar $ DMap.insert q (Compose var) cache

        makeRequestAndCache :: ProtoInfo -> NodeQuery a' -> BlockHash -> IO (Either CacheError (a', DirtyBit))
        makeRequestAndCache protoInfo q qBranch = runLoggingEnv logger $ flip runReaderT dsrc $
          tryFetchFromCache chainId (_nodeDataSource_pool dsrc) q >>= \case
            Just x -> pure $ Right $ fmap Just x
            Nothing -> atomicallyWith (pickNode qBranch) >>= \case
              Nothing -> pure $ Left CacheError_NoSuitableNode
              Just anyNode -> do
                let
                  ctx = NodeRPCContext (_nodeDataSource_httpMgr dsrc) (Uri.render anyNode)

                  nodeQueryViaCache :: forall b. NodeQuery b -> IO (Either CacheError b)
                  nodeQueryViaCache qInner = runReaderT (runExceptT $ nodeQueryDataSource qInner) dsrc

                liftIO $ (fmap.fmap) (,Nothing) $ nodeQueryDataSourceImpl (_nodeDataSource_chain dsrc) protoInfo ctx logger nodeQueryViaCache q

pickNode
  :: (HasNodeDataSource r, MonadSTM m, MonadReader r m)
  => BlockHash -> m (Maybe URI)
pickNode branch = do
  dsrc <- asks (^. nodeDataSource)
  nodeHeads <- readTVar' $ _nodeDataSource_nodes dsrc
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
  -> (forall b. NodeQuery b -> IO (Either CacheError b))
  -> NodeQuery a
  -> IO (Either CacheError a)
nodeQueryDataSourceImpl chainId _proto ctx logger self' q = runExceptT $ case q of
  NodeQuery_BakingRights branch targetLevel ->
    nodeRPC' $ rBakingRights chainId branch $ Set.singleton $ Left targetLevel
  NodeQuery_EndorsingRights branch targetLevel ->
    nodeRPC' $ rEndorsingRights chainId branch $ Set.singleton $ Left targetLevel
  NodeQuery_Account branch contractId ->
    nodeRPC' $ rContract chainId branch contractId
  NodeQuery_Block branch -> nodeRPC' $ rBlock chainId branch
  NodeQuery_BlockBaker branch _lvl -> fmap getBakerFromBlock $ self $ NodeQuery_Block branch
  NodeQuery_DelegateInfo branch _lvl pkh -> fmap toCacheDelegateInfo $ nodeRPC' $ rDelegateInfo chainId branch pkh
  where
    nodeRPC' :: forall c. (forall repr. (BlockType repr ~ Block, QueryNode repr, QueryHistory repr, QueryBlock repr) => repr c) -> ExceptT CacheError IO c
    nodeRPC' q' = runReaderT (runLoggingEnv logger $ nodeRPC q') ctx
    {-# INLINE nodeRPC' #-}

    self :: forall b. NodeQuery b -> ExceptT CacheError IO b
    self = ExceptT . self'


withCache
  :: forall nds a m. (HasNodeDataSource nds, MonadSTM m)
  => nds -> a -> (ProtoInfo -> m a) -> m a
withCache nds dft action = do
  let dsrc = nds ^. nodeDataSource
  protoInfo <- readTVar' $ _nodeDataSource_parameters dsrc
  fromMaybe dft <$> traverse action protoInfo

calculateBakerStats ::
  ( TraversableWithIndex (PublicKeyHash, RawLevel) f
  , MonadReader r m, HasNodeDataSource r
  , MonadIO m
  )
  => f a
  -> m (f (First (Maybe (BakeEfficiency, Account)), a))
calculateBakerStats pkhs = do
  nds <- asks (^. nodeDataSource)
  liftIO (atomically (dataSourceHead nds)) >>= \case
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
  , MonadError CacheError m
  )
  => RawLevel -> BlockHash -> m [BlockHash]
ancestors (RawLevel n) branch = do
  hist <- liftIO . readTVarIO =<< asks (_nodeDataSource_history . view nodeDataSource)
  case Map.lookup branch (_cachedHistory_blocks hist) of
    Just branchPath -> return $ fmap fst $ take n $ LCA.toList branchPath
    Nothing -> throwError $ RpcError_UnexpectedStatus 404 "NO BRANCH" ^. re asRpcError

calculateBakeEfficiency ::
  ( MonadIO m
  , MonadReader s m , HasNodeDataSource s
  , MonadError CacheError m
  , BlockLike b
  )
  => b -> RawLevel -> PublicKeyHash -> m BakeEfficiency
calculateBakeEfficiency branch len baker = do
  withNDSLogging $ $(logDebugSH) ("bake efficiency requested" :: Text, branch ^. hash, len, baker)

  let
    branchLevel = branch ^. level
    branchHash = branch ^. hash
    levels = [branchLevel - len..branchLevel]
  branchHashes <- ancestors len branchHash

  rights <- (fmap.fmap) bakingRightsMap $ for levels $ nodeQueryDataSource . NodeQuery_BakingRights branchHash
  bakers <- for branchHashes $ fmap (^. block_metadata . blockMetadata_baker) . nodeQueryDataSource . NodeQuery_Block
  let result = fold $ efficiencyOfBlock <$> ZipList rights <*> ZipList bakers
  withNDSLogging $ $(logDebugSH) ("efficiency" :: Text, baker, result)
  return result
  where
    efficiencyOfBlock :: Map PublicKeyHash Priority -> PublicKeyHash -> BakeEfficiency
    efficiencyOfBlock rights blockBaker = BakeEfficiency
      { _bakeEfficiency_bakedBlocks = if blockBaker == baker then 1 else 0
      , _bakeEfficiency_bakingRights = case (Map.lookup blockBaker rights, Map.lookup baker rights) of
          (_, Nothing) -> 0
          (Just them, Just us) -> if us <= them then 1 else 0
          (Nothing, _) -> 0 -- error "Very wrong"
      }

    bakingRightsMap :: Foldable f => f BakingRights -> Map PublicKeyHash Priority -- map from baker to
    bakingRightsMap xs = Map.fromList
      [ (d, prio)
      | BakingRights _lvl d prio _ <- toList xs
      ]

tryFetchFromCache
  :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
  => ChainId -> Pool Postgresql -> NodeQuery a -> m (Maybe (a, Id GenericCacheEntry))
tryFetchFromCache chainId db q = do
  let
    qJson = Json $ requestToJSON q
  resultM :: Map (Id GenericCacheEntry) GenericCacheEntry <- runDb (Identity db) $ selectMap GenericCacheEntryConstructor
    $  (GenericCacheEntry_keyField ==. qJson
    &&. GenericCacheEntry_chainIdField ==. chainId) -- we select this to use the unique constraint index
  case nonEmpty $ Map.toList resultM of
    Nothing -> return Nothing
    Just ((rid, result) :| _) -> case requestResponseFromJSON q of
      Dict -> case Aeson.fromJSON (unJson $ _genericCacheEntry_value result) of
        Aeson.Success v -> return $ Just (v, rid)
        Aeson.Error bad -> do
          $(logWarnSH) $ "tryFetchFromCache failed to decode: " <> bad
          return Nothing

deriveGEq ''NodeQuery
deriveGCompare ''NodeQuery
deriveGShow ''NodeQuery
makeRequestForData ''NodeQuery
concat <$> traverse deriveTezosJson
  [ ''CacheDelegateInfo
  ]

-- TODO: Is this worth keeping?
instance Hashable (NodeQuery a) where
  hashWithSalt s = hashWithSalt s . requestToJSON
