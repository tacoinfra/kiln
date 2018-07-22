{-# LANGUAGE BangPatterns #-}
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

module Backend where

import Control.Applicative (ZipList (..), liftA2, (<|>))
import Control.Category ((.))
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Exception.Safe (Handler (..), catch, catches, finally, throwIO)
import Control.Lens (ifor, ifor_, ix, to, (.~), (<&>), (^.), (^?), _Just, _Right)
import Control.Monad (join, unless, void, when, (<=<))
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as Aeson
import qualified Data.AppendMap as AppendMap
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy as LBS
import Data.Default (def)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Foldable (fold, foldl', for_, toList, traverse_)
import Data.Function (on, (&))
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortBy)
import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup (Semigroup, Sum (..), getSum, (<>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Traversable (for)
import Data.Word (Word64)
import Database.Groundhog.Generic.Migration (getTableAnalysis)
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple as Pg
import qualified Network.HTTP.Client as Http (Manager, newManager)
import qualified Network.HTTP.Client.TLS as Https
import qualified Network.HTTP.Simple as Http
import Network.Mail.Mime (Address (..), Mail, simpleMail')
import Obelisk.Asset.Serve.Snap (serveAssets)
import Obelisk.ExecutableConfig.Inject (injectPure)
import Prelude hiding ((.))
import Reflex.Dom.Core (renderStatic)
import Rhyolite.Backend (withDb)
import Rhyolite.Backend.Account (migrateAccount)
import qualified Rhyolite.Backend.App as RhyoliteApp
import Rhyolite.Backend.DB (RunDb, getTime, openDb, runDb, selectMap)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (In (..), Only (..), PostgresRaw, Values (..), executeQ, queryQ)
import qualified Rhyolite.Backend.Email as RhyoliteEmail
import Rhyolite.Backend.EmailWorker (clearMailQueue, migrateQueuedEmail)
import Rhyolite.Backend.Listen (NotificationType (..), insertAndNotify, insertAndNotify_, notifyEntityId,
                                updateAndNotify)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Backend.Snap (appConfig_initialHead, serveApp)
import Rhyolite.Concurrent (worker)
import Rhyolite.Route (RouteEnv)
import Rhyolite.Schema (Id (..), Json (..))
import Safe (maximumByMay, maximumMay)
import Say (say, sayErr, sayShow)
import Snap.Core (MonadSnap, route)
import qualified Snap.Http.Server as SnapServer
import Snap.Util.FileServe (serveDirectory)
import System.Console.GetOpt (ArgDescr (ReqArg), OptDescr (Option))
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr)
import System.IO.Error (isDoesNotExistError)
import Text.URI (URI)
import qualified Text.URI.Lens as Uri

import Backend.ChainHealth (scanForkInfo)
import Backend.Config (AppConfig (..), HasAppConfig, getAppConfig)
import Backend.Errors
import Backend.NodeRPC (HasNodeRPC, NodeRPCContext (..), nodeRPC)
import Backend.NotifyHandler (notifyHandler)
import Backend.RequestHandler
import Backend.Schema
import Backend.ViewSelectorHandler (viewSelectorHandler)
import Common (tshow)
import Common.Base16ByteString (unbase16ByteString)
import qualified Common.Config as Config
import Common.Json (TezosWord64 (..))
import Common.Operation (sumFees)
import Common.PublicKeyHash (PublicKeyHash, toPublicKeyHashText, tryReadPublicKeyHashText)
import Common.Schema
import Common.TaggedHash
import Common.URI (mkRootUri)
import Common.Verification (ForkInfoF (..), ForkStatusF (..), validateForkyBlocks)
import Frontend (frontend)


seconds :: Int -> Int
seconds = (* 10^(6 :: Int))

addNode
  :: (PostgresRaw m, Monad m, PersistBackend m)
  => Node
  -> m (Id Node)
addNode node = do
  let addr = _node_address node
  [queryQ| SELECT id FROM "Node" WHERE address = ?addr |] >>= \case
    (Only (nodeId :: Id Node):_) -> do
      updateAndNotify nodeId
        [ Node_addressField =. addr
        , Node_headLevelField =. _node_headLevel node
        , Node_peerCountField =. _node_peerCount node
        , Node_networkStatField =. _node_networkStat node
        ]
      return nodeId
    _ -> insertAndNotify node

nodeWorker
  :: Int -- delay between checking for updates, in seconds
  -> AppConfig
  -> Http.Manager
  -> Pool Postgresql
  -> IO (IO ())
nodeWorker delay appConfig httpMgr db = do
  worker (seconds delay) $ do
    say "Update node cycle."
    runNoLoggingT $ runDb (Identity db) $ flip runReaderT appConfig $ do
      nodes :: [(Id Node, ClientAddress)] <- fmap (first toId) <$>
        project (AutoKeyField, Node_addressField) (Node_deletedField ==. False)

      for nodes $ \(nodeId :: Id Node, nodeAddr) -> do
        say $ "Updating node at " <> nodeAddr
        let ctx = NodeRPCContext httpMgr nodeAddr -- "http://127.0.0.1:18731"
        runReaderT (runExceptT $ nodeRPC $ RProtoConstants headId) ctx >>= \case
          Left e -> reportInaccessibleEndpointError EndpointType_Node nodeAddr
          Right protoInfo -> do
            clearInaccessibleEndpointError EndpointType_Node nodeAddr
            [queryQ| SELECT id FROM "Parameters" WHERE node = ?nodeId |] >>= \case
              (Only (pid :: Id Parameters): _) ->
                updateAndNotify pid [Parameters_protoInfoField =. protoInfo]
              _ ->
                insertAndNotify_ Parameters {_parameters_node = nodeId, _parameters_protoInfo = protoInfo}

        headBlockRsp <- runReaderT (runExceptT $ nodeRPC $ RBlock headId) ctx
        for_ headBlockRsp $ \headBlockInfo -> do
          updateAndNotify nodeId
            [ Node_headLevelField =. Just (headBlockInfo ^. blockInfo_header . blockInfoHeader_level)
            , Node_headBlockHashField =. Just (headBlockInfo ^. blockInfo_hash)
            , Node_fitnessField =. Just (headBlockInfo ^. blockInfo_header . blockInfoHeader_fitness)
            ]


-- I'm fairly sure this is not 100% correct, but I'm also not 100% sure what the correct thing is. Which block's protocol constants should be
-- inspected when determining the rewards for a block which is baked? I'm basically assuming that the constants are sufficiently constant for now.
queryBestNode :: (Monad m, PersistBackend m, PostgresRaw m) => m (Maybe (Id Node, Node, ProtoInfo))
queryBestNode = do
  nodeIds :: Maybe (Id Node, Id Parameters) <- listToMaybe <$> [queryQ|
    SELECT n.id, p.id
      FROM "Node" n JOIN "Parameters" p ON n.id = p.node
     WHERE n."headLevel" IS NOT NULL AND NOT n.deleted
     ORDER BY n."headLevel" DESC
     LIMIT 1 |]

  for nodeIds $ \(nodeId, paramId) -> do
    Just node <- get (fromId nodeId)
    Just params <- get (fromId paramId)
    return (nodeId, node, _parameters_protoInfo params)


clientWorker
  :: Int -- delay between checking for updates, in seconds
  -> AppConfig
  -> Http.Manager
  -> Pool Postgresql
  -> IO (IO ())
clientWorker delay appConfig httpMgr db = worker (seconds delay) $ runNoLoggingT $ runDb (Identity db) $ flip runReaderT appConfig $ do
  say "Update client cycle."
  now <- getTime
  let maxTime = Just (addUTCTime (- fromIntegral delay) now)
  (queryBestNode >>=) $ traverse_ $ \(nodeId, bestNode, protoInfo) -> do
    let blockHeightTimeout :: NominalDiffTime = fromIntegral $ max 15 $ (5*) $ sum $ take 3 $ toList $ _protoInfo_timeBetweenBlocks protoInfo

    toUpdate :: [(Id Client, ClientAddress)] <- [queryQ|
      SELECT id, address
      FROM "Client" c
      WHERE (c.updated < ?maxTime OR c.updated IS NULL) AND NOT c.deleted
      ORDER BY updated NULLS FIRST
    |]

    clientDelegates <- for toUpdate $ \(cid, address) -> do
      let handlingHttpExc f = (Just <$> f) `catches`
            [ Handler $ \(e :: Http.JSONException) -> sayErr (tshow e) $> Nothing
            , Handler $ \(e :: Http.HttpException) -> sayErr (tshow e) $> Nothing
            ]

      result <- handlingHttpExc $ do
        say $ "Updating client at " <> address

        -- TODO: abstract this into a ClientRPC like the way there's a NodeRPC
        clientConfig :: ClientConfig <- fmap Http.getResponseBody $ Http.httpJSON =<< Http.parseRequest (T.unpack address <> "/config")
        let clientConfigJson = Json clientConfig

        report :: Report <- fmap Http.getResponseBody $ Http.httpJSON =<< Http.parseRequest (T.unpack address <> "/events")
        let reportJson = Json report

        for_ (maximumByMay (compare `on` _event_time) $ _report_seen report) $ \seenEvent ->
          if addUTCTime blockHeightTimeout (_event_time seenEvent) < now then
            reportNoBakerHeartbeatError cid (_event_detail seenEvent)
          else
            clearNoBakerHeartbeatError cid

        let bakingReward blk = _protoInfo_blockReward protoInfo + getSum ((foldMap . foldMap) (Sum . sumFees . unbase16ByteString . _bakedEventOperation_data) (_bakedEvent_operations $ _event_detail blk))
            rewardDelay l =
              let c = fromIntegral l `div` _protoInfo_blocksPerCycle protoInfo + 1
                  rc = c + (let Cycle x = _protoInfo_preservedCycles protoInfo in fromIntegral x)
              in rc * _protoInfo_blocksPerCycle protoInfo
            insertValues = Values ["text", "varchar", "int8", "int8"]
              [ (delegatePkh, toBase58Text (_bakedEvent_hash $ _event_detail b), rewardDelay (blockLevel b) , bakingReward b)
              | b <- _report_baked report
              , delegatePkh <- _clientConfig_delegates clientConfig
              ]
        unless (null $ _report_baked report) $ void $ [executeQ|
          INSERT INTO "PendingReward" (delegate, hash, level, amount)
          SELECT d.id, x.hash, x.level, x.amount
          FROM ?insertValues x (delegate_pkh, hash, level, amount)
          JOIN "Delegate" d ON d."publicKeyHash" = x.delegate_pkh
          ON CONFLICT DO NOTHING |]

        _ <- [executeQ| INSERT INTO "ClientInfo" (client, report, config)
                        VALUES (?cid, ?reportJson, ?clientConfigJson)
                        ON CONFLICT (client) DO UPDATE SET
                          report = ?reportJson
                        , config = ?clientConfigJson
                        |]
        forkInfo <- scanForkInfo httpMgr now report bestNode
        validateForkyBlocks sayShow forkInfo

        updateAndNotify cid [Client_updatedField =. Just now]

      -- TODO: Add back errors reported by client RPC

        -- case sortBy (compare `on` _event_time) (_report_errors report) of
        --   [] -> return ()
        --   es -> do
        --     lastError <- liftIO $ readIORef lastErrorRef
        --     let (new,_) = span ((>= lastError) . Just . _error_time) (mkErr <$> es)
        --     case new of
        --       [] -> return ()
        --       (x:_) -> do
        --         liftIO $ writeIORef lastErrorRef (Just $ _error_time x)
        --         queueAllEmails new
        -- TODO.  debounce below as above
        flip validateForkyBlocks forkInfo $ \errors -> case nonEmpty errors of
          Nothing -> clearNodeOnForkError nodeId
          Just es -> for_ es $ \e -> do
            let tooOld = case _forkInfo_forkStatus e of
                  ForkStatus_TooOld -> True
                  _ -> False
            reportNodeOnForkError nodeId tooOld (_forkInfo_hash e) (_forkInfo_time e)

        return $ _clientConfig_delegates clientConfig

      case result of
        Nothing -> [] <$ reportInaccessibleEndpointError EndpointType_Client address
        Just xs -> xs <$ clearInaccessibleEndpointError EndpointType_Client address

    insertClientDelegates (Set.fromList $ concat clientDelegates)


insertClientDelegates :: (Monad m, PersistBackend m, PostgresRaw m) => Set PublicKeyHash -> m ()
insertClientDelegates pkhs = do
  let inPkhs = Pg.In $ Set.toList pkhs
  (existingIds :: [Id Delegate], existingPkhs :: [PublicKeyHash]) <-
    first (map toId) . unzip <$> project (AutoKeyField, Delegate_publicKeyHashField) CondEmpty

  let newPkhs = pkhs `Set.difference` Set.fromList existingPkhs
  for_ newPkhs $ \pkh -> insertAndNotify $ Delegate pkh False


delegateWorker
  :: MonadIO m
  => Int
  -> Http.Manager
  -> Pool Postgresql
  -> m (IO ())
delegateWorker delay httpMgr db = worker (seconds delay) $ runNoLoggingT $ runDb (Identity db) $ (queryBestNode >>=) $ traverse_ $ \(nid, bestNode, protoInfo) ->
  flip runReaderT (NodeRPCContext httpMgr $ _node_address bestNode) $ do
    say "Update delegate cycle."
    let
      headLevel :: Integer = fromIntegral $ fromMaybe (error "queryBestNode returned unfit node") $ _node_headLevel bestNode
      latestCycle = headLevel `div` fromIntegral (_protoInfo_blocksPerCycle protoInfo)
      levelRange = [max 0 (headLevel - 10) .. headLevel]
      headBlockHash = fromMaybe (error "have block level but not hash!") $ _node_headBlockHash bestNode
    say $ "Head level is " <> tshow headLevel <> " in cycle " <> tshow latestCycle
    delegates :: Map (Id Delegate) Delegate <- selectMap DelegateConstructor (Delegate_deletedField ==. False)
    -- TODO: rights don't change very much, and the node is very slow at computing large ranges of rights.  build up a set of rights slowly and cache them.
    runExceptT (nodeRPC (RBakingRights (blockHashId headBlockHash) (Set.fromList $ Left . RawLevel . fromIntegral <$> levelRange))) >>= \case
      Left e -> sayShow e
      Right allBakingRights -> do
        -- Filter out baking rights that apply to levels in the future.
        -- map is from delegate*level to priorotiy
        let bakingRights :: AppendMap.AppendMap PublicKeyHash (Map RawLevel Priority) =
              AppendMap.filter (not . null) $ Map.filterWithKey (\k _ -> k <= fromIntegral headLevel) <$> bakingRightsMap allBakingRights
        ifor_ delegates $ \dId delegate -> do
          let pkh = _delegate_publicKeyHash delegate
          say $ "Updating delegate " <> toPublicKeyHashText pkh
          accountStatusResp <- runExceptT $ nodeRPC (RContract headId (_delegate_publicKeyHash delegate))
          -- TODO: report errors here
          let accountStatus = either (const Nothing) Just accountStatusResp
          bakingRightsUtilized <- ifor (fromMaybe mempty $ bakingRights ^? ix pkh) $ \levelWithRight delegatePriority -> do
            runExceptT (nodeRPC (RBlock $ blockHashIdPred headBlockHash (fromIntegral headLevel - fromIntegral levelWithRight))) >>= \case
              Left e -> sayShow e $> Nothing
              Right blockWithRights -> return $
                let
                  -- The ID of the baker who baked this block
                  baker = blockWithRights ^. blockInfo_metadata . blockInfoMetadata_baker
                in if baker == pkh then Just True -- Our delegate baked this block so point for us!
                  else case bakingRights ^? ix baker . ix levelWithRight of
                      Nothing -> Nothing -- Can't find this baker in the table of rights
                      Just bakerPriority -> if bakerPriority < delegatePriority
                        then Nothing -- The baker baked with higher priority so this block doesn't count either way.
                        else Just False -- The baker baked with lower priority, so point against us.

          let
            calcBakingEfficiency (numBakedAcc, numOpportunitiesAcc) = \case
              Nothing -> (numBakedAcc, numOpportunitiesAcc)
              Just True -> (numBakedAcc + 1, numOpportunitiesAcc + 1)
              Just False -> (numBakedAcc, numOpportunitiesAcc + 1)

            (numBaked, numOpportunities) = foldl' calcBakingEfficiency (0, 0) bakingRightsUtilized

          delegateStatsId :: Maybe (Id DelegateStats) <- listToMaybe . fmap toId <$> project AutoKeyField ((DelegateStats_delegateField ==. dId) `limitTo` 1)
          case delegateStatsId of
            Nothing -> insertAndNotify_ DelegateStats
              { _delegateStats_delegate = dId
              , _delegateStats_efficiency = BakeEfficiency numBaked numOpportunities
              , _delegateStats_accountBalance = _account_balance <$> accountStatus
              , _delegateStats_accountSpendable = _account_spendable <$> accountStatus
              , _delegateStats_accountSetable = _accountDelegate_setable . _account_delegate <$> accountStatus
              , _delegateStats_accountValue = _accountDelegate_value <$> _account_delegate =<< accountStatus
              , _delegateStats_accountCounter = _account_counter <$> accountStatus
              }
            Just dsId -> updateAndNotify dsId
              [ DelegateStats_efficiencyField =. BakeEfficiency numBaked numOpportunities
              , DelegateStats_accountBalanceField =. (_account_balance <$> accountStatus)
              , DelegateStats_accountSpendableField =. (_account_spendable <$> accountStatus)
              , DelegateStats_accountSetableField =. (_accountDelegate_setable . _account_delegate <$> accountStatus)
              , DelegateStats_accountValueField =. (_accountDelegate_value <$> _account_delegate =<< accountStatus)
              , DelegateStats_accountCounterField =. (_account_counter <$> accountStatus)
              ]

          return ()


timeit :: MonadIO m => Text -> (e -> m a) -> ExceptT e m a -> m a
timeit note errback action = do
  !now <- liftIO getCurrentTime
  !result <- either errback return =<< runExceptT action
  !later <- liftIO getCurrentTime
  sayShow (note, diffUTCTime later now)
  return result


onRpcError :: (MonadError Text m, Show a) => Either a b -> m b
onRpcError = either (throwError . tshow) pure


cacheOneBlock
  ::( MonadLogger m
    , MonadBaseControl IO m
    , MonadIO m
    , MonadError Text m
    )
  => NodeRPCContext
  -> ChainId
  -> Id CachedChainCycle
  -> RawLevel
  -> BlockHash
  -> DbPersist Postgresql m (Id CachedBlock)
cacheOneBlock ctx chain chainCycleId cyclePosition blkHash = do
  details <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RBlock $ blockHashId' chain blkHash) ctx
  let baker = _blockInfoMetadata_baker $ _blockInfo_metadata details
  let endorsers = Json mempty -- TODO: this requres operations parsing
  let blkPred = _blockInfoHeader_predecessor $ _blockInfo_header details
  let cb = CachedBlock chainCycleId baker endorsers cyclePosition blkHash blkPred
  say $ T.concat
    [ "\n\t"
    , toBase58Text $ _cachedBlock_hash cb
    , " -> "
    , toBase58Text $ _cachedBlock_predecessor cb
    , "(\\x"
    , decodeUtf8 $ BS16.encode $ unHashedValue $ _cachedBlock_hash cb
    , ")"
    ]
  toId <$> insert cb

data NodeQuery a where
  NodeQuery_GenesisParameters :: ChainId -> NodeQuery ProtoInfo
  NodeQuery_BakingRights :: ChainId -> BlockHash -> RawLevel -> NodeQuery (Seq BakingRights)
  NodeQuery_Baker :: ChainId -> BlockHash -> RawLevel -> NodeQuery (PublicKeyHash, Priority)

deriveGEq ''NodeQuery
deriveGCompare ''NodeQuery

nodeQueryDataSourceCached
  :: (MonadIO m, MonadReader s m, HasNodeRPC s, MonadError RpcError m)
  => IORef (DMap NodeQuery Identity) -> NodeQuery a -> m a
nodeQueryDataSourceCached cacheRef q = do
  cache <- liftIO $ readIORef cacheRef
  case DMap.lookup q cache of
    Just (Identity a) -> pure a
    Nothing -> do
      res <- nodeQueryDataSource (nodeQueryDataSourceCached cacheRef) q
      liftIO $ atomicModifyIORef' cacheRef $ \oldMap ->
        (DMap.insert q (Identity res) oldMap, ())
      pure res

nodeQueryDataSource
  :: (MonadIO m, MonadReader s m, HasNodeRPC s, MonadError RpcError m)
  => (forall a. NodeQuery a -> m a) -> NodeQuery a -> m a
nodeQueryDataSource self = \case
  NodeQuery_GenesisParameters chainId -> do
    currentHead <- nodeRPC $ RBlock $ headId' chainId
    let headLevel = currentHead ^. blockInfo_header . blockInfoHeader_level
    --TODO: if headLevel == 0 then error "error"
    nodeRPC $ RProtoConstants $ blockHashIdPred' chainId (_blockInfo_hash currentHead) (headLevel - 1)

  NodeQuery_BakingRights chainId branch targetLevel -> do
    proto <- self $ NodeQuery_GenesisParameters chainId
    let
      cycleForLevel n = Cycle $ unRawLevel $ n `div` _protoInfo_blocksPerCycle proto
      cycleDeterminingRightsForLevel n = max 0 $ cycleForLevel n - _protoInfo_preservedCycles proto

      cycleToQuery = Set.singleton $ Right $ cycleDeterminingRightsForLevel targetLevel

    branchBlock <- nodeRPC $ RBlock $ blockHashId' chainId branch
    let
      blockLevel = branchBlock ^. blockInfo_metadata . blockInfoMetadata_level
      cyclesAgo = blockLevel ^. level_cycle - cycleDeterminingRightsForLevel targetLevel
      levelsAgo = RawLevel (unCycle cyclesAgo) * _protoInfo_blocksPerCycle proto - blockLevel ^. level_cyclePosition

    if levelsAgo < 0 then sayErr "NEGALEVEL: " else pure ()
    if levelsAgo == 0 then
      nodeRPC $ RBakingRights (blockHashId' chainId branch) cycleToQuery
    else do
      targetBlock <- nodeRPC $ RBlock $ blockHashIdPred' chainId branch levelsAgo
      self $ NodeQuery_BakingRights chainId
        (targetBlock ^. blockInfo_hash)
        (targetLevel `div` _protoInfo_blocksPerCycle proto * _protoInfo_blocksPerCycle proto)

  NodeQuery_Baker chainId branch rawLevel -> do
    branchBlock <- nodeRPC $ RBlock $ blockHashId' chainId branch
    let levelsAgo = branchBlock ^. blockInfo_header . blockInfoHeader_level - rawLevel
    targetBlock <- nodeRPC $ RBlock $ blockHashIdPred' chainId branch levelsAgo
    pure ( targetBlock ^. blockInfo_metadata . blockInfoMetadata_baker
         , targetBlock ^. blockInfo_header . blockInfoHeader_priority
         )

calculateBakeEfficiency
  :: (MonadIO m, MonadReader s m, HasNodeRPC s, MonadError RpcError m)
  => IORef (DMap NodeQuery Identity) -> ChainId -> BlockHash -> PublicKeyHash -> m BakeEfficiency
calculateBakeEfficiency cache chainId branch delegate = do
  branchBlock <- nodeRPC $ RBlock $ blockHashId' chainId branch
  let branchLevel = branchBlock ^. blockInfo_header . blockInfoHeader_level

  let levels = [branchLevel - 2..branchLevel]
  rights <- fmap (fmap bakingRightsMap) $ for levels $ nodeQueryDataSourceCached cache . NodeQuery_BakingRights chainId branch
  bakers <- for levels $ fmap fst . nodeQueryDataSourceCached cache . NodeQuery_Baker chainId branch

  --let rightsInOrder = fmap (\lvl -> Map.findWithDefault mempty lvl rights) levels
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


doThing
  :: ChainId
  -> Pool Postgresql
  -> Http.Manager
  -> Text
  -> MonitorBlock
  -> IO ()
doThing chain db mgr nodeUrl newBlock = timeit "doThing" say $ runNoLoggingT $ runDb (Identity db) $ do
  let ctx = NodeRPCContext mgr nodeUrl
  let blockHash = _monitorBlock_hash newBlock
  let blockPredecessor = _monitorBlock_predecessor newBlock
  -- if non empty, we're done!
  sayShow (T.pack "NEW BLOCK", nodeUrl, blockHash)
  fmap listToMaybe (select (CachedBlock_hashField ==. blockHash)) >>= \case
    Just _ -> return () -- sayShow (T.pack "have block, DONE", blockHash)
    Nothing -> do
      fmap listToMaybe [queryQ|
          SELECT cb.chain, cb."cyclePosition"
          FROM "CachedBlock" cb
          JOIN "CachedChainCycle" ccc
            ON cb.chain = ccc.id
          JOIN "CachedProtocolConstants" cpc
            ON ccc.constants = cpc.id
          WHERE cb.hash = ?blockPredecessor
            AND cb."cyclePosition" < (cpc."blocksPerCycle" - 1)
        |] >>= \case
        Just (chainCycleId, predCyclePosition) -> do
          -- sayShow (T.pack "Have predecessor in chain", blockPredecessor)
          cacheOneBlock ctx chain chainCycleId (predCyclePosition + 1) blockHash
          return ()
          -- insert_ $ CachedBlock chainCycleId Nothing (Json mempty) (predCyclePosition + 1) (_monitorBlock_hash newBlock) (_monitorBlock_predecessor newBlock)
        -- we're not caught up yet :(
        Nothing -> do
          -- sayShow (T.pack "need chain history", blockHash)
          block <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RBlock $ blockHashId' chain blockHash) ctx
          let protoHash = _blockInfoMetadata_protocol $ _blockInfo_metadata block
          let cycle = _level_cycle $ _blockInfoMetadata_level $ _blockInfo_metadata block
          let cyclePosition = _level_cyclePosition $ _blockInfoMetadata_level $ _blockInfo_metadata block
          -- if we're at position n we need a result of length n + 1 to include the first block of the current cycle.
          cycleInitBlock <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RBlock $ blockHashIdPred' chain blockHash $ fromIntegral cyclePosition) ctx
          let cycleInitHash :: BlockHash = _blockInfo_hash cycleInitBlock
          --sayShow cycleInitBlock
          -- we now have enough information to get our metadata in sync

          (chainCycleId, preservedCycles) :: (Id CachedChainCycle, Cycle) <- listToMaybe <$> [queryQ|
              SELECT ccc.id, cpc."preservedCycles"
              FROM "CachedChainCycle" ccc
              JOIN "CachedProtocolConstants" cpc
                ON ccc.constants = cpc.id
              WHERE hash = ?cycleInitHash
            |] >>= \case
            Nothing -> do
              -- sayShow (T.pack "need chain metadata")
              (protoId, proto) :: (Id CachedProtocolConstants, CachedProtocolConstants) <- listToMaybe <$> [queryQ|
                  SELECT "id", "protocol", "blocksPerCycle", "preservedCycles"
                  FROM "CachedProtocolConstants"
                  WHERE "protocol" = ?protoHash
                |] >>= \case
                Nothing -> do
                  proto <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RProtoConstants $ blockHashId' chain blockHash) ctx
                  let
                    proto' = CachedProtocolConstants
                      { _cachedProtocolConstants_protocol = protoHash
                      , _cachedProtocolConstants_blocksPerCycle = _protoInfo_blocksPerCycle proto
                      , _cachedProtocolConstants_preservedCycles = _protoInfo_preservedCycles proto
                      }
                  protoId' <- insert proto'
                  return (toId protoId', proto')
                Just (protoId', p, bpc, pc) -> return (protoId', CachedProtocolConstants p bpc pc)

              previousCycleHash <- fmap _blockInfo_hash . onRpcError <=< flip runReaderT ctx $ runExceptT $ nodeRPC $
                RBlock (blockHashIdPred' chain cycleInitHash $ fromIntegral $ _cachedProtocolConstants_blocksPerCycle proto)
              -- protocol version data is now in sync
              cid' <- toId <$> insert CachedChainCycle
                { _cachedChainCycle_chainId = chain
                , _cachedChainCycle_constants = protoId
                , _cachedChainCycle_cycle = cycle
                , _cachedChainCycle_hash = cycleInitHash
                , _cachedChainCycle_predecessor = previousCycleHash
                }
              return (cid', _cachedProtocolConstants_preservedCycles proto)
            Just (cid', pc) -> do
              -- sayShow ("What actually happened?")
              return (cid', pc)
          -- chain/cycle is now in sync
          -- which blocks are still missing?
          ancestorMap <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RBlocks (DynamicParamChainId_ChainId chain) (1 + cyclePosition) $ Set.singleton blockHash) ctx
          ancestors <- maybe (throwError $ "bad heads response from node, missing hash:" <> toBase58Text blockHash) return $ Map.lookup blockHash ancestorMap
          -- sayShow ("foundAncestors:", Seq.length ancestors, Seq.take 3 ancestors)
          let inAncestors = In $ toList ancestors
          maxGoodAncestor :: RawLevel <- head . stripOnly <$> [queryQ|
              SELECT COALESCE(MAX("cyclePosition"), -1)
              FROM "CachedBlock"
              WHERE hash in ?inAncestors
            |]
          -- sayShow ("haveAncestors:", maxGoodAncestor)
          -- let needAncestors = Seq.take (Seq.length ancestors - maxGoodAncestor) ancestors
          -- sayShow ("needAncestors:", Seq.length needAncestors, Seq.take 3 needAncestors)

          let blocks = cacheOneBlock ctx chain chainCycleId
                <$> ZipList [cyclePosition,cyclePosition-1..maxGoodAncestor+1]
                <*> ZipList (blockHash : toList ancestors)
          sequence_ blocks
          -- TODO: it makes sense to insertAndNotify if we actually inserted the MonitorBlock we just recieved...
          -- we now have our history... all that's left is rights.
          -- in context $cycleInitBlock, we can find rights for cycles in range [$cycle .. ($cycle - $preservedCycles - 1))]
          -- but really, the cycle rights that are *determined* by $cycle is just $cycle + $preservedCycles
          -- except for cycles [0 .. $preservedCycles], which are all determined by the genesis block and not too important anyway.
          knownRights :: Maybe (Id CachedBlockRights) <- listToMaybe . stripOnly <$> [queryQ|
              SELECT id
              FROM "CachedBlockRights"
              WHERE cycle = ?chainCycleId
              LIMIT 1
              |]
          case knownRights of
            Just _ -> return ()
            Nothing -> do
              -- TODO:  if cycle == 0, request [0..preservedCycles]
              bRights <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RBakingRights (blockHashId' chain blockHash) $ Set.singleton $ Right . Cycle . fromIntegral $ cycle + preservedCycles) ctx
              eRrights <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ REndorsingRights (blockHashId' chain blockHash) $ Set.singleton $ Right . Cycle . fromIntegral $ cycle + preservedCycles) ctx
              let cachedRights = CachedBlockRights chainCycleId (fromIntegral $ cycle + preservedCycles) (Json bRights) (Json eRrights)
              insertAndNotify_ cachedRights

backend :: IO ()
backend = do
  hSetBuffering stderr LineBuffering -- Decrease likelihood of output from multiple threads being interleaved

  let cfg0 = SnapServer.defaultConfig & SnapServer.setOther mempty
  cfg <- SnapServer.extendedCommandLineConfig (SnapServer.optDescrs cfg0 <> optsArgDescr) (<>) cfg0

  emailFromAddress <- Address (Just "Tezos Bake Monitor") . fromMaybe "noreply@obsidian.systems" <$>
    liftA2 (<|>)
      (pure $ _opts_emailFromAddress =<< SnapServer.getOther cfg)
      (getConfigFromFile Just $ configPath Config.emailFromAddress)

  routeEnv :: Maybe RouteEnv <- liftA2 (<|>)
    (pure $
      fromMaybe (error "invalid URL") . uriToRouteEnv <$>
        (_opts_route =<< SnapServer.getOther cfg))
    (getConfigFromFile (Aeson.decodeStrict . encodeUtf8) $ configPath Config.route)

  blockExplorer :: Maybe URI <- liftA2 (<|>)
    (pure $ _opts_blockExplorer =<< SnapServer.getOther cfg)
    (getConfigFromFile (Just . mkRootUriOrError) $ configPath Config.blockExplorer)

  staticHead <- fmap mconcat $ traverse (fmap snd . renderStatic) $ catMaybes
    [ Just $ fst frontend
    , injectPure Config.route . decodeUtf8 . LBS.toStrict . Aeson.encode <$> routeEnv
    , injectPure Config.blockExplorer . tshow <$> blockExplorer
    ]

  let pgConnStr = _opts_pgConnectionString =<< SnapServer.getOther cfg
  withGargoyleOrConnStr (maybe (Left Config.db) Right pgConnStr) $ \db -> do
    runNoLoggingT $ runDb (Identity db) $ do
      tableInfo <- getTableAnalysis
      runMigration $ do
        migrateAccount tableInfo
        migrateQueuedEmail tableInfo
        migrateSchema tableInfo

    finalizers <- newTVarIO (return ())
    let addFinalizer f = atomically $ modifyTVar finalizers (f *>)

    -- Start a thread to send queued emails
    addFinalizer <=< worker (seconds 10) $ runNoLoggingT (clearMailQueueWithDynamicEmailEnv $ Identity db)

    httpMgr <- Http.newManager Https.tlsManagerSettings

    (handleListen, wsFinalizer) <- RhyoliteApp.serveDbOverWebsockets db
      (requestHandler emailFromAddress httpMgr db)
      (notifyHandler db)
      (viewSelectorHandler db)
      (RhyoliteApp.queryMorphismPipeline $ RhyoliteApp.transposeMonoidMap . RhyoliteApp.monoidMapQueryMorphism)
    addFinalizer wsFinalizer

    -- TODO: move this to nodeWorker
    let nodeCtx = NodeRPCContext httpMgr "http://127.0.0.1:18731"
    cache <- newIORef mempty
    runReaderT (runExceptT $ nodeRPC $ RBlock headId) nodeCtx >>= \case
      Right blockInfo -> do
        let chain = _blockInfo_chainId blockInfo
        void $ flip runReaderT nodeCtx $ runExceptT $ nodeRPC $ RMonitorHeads
          (\case
            Left e -> sayErr $ "Bad monitor block: " <> tshow e
            Right monitorBlock -> do
              let pkh = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
              eff <- flip runReaderT nodeCtx $ runExceptT $
                calculateBakeEfficiency cache chain (_monitorBlock_hash monitorBlock) pkh
              sayShow eff
          )
          --(either sayShow $ doThing chain db httpMgr "http://127.0.0.1:18731")
          (DynamicParamChainId_ChainId chain)
      Left bad -> sayShow bad

    let appConfig = AppConfig emailFromAddress
    addFinalizer =<< nodeWorker 30 appConfig httpMgr db
    addFinalizer =<< clientWorker 10 appConfig httpMgr db
    addFinalizer =<< delegateWorker 10 httpMgr db

    SnapServer.httpServe cfg (route
      [ ("", rootHandler staticHead)
      , ("/listen", handleListen)
      , ("static", serveAssets "static" "static")
      , ("", serveDirectory "frontend.jsexe")
      ]) `finally` join (readTVarIO finalizers)

rootHandler :: MonadSnap m => ByteString -> m ()
rootHandler pageHead =
  serveApp "" $ def
    & appConfig_initialHead .~ Just pageHead


clearMailQueueWithDynamicEmailEnv
  :: forall m f.
  ( RunDb f
  , MonadIO m
  , MonadBaseControl IO m
  , MonadLogger m
  )
  => f (Pool Postgresql)
  -> m ()
clearMailQueueWithDynamicEmailEnv db = do
  emailEnv <- runDb db $ do
    getDefaultMailServer <&> \case
      Nothing -> error "No mail server configuration found"
      Just (_, c) ->
        ( T.unpack $ _mailServerConfig_hostName c
        , case _mailServerConfig_smtpProtocol c of
          SmtpProtocol_Plain -> RhyoliteEmail.SMTPProtocol_Plain
          SmtpProtocol_Ssl -> RhyoliteEmail.SMTPProtocol_SSL
          SmtpProtocol_Starttls -> RhyoliteEmail.SMTPProtocol_STARTTLS
        , fromIntegral (_mailServerConfig_portNumber c)
        , T.unpack $ _mailServerConfig_userName c
        , T.unpack $ _mailServerConfig_password c
        )

  clearMailQueue db emailEnv


getConfigFromFile :: (Text -> Maybe a) -> FilePath -> IO (Maybe a)
getConfigFromFile parser f = (parser . T.strip <$> T.readFile f)
  `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e


withGargoyleOrConnStr :: Either FilePath Text -> (Pool Postgresql -> IO a) -> IO a
withGargoyleOrConnStr cfg f = case cfg of
  Left dbPath -> withDb dbPath f
  Right connStr -> f =<< openDb (encodeUtf8 connStr)


uriToRouteEnv :: URI -> Maybe RouteEnv
uriToRouteEnv uri = (,,)
  <$> (uri ^? Uri.uriScheme . _Just . Uri.unRText . to (<> ":") . to T.unpack)
  <*> (uri ^? Uri.uriAuthority . _Right . to renderBeforePort . to T.unpack)
  <*> Just (T.unpack renderPortAndAfter)
  where
    renderBeforePort a = maybe "" ((<> "@") . renderUserInfo) (a ^. Uri.authUserInfo)
      <> (a ^. Uri.authHost . Uri.unRText)
    renderUserInfo u = (u ^. Uri.uiUsername . Uri.unRText) <> maybe "" (":" <>) (u ^? Uri.uiPassword . _Just . Uri.unRText)
    renderPortAndAfter =
      fromMaybe "" (uri ^? Uri.uriAuthority . _Right . Uri.authPort . _Just . to tshow . to (":" <>))
      <>
      (if null $ uri ^. Uri.uriPath then "" else renderPieces $ uri ^. Uri.uriPath)
    renderPieces pieces = "/" <> T.intercalate "/" (map (^. Uri.unRText) pieces)

data Opts = Opts
  { _opts_pgConnectionString :: Maybe Text
  , _opts_route :: Maybe URI
  , _opts_emailFromAddress :: Maybe Text
  , _opts_blockExplorer :: Maybe URI
  }

instance Semigroup Opts where
  a <> b = Opts -- Right biased
    { _opts_pgConnectionString = _opts_pgConnectionString b <|> _opts_pgConnectionString a
    , _opts_route = _opts_route b <|> _opts_route a
    , _opts_emailFromAddress = _opts_emailFromAddress b <|> _opts_emailFromAddress a
    , _opts_blockExplorer = _opts_blockExplorer b <|> _opts_blockExplorer a
    }

instance Monoid Opts where
  mempty = Opts Nothing Nothing Nothing Nothing
  mappend = (<>)

optsArgDescr :: MonadSnap m => [OptDescr (Maybe (SnapServer.Config m Opts))]
optsArgDescr =
  [ Option [] ["pg-connection"] (mkReqArg "CONNSTRING" $ \x -> mempty { _opts_pgConnectionString = Just $ T.pack x }) $
      "Connection string or URI to PostgreSQL database. If blank, use connection string in '" <> Config.db <> "' file or create a database there if empty."
  , Option [] [Config.route] (mkReqArg "URL" $ \x -> mempty { _opts_route = Just $ mkRootUriOrError $ T.pack x }) $
      "Root URL for this service as seen by external users. If blank, use contents of '" <> configPath Config.route <> "'."
  , Option [] [Config.emailFromAddress] (mkReqArg "EMAIL" $ \x -> mempty { _opts_emailFromAddress = Just $ T.pack x }) $
      "Email address to use for 'From' field in email notifications. If blank, use contents of '" <> configPath Config.emailFromAddress <> "'."
  , Option [] [Config.blockExplorer] (mkReqArg "URL" $ \x -> mempty { _opts_blockExplorer = Just $ mkRootUriOrError $ T.pack x }) $
      "URL of the block explorer to use for links. If blank, use contents of '" <> configPath Config.blockExplorer <> "'."
  ]
  where
    mkReqArg var f = ReqArg (\x -> Just $ SnapServer.setOther (f x) mempty) var


configPath :: FilePath -> FilePath
configPath = ("config" </>)

mkRootUriOrError :: Text -> URI
mkRootUriOrError x = either (\e -> error $ T.unpack $ e <> ": " <> x) id $ mkRootUri x
