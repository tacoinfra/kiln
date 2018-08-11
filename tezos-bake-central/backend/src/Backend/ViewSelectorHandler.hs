{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Backend.ViewSelectorHandler where

import Control.Lens (ifor, imap, itraverse, (<&>))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as Map
import qualified Data.AppendMap as AppendMap
import Data.Bifunctor (first, second)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity (..))
import Data.Maybe (isJust, listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup (First (..), Semigroup, (<>))
import qualified Data.Set as Set
import Data.Time (UTCTime)
import Data.Traversable (for)
import Data.Word (Word64)
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple as Pg
import Rhyolite.App (single)
import Rhyolite.Backend.App (QueryHandler (..))
import Rhyolite.Backend.DB (runDb, selectMap')
import Rhyolite.Backend.DB.PsqlSimple (In (..), PostgresRaw, queryQ)
import Rhyolite.Backend.Schema (toId)
import Rhyolite.Schema (Id)
import Say

import Backend.BalanceTracking
import Backend.Graphs
import Backend.Schema
import Common (whenJust)
import Common.App
import Common.AppendIntervalMap (AppendIntervalMap, ClosedInterval (..), WithInfinity (..), getBounded)
import qualified Common.AppendIntervalMap as AppendIMap
import Common.Schema
import Tezos.Account
import Tezos.Json (TezosWord64 (..))
import Tezos.NodeRPC.Types
import Tezos.PublicKeyHash
import Tezos.Tez

import Backend.CachedNodeRPC

viewSelectorHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a, Semigroup a, Show a)
  => NodeDataSource
  -> Pool Postgresql
  -> QueryHandler (BakeViewSelector a) m
viewSelectorHandler nds db = QueryHandler $ \vs -> runNoLoggingT . runDb (Identity db) $ do
  clientAddresses <- whenJust (_bakeViewSelector_clientAddresses vs) $ \a -> do
    rs <- [queryQ| SELECT c.id, c.address FROM "Client" c WHERE NOT c.deleted|]
    return $ Map.fromList [(cid, (First (Just addr), a)) | (cid, addr) <- rs]
  clients <- do
    let selClients = In (Map.keys (_bakeViewSelector_clients vs))
    rs <- [queryQ| SELECT c.id, i.report, i.config
                   FROM "Client" c LEFT JOIN "ClientInfo" i ON c.id = i.client
                   WHERE c.id IN ?selClients AND NOT c.deleted|]
    let clientInfo = Map.fromList $ do
          (cid, report, config) <- rs
          return (cid, First (ClientInfo cid <$> report <*> config))
    return $ Map.intersectionWith (,) clientInfo (_bakeViewSelector_clients vs)
  parameters <- whenJust (_bakeViewSelector_parameters vs) $ \a -> do
    param :: Maybe Parameters <- fmap listToMaybe $ select $ CondEmpty `limitTo` 1
    return $ single (_parameters_protoInfo <$> param) a
  nodeAddresses <- whenJust (_bakeViewSelector_nodeAddresses vs) $ \a -> do
    rs <- [queryQ| SELECT n.id, n.address from "Node" n WHERE NOT n.deleted |]
    return $ Map.fromList [(nid, (First (Just n), a)) | (nid, n) <- rs]
  nodes <- do
    let
      selNodesUniversal = isJust $ _universalMap_universe $ _bakeViewSelector_nodes vs
      selNodes = In $ if selNodesUniversal then mempty else Map.keys $ _universalMap_only $ _bakeViewSelector_nodes vs
    rs <- [queryQ|
      SELECT n.id
        , n.address, n.identity, n."headLevel", n."headBlockHash", n."peerCount"
        , n."networkStat#totalSent" , n."networkStat#totalRecv" , n."networkStat#currentInflow" , n."networkStat#currentOutflow"
        , n."fitness", n."lastHeartbeat" AT TIME ZONE 'UTC'
      FROM "Node" n
      WHERE (?selNodesUniversal OR n.id IN ?selNodes) AND NOT n.deleted|]
    let nodeInfo = Map.fromList $ do
          (nid, addr, ident) Pg.:. (headLevel, headBlockHash) Pg.:. (peerCount, totalSent, totalRecv, currentInflow, currentOutflow, fitness, lastHeartbeat) <- rs
          return (nid, First $ Just Node
            { _node_address = addr
            , _node_identity = ident
            , _node_headLevel = headLevel
            , _node_headBlockHash = headBlockHash
            , _node_peerCount = peerCount
            , _node_networkStat = NetworkStat totalSent totalRecv currentInflow currentOutflow
            , _node_fitness = fitness
            , _node_deleted = False
            , _node_lastHeartbeat = lastHeartbeat
            })
    return (uintersectionWith (,) nodeInfo (_bakeViewSelector_nodes vs))

  delegates <- whenJust (_bakeViewSelector_delegates vs) $ \a -> do
    flip single a . Just . Set.fromList <$> project Delegate_publicKeyHashField (Delegate_deletedField ==. False)

  maybeCurrentHead <- runReaderT dataSourceHead nds

  delegateStats <- whenJust maybeCurrentHead $ \currentHead -> do
    let keys = Map.keys (_bakeViewSelector_delegateStats vs)
    efficiencies <- flip runReaderT nds $ do
      fmap Map.fromList $ for keys $ \delegate -> do
        efficiency <- runExceptT $ withCache mempty $ const $ calculateBakeEfficiency currentHead 50 delegate
        return (delegate, either (const mempty) id efficiency)
    let inKeys = In keys
    rs :: [(PublicKeyHash, Maybe (Id Delegate), Maybe Word64, Maybe Word64, Maybe Tez, Maybe Bool, Maybe Bool, Maybe PublicKeyHash, Maybe TezosWord64)]
      <- [queryQ|
          SELECT d."publicKeyHash"
            ,ds."delegate"
            ,ds."efficiency#bakedBlocks"
            ,ds."efficiency#bakingRights"
            ,ds."accountBalance"
            ,ds."accountSpendable"
            ,ds."accountSetable"
            ,ds."accountValue"
            ,ds."accountCounter"
          FROM "Delegate" d
          LEFT OUTER JOIN "DelegateStats" ds
            ON d."id" = ds."delegate"
          WHERE d."publicKeyHash" IN ?inKeys AND NOT d.deleted|]

    let
      toRsMap
        :: (PublicKeyHash, Maybe (Id Delegate), Maybe Word64, Maybe Word64, Maybe Tez, Maybe Bool, Maybe Bool, Maybe PublicKeyHash, Maybe TezosWord64)
        -> (PublicKeyHash, Maybe (BakeEfficiency, Account))
      toRsMap (publicKeyHash, dId, bakedBlocks, bakingRights, accountBalance, accountSpendable, accountSetable, accountValue, accountCounter) = (publicKeyHash, unDelegateStats publicKeyHash =<< delegateStats)
        where
          -- efficiency = BakeEfficiency <$> bakedBlocks <*> bakingRights
          delegateStats :: Maybe DelegateStats
          delegateStats = DelegateStats
            <$> dId
            <*> Map.lookup publicKeyHash efficiencies
            <*> pure accountBalance
            <*> pure accountSpendable
            <*> pure accountSetable
            <*> pure accountValue
            <*> pure accountCounter
    let rsMap = Map.fromList $ toRsMap <$> rs
    return $ Map.intersectionWith (,) (First <$> rsMap) (_bakeViewSelector_delegateStats vs)

  notificatees <- whenJust (_bakeViewSelector_notificatees vs) $ \a -> do
    rs <- selectMap' NotificateeConstructor CondEmpty
    return $ (\n -> (First (Just (_notificatee_email n)), a)) <$> rs
  mailServer <- whenJust (_bakeViewSelector_mailServer vs) $ \a -> do
    ms <- fmap listToMaybe $ select $ CondEmpty `limitTo` 1
    return $ single (mailServerConfigToView <$> ms) a
  maxLevel <- getMaxLevel
  summaryGraph <- case (_bakeViewSelector_summary vs, maxLevel) of
    (Just a, Just l) -> do
      rewards <- getAllRewards a
      mGraph <- liftIO $ cumulativeRewardsGraph (fromIntegral l) (fmap (getFirst . fst) rewards)
      return $ single mGraph a
    _ -> return mempty
  summary <- case _bakeViewSelector_summary vs of
    Nothing -> return mempty
    Just a -> do
      report <- getSummaryReport
      return $ single report a
  errors <- getErrorLogs $ _bakeViewSelector_errors vs
  return BakeView
    { _bakeView_clients = clients
    , _bakeView_clientAddresses = clientAddresses
    , _bakeView_parameters = parameters
    , _bakeView_nodes = nodes
    , _bakeView_nodeAddresses = nodeAddresses
    , _bakeView_delegateStats = delegateStats
    , _bakeView_notificatees = notificatees
    , _bakeView_mailServer = mailServer
    , _bakeView_summaryGraph = summaryGraph
    , _bakeView_summary = summary
    , _bakeView_graphs = mempty
    , _bakeView_delegates = delegates
    , _bakeView_errors = first AppendMap.keysSet <$> errors
    , _bakeView_errorsById = fold $ fst <$> errors
    }

getErrorLogs
  :: (Monad m, PostgresRaw m, Semigroup a, MonadIO m, Show a)
  => AppendIntervalMap TimeWindow a
  -> m (AppendIntervalMap TimeWindow
      (AppendMap (Id ErrorLog) (First (Maybe (ErrorLog, ErrorLogView))), a))
getErrorLogs intervalMap = do
  let flattenedIntervalMap = AppendIMap.flattenWithClosedInterval (<>) intervalMap
  allLogs :: AppendMap (Id ErrorLog) (ErrorLog, ErrorLogView)
    <- leftBiasedUnions <$> for (AppendIMap.keys flattenedIntervalMap) runQueries

  -- Unflatten the results by finding which interval each log corresponded to.
  pure $ fold $ flip imap allLogs $ \logId (errorLog@(ErrorLog started stopped _ _), view) ->
      let relevantIntervals = intervalMap `AppendIMap.intersecting` ClosedInterval (Bounded started) (maybe UpperInfinity Bounded stopped)
      in relevantIntervals <&> \a ->
          (AppendMap.singleton logId $ First (Just (errorLog, view)), a)

  where
    runQueries (ClosedInterval lowWithInf highWithInf) = do
      let (low, high) = (getBounded lowWithInf, getBounded highWithInf)
      leftBiasedUnions <$> sequenceA
        [ [queryQ|
          SELECT
              el.id
            , el.started AT TIME ZONE 'UTC'
            , el.stopped AT TIME ZONE 'UTC'
            , el."lastSeen" AT TIME ZONE 'UTC'
            , el."noticeSentAt" AT TIME ZONE 'UTC'
            , t.type, t.address
          FROM "ErrorLog" el
          JOIN "ErrorLogInaccessibleEndpoint" t ON t.log = el.id
          LEFT JOIN "Node" n ON n.address = t.address
          LEFT JOIN "Client" c ON c.address = t.address
          WHERE
            NOT n.deleted AND NOT c.deleted AND
            (((?low IS NULL OR el.started >= ?low) AND
             (?high IS NULL OR el.started <= ?high)) OR
             ((?low IS NULL OR el.stopped >= ?low) AND
             (?high IS NULL OR el.stopped <= ?high)))
          ORDER BY el.id ASC
          |] <&> \rows -> AppendMap.fromAscList $ flip map rows $ \(elId, elStarted, elStopped, elLastSeen, elNoticeSentAt, tType, tAddress) ->
            ( elId :: Id ErrorLog
            , ( ErrorLog
                  { _errorLog_started = elStarted
                  , _errorLog_stopped = elStopped
                  , _errorLog_lastSeen = elLastSeen
                  , _errorLog_noticeSentAt = elNoticeSentAt
                  }
              , ErrorLogView_InaccessibleEndpoint $ ErrorLogInaccessibleEndpoint elId tType tAddress
              )
            )

        , [queryQ|
          SELECT
              el.id
            , el.started AT TIME ZONE 'UTC'
            , el.stopped AT TIME ZONE 'UTC'
            , el."lastSeen" AT TIME ZONE 'UTC'
            , el."noticeSentAt" AT TIME ZONE 'UTC'
            , t."lastLevel", t."lastBlockHash", t.client
          FROM "ErrorLog" el
          JOIN "ErrorLogBakerNoHeartbeat" t ON t.log = el.id
          JOIN "Client" c ON c.id = t.client
          WHERE
            NOT c.deleted AND
            (((?low IS NULL OR el.started >= ?low) AND
             (?high IS NULL OR el.started <= ?high)) OR
             ((?low IS NULL OR el.stopped >= ?low) AND
             (?high IS NULL OR el.stopped <= ?high)))
          ORDER BY el.id ASC
          |] <&> \rows -> AppendMap.fromAscList $ flip map rows $ \(elId, elStarted, elStopped, elLastSeen, elNoticeSentAt, tLastLevel, tLastBlockHash, tClient) ->
            ( elId :: Id ErrorLog
            , ( ErrorLog
                  { _errorLog_started = elStarted
                  , _errorLog_stopped = elStopped
                  , _errorLog_lastSeen = elLastSeen
                  , _errorLog_noticeSentAt = elNoticeSentAt
                  }
              , ErrorLogView_BakerNoHeartbeat $
                  ErrorLogBakerNoHeartbeat elId tLastLevel tLastBlockHash tClient
              )
            )

        , [queryQ|
          SELECT
              el.id
            , el.started AT TIME ZONE 'UTC'
            , el.stopped AT TIME ZONE 'UTC'
            , el."lastSeen" AT TIME ZONE 'UTC'
            , el."noticeSentAt" AT TIME ZONE 'UTC'
            , t."node", t."tooOld", t."bakedBlock", t."bakedBlockTime"
          FROM "ErrorLog" el
          JOIN "ErrorLogNodeOnFork" t ON t.log = el.id
          JOIN "Node" n ON n.id = t.node
          WHERE
            NOT n.deleted AND
            (((?low IS NULL OR el.started >= ?low) AND
             (?high IS NULL OR el.started <= ?high)) OR
             ((?low IS NULL OR el.stopped >= ?low) AND
             (?high IS NULL OR el.stopped <= ?high)))
          ORDER BY el.id ASC
          |] <&> \rows -> AppendMap.fromAscList $ flip map rows $ \(elId, elStarted, elStopped, elLastSeen, elNoticeSentAt, tNode, tTooOld, tBakedBlock, tBakedBlockTime) ->
            ( elId :: Id ErrorLog
            , ( ErrorLog
                  { _errorLog_started = elStarted
                  , _errorLog_stopped = elStopped
                  , _errorLog_lastSeen = elLastSeen
                  , _errorLog_noticeSentAt = elNoticeSentAt
                  }
              , ErrorLogView_NodeOnFork $
                  ErrorLogNodeOnFork elId tNode tTooOld tBakedBlock tBakedBlockTime
              )
            )

        , [queryQ|
          SELECT
              el.id
            , el.started AT TIME ZONE 'UTC'
            , el.stopped AT TIME ZONE 'UTC'
            , el."lastSeen" AT TIME ZONE 'UTC'
            , el."noticeSentAt" AT TIME ZONE 'UTC'
            , t."publicKeyHash", t.client, t.worker
          FROM "ErrorLog" el
          JOIN "ErrorLogMultipleBakersForSameDelegate" t ON t.log = el.id
          JOIN "Delegate" d ON d."publicKeyHash" = t."publicKeyHash"
          WHERE
            NOT d.deleted AND
            (((?low IS NULL OR el.started >= ?low) AND
             (?high IS NULL OR el.started <= ?high)) OR
             ((?low IS NULL OR el.stopped >= ?low) AND
             (?high IS NULL OR el.stopped <= ?high)))
          ORDER BY el.id ASC
          |] <&> \rows -> AppendMap.fromAscList $ flip map rows $ \(elId, elStarted, elStopped, elLastSeen, elNoticeSentAt, tPublicKeyHash, tClient, tWorker) ->
            ( elId :: Id ErrorLog
            , ( ErrorLog
                  { _errorLog_started = elStarted
                  , _errorLog_stopped = elStopped
                  , _errorLog_lastSeen = elLastSeen
                  , _errorLog_noticeSentAt = elNoticeSentAt
                  }
              , ErrorLogView_MultipleBakersForSameDelegate $
                  ErrorLogMultipleBakersForSameDelegate elId tPublicKeyHash tClient tWorker
              )
            )
        ]

    leftBiasedUnions = AppendMap.unionsWith const
