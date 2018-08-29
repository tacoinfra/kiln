{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Backend.ViewSelectorHandler where

import Control.Lens (ifor, imap, itraverse, (<&>), (^.))
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
import qualified Data.Monoid
import Data.Pool (Pool)
import Data.Semigroup (Option(..), First (..), Semigroup, (<>))
import qualified Data.Set as Set
import Data.Time (UTCTime)
import Data.Traversable (for)
import Data.Version (Version)
import Data.Word (Word64)
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple as Pg
-- import Rhyolite.App (single)
import Rhyolite.Backend.App (QueryHandler (..))
import Rhyolite.Backend.DB (runDb, selectMap')
import Rhyolite.Backend.DB.PsqlSimple (In (..), PostgresRaw, queryQ)
import Rhyolite.Backend.Schema (toId)
import Rhyolite.Schema (Id, Email)
import Say
import qualified Data.IntervalMap.Generic.Lazy as IMap

import Tezos.Account
import Tezos.Json (TezosWord64 (..))
import Tezos.NodeRPC.Types
import Tezos.PublicKeyHash
import Tezos.Tez
import Tezos.Types
import Reflex.FunctorMaybe

import Backend.BalanceTracking
import Backend.CachedNodeRPC
import Backend.Graphs
import Backend.Schema
import Common (whenJust)
import Common.App
import Common.AppendIntervalMap (AppendIntervalMap, ClosedInterval (..), WithInfinity (..), getBounded)
import qualified Common.AppendIntervalMap as AppendIMap
import Common.Schema
import Common.Vassal


viewSelectorHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a, Semigroup a, Show a)
  => NodeDataSource
  -> Pool Postgresql
  -> QueryHandler (BakeViewSelector a) m
viewSelectorHandler nds db = QueryHandler $ \vs -> (<* say "************ viewSelectorHandler END") . (say "************ viewSelectorHandler START" *>) . runNoLoggingT . runDb (Identity db) $ do
  let clientAddresses = mempty
  -- clientAddresses <- whenJust (_bakeViewSelector_clientAddresses vs) $ \a -> do
  --   rs <- [queryQ| SELECT c.id, c.address FROM "Client" c WHERE NOT c.deleted|]
  --   return $ Map.fromList [(cid, (First (Just addr), a)) | (cid, addr) <- rs]
  -- clients <- do
  --   let selClients = In (Map.keys (_bakeViewSelector_clients vs))
  --   rs <- [queryQ| SELECT c.id, i.report, i.config
  --                  FROM "Client" c LEFT JOIN "ClientInfo" i ON c.id = i.client
  --                  WHERE c.id IN ?selClients AND NOT c.deleted|]
  --   let clientInfo = Map.fromList $ do
  --         (cid, report, config) <- rs
  --         return (cid, First (ClientInfo cid <$> report <*> config))
  --   return $ Map.intersectionWith (,) clientInfo (_bakeViewSelector_clients vs)
  parameters <- whenJust (getOption $ unMaybeSelector $ _bakeViewSelector_parameters vs) $ \a -> do
    param :: Maybe Parameters <- fmap listToMaybe $ select $ CondEmpty `limitTo` 1
    return $ MaybeView $ Option $ ((,a) . First . _parameters_protoInfo) <$> param -- MaybeView (Option $ First $ _parameters_protoInfo <$> param, a)
  nodeAddresses <- -- whenJust (_ $ unRangeSelector $ _bakeViewSelector_nodeAddresses vs) $ \a -> do
    let as = _bakeViewSelector_nodeAddresses vs
    in if null as
      then return mempty
      else do
        rs :: [(Id Node, ClientAddress)] <- [queryQ| SELECT n.id, n.address from "Node" n WHERE NOT n.deleted |]
        return $ toRangeView as $ fmap (first Bounded) rs -- Map.fromList [(nid, (First (Just n), _a)) | (nid, n) <- rs]
  tzscan <- whenJust (getOption $ unMaybeSelector $ _bakeViewSelector_tzscan vs) $ \a ->
    toMaybeView (_bakeViewSelector_tzscan vs) . listToMaybe <$> select ((TzScan_chainIdField ==. _nodeDataSource_chain nds) `limitTo` 1)
  nodes <- do
    let
      selNodesUniversal = isCompleteSelector $ _bakeViewSelector_nodes vs
      selNodes = In $ iMapSelectorKeys $ _bakeViewSelector_nodes vs
    rs <- [queryQ|
      SELECT n.id
        , n.address, n.identity, n."headLevel", n."headBlockHash", n."peerCount"
        , n."networkStat#totalSent" , n."networkStat#totalRecv" , n."networkStat#currentInflow" , n."networkStat#currentOutflow"
        , n."fitness", n."lastHeartbeat" AT TIME ZONE 'UTC'
      FROM "Node" n
      WHERE (?selNodesUniversal OR n.id IN ?selNodes) AND NOT n.deleted|]
    let nodeInfo = Map.fromList $ do
          (nid, addr, ident) Pg.:. (headLevel, headBlockHash) Pg.:. (peerCount, totalSent, totalRecv, currentInflow, currentOutflow, fitness, lastHeartbeat) <- rs
          return (Bounded nid, Node
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
    return $ tightenView $ RangeView (unRangeSelector $ _bakeViewSelector_nodes $ vs) nodeInfo
      -- (_ uintersectionWith nodeInfo (if selNodesUniversal then fromKnownComplete else fromKnownAbsent $ _bakeViewSelector_nodes vs))

  delegates :: RangeView' PublicKeyHash () a  <- if not $ null $ _bakeViewSelector_delegates vs
    then do
      xs <- project Delegate_publicKeyHashField (Delegate_deletedField ==. False)
      return $ tightenView $ RangeView (unRangeSelector $ _bakeViewSelector_delegates vs) $ AppendMap.fromList $ fmap ((,()) . Bounded) xs
    else pure mempty

  maybeCurrentHead <- runReaderT dataSourceHead nds

  -- delegateStats :: AppendMap(PublicKeyHash, RawLevel) (First(Maybe(BakeEfficiency,Account)),a) <- whenJust maybeCurrentHead $ \currentHead -> do
  delegateStats -- :: ComposeView (RangeSelector PublicKeyHash Account) (IntervalSelector RawLevel BakeEfficiency) a
    <- pure mempty
  --   <- whenJust maybeCurrentHead $ \currentHead -> do
  --   forRWT nds $ withCache mempty $ \_protoInfo -> do
  --     flip itraverse (_bakeViewSelector_delegateStats vs) $ \(i, j) -> _
  --     -- calculateDelegateStats (_bakeViewSelector_delegateStats vs)

  notificatees :: RangeView' (Id Notificatee) Email a <-
    if not $ null $ _bakeViewSelector_notificatees vs
      then do
        rs <- selectMap' NotificateeConstructor CondEmpty
        return $ tightenView $ RangeView (unRangeSelector $ _bakeViewSelector_notificatees vs) $ fmap _notificatee_email $ AppendMap.mapKeys Bounded rs
        -- $ (\n -> (First (Just (_notificatee_email n)), a)) <$> rs
      else pure mempty
  mailServer <- whenJust (getOption $ unMaybeSelector $_bakeViewSelector_mailServer vs) $ \a -> do
    ms <- fmap listToMaybe $ select $ CondEmpty `limitTo` 1
    let ms' = Just $ mailServerConfigToView <$> ms
    return $ toMaybeView (_bakeViewSelector_mailServer vs) ms'
  maxLevel <- getMaxLevel
  -- summaryGraph <- case (_bakeViewSelector_summary vs, maxLevel) of
  --   (Just a, Just l) -> do
  --     rewards <- getAllRewards a
  --     mGraph <- liftIO $ cumulativeRewardsGraph (fromIntegral l) (fmap (getFirst . fst) rewards)
  --     return $ single mGraph a
  --   _ -> return mempty
  summary <- whenJust (getOption $ unMaybeSelector $ _bakeViewSelector_summary vs) $ \_ -> do
    toMaybeView (_bakeViewSelector_summary vs) <$> getSummaryReport

  let errorsVS = _bakeViewSelector_errors vs
  errors <- getErrorLogs $ unIntervalSelector $ errorsVS
  upgrade <- whenJust (getOption $ unMaybeSelector $ _bakeViewSelector_upgrade vs) $ \a -> do -- case _bakeViewSelector_upgrade vs of
    toMaybeView (_bakeViewSelector_upgrade vs) <$> getUpgradeNotice
    -- Nothing -> return mempty
    -- Just a -> flip single a <$> getUpgradeNotice
  return BakeView
    { _bakeView_clients = mempty -- clients
    , _bakeView_clientAddresses = clientAddresses
    , _bakeView_parameters = parameters
    , _bakeView_tzscan = tzscan
    , _bakeView_nodes = nodes
    , _bakeView_nodeAddresses = nodeAddresses
    , _bakeView_delegateStats = delegateStats
    , _bakeView_notificatees = notificatees
    , _bakeView_mailServer = mailServer
    -- , _bakeView_summaryGraph = summaryGraph
    , _bakeView_summary = summary
    -- , _bakeView_graphs = mempty
    , _bakeView_delegates = delegates
    , _bakeView_errors = IntervalView (unIntervalSelector errorsVS) errors
    -- , _bakeView_errorsById = fold $ fst <$> errors
    , _bakeView_upgrade = upgrade
    }

getErrorLogs
  :: (Monad m, PostgresRaw m, Semigroup a, MonadIO m)
  => AppendIntervalMap (ClosedInterval (WithInfinity UTCTime)) a
  -> m (AppendMap (Id ErrorLog) (First (ErrorInfo, ClosedInterval (WithInfinity UTCTime))))
      -- (AppendIntervalMap (ClosedInterval (WithInfinity UTCTime))
      -- (AppendMap (Id ErrorLog) (First (Maybe (ErrorLog, ErrorLogView))), a))

getErrorLogs intervalMap = do
  let flattenedIntervalMap = AppendIMap.flattenWithClosedInterval (<>) intervalMap
  -- allLogs :: AppendMap (Id ErrorLog) (ErrorLog, ErrorLogView) <- 
  fmap getErrorInterval . leftBiasedUnions <$> for (AppendIMap.keys flattenedIntervalMap) runQueries
  -- -- Unflatten the results by finding which interval each log corresponded to.
  -- pure $ fold $ flip imap allLogs $ \logId (errorLog@(ErrorLog started stopped _ _), view) ->
  --     let relevantIntervals = intervalMap `AppendIMap.intersecting` ClosedInterval (Bounded started) (maybe UpperInfinity Bounded stopped)
  --     in relevantIntervals <&> \a ->
  --         (AppendMap.singleton logId $ First (Just (errorLog, view)), a)

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

getUpgradeNotice
  :: (Monad m, PostgresRaw m, MonadIO m)
  => m (Maybe (ErrorLog, Either UpgradeCheckError Version))
getUpgradeNotice = do
  row <- listToMaybe <$> [queryQ|
      SELECT
          el.started AT TIME ZONE 'UTC'
        , el.stopped AT TIME ZONE 'UTC'
        , el."lastSeen" AT TIME ZONE 'UTC'
        , el."noticeSentAt" AT TIME ZONE 'UTC'
        , t.error, t."newVersion"
      FROM "ErrorLog" el
      JOIN "ErrorLogUpgradeNotice" t ON t.log = el.id
      WHERE el.stopped IS NULL
      ORDER BY el.started DESC
      LIMIT 1|]
  pure $ row <&> \(elStarted, elStopped, elLastSeen, elNoticeSentAt, tError, tNewVersion) ->
    (ErrorLog
      { _errorLog_started = elStarted
      , _errorLog_stopped = elStopped
      , _errorLog_lastSeen = elLastSeen
      , _errorLog_noticeSentAt = elNoticeSentAt
      }
    , case tError of
        Just e -> Left e
        Nothing -> case tNewVersion of
          Just tNewVersion -> Right tNewVersion
          Nothing -> error "Bad upgrade notice record"
    )
