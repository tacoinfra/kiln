{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Backend.ViewSelectorHandler where

import Control.Arrow ((***))
import Control.Lens ((<&>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity (..))
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Maybe (listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup (First (..), Semigroup, (<>))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Traversable (for)
import Data.Version (Version)
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple as Pg
import Rhyolite.Backend.App (QueryHandler (..))
import Rhyolite.Backend.DB (runDb, selectMap')
import Rhyolite.Backend.DB.PsqlSimple (In (..), PostgresRaw, queryQ)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Schema (Id)
import Text.URI (URI)

import Tezos.NodeRPC.Types
import Tezos.PublicKeyHash
import Tezos.Types

import Backend.BalanceTracking
import Backend.CachedNodeRPC
-- import Backend.Graphs
import Backend.Schema
import Common
import Common.App
import Common.AppendIntervalMap (AppendIntervalMap, ClosedInterval (..), WithInfinity (..), getBounded)
import qualified Common.AppendIntervalMap as AppendIMap
import Common.Schema
import Common.Vassal


viewSelectorHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a)
  => Maybe NamedChain
  -> NodeDataSource
  -> Pool Postgresql
  -> QueryHandler (BakeViewSelector a) m
viewSelectorHandler namedChain nds db = QueryHandler $ \vs -> runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $ do
  let
    maybeViewHandler getVS query = whenM (not $ null $ getVS vs) $
      toMaybeView (getVS vs) <$> query

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
  parameters <- maybeViewHandler _bakeViewSelector_parameters $
    fmap _parameters_protoInfo . listToMaybe <$> select (CondEmpty `limitTo` 1)

  let nodeAddrVS = _bakeViewSelector_nodeAddresses vs
  nodeAddresses <- whenM (not $ null nodeAddrVS) $ do
    rs :: [(Id Node, URI, Maybe Text)] <- [queryQ| SELECT n.id, n.address, n.alias from "Node" n WHERE NOT n.deleted |]
    return $ toRangeView nodeAddrVS $ fmap (first Bounded . \(x,y,z) -> (x,First (Just (NodeSummary y z 0)))) rs

  let pncVS = _bakeViewSelector_publicNodeConfig vs
  publicNodeConfig <- whenM (not $ null pncVS) $ do
    xs :: [PublicNodeConfig] <- select CondEmpty
    pure $ toRangeView pncVS [(_publicNodeConfig_source x, x) | x <- xs]

  let pnhVS = _bakeViewSelector_publicNodeHeads vs
  publicNodeHeads <- whenM (not $ null pnhVS) $
    toRangeView pnhVS . fmap (first Bounded) . MMap.toList <$> selectMap' PublicNodeHeadConstructor
      (PublicNodeHead_chainField ==. (NamedChainOrChainId $ maybe (Right $ _nodeDataSource_chain nds) Left namedChain)
      )

  let nodesVS = _bakeViewSelector_nodes vs
  nodes <- whenM (not $ null nodesVS) $ do
    let
      selNodesUniversal = isCompleteSelector nodesVS
      selNodes = In $ iMapSelectorKeys nodesVS
    rs <- [queryQ|
      SELECT n.id
        , n.address, n.alias, n.identity, n."headLevel", n."headBlockHash", n."headBlockBakedAt" AT TIME ZONE 'UTC'
        , n."peerCount", n."networkStat#totalSent" , n."networkStat#totalRecv" , n."networkStat#currentInflow", n."networkStat#currentOutflow"
        , n."fitness", n."updated" AT TIME ZONE 'UTC'
      FROM "Node" n
      WHERE (?selNodesUniversal OR n.id IN ?selNodes) AND NOT n.deleted|]
    return $ toRangeView nodesVS $ rs <&>
      \((nid, addr, alias, ident) Pg.:. (headLevel, headBlockHash, headBlockBakedAt) Pg.:. (peerCount, totalSent, totalRecv, currentInflow, currentOutflow, blockFitness, updated)) ->
        (Bounded nid, First $ Just Node
          { _node_address = addr
          , _node_alias = alias
          , _node_identity = ident
          , _node_headLevel = headLevel
          , _node_headBlockHash = headBlockHash
          , _node_headBlockBakedAt = headBlockBakedAt
          , _node_peerCount = peerCount
          , _node_networkStat = NetworkStat totalSent totalRecv currentInflow currentOutflow
          , _node_fitness = blockFitness
          , _node_deleted = False
          , _node_updated = updated
          })

  let delegatesVS = _bakeViewSelector_delegates vs
  delegates :: RangeView' PublicKeyHash (Deletable ()) a <- whenM (not $ null delegatesVS) $ do
    xs <- project Delegate_publicKeyHashField (Delegate_deletedField ==. False)
    return $ toRangeView delegatesVS $ (, First $ Just ()) . Bounded <$> xs

  maybeCurrentHead <- runReaderT dataSourceHead nds

  -- delegateStats :: AppendMap(PublicKeyHash, RawLevel) (First(Maybe(BakeEfficiency,Account)),a) <- whenJust maybeCurrentHead $ \currentHead -> do
  let delegateStats -- :: ComposeView (RangeSelector PublicKeyHash Account) (IntervalSelector RawLevel BakeEfficiency) a
       = mempty
  --   <- whenJust maybeCurrentHead $ \currentHead -> do
  --   forRWT nds $ withCache mempty $ \_protoInfo -> do
  --     flip itraverse (_bakeViewSelector_delegateStats vs) $ \(i, j) -> _
  --     -- calculateDelegateStats (_bakeViewSelector_delegateStats vs)

  let notificateesVS = _bakeViewSelector_notificatees vs
  notificatees <- whenM (not $ null notificateesVS) $ do
    rs <- selectMap' NotificateeConstructor CondEmpty
    return $ tightenView $ toRangeView notificateesVS $ MMap.toList $ First . Just . _notificatee_email <$> MMap.mapKeys Bounded rs

  mailServer <- maybeViewHandler _bakeViewSelector_mailServer $
    fmap (fmap (Just . mailServerConfigToView) . listToMaybe) $ select $ CondEmpty `limitTo` 1

  summaryView <- maybeViewHandler _bakeViewSelector_summary getSummaryReport

  let errorsVS = _bakeViewSelector_errors vs
  errors <- getErrorLogs $ unIntervalSelector errorsVS

  upgrade <- maybeViewHandler _bakeViewSelector_upgrade getUpgradeNotice

  let
    tcVS = _bakeViewSelector_telegramConfig vs
    trVS = _bakeViewSelector_telegramRecipients vs
  (telegramConfig, telegramRecipients) <-
    whenM (not (null tcVS) || not (null trVS)) $ do
      cfgs <- selectMap' TelegramConfigConstructor $ CondEmpty `limitTo` 1

      telegramConfig <- maybeViewHandler _bakeViewSelector_telegramConfig $
        pure $ listToMaybe $ MMap.elems cfgs

      telegramRecipients <- whenM (not $ null trVS) $ do
        recipients <- for (listToMaybe $ MMap.keys cfgs) $ \cid ->
          selectMap' TelegramRecipientConstructor $ TelegramRecipient_configField ==. cid
        pure $ toRangeView trVS $ map (Bounded *** First . Just) $ maybe [] MMap.toList recipients

      pure (telegramConfig, telegramRecipients)

  return BakeView
    { _bakeView_clients = mempty -- clients
    , _bakeView_clientAddresses = clientAddresses
    , _bakeView_parameters = parameters
    , _bakeView_publicNodeConfig = publicNodeConfig
    , _bakeView_publicNodeHeads = publicNodeHeads
    , _bakeView_nodes = nodes
    , _bakeView_nodeAddresses = nodeAddresses
    , _bakeView_delegateStats = delegateStats
    , _bakeView_notificatees = notificatees
    , _bakeView_mailServer = mailServer
    -- , _bakeView_summaryGraph = summaryGraph
    , _bakeView_summary = summaryView
    -- , _bakeView_graphs = mempty
    , _bakeView_delegates = delegates
    , _bakeView_errors = IntervalView (unIntervalSelector errorsVS) errors
    , _bakeView_upgrade = upgrade
    , _bakeView_telegramConfig = telegramConfig
    , _bakeView_telegramRecipients = telegramRecipients
    }


getErrorLogs
  :: (Monad m, PostgresRaw m, Semigroup a)
  => AppendIntervalMap (ClosedInterval (WithInfinity UTCTime)) a
  -> m (MonoidalMap (Id ErrorLog) (First (ErrorInfo, ClosedInterval (WithInfinity UTCTime))))
getErrorLogs intervalMap = do
  let flattenedIntervalMap = AppendIMap.flattenWithClosedInterval (<>) intervalMap
  fmap getErrorInterval . leftBiasedUnions <$> for (AppendIMap.keys flattenedIntervalMap) runQueries
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
            , t.node, t.address, t.alias
          FROM "ErrorLog" el
          JOIN "ErrorLogInaccessibleNode" t ON t.log = el.id
          JOIN "Node" n ON n.id = t.node
          WHERE
            NOT n.deleted AND
            (((?low IS NULL OR el.started >= ?low) AND
             (?high IS NULL OR el.started <= ?high)) OR
             ((?low IS NULL OR el.stopped >= ?low) AND
             (?high IS NULL OR el.stopped <= ?high)))
          ORDER BY el.id ASC
          |] <&> \rows -> MMap.fromAscList $ flip map rows $ \(elId, elStarted, elStopped, elLastSeen, elNoticeSentAt, tNode, tAddress, tAlias) ->
            ( elId :: Id ErrorLog
            , ( ErrorLog
                  { _errorLog_started = elStarted
                  , _errorLog_stopped = elStopped
                  , _errorLog_lastSeen = elLastSeen
                  , _errorLog_noticeSentAt = elNoticeSentAt
                  }
              , ErrorLogView_InaccessibleNode $ ErrorLogInaccessibleNode elId tNode tAddress tAlias
              )
            )

        , [queryQ|
            SELECT
                el.id
              , el.started AT TIME ZONE 'UTC'
              , el.stopped AT TIME ZONE 'UTC'
              , el."lastSeen" AT TIME ZONE 'UTC'
              , el."noticeSentAt" AT TIME ZONE 'UTC'
              , t.node, t.address, t.alias, t."expectedChainId", t."actualChainId"
            FROM "ErrorLog" el
            JOIN "ErrorLogNodeWrongChain" t ON t.log = el.id
            JOIN "Node" n ON n.id = t.node
            WHERE
              NOT n.deleted AND
              (((?low IS NULL OR el.started >= ?low) AND
               (?high IS NULL OR el.started <= ?high)) OR
               ((?low IS NULL OR el.stopped >= ?low) AND
               (?high IS NULL OR el.stopped <= ?high)))
            ORDER BY el.id ASC
            |] <&> \rows -> MMap.fromAscList $ flip map rows $ \(elId, elStarted, elStopped, elLastSeen, elNoticeSentAt, tNode, tAddress, tAlias, tExpectedChainId, tActualChainId) ->
              ( elId :: Id ErrorLog
              , ( ErrorLog
                    { _errorLog_started = elStarted
                    , _errorLog_stopped = elStopped
                    , _errorLog_lastSeen = elLastSeen
                    , _errorLog_noticeSentAt = elNoticeSentAt
                    }
                , ErrorLogView_NodeWrongChain $ ErrorLogNodeWrongChain elId tNode tAddress tAlias tExpectedChainId tActualChainId
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
          |] <&> \rows -> MMap.fromAscList $ flip map rows $ \(elId, elStarted, elStopped, elLastSeen, elNoticeSentAt, tLastLevel, tLastBlockHash, tClient) ->
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
            , t.node, t.lca, t."nodeHead", t."latestHead"
          FROM "ErrorLog" el
          JOIN "ErrorLogBadNodeHead" t ON t.log = el.id
          JOIN "Node" n ON n.id = t.node
          WHERE
            NOT n.deleted AND
            (((?low IS NULL OR el.started >= ?low) AND
             (?high IS NULL OR el.started <= ?high)) OR
             ((?low IS NULL OR el.stopped >= ?low) AND
             (?high IS NULL OR el.stopped <= ?high)))
          ORDER BY el.id ASC
          |] <&> \rows -> MMap.fromAscList $ flip map rows $ \
              (elId, elStarted, elStopped, elLastSeen, elNoticeSentAt, tNode, tLca, tNodeHead, tLatestHead) ->
            ( elId :: Id ErrorLog
            , ( ErrorLog
                  { _errorLog_started = elStarted
                  , _errorLog_stopped = elStopped
                  , _errorLog_lastSeen = elLastSeen
                  , _errorLog_noticeSentAt = elNoticeSentAt
                  }
              , ErrorLogView_BadNodeHead
                  ErrorLogBadNodeHead
                    { _errorLogBadNodeHead_log = elId
                    , _errorLogBadNodeHead_node = tNode
                    , _errorLogBadNodeHead_lca = tLca
                    , _errorLogBadNodeHead_nodeHead =tNodeHead
                    , _errorLogBadNodeHead_latestHead = tLatestHead
                    }

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
          |] <&> \rows -> MMap.fromAscList $ flip map rows $ \(elId, elStarted, elStopped, elLastSeen, elNoticeSentAt, tPublicKeyHash, tClient, tWorker) ->
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

    leftBiasedUnions = MMap.unionsWith const

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
        Nothing -> maybe (error "Bad upgrade notice record") Right tNewVersion
    )
