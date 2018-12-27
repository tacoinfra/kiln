{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults #-}

module Backend.ViewSelectorHandler where

import Control.Concurrent.STM (atomically)
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Align (alignWith)
import Data.Bifunctor (bimap, first)
import Data.Functor.Identity (Identity (..))
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap(..))
import qualified Data.Map.Monoidal as MMap
import Data.Pool (Pool)
import Data.Semigroup (Max(..))
import Data.Time (UTCTime)
import Data.These (these)
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple as Pg
import Rhyolite.Backend.App (QueryHandler (..))
import Rhyolite.Backend.DB (runDb, selectMap', selectSingle)
import Rhyolite.Backend.DB.PsqlSimple (In (..), PostgresRaw, query, queryQ)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Schema (Id)
import Safe (maximumMay)
import Text.URI (URI)

import Tezos.NodeRPC.Types
import Tezos.PublicKeyHash
import Tezos.Types

import Backend.BalanceTracking
import Backend.CachedNodeRPC
import Backend.Schema
import Backend.STM (atomicallyWith)
import Common.Alerts(AlertsFilter(..))
import Common.App
import Common.AppendIntervalMap (AppendIntervalMap, ClosedInterval (..), WithInfinity (..))
import qualified Common.AppendIntervalMap as AppendIMap
import Common.Config (FrontendConfig)
import Common.Schema
import Common.Vassal

import ExtraPrelude

viewSelectorHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a)
  => FrontendConfig
  -> Maybe NamedChain
  -> NodeDataSource
  -> Pool Postgresql
  -> QueryHandler (BakeViewSelector a) m
viewSelectorHandler frontendConfig namedChain nds db = QueryHandler $ \vs -> runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $ do
  let
    maybeViewHandler
      :: Applicative m'
      => (BakeViewSelector a -> MaybeSelector v a)
      -> m' (Maybe v)
      -> m' (View (MaybeSelector v) a)
    maybeViewHandler getVS xs = whenM (not $ null $ getVS vs) $
      toMaybeView (getVS vs) <$> xs

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
    fmap _parameters_protoInfo <$> selectSingle CondEmpty

  let nodeAddrVS = _bakeViewSelector_nodeAddresses vs
  nodeAddresses <- whenM (not $ null nodeAddrVS) $ do
    -- TODO: nodeAddrVS is a RangeView.  select individual nodes upon request.
    toRangeView nodeAddrVS <$> getNodeAddresses Nothing

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
        , n.address, n.alias, n.identity, n."headLevel", n."headBlockHash", n."headBlockPred", n."headBlockBakedAt" AT TIME ZONE 'UTC'
        , n."peerCount", n."networkStat#totalSent" , n."networkStat#totalRecv" , n."networkStat#currentInflow", n."networkStat#currentOutflow"
        , n."fitness", n."updated" AT TIME ZONE 'UTC'
      FROM "Node" n
      WHERE (?selNodesUniversal OR n.id IN ?selNodes) AND NOT n.deleted|]
    return $ toRangeView nodesVS $ rs <&>
      \((nid, addr, alias, ident) Pg.:. (headLevel, headBlockHash, headBlockPred, headBlockBakedAt) Pg.:. (peerCount, totalSent, totalRecv, currentInflow, currentOutflow, blockFitness, updated)) ->
        (Bounded nid, First $ Just Node
          { _node_address = addr
          , _node_alias = alias
          , _node_identity = ident
          , _node_headLevel = headLevel
          , _node_headBlockHash = headBlockHash
          , _node_headBlockPred = headBlockPred
          , _node_headBlockBakedAt = headBlockBakedAt
          , _node_peerCount = peerCount
          , _node_networkStat = NetworkStat totalSent totalRecv currentInflow currentOutflow
          , _node_fitness = blockFitness
          , _node_deleted = False
          , _node_updated = updated
          })

  let bakerAddrVS = _bakeViewSelector_bakerAddresses vs
  bakerAddresses :: RangeView' PublicKeyHash (Deletable BakerSummary) a <- whenM (not $ null bakerAddrVS) $ do
    -- TODO: bakerAddrVS is a RangeView.  select individual bakers upon request.
    toRangeView bakerAddrVS <$> getBakerAddresses nds Nothing

  let bakerDetailsVS = _bakeViewSelector_bakerDetails vs
  bakerDetails :: RangeView' PublicKeyHash (Deletable BakerDetails) a <- whenM (not $ null bakerDetailsVS) $
    toRangeView bakerDetailsVS . fmap (\x -> (Bounded $ _bakerDetails_publicKeyHash x, First $ Just x)) <$> select CondEmpty

  -- maybeCurrentHead <- runReaderT dataSourceHead nds

  -- bakerStats :: AppendMap(PublicKeyHash, RawLevel) (First(Maybe(BakeEfficiency,Account)),a) <- whenJust maybeCurrentHead $ \currentHead -> do
  let bakerStats -- :: ComposeView (RangeSelector PublicKeyHash Account) (IntervalSelector RawLevel BakeEfficiency) a
       = mempty
  --   <- whenJust maybeCurrentHead $ \currentHead -> do
  --   forRWT nds $ withCache mempty $ \_protoInfo -> do
  --     flip itraverse (_bakeViewSelector_bakerStats vs) $ \(i, j) -> _
  --     -- calculateBakerStats (_bakeViewSelector_bakerStats vs)


  mailServer <- maybeViewHandler _bakeViewSelector_mailServer $ do
    rs <- fmap _notificatee_email . toList <$> selectMap' NotificateeConstructor CondEmpty
    fmap (Just . fmap (flip mailServerConfigToView rs)) $ selectSingle CondEmpty

  summaryView <- maybeViewHandler _bakeViewSelector_summary getSummaryReport

  let errorsVS = _bakeViewSelector_errors vs
  errors <- itraverse getErrorLogs errorsVS

  upgrade <- maybeViewHandler _bakeViewSelector_upstreamVersion $ selectSingle CondEmpty

  let
    tcVS = _bakeViewSelector_telegramConfig vs
    trVS = _bakeViewSelector_telegramRecipients vs
  (telegramConfig, telegramRecipients) <-
    whenM (not (null tcVS) || not (null trVS)) $ do
      cfgs <- selectMap' TelegramConfigConstructor $ CondEmpty `limitTo` 1

      telegramConfig <- maybeViewHandler _bakeViewSelector_telegramConfig $
        pure $ Just $ listToMaybe $ MMap.elems cfgs

      telegramRecipients <- whenM (not $ null trVS) $ do
        recipients <- for (listToMaybe $ MMap.keys cfgs) $ \cid ->
          selectMap' TelegramRecipientConstructor $ TelegramRecipient_configField ==. cid
        pure $ toRangeView trVS $ map (Bounded *** First . Just) $ maybe [] MMap.toList recipients

      pure (telegramConfig, telegramRecipients)

  alertCount <- maybeViewHandler _bakeViewSelector_alertCount getAlertCount
  config <- maybeViewHandler _bakeViewSelector_config $ pure $ Just frontendConfig
  latestHead <- maybeViewHandler _bakeViewSelector_latestHead $ liftIO $ atomically $ dataSourceHead nds

  return BakeView
    { _bakeView_config = config
    , _bakeView_clients = mempty -- clients
    , _bakeView_clientAddresses = clientAddresses
    , _bakeView_parameters = parameters
    , _bakeView_publicNodeConfig = publicNodeConfig
    , _bakeView_publicNodeHeads = publicNodeHeads
    , _bakeView_nodes = nodes
    , _bakeView_nodeAddresses = nodeAddresses
    , _bakeView_bakerAddresses = bakerAddresses
    , _bakeView_bakerStats = bakerStats
    , _bakeView_mailServer = mailServer
    -- , _bakeView_summaryGraph = summaryGraph
    , _bakeView_summary = summaryView
    -- , _bakeView_graphs = mempty
    , _bakeView_bakerDetails = bakerDetails
    , _bakeView_errors = errors
    , _bakeView_latestHead = latestHead
    , _bakeView_upstreamVersion = upgrade
    , _bakeView_telegramConfig = telegramConfig
    , _bakeView_telegramRecipients = telegramRecipients
    , _bakeView_alertCount = alertCount
    }

getErrorLogs
  :: forall m a e.
  ( MonadLogger m
  , PostgresRaw m
  , Semigroup a
  )
  => AlertsFilter
  ->          IntervalSelector' UTCTime (Id ErrorLog) e a
  -> m (View (IntervalSelector' UTCTime (Id ErrorLog) (Deletable ErrorInfo)) a)
getErrorLogs flt (IntervalSelector vs0) = fmap (IntervalView vs0 . (fmap.fmap.first) (First . Just)) $ getErrorLogsImpl flt vs0

getErrorLogsImpl
  :: forall m a.
  ( MonadLogger m
  , PostgresRaw m
  , Semigroup a
  )
  => AlertsFilter
  -> AppendIntervalMap (ClosedInterval (WithInfinity UTCTime)) a
  -> m (MonoidalMap (Id ErrorLog) (First (ErrorInfo, ClosedInterval (WithInfinity UTCTime))))
getErrorLogsImpl flt intervalMap = do
  let flattenedIntervalMap = AppendIMap.flattenWithClosedInterval (<>) intervalMap
  $(logDebugSH) ("getErrorLogs", void flattenedIntervalMap)

  fmap getErrorInterval . leftBiasedUnions <$> for (AppendIMap.keys flattenedIntervalMap) runQueries
  where
    queryNodeAlert, queryBakerAlert, queryClientDaemonAlert
      :: Pg.FromRow row
      => Pg.Query
      -> [Pg.Query]
      -> (Id ErrorLog -> row -> b)
      -> ClosedInterval (WithInfinity UTCTime)
      -> m (MonoidalMap (Id ErrorLog) (ErrorLog, b))
    queryNodeAlert sqlTable sqlFields =
      queryAlert sqlTable sqlFields (Just ("Node", "id", "node"))
    queryClientDaemonAlert sqlTable sqlFields =
      queryAlert sqlTable sqlFields (Just ("Client", "id", "client"))
    queryBakerAlert sqlTable sqlFields =
      queryAlert sqlTable sqlFields (Just ("Baker", "publicKeyHash", "publicKeyHash"))

    queryAlert
      :: (Monad f, PostgresRaw f, Pg.FromRow row)
      => Pg.Query
      -> [Pg.Query]
      -> Maybe (Pg.Query, Pg.Query, Pg.Query)
      -> (Id ErrorLog -> row -> b)
      -> ClosedInterval (WithInfinity UTCTime)
      -> f (MonoidalMap (Id ErrorLog) (ErrorLog, b))
    queryAlert sqlTable sqlFields related ctor (ClosedInterval lowWithInf highWithInf) = do
      let
        build = \rows -> MMap.fromAscList $ flip map rows $ \((elId, elStarted, elStopped, elLastSeen, elNoticeSentAt) Pg.:. t) ->
          ( elId :: Id ErrorLog
          , ( ErrorLog
                { _errorLog_started = elStarted
                , _errorLog_stopped = elStopped
                , _errorLog_lastSeen = elLastSeen
                , _errorLog_noticeSentAt = elNoticeSentAt
                }
            , ctor elId t
            )
          )
        qBase =
          "SELECT \
          \    el.id \
          \   , el.started AT TIME ZONE 'UTC' \
          \   , el.stopped AT TIME ZONE 'UTC' \
          \   , el.\"lastSeen\" AT TIME ZONE 'UTC' \
          \   , el.\"noticeSentAt\" AT TIME ZONE 'UTC' \
          \   " <> foldMap (\fld -> ", t.\"" <> fld <> "\"") sqlFields <> " \
          \ FROM \"ErrorLog\" el \
          \ JOIN \"" <> sqlTable <> "\" t ON t.log = el.id "
          <> maybe "" (\(relatedTbl, relatedColumn, tColumn) ->
                        " JOIN \"" <> relatedTbl
                        <> "\" n ON n.\"" <> relatedColumn <> "\" = t.\"" <> tColumn <> "\"") related
          <> " WHERE "
          <> bool " TRUE " "   NOT n.deleted" (isJust related)
          <> qFlt
        qFlt = case flt of
          AlertsFilter_All -> ""
          AlertsFilter_ResolvedOnly -> " AND el.stopped IS NOT NULL"
          AlertsFilter_UnresolvedOnly -> " AND el.stopped IS NULL"
      build <$> query (
        qBase <>
          " AND tsrange(el.started, el.\"lastSeen\", '[]') && tsrange(?, ?, '[]') \
          \ ORDER BY el.id ASC") -- this ORDER BY abides the 'MMap.fromAscList' above.
        (lowWithInf, highWithInf)

    runQueries :: ClosedInterval (WithInfinity UTCTime) -> m (MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView))
    runQueries window = do
      leftBiasedUnions <$> sequenceA
        [ queryNodeAlert "ErrorLogInaccessibleNode" ["node", "address", "alias"]
            (\elId (tNode, tAddress, tAlias) -> ErrorLogView_NodeError $ NodeErrorLogView_InaccessibleNode $ ErrorLogInaccessibleNode elId tNode tAddress tAlias)
            window

        , queryNodeAlert "ErrorLogNodeWrongChain" ["node", "address", "alias", "expectedChainId", "actualChainId"]
            (\elId (tNode, tAddress, tAlias, tExpectedChainId, tActualChainId) ->
                ErrorLogView_NodeError $ NodeErrorLogView_NodeWrongChain $ ErrorLogNodeWrongChain elId tNode tAddress tAlias tExpectedChainId tActualChainId)
            window
        , queryClientDaemonAlert "ErrorLogBakerNoHeartbeat" ["lastLevel", "lastBlockHash", "client"]
          (\elId (tLastLevel, tLastBlockHash, tClient) -> ErrorLogView_BakerNoHeartbeat $ ErrorLogBakerNoHeartbeat elId tLastLevel tLastBlockHash tClient)
            window

        , queryNodeAlert "ErrorLogBadNodeHead" ["node", "lca", "nodeHead", "latestHead"]
          (\elId (tNode, tLca, tNodeHead, tLatestHead) -> ErrorLogView_NodeError $ NodeErrorLogView_BadNodeHead
                  ErrorLogBadNodeHead
                    { _errorLogBadNodeHead_log = elId
                    , _errorLogBadNodeHead_node = tNode
                    , _errorLogBadNodeHead_lca = tLca
                    , _errorLogBadNodeHead_nodeHead =tNodeHead
                    , _errorLogBadNodeHead_latestHead = tLatestHead
                    }) window
        , queryBakerAlert "ErrorLogMultipleBakersForSameBaker" ["publicKeyHash", "client", "worker"]
          (\elId (tPublicKeyHash, tClient, tWorker) -> ErrorLogView_BakerError $ BakerErrorLogView_MultipleBakersForSameBaker $
                  ErrorLogMultipleBakersForSameBaker elId tPublicKeyHash tClient tWorker)
          window
        ]

    leftBiasedUnions = MMap.unionsWith const

getAlertCount
  :: (Monad m, PostgresRaw m)
  => m (Maybe Int)
getAlertCount =
  fmap Pg.fromOnly . listToMaybe <$> [queryQ|
    SELECT
      COUNT(*)
    FROM "ErrorLog" el
    WHERE el.stopped IS NULL|]

getBakerAddresses
  :: forall m. (PostgresRaw m, MonadIO m)
  => NodeDataSource
  -> Maybe (PublicKeyHash)
  -> m [(WithInfinity PublicKeyHash, First (Maybe BakerSummary))]
getBakerAddresses nds bid = do
  rs :: Map.Map PublicKeyHash (Maybe Text, Int) <- [queryQ|
      SELECT b."publicKeyHash", b.alias,
        ( SELECT COUNT(e.id)
          FROM "ErrorLog" e
          JOIN "ErrorLogBakerMissed" elbm
            ON elbm.log = e.id
          WHERE e.stopped IS NULL
            AND elbm."baker#baker#publicKeyHash" = b."publicKeyHash"
        )
      FROM "Baker" b
      WHERE NOT b.deleted
        AND CASE WHEN ?bid is NULL THEN true ELSE b."publicKeyHash" = ?bid END
    |] <&> Map.fromList . fmap (\(pkh, alias, alertCount) -> (pkh, (alias, alertCount)))
  -- grab the hashes of the cycle starts, if they exist
  rightsInfoAndFriends :: (Maybe RawLevel, Maybe RawLevel, [RightsCycleInfo]) <- flip runReaderT nds $ atomicallyWith $
    withCache nds (Nothing, Nothing, []) $ \protoInfo -> do
      nds' <- ask
      headM <- dataSourceHead nds'
      rightsInfo <- fromMaybe [] . join <$> traverse (cycleStartHashes . view hash) headM

      return (view level <$> headM, Just (firstLevelInCycle protoInfo (_protoInfo_preservedCycles protoInfo + 1) - 1), rightsInfo)

  let
    (headLevelM, rightsLookAheadM, rightsInfo) = rightsInfoAndFriends
    rightsHashes :: Pg.In [BlockHash] = Pg.In $ _rightsCycleInfo_branch <$> rightsInfo
    bakerHashes :: Pg.In [PublicKeyHash] = Pg.In $ Map.keys rs
    chainId = _nodeDataSource_chain nds
    maxProgress :: Maybe RawLevel = (+) <$> rightsLookAheadM <*> maximumMay (_rightsCycleInfo_maxLevel <$> rightsInfo)

  nextBakeRightsL <- case headLevelM of
    Nothing -> pure []
    Just headLevel -> [queryQ|
      SELECT brcp."publicKeyHash",
        ( SELECT MAX(progress) -- this is a subselect so that we get the highest result even if "BakerRight" rows are found
          FROM "BakerRightsCycleProgress" b1
          WHERE b1."publicKeyHash" = brcp."publicKeyHash"
            AND b1."chainId" = ?chainId
            AND b1."branch" in ?rightsHashes
        ), br."right", MIN(br.level)
      FROM "BakerRightsCycleProgress" brcp
      LEFT OUTER JOIN "BakerRight" br
        ON br.branch = brcp.id
        AND br.level > ?headLevel
      WHERE brcp."chainId" = ?chainId
        AND brcp.branch in ?rightsHashes
        AND brcp."publicKeyHash" in ?bakerHashes
      GROUP BY brcp."publicKeyHash", br."right"
      |]

  let
    nextBakeRights :: MonoidalMap PublicKeyHash (Max RawLevel, Map.Map RightKind RawLevel)
    nextBakeRights = foldMap (\(pkh, progress, rightKind, rightLvl) -> MMap.singleton pkh (Max progress, fromMaybe mempty $ Map.singleton <$> rightKind <*> rightLvl)) $ nextBakeRightsL
    result =  fmap (bimap Bounded (First . Just)) $ Map.toList $ Map.mapMaybe id $ alignWith
      (these
        (\(alias, alertCount) -> Just $ BakerSummary alias alertCount Map.empty 1) -- TODO: we can do better to estimate this value, but for now the only thing we display is "yes/no" are we fetching more data.
        (const Nothing)
        (\(alias, alertCount) (Max progress, rights) -> Just $ BakerSummary alias alertCount rights (maybe 0 (subtract progress) maxProgress)) -- if maxProgress is Nothing, then we don't yet have enough history to say much of anything about how much work we still need to do per baker
      ) rs (getMonoidalMap nextBakeRights)

  -- $(logDebug) ("ViewSelectorHandler::getBakerAddresses " <> T.decodeUtf8 (LBS.toStrict $ Aeson.encode result))
  return result

getNodeAddresses
  :: forall m. (Monad m, PostgresRaw m)
  => Maybe (Id Node)
  -> m [(WithInfinity (Id Node), First (Maybe NodeSummary))]
getNodeAddresses nid = do
  rs :: [(Id Node, URI, Maybe Text, Int)] <- [queryQ|
      SELECT n.id, n.address, n.alias,
        (SELECT COUNT(ein.id)
         FROM "ErrorLogInaccessibleNode" ein
         JOIN "ErrorLog" e
          ON e.id = ein.log
         WHERE e.stopped IS NULL
           AND ein.node = n.id)
        + (SELECT COUNT(ein.id)
         FROM "ErrorLogBadNodeHead" ein
         JOIN "ErrorLog" e
          ON e.id = ein.log
         WHERE e.stopped IS NULL
           AND ein.node = n.id)
        + (SELECT COUNT(ein.id)
         FROM "ErrorLogNodeWrongChain" ein
         JOIN "ErrorLog" e
          ON e.id = ein.log
         WHERE e.stopped IS NULL
           AND ein.node = n.id)
      FROM "Node" n
      WHERE NOT n.deleted
        AND CASE WHEN ?nid is NULL THEN true ELSE n.id = ?nid END|]
  return $ fmap (first Bounded . \(x,y,z,w) -> (x,First (Just (NodeSummary y z w)))) rs
