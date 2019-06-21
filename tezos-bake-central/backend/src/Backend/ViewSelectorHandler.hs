{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-redundant-constraints #-}

module Backend.ViewSelectorHandler where

import Control.Concurrent.STM (atomically)
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger
import Control.Exception.Safe (MonadMask)
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.Trans.State (modify)
import Data.Align (alignWith)
import Data.Bifunctor (bimap, first)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Apply (liftF2)
import Data.Dependent.Sum (DSum (..))
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap(..))
import qualified Data.Map.Monoidal as MMap
import Data.Pool (Pool)
import Data.Semigroup (Max(..))
import Data.Some (Some(..))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Data.These (these)
import Data.Tuple (swap)
import Data.Universe (universe)
import Database.Groundhog.Core (ConstructorMarker)
import Database.Groundhog.Core (EntityConstr)
import Database.Groundhog.Core (FieldChain)
import Database.Groundhog.Core (PersistEntity)
import Database.Groundhog.Core (PersistValue)
import Database.Groundhog.Core (Utf8)
import Database.Groundhog.Core (constrParams)
import Database.Groundhog.Core (constructors)
import Database.Groundhog.Core (entityConstrNum)
import Database.Groundhog.Core (entityDef)
import Database.Groundhog.Core (fieldChain)
import Database.Groundhog.Core (fromEntityPersistValues)
import Database.Groundhog.Core (fromPersistValues)
import Database.Groundhog.Core (fromUtf8)
import Database.Groundhog.Core (toPrimitivePersistValue)
import Database.Groundhog.Generic (mapAllRows)
import Database.Groundhog.Generic.Sql (RenderConfig(..))
import Database.Groundhog.Generic.Sql (flatten)
import Database.Groundhog.Generic.Sql (renderChain)
import Database.Groundhog.Generic.Sql (tableName)
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple as Pg
import Rhyolite.Backend.App (QueryHandler (..))
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (runDb, selectMap', selectSingle)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, queryQ)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Backend.Schema.Class (singleConstructor)
import Rhyolite.Schema (Id(..))
import Safe (maximumMay)
import Safe (minimumByMay)

import Tezos.PublicKeyHash
import Tezos.Types

import Backend.BalanceTracking
import Backend.CachedNodeRPC
import Backend.IndexQueries (RightsCycleInfo(..), cycleStartHashes, lastLevelInCycle)
import Backend.Schema
import Common.Alerts(AlertsFilter(..))
import Common.App
import Common.AppendIntervalMap (AppendIntervalMap, ClosedInterval (..), WithInfinity (..))
import qualified Common.AppendIntervalMap as AppendIMap
import Common.Config (FrontendConfig)
import Common.Schema
import Common.Vassal

import ExtraPrelude

viewSelectorHandler
  :: forall m a. (MonadBaseNoPureAborts IO m, MonadIO m, Monoid a, MonadMask m)
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
  --   rs <- [queryQ| SELECT c.id, c.address FROM "Client" c WHERE NOT c.data#deleted|]
  --   return $ Map.fromList [(cid, (First (Just addr), a)) | (cid, addr) <- rs]
  -- clients <- do
  --   let selClients = In (Map.keys (_bakeViewSelector_clients vs))
  --   rs <- [queryQ| SELECT c.id, i.report, i.config
  --                  FROM "Client" c LEFT JOIN "ClientInfo" i ON c.id = i.client
  --                  WHERE c.id IN ?selClients AND NOT c.data#deleted|]
  --   let clientInfo = Map.fromList $ do
  --         (cid, report, config) <- rs
  --         return (cid, First (ClientInfo cid <$> report <*> config))
  --   return $ Map.intersectionWith (,) clientInfo (_bakeViewSelector_clients vs)
  let paramsVS = _bakeViewSelector_parameters vs
  parameters <- whenM (not $ null paramsVS) $ do
    let selectedProtocols :: [ProtocolHash] =
          map (\(ClosedInterval l r) -> if l == r then r else error "Protocols don't actually support range")
          <$> AppendIMap.keys $ unRangeSelector paramsVS
    protocols :: [ProtocolIndex] <- select
      ( ProtocolIndex_hashField `in_` selectedProtocols &&.
        ProtocolIndex_chainIdField ==. _nodeDataSource_chain nds)
    pure $ toRangeView paramsVS [(_protocolIndex_hash x, x) | x <- protocols]

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

  -- TODO Dan Bornside says this could be more efficient.
  let nodeDetailsVS = _bakeViewSelector_nodeDetails vs
  nodeDetails :: RangeView' (Id Node) NodeDetailsData a <- whenM (not $ null nodeDetailsVS) $
    toRangeView nodeDetailsVS . fmap (\x -> (Bounded $ _nodeDetails_id x, _nodeDetails_data x)) <$> select CondEmpty

  let bakerAddrVS = _bakeViewSelector_bakerAddresses vs
  bakerAddresses :: RangeView' PublicKeyHash (Deletable BakerSummary) a <- whenM (not $ null bakerAddrVS) $ do
    -- TODO: bakerAddrVS is a RangeView.  select individual bakers upon request.
    toRangeView bakerAddrVS <$> getBakerAddresses nds Nothing

  -- TODO Dan Bornside says this could be more efficient.
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

  let amendmentVS = _bakeViewSelector_amendment vs
  amendment <- whenM (not $ null amendmentVS) $ do
    as <- select $ Amendment_chainIdField ==. _nodeDataSource_chain nds
    pure $ toRangeView amendmentVS $ flip fmap as $ \a -> (_amendment_period a, First $ Just a)

  periodProposals <- maybeViewHandler _bakeViewSelector_proposals $ Just <$>
    select (PeriodProposal_chainIdField ==. _nodeDataSource_chain nds)
  periodTestingVote <- maybeViewHandler _bakeViewSelector_periodTestingVote $ Just <$>
    selectSingle (PeriodTestingVote_periodVoteField ~> PeriodVote_chainIdSelector ==. _nodeDataSource_chain nds)
  periodTesting <- maybeViewHandler _bakeViewSelector_periodTesting $ Just <$>
    selectSingle (PeriodTesting_chainIdField ==. _nodeDataSource_chain nds)
  periodPromotionVote <- maybeViewHandler _bakeViewSelector_periodPromotionVote $ Just <$>
    selectSingle (PeriodPromotionVote_periodVoteField ~> PeriodVote_chainIdSelector ==. _nodeDataSource_chain nds)

  connectedLedger <- maybeViewHandler _bakeViewSelector_connectedLedger $ Just <$> selectSingle CondEmpty

  let showLedgerVS = _bakeViewSelector_showLedger vs
  showLedger <- whenM (not $ null showLedgerVS) $ do
    las <- select CondEmpty -- Expect very few records here, so just select them all
    let rangeView = toRangeView showLedgerVS $ flip fmap las $ \la ->
          ( _ledgerAccount_secretKey la
          , First $ (,) <$> _ledgerAccount_publicKeyHash la <*> _ledgerAccount_balance la
          )
    pure rangeView

  let promptingVS = _bakeViewSelector_prompting vs
  prompting <- whenM (not $ null promptingVS) $ do
    las <- select CondEmpty
    let rangeView = toRangeView promptingVS $ flip fmap las $ \la ->
          ( _ledgerAccount_secretKey la
          , First $ Just $ mempty
            { _setupState_import = if _ledgerAccount_imported la then Just (First ImportSecretKeyStep_Done) else Nothing
            }
          )
    pure rangeView

  let rnsVS = _bakeViewSelector_rightNotificationSettings vs
  rightNotificationSettings <- whenM (not $ null rnsVS) $ do
    rnss <- select CondEmpty
    let rangeView = toRangeView rnsVS $ flip fmap rnss $ \rns ->
          ( _rightNotificationSettings_rightKind rns
          , First $ Just $ _rightNotificationSettings_limit rns
          )
    pure rangeView

  return BakeView
    { _bakeView_config = config
    , _bakeView_clients = mempty -- clients
    , _bakeView_clientAddresses = clientAddresses
    , _bakeView_parameters = parameters
    , _bakeView_publicNodeConfig = publicNodeConfig
    , _bakeView_publicNodeHeads = publicNodeHeads
    , _bakeView_nodeAddresses = nodeAddresses
    , _bakeView_nodeDetails = nodeDetails
    , _bakeView_bakerAddresses = bakerAddresses
    , _bakeView_bakerStats = bakerStats
    , _bakeView_mailServer = mailServer
    -- , _bakeView_summaryGraph = summaryGraph
    , _bakeView_summary = summaryView
    -- , _bakeView_graphs = mempty
    , _bakeView_bakerDetails = bakerDetails
    , _bakeView_errors = errors
    , _bakeView_latestHead = latestHead
    , _bakeView_amendment = amendment
    , _bakeView_proposals = periodProposals
    , _bakeView_periodTestingVote = periodTestingVote
    , _bakeView_periodTesting = periodTesting
    , _bakeView_periodPromotionVote = periodPromotionVote
    , _bakeView_upstreamVersion = upgrade
    , _bakeView_telegramConfig = telegramConfig
    , _bakeView_telegramRecipients = telegramRecipients
    , _bakeView_alertCount = alertCount
    , _bakeView_connectedLedger = connectedLedger
    , _bakeView_showLedger = showLedger
    , _bakeView_prompting = prompting
    , _bakeView_rightNotificationSettings = rightNotificationSettings
    }

getErrorLogs
  :: forall m a e.
  ( MonadLogger m
  , PersistBackend m
  , Semigroup a
  )
  => AlertsFilter
  ->          IntervalSelector' UTCTime (Id ErrorLog) e a
  -> m (View (IntervalSelector' UTCTime (Id ErrorLog) (Deletable ErrorInfo)) a)
getErrorLogs flt (IntervalSelector vs0) = fmap (IntervalView vs0 . (fmap.fmap.first) (First . Just)) $ getErrorLogsImpl flt vs0

pg :: Proxy Postgresql
pg = Proxy @Postgresql

proxify :: proxy x -> Proxy x
proxify = const Proxy

phantomize :: f x -> x
phantomize = error "tried to touch a phantom"

renderQualifiedField :: Utf8 -> FieldChain -> [Utf8]
renderQualifiedField q fld = renderChain (RenderConfig $ \x -> q <> ".\"" <> x <> "\"") fld []

traceQuery :: (MonadLogger f, PersistBackend f) => Utf8 -> ([PersistValue] -> [PersistValue]) -> ([PersistValue] -> f r) -> f [r]
traceQuery sql params f = do
  $(logDebugS) "SQL" (tshow sql)
  $(logDebugS) "SQL" (tshow $ params [])
  queryRaw False (T.unpack $ decodeUtf8 $ fromUtf8 sql) (params []) $ mapAllRows f

getErrorLogsImpl
  :: forall m a.
  ( MonadLogger m
  , PersistBackend m
  , Semigroup a
  )
  => AlertsFilter
  -> AppendIntervalMap (ClosedInterval (WithInfinity UTCTime)) a
  -> m (MonoidalMap (Id ErrorLog) (First (ErrorInfo, ClosedInterval (WithInfinity UTCTime))))
getErrorLogsImpl flt intervalMap = do
  let flattenedIntervalMap = AppendIMap.flattenWithClosedInterval (<>) intervalMap
  $(logDebugSH) ("getErrorLogs" :: Text, void flattenedIntervalMap)

  fmap getErrorInterval . leftBiasedUnions <$> for (AppendIMap.keys flattenedIntervalMap) runQueries
  where
    --queryClientDaemonAlert sqlTable sqlFields =
    --  queryAlert sqlTable sqlFields (Just ("Client", "id", "client"))
    -- TODO: make every bakeralert work with the Id Baker column, probably

    {-# INLINE queryAlert #-}
    queryAlert
      :: forall f c b. (Monad f, PersistBackend f, MonadLogger f, PersistEntity b, EntityConstr b c)
      => c (ConstructorMarker b)
      -> [Some (Related b c)]
      -> ClosedInterval (WithInfinity UTCTime)
      -> f (MonoidalMap (Id ErrorLog) (ErrorLog, b))
    queryAlert ctor related window = do
      let
        build :: [PersistValue] -> f (Id ErrorLog, (ErrorLog, b))
        build = evalStateT $ do
          elId :: Id ErrorLog <- StateT fromPersistValues
          modify (toPrimitivePersistValue pg (0 :: Int):)
          eLog :: ErrorLog <- StateT fromEntityPersistValues
          modify (toPrimitivePersistValue pg constrNum:)
          extras :: b <- StateT fromEntityPersistValues
          pure (elId, (eLog, extras))
        entityD = entityDef pg (undefined :: b)
        constrNum = entityConstrNum (Proxy @b) ctor
        constrD = constructors entityD !! constrNum
        sqlTable = tableName id entityD constrD
        sqlFields = foldr (flatten id) [] $ constrParams constrD
        qCond :: [Utf8]
        qCond = flip map related $ \case
          This r@(Related fld fk) ->
            let ctor2 = singleConstructor $ proxify r
                entityD2 = entityDef pg $ phantomize $ Compose ctor2
                constrNum2 = entityConstrNum (Compose ctor2) ctor2
                constrD2 = constructors entityD2 !! constrNum2
                relatedTbl = tableName id entityD2 constrD2
                tColumns = renderQualifiedField "t" $ fieldChain pg fld
                relatedColumns = renderQualifiedField "n" $ case fk of
                  ForeignKey_AutoId -> fieldChain pg $ (const AutoKeyField :: d (ConstructorMarker r) -> AutoKeyField r d) ctor2
                  ForeignKey_UniqueId -> fieldChain pg $ (undefined :: DefaultKey r ~ Key r (Unique u) => d (ConstructorMarker r) -> u (UniqueMarker r)) ctor2
                  ForeignKey_UniqueIdData -> fieldChain pg $ (undefined :: DefaultKey r ~ Key r (Unique u) => d (ConstructorMarker r) -> u (UniqueMarker r)) ctor2
                  ForeignKey_Field fld2 -> fieldChain pg fld2
            in
              "EXISTS (SELECT 1 FROM \"" <> relatedTbl
              <> "\" n WHERE " <> mconcat (intersperse " AND " $ "NOT n.\"data#deleted\"" : zipWith (\x y -> x <> " = " <> y) relatedColumns tColumns) <> ")"
        qBase :: Utf8
        qBase =
          "SELECT \
          \     el.id \
          \   , el.started AT TIME ZONE 'UTC' \
          \   , el.stopped AT TIME ZONE 'UTC' \
          \   , el.\"lastSeen\" AT TIME ZONE 'UTC' \
          \   , el.\"noticeSentAt\" AT TIME ZONE 'UTC' \
          \   " <> foldMap (\fld -> ", t.\"" <> fld <> "\"") sqlFields <> " \
          \ FROM \"ErrorLog\" el \
          \ JOIN \"" <> sqlTable <> "\" t ON t.log = el.id \
          \ WHERE ("
          <> bool (mconcat $ intersperse " OR " qCond) "TRUE" (null related)
          <> " AND COALESCE(el.started != el.stopped, true)"
          <> ")"
          <> qFlt
        qFlt = case flt of
          AlertsFilter_All -> ""
          AlertsFilter_ResolvedOnly -> " AND el.stopped IS NOT NULL"
          AlertsFilter_UnresolvedOnly -> " AND el.stopped IS NULL"
        (qWindow, qWindowArgs) = case window of
          ClosedInterval lowerEnd upperEnd -> let
            qEndpoint = \case
              LowerInfinity -> ("'-infinity'", id)
              Bounded pt -> ("?", (toPrimitivePersistValue pg pt:))
              UpperInfinity -> ("'infinity'", id)
            (lowerQ, lowerArgs) = qEndpoint lowerEnd
            (upperQ, upperArgs) = qEndpoint upperEnd
            in ("tsrange(" <> lowerQ <> ", " <> upperQ <> ", '[]')", lowerArgs . upperArgs)

      $(logDebugSH) ("queryAlert" :: Text, sqlTable, window)
      MMap.fromDistinctAscList <$> traceQuery (
        qBase <>
          " AND tsrange(el.started, el.\"lastSeen\", '[]') && " <> qWindow <> " \
          \ ORDER BY el.id ASC") -- this ORDER BY justifies the 'MMap.fromDistinctAscList' above.
        qWindowArgs build

    runQuery :: LogTag e -> ClosedInterval (WithInfinity UTCTime) -> m (MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView))
    runQuery lTag window = (fmap.fmap.fmap) (\x -> lTag :=> Identity x) $
      logAssume lTag (queryAlert (singleConstructor $ proxify lTag) (logDep lTag) window)

    runQueries :: ClosedInterval (WithInfinity UTCTime) -> m (MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView))
    runQueries window = do
      leftBiasedUnions <$> traverse (\(This lTag) -> do { x <- runQuery lTag window; $(logDebugSH) x; pure x }) universe

        --, queryClientDaemonAlert "ErrorLogBakerNoHeartbeat" ["lastLevel", "lastBlockHash", "client"]
        --  (\elId (tLastLevel, tLastBlockHash, tClient) -> LogTag_BakerNoHeartbeat $ ErrorLogBakerNoHeartbeat elId tLastLevel tLastBlockHash tClient)
        --    window

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
  :: forall m. (PostgresRaw m, MonadIO m, PersistBackend m, MonadLogger m, MonadMask m)
  => NodeDataSource
  -> Maybe PublicKeyHash
  -> m [(WithInfinity PublicKeyHash, Deletable BakerSummary)]
getBakerAddresses nds bid = do
  let qCount :: [Utf8]
      qCount = flip map universe $ \(This bTag) -> logAssume (LogTag_Baker bTag) $ case bakerLogDep bTag of
        r@(Related fld fk) ->
          let ctor = singleConstructor $ proxify bTag
              entityD = entityDef pg $ phantomize $ Compose ctor
              constrNum = entityConstrNum (Compose ctor) ctor
              constrD = constructors entityD !! constrNum
              extraTbl = tableName id entityD constrD
              ctor2 = singleConstructor $ proxify r
              tColumns = renderQualifiedField "elbm" $ fieldChain pg fld
              relatedColumns = renderQualifiedField "b" $ case fk of
                ForeignKey_UniqueId -> fieldChain pg $ (undefined :: DefaultKey r ~ Key r (Unique u) => d (ConstructorMarker r) -> u (UniqueMarker r)) ctor2
                ForeignKey_UniqueIdData -> fieldChain pg $ (undefined :: DefaultKey r ~ Key r (Unique u) => d (ConstructorMarker r) -> u (UniqueMarker r)) ctor2
                ForeignKey_Field fld2 -> fieldChain pg fld2
          in
            "(SELECT COUNT(e.id) FROM \"" <> extraTbl
            <> "\" elbm JOIN \"ErrorLog\" e on e.id = elbm.log WHERE " <> mconcat (intersperse " AND " $ "e.stopped IS NULL" : zipWith (\x y -> x <> " = " <> y) relatedColumns tColumns) <> ")"
      qFull = "\
        \ SELECT b.\"publicKeyHash\", b.\"data#data#alias\", "
        <> mconcat (intersperse " + " qCount) <> " \
        \ FROM \"Baker\" b \
        \ WHERE NOT b.\"data#deleted\" \
        \   AND COALESCE(?,b.\"publicKeyHash\") = b.\"publicKeyHash\" \
        \ ORDER BY b.\"publicKeyHash\""
      buildRs :: (Monad f, PersistBackend f) => [PersistValue] -> f (PublicKeyHash, (Maybe Text, Int))
      buildRs = evalStateT $ do
        pkh :: PublicKeyHash <- StateT fromPersistValues
        alias :: Maybe Text <- StateT fromPersistValues
        errorCount :: Int <- StateT fromPersistValues
        pure (pkh, (alias, errorCount))
  rs <- Map.fromAscList <$> traceQuery
      qFull
      (toPrimitivePersistValue pg bid :)
      buildRs
  int :: Map.Map PublicKeyHash (Bool, SecretKey, (Int, Bool)) <- [queryQ|
      SELECT b."data#data#publicKeyHash", b."data#data#insufficientFunds", p."control",
        la."secretKey#ledgerIdentifier", la."secretKey#signingCurve", la."secretKey#derivationPath",
        ( SELECT COUNT(e.id)
          FROM "ErrorLog" e
          JOIN "ErrorLogBakerMissed" elbm
            ON elbm.log = e.id
          WHERE e.stopped IS NULL
            AND elbm."baker#publicKeyHash" = b."data#data#publicKeyHash"
        )
      FROM "BakerDaemonInternal" b
      JOIN "ProcessData" p ON p.id = b."data#data#bakerProcessData"
      JOIN "LedgerAccount" la ON la."publicKeyHash" = b."data#data#publicKeyHash"
      WHERE NOT b."data#deleted"
    |] <&> Map.fromList . fmap (\(pkh, insufficientFunds, control, li, sc, dp, alertCount) ->
      let sk = SecretKey
            { _secretKey_ledgerIdentifier = li
            , _secretKey_signingCurve = sc
            , _secretKey_derivationPath = dp
            }
      in (pkh, (control == ProcessControl_Run, sk, (alertCount, insufficientFunds))))
  -- TODO: this is rather inelegant: we need something like this; to give you
  -- your next rights we need to know what level we're at now.  there's not an
  -- elegant way to do that today, from the postgres level.  a "current level"
  --
  -- we need to do this *here* instead of, say, on bakerdetails, because we
  -- need to show a grey dot when we "cant" show this, in the baker list.
  -- grab the hashes of the cycle starts, if they exist
  latestHead' <- liftIO $ atomically $ dataSourceHead nds -- TODO: Add schema so this can be DB-based
  maxProgress_rightsInfo :: Either CacheError (Maybe (Maybe RawLevel, [RightsCycleInfo])) <- case latestHead' of
    Nothing -> pure $ Left CacheError_NotEnoughHistory
    Just latestHead -> flip runReaderT nds $ runExceptT $ tryNodeQueryT $ do
      rightsInfo <- cycleStartHashes $ latestHead ^. hash
      -- WARNING: We're looking up information in the future which might be wrong. We assume the following
      -- protocol constants won't ever change, even with a new protocol:
      --    $PRESERVED_CYCLES
      --    $BLOCKS_PER_CYCLE
      headProtoInfo <- nodeQueryDataSourceSafe $ NodeQuery_ProtocolConstants $ latestHead ^. hash
      maxProgress <- for (maximumMay $ _rightsCycleInfo_cycle <$> rightsInfo) $ \highestRightsCycle ->
        lastLevelInCycle (latestHead ^. hash) $ highestRightsCycle + headProtoInfo ^. protoInfo_preservedCycles + 1
      pure (maxProgress, rightsInfo)

  let
    maxProgress = maxProgress_rightsInfo ^? _Right . _Just . _1 . _Just
    rightsInfo = fromMaybe [] $ maxProgress_rightsInfo ^? _Right . _Just . _2
    rightsHashes :: Pg.In [BlockHash] = Pg.In $ _rightsCycleInfo_branch <$> rightsInfo
    bakerHashes :: Pg.In [PublicKeyHash] = Pg.In $ Map.keys bakers
    -- Insert pkh from Internal if present
    bakers = Map.union (fmap (\(b, li, c) -> (Right (BakerInternalData li b), c)) int) $
      fmap (\(a, c) -> (Left (BakerData a), (c, False))) rs
    chainId = _nodeDataSource_chain nds

  nextBakeRightsL <- case latestHead' ^? _Just . level of
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
        AND br.level > ?headLevel + CASE WHEN br."right" = 'RightKind_Endorsing' THEN -1 ELSE 0 END -- if the endorsement is of the current block, you haven't missed it yet.
      WHERE brcp."chainId" = ?chainId
        AND brcp.branch in ?rightsHashes
        AND brcp."publicKeyHash" in ?bakerHashes
      GROUP BY brcp."publicKeyHash", br."right"
      |]

  let

    getNextRight rights progress insufficientFunds = case minimumByMay (on compare swap) $ Map.toList rights of
      Just v -> BakerNextRight_KnownRights v
      Nothing -> if insufficientFunds
        then BakerNextRight_KnownNoRights
        else case subtract progress <$> maxProgress of
          Just 0 -> BakerNextRight_WaitingForRights
          Just _ -> BakerNextRight_GatheringData
          Nothing -> BakerNextRight_GatheringData
        -- if maxProgress is Nothing, then we don't yet have enough history to say much of anything about how much work we still need to do per baker
    nextBakeRights :: MonoidalMap PublicKeyHash (Max RawLevel, Map.Map RightKind RawLevel)
    nextBakeRights = foldMap (\(pkh, progress, rightKind, rightLvl) -> MMap.singleton pkh (Max progress, fromMaybe mempty $ Map.singleton <$> rightKind <*> rightLvl)) nextBakeRightsL
    result = fmap (bimap Bounded (First . Just)) $ Map.toList $ Map.mapMaybe id $ alignWith
      (these
        (\(b, (alertCount, _)) -> Just $ BakerSummary b alertCount BakerNextRight_GatheringData)
        (const Nothing)
        (\(b, (alertCount, insufficientFunds)) (Max progress, rights) -> Just $ BakerSummary b alertCount (getNextRight rights progress insufficientFunds))
      ) bakers (getMonoidalMap nextBakeRights)

  return result

getNodeAddresses
  :: forall m. (Monad m, PostgresRaw m, MonadLogger m, PersistBackend m)
  => Maybe (Id Node)
  -> m [(WithInfinity (Id Node), Deletable NodeSummary)]
getNodeAddresses nid = do
  ext :: Map.Map (WithInfinity (Id Node)) NodeExternalData <- [queryQ|
      SELECT n.id, n."data#data#address", n."data#data#alias", n."data#data#minPeerConnections"
      FROM "NodeExternal" n
      WHERE NOT n."data#deleted"
        AND CASE WHEN ?nid is NULL THEN true ELSE n.id = ?nid END|]
    <&> Map.fromList . fmap (\(nid', uri, alias, mpc) -> (Bounded nid',
    NodeExternalData
      { _nodeExternalData_address = uri
      , _nodeExternalData_alias = alias
      , _nodeExternalData_minPeerConnections = mpc
      }))
  int :: Map.Map (WithInfinity (Id Node)) ProcessData <- [queryQ|
      SELECT n.id, p.control, p.state, p.updated AT TIME ZONE 'UTC', p.backend
        FROM "NodeInternal" n
        JOIN "ProcessData" p ON p.id = n."data#data"
      WHERE NOT n."data#deleted"
        AND CASE WHEN ?nid is NULL THEN true ELSE n.id = ?nid END|]
    <&> Map.fromList . fmap (\(nid', control, state, updated, backend) -> (Bounded nid',
      ProcessData
      { _processData_control = control
      , _processData_state = state
      , _processData_updated = updated
      , _processData_backend = backend
      }))
  let qCount :: [Utf8]
      qCount = flip map universe $ \(This nTag) -> logAssume (LogTag_Node nTag) $ case nodeLogDep nTag of
        r@(Related fld fk) ->
          let ctor = singleConstructor $ proxify nTag
              entityD = entityDef pg $ phantomize $ Compose ctor
              constrNum = entityConstrNum (Compose ctor) ctor
              constrD = constructors entityD !! constrNum
              extraTbl = tableName id entityD constrD
              ctor2 = singleConstructor $ proxify r
              tColumns = renderQualifiedField "ein" $ fieldChain pg fld
              relatedColumns = renderQualifiedField "n" $ case fk of
                ForeignKey_AutoId -> fieldChain pg $ (const AutoKeyField :: d (ConstructorMarker r) -> AutoKeyField r d) ctor2
                ForeignKey_Field fld2 -> fieldChain pg fld2
          in
            "(SELECT COUNT(ein.log) FROM \"" <> extraTbl
            <> "\" ein JOIN \"ErrorLog\" e on e.id = ein.log WHERE " <> mconcat (intersperse " AND " $ "e.stopped IS NULL" : zipWith (\x y -> x <> " = " <> y) relatedColumns tColumns) <> ")"
      qCounts = "SELECT n.id, " <> mconcat (intersperse " + " qCount) <> " \
        \ FROM ( \
        \   SELECT n1.id FROM \"NodeExternal\" n1 \
        \   WHERE NOT n1.\"data#deleted\" \
        \   UNION \
        \   SELECT n2.id FROM \"NodeInternal\" n2 \
        \   WHERE NOT n2.\"data#deleted\") n \
        \ WHERE COALESCE(?,n.id) = n.id \
        \ ORDER BY n.id"
      buildCounts :: (Monad f, PersistBackend f) => [PersistValue] -> f (WithInfinity (Id Node), Int)
      buildCounts = evalStateT $ do
        nodeId :: Id Node <- StateT fromPersistValues
        errorCount :: Int <- StateT fromPersistValues
        pure (Bounded nodeId, errorCount)
  counts <- Map.fromAscList <$> traceQuery
      qCounts
      (toPrimitivePersistValue pg nid :)
      buildCounts
  let
    intExt :: Map.Map (WithInfinity (Id Node)) (Either NodeExternalData ProcessData)
    intExt = fmap Left ext `Map.union` fmap Right int
  return $ Map.toList $ fmap (First . Just) $ liftF2 NodeSummary intExt counts
