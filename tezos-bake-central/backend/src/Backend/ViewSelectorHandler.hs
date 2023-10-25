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
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-redundant-constraints #-}

module Backend.ViewSelectorHandler where

import Control.Concurrent.STM (atomically)
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger
import Control.Exception.Safe (MonadMask)
import Control.Monad.Trans.State (StateT(..), evalStateT, modify)
import Data.Aeson (decode')
import Data.Align (alignWith)
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Base16 as B16
import Data.Functor.Identity (Identity (..))
import Data.Functor.Apply (liftF2)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum(..))
import Data.List (dropWhileEnd, intersperse, minimumBy, maximumBy)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap(..))
import qualified Data.Map.Monoidal as MMap
import Data.Ord (comparing)
import Data.Pool (Pool)
import Data.Semigroup (sconcat)
import Data.Some (Some(..))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Data.These (these)
import Data.Validation hiding (ensure)
import Data.Universe (universe)
import Database.Groundhog.Core
  (ConstructorMarker, EntityConstr, FieldChain, PersistEntity, PersistValue, Utf8(..),
  constrParams, constructors, entityConstrNum, entityDef, fieldChain, fromEntityPersistValues,
  fromPersistValues, fromUtf8, fromUtf8, toPrimitivePersistValue)
import Database.Groundhog.Generic (mapAllRows)
import Database.Groundhog.Generic.Sql (RenderConfig(..), flatten, renderChain, tableName)
import Database.Groundhog.Postgresql
import Database.Id.Class
import qualified Database.PostgreSQL.Simple as Pg
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http
import Rhyolite.Backend.App (QueryHandler (..))
import Rhyolite.Backend.DB (MonadBaseNoPureAborts, runDb, selectMap', selectSingle)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, queryQ)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Backend.Schema.Class (singleConstructor)
import Safe (headMay)
import Text.URI (render, URI)

import Tezos.Types

import Backend.Http (doRequestLBS)
import Backend.IndexQueries (endOfPreservedCycles)
import Backend.NodeRPC
import Backend.Schema
import Backend.Workers.TezosRelease (getLatestTezosRelease)
import Common.Alerts(AlertsFilter(..))
import Common.App
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import qualified Common.AppendIntervalMap as AppendIMap
import Common.Config (FrontendConfig(..))
import Common.Schema
import Common.Vassal
import ExtraPrelude

viewSelectorHandler
  :: forall m a. (MonadBaseNoPureAborts IO m, MonadIO m, Monoid a, MonadMask m, Show a)
  => FrontendConfig
  -> NodeDataSource
  -> Pool Postgresql
  -> QueryHandler (BakeViewSelector a) m
viewSelectorHandler frontendConfig nds db = QueryHandler $ \vs -> runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $ do
  let
    maybeViewHandler
      :: Applicative m'
      => (BakeViewSelector a -> MaybeSelector v a)
      -> m' (Maybe v)
      -> m' (View (MaybeSelector v) a)
    maybeViewHandler getVS xs = whenM (not $ null $ getVS vs) $
      toMaybeView (getVS vs) <$> xs
    chainId = _nodeDataSource_chain nds

  let paramsVS = _bakeViewSelector_parameters vs
  parameters <- whenM (not $ null paramsVS) $ do
    let selectedProtocols :: [ProtocolHash] = MMap.keys $ unMapSelector paramsVS
    protocols :: [ProtocolIndex] <- select
      ( ProtocolIndex_hashField `in_` selectedProtocols &&.
        ProtocolIndex_chainIdField ==. chainId)
    let findProtocol k a = fmap (\v -> (First v, a)) $ Prelude.lookup k $ map (\p -> (_protocolIndex_hash p, p)) protocols
    pure $ MapView $ MMap.mapMaybeWithKey (\k a -> findProtocol k a ) $ unMapSelector paramsVS

  let nodeAddrVS = _bakeViewSelector_nodeAddresses vs
  nodeAddresses <- whenM (not $ null nodeAddrVS) $ do
    -- TODO: nodeAddrVS is a RangeView.  select individual nodes upon request.
    toRangeView nodeAddrVS <$> getNodeAddresses Nothing

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

  let bakerAlertsVS = _bakeViewSelector_bakerAlerts vs
  bakerAlerts <- whenM (not $ null bakerAlertsVS) $
    toRangeView bakerAlertsVS . fmap (\(pkh, v) -> (Bounded pkh, First $ Just v)) <$> getBakerAlert chainId

  mailServer <- maybeViewHandler _bakeViewSelector_mailServer $ do
    rs <- fmap _notificatee_email . toList <$> selectMap' NotificateeConstructor CondEmpty
    fmap (Just . fmap (flip mailServerConfigToView rs)) $ selectSingle CondEmpty

  let errorsVS = _bakeViewSelector_errors vs
  errors <- itraverse (getErrorLogs chainId) errorsVS

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

  alertCount <- maybeViewHandler _bakeViewSelector_alertCount $ Just <$> getAlertCount chainId
  config <- maybeViewHandler _bakeViewSelector_config $ pure $ Just frontendConfig
  latestHead <- maybeViewHandler _bakeViewSelector_latestHead $ liftIO $ atomically $ dataSourceFinalHead nds

  snapshotMeta <- maybeViewHandler _bakeViewSelector_snapshotMeta $ selectSingle CondEmpty

  let amendmentVS = _bakeViewSelector_amendment vs
  amendment <- whenM (not $ null amendmentVS) $ do
    as <- select $ Amendment_chainIdField ==. chainId
    pure $ toRangeView amendmentVS $ flip fmap as $ \a -> (_amendment_period a, First $ Just a)

  let periodProposalsVS = _bakeViewSelector_proposals vs
  periodProposals <- whenM (not $ null periodProposalsVS) $ do
    toRangeView periodProposalsVS . fmap (bimap Bounded (First . Just)) <$> getProposals

  bakerVote <- maybeViewHandler _bakeViewSelector_bakerVote $ Just <$> do
    results <- [queryQ|
      SELECT v.pkh, v."proposalHash", v.ballot, v.included, v.attempted, v."chainId"
      FROM "BakerVote" v
      WHERE v."chainId" = ?chainId
      LIMIT 1
    |]
    pure $ listToMaybe $ results <&> \(pkh, proposalHash, ballot, included, attempted, chId) -> BakerVote
      { _bakerVote_pkh = pkh
      , _bakerVote_proposalHash = proposalHash
      , _bakerVote_ballot = ballot
      , _bakerVote_included = included
      , _bakerVote_attempted = attempted
      , _bakerVote_chainId = chId
      }

  periodTestingVote <- maybeViewHandler _bakeViewSelector_periodTestingVote $ Just <$> do
    results <- [queryQ|
      SELECT v."proposalHash", v."periodVote#ballots#yay", v."periodVote#ballots#nay", v."periodVote#ballots#pass", v."periodVote#quorum", v."periodVote#totalVotingPower", v."chainId"
      FROM "PeriodTestingVote" v
      WHERE v."chainId" = ?chainId
      LIMIT 1
    |]
    pure $ listToMaybe $ results <&> \(ph, by, bn, bp, q, t, chId) -> PeriodTestingVote
      { _periodTestingVote_proposalHash = ph
      , _periodTestingVote_chainId = chId
      , _periodTestingVote_periodVote = PeriodVote
        { _periodVote_ballots = ProtoAgnosticBallots
          { _protoAgnosticBallots_yay = by
          , _protoAgnosticBallots_nay = bn
          , _protoAgnosticBallots_pass = bp
          }
        , _periodVote_quorum = q
        , _periodVote_totalVotingPower = t
        }
      }

  periodTesting <- maybeViewHandler _bakeViewSelector_periodTesting $ Just <$> do
    results <- [queryQ|
      SELECT t."proposalHash", t."chainId"
      FROM "PeriodTesting" t
      WHERE t."chainId" = ?chainId
      LIMIT 1
    |]
    pure $ listToMaybe $ results <&> \(ph, chId) -> PeriodTesting
      { _periodTesting_proposalHash = ph
      , _periodTesting_chainId = chId
      }

  periodPromotionVote <- maybeViewHandler _bakeViewSelector_periodPromotionVote $ Just <$> do
    results <- [queryQ|
      SELECT v."proposalHash", v."periodVote#ballots#yay", v."periodVote#ballots#nay", v."periodVote#ballots#pass", v."periodVote#quorum", v."periodVote#totalVotingPower", v."chainId"
      FROM "PeriodPromotionVote" v
      WHERE v."chainId" = ?chainId
      LIMIT 1
    |]
    pure $ listToMaybe $ results <&> \(ph, by, bn, bp, q, t, chId) -> PeriodPromotionVote
      { _periodPromotionVote_proposalHash = ph
      , _periodPromotionVote_chainId = chId
      , _periodPromotionVote_periodVote = PeriodVote
        { _periodVote_ballots = ProtoAgnosticBallots
          { _protoAgnosticBallots_yay = by
          , _protoAgnosticBallots_nay = bn
          , _protoAgnosticBallots_pass = bp
          }
        , _periodVote_quorum = q
        , _periodVote_totalVotingPower = t
        }
      }

  periodAdoption <- maybeViewHandler _bakeViewSelector_periodAdoption $ Just <$> do
    results <- [queryQ|
      SELECT v."proposalHash", v."periodVote#ballots#yay", v."periodVote#ballots#nay", v."periodVote#ballots#pass", v."periodVote#quorum", v."periodVote#totalVotingPower", v."chainId"
      FROM "PeriodAdoption" v
      WHERE v."chainId" = ?chainId
      LIMIT 1
    |]
    pure $ listToMaybe $ results <&> \(ph, by, bn, bp, q, t, chId) -> PeriodAdoption
      { _periodAdoption_proposalHash = ph
      , _periodAdoption_chainId = chId
      , _periodAdoption_periodVote = PeriodVote
        { _periodVote_ballots = ProtoAgnosticBallots
          { _protoAgnosticBallots_yay = by
          , _protoAgnosticBallots_nay = bn
          , _protoAgnosticBallots_pass = bp
          }
        , _periodVote_quorum = q
        , _periodVote_totalVotingPower = t
        }
      }

  connectedLedger <- maybeViewHandler _bakeViewSelector_connectedLedger $ Just <$> selectSingle CondEmpty

  let showLedgerVS = _bakeViewSelector_showLedger vs
  showLedger <- whenM (not $ null showLedgerVS) $ do
    las <- select CondEmpty -- Expect very few records here, so just select them all
    let rangeView = toRangeView showLedgerVS $ flip fmap las $ \la ->
          ( _ledgerAccount_secretKey la
          , fromEither $ liftA2 (,) (maybe (Left (First "Importing PKH...")) Right $ _ledgerAccount_publicKeyHash la) (Right $ _ledgerAccount_balance la)
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

  let votePromptingVS = _bakeViewSelector_votePrompting vs
  votePrompting <- whenM (not $ null votePromptingVS) $ do
    las <- select CondEmpty
    let rangeView = toRangeView votePromptingVS $ flip fmap las $ \la ->
          ( _ledgerAccount_secretKey la
          , First $ Just $ VoteState Nothing mempty
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

  let nodeVersionsVS = _bakeViewSelector_nodeVersions vs
      internalURI = _nodeDataSource_kilnNodeUri nds
      mgr = _nodeDataSource_httpMgr nds

  nodeVersions <- toRangeView nodeVersionsVS <$> getNodeVersions mgr internalURI Nothing

  let latestTezosReleaseVS = _bakeViewSelector_latestTezosRelease vs

  let projId = _frontendConfig_tezosGitlabProjectId frontendConfig
  let mrelease = _frontendConfig_tezosRelease frontendConfig
  latestTezosRelease <- toMaybeView latestTezosReleaseVS . pure <$> getLatestTezosRelease mgr projId mrelease

  return BakeView
    { _bakeView_config = config
    , _bakeView_parameters = parameters
    , _bakeView_nodeAddresses = nodeAddresses
    , _bakeView_nodeVersions = nodeVersions
    , _bakeView_nodeDetails = nodeDetails
    , _bakeView_latestTezosRelease = latestTezosRelease
    , _bakeView_bakerAddresses = bakerAddresses
    , _bakeView_bakerAlerts = bakerAlerts
    , _bakeView_mailServer = mailServer
    , _bakeView_bakerDetails = bakerDetails
    , _bakeView_errors = errors
    , _bakeView_latestHead = latestHead
    , _bakeView_amendment = amendment
    , _bakeView_proposals = periodProposals
    , _bakeView_bakerVote = bakerVote
    , _bakeView_periodTestingVote = periodTestingVote
    , _bakeView_periodTesting = periodTesting
    , _bakeView_periodPromotionVote = periodPromotionVote
    , _bakeView_periodAdoption = periodAdoption
    , _bakeView_upstreamVersion = upgrade
    , _bakeView_telegramConfig = telegramConfig
    , _bakeView_telegramRecipients = telegramRecipients
    , _bakeView_alertCount = alertCount
    , _bakeView_snapshotMeta = snapshotMeta
    , _bakeView_connectedLedger = connectedLedger
    , _bakeView_showLedger = showLedger
    , _bakeView_prompting = prompting
    , _bakeView_votePrompting = votePrompting
    , _bakeView_rightNotificationSettings = rightNotificationSettings
    , _bakeView_bakerRegistered = mempty
    }

pg :: Proxy Postgresql
pg = Proxy @Postgresql

proxify :: proxy x -> Proxy x
proxify = const Proxy

phantomize :: f x -> x
phantomize = error "tried to touch a phantom"

renderQualifiedField :: Utf8 -> FieldChain -> [Utf8]
renderQualifiedField q fld = renderChain (RenderConfig $ \x -> q <> ".\"" <> x <> "\"") fld []

renderChainId :: ChainId -> Utf8
renderChainId = Utf8 . ("'\\x" <>) . (<>"'") .BS.byteString . B16.encode . fromShort . unHashedValue

traceQuery :: (MonadLogger f, PersistBackend f) => Utf8 -> ([PersistValue] -> [PersistValue]) -> ([PersistValue] -> f r) -> f [r]
traceQuery sql params f = do
  $(logDebugS) "SQL" (tshow sql)
  $(logDebugS) "SQL" (tshow $ params [])
  queryRaw False (T.unpack $ decodeUtf8 $ fromUtf8 sql) (params []) $ mapAllRows f

getErrorLogs
  :: forall m a .
  ( MonadLogger m
  , PersistBackend m
  , Semigroup a
  )
  => ChainId
  -> AlertsFilter
  ->          Compose (MapSelector (Some LogTag) ()) (IntervalSelector' UTCTime (Id ErrorLog) (Deletable ErrorInfo)) a
  -> m (View (Compose (MapSelector (Some LogTag) ()) (IntervalSelector' UTCTime (Id ErrorLog) (Deletable ErrorInfo))) a)
getErrorLogs chainId flt sel = do
  vals <- getErrorLogsImpl chainId flt $ getCompose sel
  let
    l :: Compose (MonoidalMap (Some LogTag)) (View (IntervalSelector' UTCTime (Id ErrorLog) (Deletable ErrorInfo))) a
    l = Compose $ MMap.fromList $ map (\(k, v, _) -> (k, v)) vals

    u :: View (MapSelector (Some LogTag) ()) a
    u = MapView $ MMap.fromList $ map (\(k, _, a) -> (k, (First (), a))) vals
  pure $ ComposeView u l

getErrorLogsImpl
  :: forall m a.
  ( MonadLogger m
  , PersistBackend m
  , Semigroup a
  )
  => ChainId
  -> AlertsFilter
  -> MapSelector (Some LogTag) () (IntervalSelector' UTCTime (Id ErrorLog) (Deletable ErrorInfo) a)
  -> m [(Some LogTag, View (IntervalSelector' UTCTime (Id ErrorLog) (Deletable ErrorInfo)) a, a)]
getErrorLogsImpl chainId flt (MapSelector logTags) = (catMaybes <$>) $ for (MMap.assocs logTags) $ \(lTag, IntervalSelector intervalMap) -> do
  let flattenedIntervalMap = AppendIMap.flattenWithClosedInterval (<>) intervalMap
  $(logDebugSH) ("getErrorLogs" :: Text, void flattenedIntervalMap)

  vals <- fmap getErrorInterval . leftBiasedUnions <$> for (AppendIMap.keys flattenedIntervalMap) (runQueries lTag)
  let ma = sconcat <$> NEL.nonEmpty (AppendIMap.elems intervalMap) :: Maybe a
  pure $ (,,) lTag ((IntervalView flattenedIntervalMap . (fmap.fmap.first) (First . Just)) vals) <$> ma
  where
    --queryClientDaemonAlert sqlTable sqlFields =
    --  queryAlert sqlTable sqlFields (Just ("Client", "id", "client"))
    -- TODO: make every bakeralert work with the Id Baker column, probably
    runQueries :: Some LogTag -> ClosedInterval (WithInfinity UTCTime) -> m (MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView))
    runQueries ltag window = do
      leftBiasedUnions <$> traverse (\(Some lTag) -> do { x <- getErrorLogForTag chainId flt lTag window; $(logDebugSH) x; pure x }) [ltag]

    leftBiasedUnions = MMap.unionsWith const


getErrorLogForTag
  :: forall m e.
  ( MonadLogger m
  , PersistBackend m
  )
  => ChainId -> AlertsFilter -> LogTag e -> ClosedInterval (WithInfinity UTCTime) -> m (MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView))
getErrorLogForTag chainId flt lTag window = (fmap.fmap.fmap) (\x -> lTag :=> Identity x) $
      logAssume lTag (queryAlert (singleConstructor $ proxify lTag) (logDep lTag) window)
  where
    {-# INLINE queryAlert #-}
    queryAlert
      :: forall f c b. (Monad f, PersistBackend f, MonadLogger f, PersistEntity b, EntityConstr b c)
      => c (ConstructorMarker b)
      -> [Some (Related b c)]
      -> ClosedInterval (WithInfinity UTCTime)
      -> f (MonoidalMap (Id ErrorLog) (ErrorLog, b))
    queryAlert ctor related window' = do
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
          Some r@(Related fld fk) ->
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
          \   , el.\"chainId\" \
          \   " <> foldMap (\fld -> ", t.\"" <> fld <> "\"") sqlFields <> " \
          \ FROM \"ErrorLog\" el \
          \ JOIN \"" <> sqlTable <> "\" t ON t.log = el.id \
          \ WHERE (("
          <> bool (mconcat $ intersperse " OR " qCond) "TRUE" (null related)
          <> " AND COALESCE(el.started != el.stopped, true))"
          <> " AND el.\"chainId\" = " <> renderChainId chainId
          <> ")"
          <> qFlt
        qFlt = case flt of
          AlertsFilter_All -> ""
          AlertsFilter_ResolvedOnly -> " AND el.stopped IS NOT NULL"
          AlertsFilter_UnresolvedOnly -> " AND el.stopped IS NULL"
        (qWindow, qWindowArgs) = case window' of
          ClosedInterval lowerEnd upperEnd -> let
            qEndpoint = \case
              LowerInfinity -> ("'-infinity'", id)
              Bounded pt -> ("?", (toPrimitivePersistValue pg pt:))
              UpperInfinity -> ("'infinity'", id)
            (lowerQ, lowerArgs) = qEndpoint lowerEnd
            (upperQ, upperArgs) = qEndpoint upperEnd
            in ("tsrange(" <> lowerQ <> ", " <> upperQ <> ", '[]')", lowerArgs . upperArgs)

      $(logDebugSH) ("queryAlert" :: Text, sqlTable, window')
      MMap.fromDistinctAscList <$> traceQuery (
        qBase <>
          " AND tsrange(el.started, el.\"lastSeen\", '[]') && " <> qWindow <> " \
          \ ORDER BY el.id ASC") -- this ORDER BY justifies the 'MMap.fromDistinctAscList' above.
        qWindowArgs build

getBakerAlert
  :: forall m.
  ( MonadLogger m
  , PersistBackend m
  )
  => ChainId
  -> m [(PublicKeyHash, NonEmpty BakerAlert)]
getBakerAlert chainId = do

  let everythingWindow = ClosedInterval LowerInfinity UpperInfinity

  allAlerts <- traverse (\(Some t) -> getErrorLogForTag chainId AlertsFilter_UnresolvedOnly (LogTag_Baker t) everythingWindow) universe
  let
    bakerErrors :: MonoidalMap PublicKeyHash [(ErrorLog, BakerErrorLogView)]
    bakerErrors = MMap.fromListWith (<>)
      [ (k, pure (l, t'))
      | (l@ErrorLog{_errorLog_stopped = Nothing}, t) <- concatMap MMap.elems allAlerts
      , Just t' <- [bakerErrorViewOnly t]
      , let k = bakerIdForBakerErrorLogView t'
      ]

    groupBakerAlerts :: [(ErrorLog, DSum BakerLogTag Identity)] -> [BakerAlert]
    groupBakerAlerts bs = bakerAlerts
      where
        groupedAlerts = bs
          <&> (\(_, tag :=> Identity elog) -> DMap.fromList [tag :=> [elog]])
          & DMap.unionsWithKey (\_ l1 l2 -> l1 ++ l2)

        bakerAlerts = DMap.toList groupedAlerts >>= \(tag :=> elogs) ->
          case tag of
            -- groupable baker alerts
            BakerLogTag_BakerMissed -> case NEL.nonEmpty elogs of
              Nothing -> []
              Just (elog :| []) -> pure $ BakerAlert_Alert (BakerLogTag_BakerMissed :=> Identity elog)
              Just ls ->
                let
                  elog = NEL.head ls
                  getLvl = _errorLogBakerMissed_level
                  getBakeTime = _errorLogBakerMissed_bakeTime
                  applyF f = (\el -> (getLvl el, getBakeTime el)) $ f (comparing getBakeTime) elogs
                in pure $ BakerAlert_GroupedAlert $  GroupedBakerAlert
                  { _groupedBakerAlert_type = GroupedAlertType_MissedBake
                  , _groupedBakerAlert_first = applyF minimumBy
                  , _groupedBakerAlert_latest = applyF maximumBy
                  , _groupedBakerAlert_right = Just $ _errorLogBakerMissed_right elog
                  , _groupedBakerAlert_baker = _errorLogBakerMissed_baker elog
                  , _groupedBakerAlert_logs = _errorLogBakerMissed_log <$> ls
                  }
            BakerLogTag_MissedEndorsementBonus -> case NEL.nonEmpty elogs of
              Nothing -> []
              Just (elog :| []) -> pure $ BakerAlert_Alert (BakerLogTag_MissedEndorsementBonus :=> Identity elog)
              Just ls ->
                let
                  elog = NEL.head ls
                  getLvl = _errorLogBakerMissedEndorsementBonus_level
                  getBakeTime = _errorLogBakerMissedEndorsementBonus_bakeTime
                  applyF f = (\el -> (getLvl el, getBakeTime el)) $ f (comparing getBakeTime) elogs
                in pure $ BakerAlert_GroupedAlert $ GroupedBakerAlert
                    { _groupedBakerAlert_type = GroupedAlertType_MissedEndorsementBonus
                    , _groupedBakerAlert_first = applyF minimumBy
                    , _groupedBakerAlert_latest = applyF maximumBy
                    , _groupedBakerAlert_right = Nothing
                    , _groupedBakerAlert_baker = _errorLogBakerMissedEndorsementBonus_baker elog
                    , _groupedBakerAlert_logs = _errorLogBakerMissedEndorsementBonus_log <$> ls
                    }

            -- non-groupable baker alerts
            _ -> map (\al -> BakerAlert_Alert $ tag :=> Identity al) elogs

  pure $ mapMaybe (\(k, v) -> fmap (k,) . NEL.nonEmpty $ groupBakerAlerts v) $ MMap.toList bakerErrors


{-# ANN getAlertCount ("HLint: ignore Use bimap" :: String) #-}
getAlertCount
  :: forall m.
  ( MonadLogger m
  , PersistBackend m
  )
  => ChainId
  -> m (DMap LogTag (Const Int))
getAlertCount chainId = DMap.fromList . concat <$> traverse (\(Some lTag) -> do
  (x, _) <- runQuery lTag
  pure $ map (\(t, v) -> t :=> Const v) x) universe
  where
    {-# INLINE queryAlert #-}
    queryAlert
      :: forall f c b. (Monad f, PersistBackend f, MonadLogger f, PersistEntity b, EntityConstr b c)
      => c (ConstructorMarker b)
      -> [Some (Related b c)]
      -> f ([Int], Proxy b)
    queryAlert ctor _related = do
      let
        build :: [PersistValue] -> f Int
        build = evalStateT $ do
          StateT fromPersistValues
        entityD = entityDef pg (undefined :: b)
        constrD = constructors entityD !! constrNum
        constrNum = entityConstrNum (Proxy @b) ctor
        sqlTable = tableName id entityD constrD
        qBase :: Utf8
        qBase =
          "SELECT \
          \     COUNT(*) \
          \ FROM \"ErrorLog\" el \
          \ JOIN \"" <> sqlTable <> "\" t ON t.log = el.id \
          \ WHERE el.stopped IS NULL"
          <> " AND el.\"chainId\" = " <> renderChainId chainId
      $(logDebugSH) ("queryAlert" :: Text, sqlTable)
      v <- traceQuery qBase id build
      pure (v, Proxy @b)

    runQuery :: LogTag e -> m ([(LogTag e, Int)] , DSum LogTag Proxy)
    runQuery lTag = do
      vus <- logAssume lTag $ queryAlert (singleConstructor $ proxify lTag) (logDep lTag)
      pure $ (\(vs, u) -> (map (\v -> (lTag, v)) vs, lTag :=> u)) vus

getBakerAddresses
  :: forall m. (PostgresRaw m, MonadIO m, PersistBackend m, MonadLogger m, MonadMask m)
  => NodeDataSource
  -> Maybe PublicKeyHash
  -> m [(WithInfinity PublicKeyHash, Deletable BakerSummary)]
getBakerAddresses nds bid = do
  let chainId = _nodeDataSource_chain nds

      isBakerMissed = \case
        Some BakerLogTag_BakerMissed -> True
        _ -> False

      escape s = "\"" <> s <> "\""
      column t c = t <> "." <> escape c
      paren s = "(" <> s <> ")"
      makeKVs ks vs = zipWith (\k v -> k <> " = " <> v) ks vs

      alertCountQueries :: [Utf8]
      alertCountQueries = flip map (filter (not . isBakerMissed) universe) $ \(Some bTag) -> logAssume (LogTag_Baker bTag) $ case bakerLogDep bTag of
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
          in paren $ mconcat $ intersperse " "
            [ "SELECT COUNT(e.id) FROM " <> escape extraTbl <> " elbm"
            , "JOIN " <> escape "ErrorLog" <> " e"
            , "on e.id = elbm.log"
            , "WHERE"
            , mconcat $ intersperse " AND " $ "e.stopped IS NULL" : makeKVs relatedColumns tColumns
            ]

      getAlertsCountQuery :: Utf8
      getAlertsCountQuery = mconcat $ intersperse " "
        [ "SELECT"
        , mconcat $ intersperse ", "
          [ column "b" "publicKeyHash"
          , column "b" "data#data#alias"
          , mconcat $ intersperse " + " alertCountQueries
          ]
        , "FROM " <> escape "Baker" <> " b"
        , "WHERE NOT " <> column "b" "data#deleted"
        , "  AND COALESCE(?," <> column "b" "publicKeyHash" <> ")" <> " = " <> column "b" "publicKeyHash"
        , "ORDER BY " <> column "b" "publicKeyHash"
        ]

      buildRs :: (Monad f, PersistBackend f) => [PersistValue] -> f (PublicKeyHash, (Maybe Text, Int))
      buildRs = evalStateT $ do
        pkh :: PublicKeyHash <- StateT fromPersistValues
        alias :: Maybe Text <- StateT fromPersistValues
        errorCount :: Int <- StateT fromPersistValues
        pure (pkh, (alias, errorCount))

  bakersAlertCount <- Map.fromAscList <$> traceQuery
      getAlertsCountQuery
      (toPrimitivePersistValue pg bid :)
      buildRs

  -- TODO make a bit prettier. Maybe use the same approach as in 'getAlertCountQuery'.
  internalBakerData :: Map.Map PublicKeyHash (ProcessData, SecretKey, (Int, Bool)) <- [queryQ|
      SELECT b."data#data#publicKeyHash",
        la."secretKey#ledgerIdentifier", la."secretKey#signingCurve", la."secretKey#derivationPath",
        p."control", p."state", p."errorLog", p."restartCount", p."restartAt" AT TIME ZONE 'UTC',
        ( SELECT COUNT(e.id) FROM "ErrorLogBakerMissedEndorsementBonus" elbm JOIN "ErrorLog" e on e.id = elbm.log
          WHERE e.stopped IS NULL AND b."data#data#publicKeyHash" = elbm."baker#publicKeyHash"
        ) +
        ( SELECT COUNT(e.id) FROM "ErrorLogBakerDeactivated" elbm JOIN "ErrorLog" e on e.id = elbm.log
            WHERE e.stopped IS NULL AND b."data#data#publicKeyHash" = elbm."publicKeyHash"
        ) +
        ( SELECT COUNT(e.id) FROM "ErrorLogBakerDeactivationRisk" elbm JOIN "ErrorLog" e on e.id = elbm.log
          WHERE e.stopped IS NULL AND b."data#data#publicKeyHash" = elbm."publicKeyHash"
        ) +
        ( SELECT COUNT(e.id) FROM "ErrorLogBakerAccused" elbm JOIN "ErrorLog" e on e.id = elbm.log
          WHERE e.stopped IS NULL AND b."data#data#publicKeyHash" = elbm."baker#publicKeyHash"
        ) +
        ( SELECT COUNT(e.id) FROM "ErrorLogBakerLedgerDisconnected" elbm JOIN "ErrorLog" e on e.id = elbm.log
          WHERE e.stopped IS NULL AND b."data#data#publicKeyHash" = elbm."baker#publicKeyHash"
        ) +
        ( SELECT COUNT(e.id) FROM "ErrorLogInsufficientFunds" elbm JOIN "ErrorLog" e on e.id = elbm.log
          WHERE e.stopped IS NULL AND b."data#data#publicKeyHash" = elbm."baker#publicKeyHash"
        ),
        EXISTS ( SELECT 1
          FROM "ErrorLog" el
          JOIN "ErrorLogInsufficientFunds" elif
            ON elif.log = el.id
          WHERE el.stopped IS NULL
            AND elif."baker#publicKeyHash" = b."data#data#publicKeyHash"
            AND el."chainId" = ?chainId
        )
      FROM "BakerDaemonInternal" b
      JOIN "ProcessData" p ON p.id = b."data#data#bakerProcessData"
      JOIN "LedgerAccount" la ON la."publicKeyHash" = b."data#data#publicKeyHash"
      WHERE NOT b."data#deleted"
    |] <&> Map.fromList . fmap (\(pkh, li, sc, dp, control, state, errorLog, restartCount, restartAt, missedAlertsCount, insufficientFundsAlert) ->
      let sk = SecretKey
            { _secretKey_ledgerIdentifier = li
            , _secretKey_signingCurve = sc
            , _secretKey_derivationPath = dp
            }
          pd = ProcessData
            { _processData_control = control
            , _processData_state = state
            , _processData_updated = Nothing
            , _processData_backend = Nothing
            , _processData_errorLog = errorLog
            , _processData_restartCount = restartCount
            , _processData_restartAt = restartAt
            }
      in (pkh, (pd, sk, (missedAlertsCount, insufficientFundsAlert))))

  -- TODO: this is rather inelegant: we need something like this; to give yo  -- grab the hashes of the cycle starts, if they exist
  (latestFinalHead', latestHeadLevel) <- liftIO $ atomically $ liftA2 (,) (dataSourceFinalHead nds) (dataSourceHeadLevel nds)
  maxProgress_rightsInfo :: Either KilnRpcError (Maybe RawLevel) <- case latestFinalHead' of
    Nothing -> pure $ Left KilnRpcError_NoKnownHeads
    Just latestFinalHeadInfo -> flip runReaderT nds $ runExceptT $ tryNodeQueryT $ do
      let protocol = latestFinalHeadInfo ^. protocolHash
      mbProtoInfo :: Maybe ProtoInfo <- fmap (fmap (view protocolIndex_constants) . headMay) $ select
        ( ProtocolIndex_hashField ==. protocol &&.
          ProtocolIndex_chainIdField ==. chainId)
      case mbProtoInfo of
        Nothing -> throwError $ KilnRpcError_UnknownProtocol protocol
        Just protoInfo -> pure $ endOfPreservedCycles (latestFinalHeadInfo ^. level + 2)
          -- Note that using @mod@ below is safe because all computations are done within the @protoInfo@
          -- known for the latest final head
          ((latestFinalHeadInfo ^. branchInfo_cyclePosition + 2) `mod` (protoInfo ^. protoInfo_blocksPerCycle)) protoInfo

  let
    maxProgress = maxProgress_rightsInfo ^? _Right . _Just
    bakerHashes :: Pg.In [PublicKeyHash] = Pg.In $ Map.keys bakers
    -- Insert pkh from Internal if present
    bakers = Map.union (fmap (\(b, li, c) -> (Right (BakerInternalData li b), c)) internalBakerData) $
      fmap (\(a, c) -> (Left (BakerData a), (c, False))) bakersAlertCount

  nextBakes <- case latestHeadLevel of
    Nothing -> pure Map.empty
    Just headLevel -> fmap (Map.fromList . map (\(pkh, pr, mbLvl) -> (pkh, (pr, mbLvl))))
      [queryQ|
        SELECT brcp."publicKeyHash",
          ( SELECT MAX(progress) -- this is a subselect so that we get the highest result even if "BakerRight" rows are found
            FROM "BakerRightsProgress" b1
            WHERE b1."publicKeyHash" = brcp."publicKeyHash"
              AND b1."chainId" = ?chainId
          ), MIN(br.level)
        FROM "BakerRightsProgress" brcp
        LEFT OUTER JOIN "BakerRight" br
          ON br.branch = brcp.id
          AND br."right" = 'RightKind_Baking'
          AND br.level > ?headLevel
        WHERE brcp."chainId" = ?chainId
          AND brcp."publicKeyHash" in ?bakerHashes
        GROUP BY brcp."publicKeyHash"
      |]

  let
    getNextRight mbBakeLvl progress insufficientFunds = case mbBakeLvl of
      Nothing ->
        if insufficientFunds
          then BakerNextRight_KnownNoRights
          else case subtract progress <$> maxProgress of
            Just 0 -> BakerNextRight_WaitingForRights
            Just _ -> BakerNextRight_GatheringData
            Nothing -> BakerNextRight_GatheringData
            -- if maxProgress is Nothing, then we don't yet have enough history to say much of
            -- anything about how much work we still need to do per baker
      Just bakeLvl -> BakerNextRight_BakeBlock bakeLvl

    result = fmap (bimap Bounded (First . Just)) $ Map.toList $ Map.mapMaybe id $ alignWith
      (these
        (\(b, (alertCount, _)) -> Just $ BakerSummary b alertCount BakerNextRight_GatheringData)
        (const Nothing)
        (\(b, (alertCount, insufficientFunds)) (progress, mbBakeLvl) -> Just $ BakerSummary b alertCount (getNextRight mbBakeLvl progress insufficientFunds))
      ) bakers nextBakes

  return result

getNodeVersions
  :: forall m. (Monad m, MonadIO m, PostgresRaw m, MonadLogger m, PersistBackend m)
  => Http.Manager
  -> URI
  -> Maybe (Id Node)
  -> m [(WithInfinity (Id Node), Maybe TezosVersion)]
getNodeVersions httpMgr internalUri = getNodeAddresses >=> (mapM . mapM) retrieveVersion
  where
    retrieveVersion :: Deletable NodeSummary -> m (Maybe TezosVersion)
    retrieveVersion dn = case getFirst dn of
       Nothing -> pure Nothing
       Just ns -> case _nodeSummary_node ns of
        Right _internal -> versionWorker httpMgr $ T.unpack $ render internalUri
        Left (_nodeExternalData_address -> extUri) -> versionWorker httpMgr (T.unpack $ render extUri)

versionWorker :: MonadIO m => Http.Manager -> String -> m (Maybe TezosVersion)
versionWorker httpMgr baseUrl = do
    let versionUrl = ensure baseUrl "version"
    versionResp' <- doRequestLBS httpMgr versionUrl
    either (const doCommit) (maybe doCommit return . decode' . Http.getResponseBody) versionResp'
  where
    ensure :: String -> String -> String
    ensure base path = dropWhileEnd (== '/') base <> "/" <> path

    doCommit = do
        let commitUrl = ensure baseUrl "monitor/commit_hash"
        commitResp' <- doRequestLBS httpMgr commitUrl
        return $ either (const Nothing) (decode' . Http.getResponseBody) commitResp'

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
      SELECT n.id, p.control, p.state, p.updated AT TIME ZONE 'UTC', p.backend, p."errorLog", p."restartCount", p."restartAt" AT TIME ZONE 'UTC'
        FROM "NodeInternal" n
        JOIN "ProcessData" p ON p.id = n."data#data"
      WHERE NOT n."data#deleted"
        AND CASE WHEN ?nid is NULL THEN true ELSE n.id = ?nid END|]
    <&> Map.fromList . fmap (\(nid', control, state, updated, backend, errorLog, restartCount, restartAt) -> (Bounded nid',
      ProcessData
      { _processData_control = control
      , _processData_state = state
      , _processData_updated = updated
      , _processData_backend = backend
      , _processData_errorLog = errorLog
      , _processData_restartCount = restartCount
      , _processData_restartAt = restartAt
      }))
  let qCount :: [Utf8]
      qCount = flip map universe $ \(Some nTag) -> logAssume (LogTag_Node nTag) $ case nodeLogDep nTag of
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

getProposals :: (Monad m, PostgresRaw m) => m [(Id PeriodProposal, (PeriodProposal, Maybe Bool))]
getProposals = do
  results <- [queryQ|
    SELECT pp.id, pp.hash, pp."chainId", pp."votingPeriod", pp.votes, bp.pkh IS NOT NULL, bp.included IS NOT NULL
    FROM "PeriodProposal" pp
    LEFT JOIN "BakerProposal" bp ON pp.id = bp.proposal
    LEFT JOIN "BakerDaemonInternal" b ON bp.pkh = b."data#data#publicKeyHash"
    WHERE COALESCE(NOT b."data#deleted", TRUE)
  |]
  pure $ flip fmap results $ \(pid, phash, chain, vp, votes, voted, included) -> (pid, (PeriodProposal
    { _periodProposal_hash = phash
    , _periodProposal_chainId = chain
    , _periodProposal_votingPeriod = vp
    , _periodProposal_votes = votes
    }, if voted then Just included else Nothing))
