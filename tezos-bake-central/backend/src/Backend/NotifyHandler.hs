{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -Werror -fmax-pmcheck-iterations=100000000 #-}

module Backend.NotifyHandler where

import Control.Lens
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import Control.Monad.Catch (MonadMask)
import Control.Concurrent.STM (atomically)
import Data.Dependent.Map (DSum(..), Some (..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (sconcat)
import Database.Groundhog.Postgresql (Postgresql(..), get, (&&.), (==.), Cond(..))
import Database.Id.Class
import Database.Id.Groundhog
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (runDb, selectMap', selectSingle)
import Rhyolite.Backend.DB.Serializable
import Rhyolite.Backend.Listen (DbNotification (..))
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Backend.Schema.Class (DefaultKeyUnique)

import Tezos.Common.NodeRPC.Sources (PublicNode)
import Tezos.Types

import Backend.CachedNodeRPC
import Backend.Schema
import Backend.ViewSelectorHandler (getAlertCount, getNodeAddresses, getBakerAddresses, getBakerAlert)
import Common.App (BakeView (..), BakeViewSelector (..), Deletable,
                   NodeSummary (..), BakerSummary (..), SetupState (..), VoteState,
                   nodeIdForNodeErrorLogView, nodeErrorViewOnly,
                   mailServerConfigToView, Deletable, BakerSummary)
import Common.App (bakerErrorViewOnly)
import Common.App (bakerIdForBakerErrorLogView)
import Common.App (errorLogIdForErrorLogView)
import qualified Common.AppendIntervalMap as AppendIMap
import Common.Alerts (alertsFilter)
import Common.Schema
import Common.Vassal
import ExtraPrelude

notifyHandler
  :: forall m a. (MonadBaseNoPureAborts IO m, MonadIO m, Monoid a, MonadMask m)
  => NodeDataSource
  -> DbNotification NotifyTag
  -> BakeViewSelector a
  -> m (BakeView a)
notifyHandler nds notification aggVS = runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity $ _nodeDataSource_pool nds) $
  --  $(logDebugS) "NotifyHandler" (T.decodeUtf8 $ LBS.toStrict $ Aeson.encode $ _notifyMessage_value notifyMessage) *>
  case _dbNotification_message notification of
    NotifyTag_Baker :=> Identity (bid, mBaker) -> handleBaker bid mBaker
    NotifyTag_BakerDetails :=> Identity bakerDetails -> handleBakerDetails bakerDetails
    NotifyTag_BakerRightsProgress :=> Identity (_x, y, _z) -> handleBakerAddress (_bakerRightsCycleProgress_publicKeyHash y)
    NotifyTag_ErrorLog tag :=> Identity eid ->
      logAssume tag $ handleErrorLog (errorLogIdForErrorLogView . (tag :=>) . Identity) tag eid
    NotifyTag_ProtocolIndex :=> Identity eid -> handleParameters eid
    NotifyTag_MailServerConfig :=> Identity (_eid, cfg) -> handleMailServer cfg
    NotifyTag_NodeExternal :=> Identity (eid, ent) -> (<>) <$> handleNodeExternal eid ent <*> alsoEveryBakerSummary
    NotifyTag_NodeInternal :=> Identity (eid, ent) -> (<>) <$> handleNodeInternal eid ent <*> alsoEveryBakerSummary
    NotifyTag_NodeDetails :=> Identity (eid, ent) -> (<>) <$> handleNodeDetails eid ent <*> alsoEveryBakerSummary
    NotifyTag_Notificatee :=> _eid -> handleNotificatee
    NotifyTag_PublicNodeConfig :=> Identity (_eid, ent) -> handlePublicNodeConfig ent
    NotifyTag_PublicNodeHead :=> Identity (eid, ent) -> handlePublicNodeHead eid ent
    NotifyTag_SnapshotMeta :=> Identity ent -> handleSnapshotMeta ent
    NotifyTag_TelegramConfig :=> Identity (_eid, ent) -> handleTelegramConfig ent
    NotifyTag_TelegramRecipient :=> Identity (eid, ent) -> handleTelegramRecipient eid ent
    NotifyTag_UpstreamVersion :=> Identity (_eid, ent) -> handleUpstreamVersion ent
    NotifyTag_ConnectedLedger :=> Identity mli -> handleConnectedLedger mli
    NotifyTag_ShowLedger :=> Identity (sk, mpkh) -> handleShowLedger sk mpkh
    NotifyTag_Prompting :=> Identity (sk, step) -> handlePrompting sk step
    NotifyTag_VotePrompting :=> Identity (sk, step) -> handleVotePrompting sk step
    NotifyTag_RightNotificationSettings :=> Identity (rk, mrnl) -> handleRightNotificationSettings rk mrnl
    NotifyTag_Amendment :=> Identity (k, ma) -> handleAmendment k ma
    NotifyTag_Proposals :=> Identity (pid, mp) -> handleProposals pid mp
    NotifyTag_PeriodTestingVote :=> Identity ma -> handlePeriodTestingVote ma
    NotifyTag_PeriodTesting :=> Identity ma -> handlePeriodTesting ma
    NotifyTag_PeriodPromotionVote :=> Identity ma -> handlePeriodPromotionVote ma
    NotifyTag_PeriodAdoption :=> Identity ma -> handlePeriodAdoption ma
    NotifyTag_BakerVote :=> Identity ma -> handleBakerVote ma
    NotifyTag_BakerRegistered :=> Identity (pkh, b) -> handleBakerRegistered pkh b
    NotifyTag_NodeVersion :=> Identity (nid, mtzversion) -> handleTezosVersion mtzversion nid
    NotifyTag_LatestTezosRelease :=> Identity mlatestTezosRelease -> handleLatestTezosRelease mlatestTezosRelease
  where

    latestTezosReleaseVS = _bakeViewSelector_latestTezosRelease aggVS

    handleLatestTezosRelease :: Maybe MajorMinorVersion -> Serializable (BakeView a)
    handleLatestTezosRelease ver = whenM (viewSelects () latestTezosReleaseVS) $ do
        pure $ mempty { _bakeView_latestTezosRelease = toMaybeView latestTezosReleaseVS $ Just ver }

    nodeVersionsVS = _bakeViewSelector_nodeVersions aggVS

    publicVersionsVS = _bakeViewSelector_publicVersions aggVS

    publicNodesVS = _bakeViewSelector_publicNodeConfig aggVS

    handleTezosVersion :: Maybe TezosVersion -> Either PublicNode (Id Node) -> Serializable (BakeView a)
    handleTezosVersion tv  = \case
        Left publicNode -> whenM (viewSelects publicNode publicNodesVS) $
            pure $ mempty { _bakeView_publicVersions = toRangeView publicVersionsVS [(Bounded publicNode, tv)] }
        Right nid -> whenM (viewSelects (Bounded nid) nodeAddressesVS) $
            pure $ mempty { _bakeView_nodeVersions = toRangeView nodeVersionsVS [(Bounded nid, tv)] }

    latestHeadVS = _bakeViewSelector_latestHead aggVS

    connectedLedgerVS = _bakeViewSelector_connectedLedger aggVS

    handleConnectedLedger :: Maybe ConnectedLedger -> Serializable (BakeView a)
    handleConnectedLedger mli
      | viewSelects () connectedLedgerVS = pure $ mempty
        { _bakeView_connectedLedger = toMaybeView connectedLedgerVS (Just mli)
        }
      | otherwise = pure mempty

    showLedgerVS = _bakeViewSelector_showLedger aggVS
    handleShowLedger :: SecretKey -> Maybe (PublicKeyHash, Tez) -> Serializable (BakeView a)
    handleShowLedger sk mpkh
      | viewSelects sk showLedgerVS = pure $ mempty
        { _bakeView_showLedger = toRangeView1 showLedgerVS sk $ Just $ First mpkh
        }
      | otherwise = pure mempty

    promptingVS = _bakeViewSelector_prompting aggVS
    handlePrompting :: SecretKey -> Maybe SetupState -> Serializable (BakeView a)
    handlePrompting sk step
      | viewSelects sk promptingVS = pure $ mempty
        { _bakeView_prompting = toRangeView1 promptingVS sk $ Just $ First step
        }
      | otherwise = pure mempty

    votePromptingVS = _bakeViewSelector_votePrompting aggVS
    handleVotePrompting :: SecretKey -> Maybe VoteState -> Serializable (BakeView a)
    handleVotePrompting sk step
      | viewSelects sk votePromptingVS = pure $ mempty
        { _bakeView_votePrompting = toRangeView1 votePromptingVS sk $ Just $ First step
        }
      | otherwise = pure mempty

    paramsVS = _bakeViewSelector_parameters aggVS

    handleParameters :: Id ProtocolIndex -> Serializable (BakeView a)
    handleParameters (Id (chainId, protoHash)) = whenM (viewSelects protoHash paramsVS) $ do
      newProto :: Maybe ProtocolIndex <- selectSingle $
        ProtocolIndex_hashField ==. protoHash &&.
        ProtocolIndex_chainIdField ==. chainId
      pure mempty
        { _bakeView_parameters = MapView $ mempty $
          liftA2 (\v p -> MMap.singleton protoHash (First p, v)) (MMap.lookup protoHash $ unMapSelector paramsVS) newProto
        }

    nodeAddressesVS :: RangeSelector' (Id Node) (Deletable NodeSummary) a
    nodeAddressesVS = _bakeViewSelector_nodeAddresses aggVS
    nodeDetailsVS = _bakeViewSelector_nodeDetails aggVS

    {-# INLINE handleNodeExternal #-}
    handleNodeExternal ::
      -- :: (Monad m', PostgresRaw m', MonadLogger m', PersistBackend m')
      Id Node -> Maybe NodeExternalData -> Serializable (BakeView a)
    handleNodeExternal nid mNodeExternalData = whenM (viewSelects (Bounded nid) nodeAddressesVS) $ do
      nodeExternalV <- case mNodeExternalData of
        Nothing -> pure [(Bounded nid, First Nothing)]
        Just _ -> getNodeAddresses (Just nid)
      pure $ mempty { _bakeView_nodeAddresses = toRangeView nodeAddressesVS nodeExternalV }

    {-# INLINE handleNodeInternal #-}
    handleNodeInternal ::
      -- :: (Monad m', PostgresRaw m', MonadLogger m', PersistBackend m')
      Id Node -> Maybe ProcessData -> Serializable (BakeView a)
    handleNodeInternal nid mProcessData = whenM (viewSelects (Bounded nid) nodeAddressesVS) $ do
      nodeInternalV <- case mProcessData of
        Nothing -> pure [(Bounded nid, First Nothing)]
        Just _ -> getNodeAddresses (Just nid)
      pure $ mempty { _bakeView_nodeAddresses = toRangeView nodeAddressesVS nodeInternalV }

    handleNodeDetails :: Id Node -> Maybe NodeDetailsData -> Serializable (BakeView a)
    handleNodeDetails nid mNodeDetailsData = mconcat <$> sequence
      [ whenM (viewSelects (Bounded nid) nodeDetailsVS) $
        pure $ mempty
          { _bakeView_nodeDetails = toRangeView1 nodeDetailsVS (Bounded nid) mNodeDetailsData
          }
      , whenM (viewSelects () latestHeadVS) $ do
          latestHead <- liftIO $ atomically $ dataSourceHead nds
          pure mempty { _bakeView_latestHead = toMaybeView latestHeadVS latestHead }
      ]

    bakerAddressesVS = _bakeViewSelector_bakerAddresses aggVS
    bakerDetailsVS = _bakeViewSelector_bakerDetails aggVS
      -- TODO: shove PKH in the NotifyMessage body so we can sample the
      -- viewselector without making a trip to the database and this whole
      -- thing can live in a withM (viewSelects ...)

    -- handleBaker :: (Monad m', MonadIO m', MonadLogger m', PersistBackend m', PostgresRaw m', MonadMask m')
    handleBaker :: Id Baker -> Maybe BakerData -> Serializable (BakeView a)
    handleBaker (Id pkh) mBaker = whenM (viewSelects (Bounded pkh) bakerAddressesVS) $
      case mBaker of
        -- fast path
        Nothing -> pure mempty {
          _bakeView_bakerAddresses = toRangeView bakerAddressesVS [(Bounded pkh, First Nothing)]
          }
        Just _ -> handleBakerAddress pkh

    -- handleBakerAddress :: (Monad m', MonadIO m', MonadLogger m', PersistBackend m', PostgresRaw m', MonadMask m')
    handleBakerAddress :: PublicKeyHash -> Serializable (BakeView a)
    handleBakerAddress pkh  = whenM (viewSelects (Bounded pkh) bakerAddressesVS) $ do
      bakerV <- getBakerAddresses nds (Just pkh)
      pure mempty { _bakeView_bakerAddresses = toRangeView bakerAddressesVS bakerV }

    -- this is a kludge; id really like a way to send only things that are "new information" to the frontend.
    -- alsoEveryBakerSummary :: (Monad m', MonadIO m', MonadLogger m', PersistBackend m', PostgresRaw m', MonadMask m') => m' (BakeView a)
    alsoEveryBakerSummary :: Serializable (BakeView a)
    alsoEveryBakerSummary = do
      bakerAddresses :: RangeView' PublicKeyHash (Deletable BakerSummary) a <- whenM (not $ null bakerAddressesVS) $
        toRangeView bakerAddressesVS <$> getBakerAddresses nds Nothing
      whenM (not $ null bakerAddresses) $
        (\x -> mempty {_bakeView_bakerAddresses = x}) . toRangeView bakerAddressesVS <$> getBakerAddresses nds Nothing

    -- handleBakerDetails :: Monad m' => BakerDetails -> m' (BakeView a)
    handleBakerDetails :: BakerDetails -> Serializable (BakeView a)
    handleBakerDetails bakerDetails = whenM (viewSelects (Bounded $ _bakerDetails_publicKeyHash bakerDetails) bakerDetailsVS) $
      pure $ mempty
        { _bakeView_bakerDetails = toRangeView1
            bakerDetailsVS
            (Bounded $ _bakerDetails_publicKeyHash bakerDetails)
            (Just $ First $ Just bakerDetails)
        }

    mailServerVS = _bakeViewSelector_mailServer aggVS

    handleNotificatee :: Serializable (BakeView a)
    handleNotificatee = whenM (viewSelects () mailServerVS) $ do
      notificatees <- fmap _notificatee_email . toList <$> selectMap' NotificateeConstructor CondEmpty
      -- TODO: do something a little more reasonable that 'listToMaybe'  what happens if there *are* more than one serverConfig?
      mailServer :: Maybe MailServerConfig <- listToMaybe . toList <$> selectMap' MailServerConfigConstructor CondEmpty
      pure $ (mempty :: BakeView a)
        { _bakeView_mailServer = toMaybeView mailServerVS $ Just $ flip mailServerConfigToView notificatees <$> mailServer
        }

    handleMailServer :: MailServerConfig -> Serializable (BakeView a)
    handleMailServer mailServer = whenM (viewSelects () mailServerVS) $ do
      notificatees <- fmap _notificatee_email . toList <$> selectMap' NotificateeConstructor CondEmpty
      pure $ (mempty :: BakeView a)
        { _bakeView_mailServer = toMaybeView mailServerVS $ Just $ Just $ mailServerConfigToView mailServer notificatees
        }

    handleErrorLog
      :: forall e. (EntityWithIdBy (DefaultKeyUnique e) e)
      => (e -> Id ErrorLog) -> LogTag e -> Id e -> Serializable (BakeView a)
    handleErrorLog = handleErrorLog' (const $ pure mempty)

    alertCountVS = _bakeViewSelector_alertCount aggVS
    handleErrorLog'
      :: forall e
      . (EntityWithIdBy (DefaultKeyUnique e) e)
      => (e -> Serializable (BakeView a))
      -> (e -> Id ErrorLog)
      -> LogTag e
      -> Id e
      -> Serializable (BakeView a)
    handleErrorLog' k getLogId tag specificLogId = do
      let toView logBody = tag :=> Identity logBody
      -- TODO: shove a time range, or perhaps an (Id ErrorLog) in the
      -- message body so that we can avoid doing some of the work if it
      -- won't be observed
      specificLog' :: Maybe e <- getIdBy specificLogId
      logNodeSummary <- for (fmap nodeIdForNodeErrorLogView . nodeErrorViewOnly . toView =<< specificLog') $ \logNodeId -> do
        whenM (viewSelects (Bounded logNodeId) nodeAddressesVS) $ do
          newNodeCounts <- getNodeAddresses $ Just logNodeId
          pure mempty
            { _bakeView_nodeAddresses = toRangeView nodeAddressesVS newNodeCounts
            }
      logBakerSummary <- for (fmap bakerIdForBakerErrorLogView . bakerErrorViewOnly . toView =<< specificLog') $ \logBakerId -> do
        whenM (viewSelects (Bounded logBakerId) bakerAddressesVS) $ do
          newBakerCounts <- getBakerAddresses nds $ Just logBakerId
          pure mempty
            { _bakeView_bakerAddresses = toRangeView bakerAddressesVS newBakerCounts
            }
      newCount <- whenM (viewSelects () alertCountVS) $ do
        alertCount <- getAlertCount (_nodeDataSource_chain nds)
        pure mempty
          { _bakeView_alertCount = toMaybeView alertCountVS (Just alertCount)
          }
      newErrors <- whenJust specificLog' $ \specificLog -> do
        let logId = getLogId specificLog
        errorLog' :: Maybe ErrorLog <- get $ fromId logId
        whenJust errorLog' $ \errorLog -> do
          let
            -- todo: Common.App.getErrorInterval does this already
            errorInterval = ClosedInterval
                  (Bounded $ _errorLog_started errorLog)
                  (maybe UpperInfinity Bounded $ _errorLog_stopped errorLog)

          pure $ flip ifoldMap (_bakeViewSelector_errors aggVS)$ \flt (Compose errorsVS) ->
            let
              tagKey = Some tag
              mErrorsIntervalVS = MMap.lookup tagKey $ unMapSelector errorsVS
              ma = sconcat <$> (NEL.nonEmpty . AppendIMap.elems . unIntervalSelector =<< mErrorsIntervalVS)
              makeBakeView a errorsIntervalVS = if viewSelects errorInterval errorsIntervalVS
                then mempty
                  { _bakeView_errors = MMap.singleton flt $ ComposeView
                      (MapView $ MMap.singleton tagKey (First (), a)) $
                      Compose $ MMap.singleton tagKey $ IntervalView (unIntervalSelector errorsIntervalVS) $ -- see comment on instance Semigroup (IntervalView) for why this is "legit"
                      MMap.singleton logId $ First (First $ alertsFilter fst flt $ Just (errorLog, toView specificLog), errorInterval)
                  }
                else mempty
            in fromMaybe mempty $ liftA2 makeBakeView ma mErrorsIntervalVS
      bakerAlerts <- for (fmap bakerIdForBakerErrorLogView . bakerErrorViewOnly . toView =<< specificLog') $ \logBakerId -> do
        let bakerAlertsVS = _bakeViewSelector_bakerAlerts aggVS
        whenM (viewSelects (Bounded logBakerId) bakerAlertsVS) $ do
          -- This could be further optimized to only fetch logBakerId' alerts
          allAlerts <- getBakerAlert (_nodeDataSource_chain nds)
          pure mempty
            { _bakeView_bakerAlerts = toRangeView1 bakerAlertsVS (Bounded logBakerId) (Just $ First $ Prelude.lookup logBakerId allAlerts)
            }
      userSupplied <- maybe (pure mempty) k specificLog'

      return $ newCount <> newErrors <> fold logNodeSummary <> fold logBakerSummary <> fold bakerAlerts <> userSupplied

    publicNodeConfigVS = _bakeViewSelector_publicNodeConfig aggVS

    handlePublicNodeConfig :: PublicNodeConfig -> Serializable (BakeView a)
    handlePublicNodeConfig pnc =
      whenM (viewSelects (_publicNodeConfig_source pnc) publicNodeConfigVS) $
        pure $ mempty { _bakeView_publicNodeConfig = toRangeView1 publicNodeConfigVS (_publicNodeConfig_source pnc) (Just pnc) }

    publicNodeHeadsVS = _bakeViewSelector_publicNodeHeads aggVS

    handlePublicNodeHead :: Id PublicNodeHead -> Maybe PublicNodeHead -> Serializable (BakeView a)
    handlePublicNodeHead nid pnh = mconcat <$> sequence
      [ whenM (viewSelects (Bounded nid) publicNodeHeadsVS) $ do
          pure $ mempty { _bakeView_publicNodeHeads = toRangeView1 publicNodeHeadsVS (Bounded nid) pnh }
      , whenM (viewSelects () latestHeadVS) $ do
          latestHead <- liftIO $ atomically $ dataSourceHead nds
          pure $ mempty { _bakeView_latestHead = toMaybeView latestHeadVS latestHead }
      ]

    snapshotMetaVS = _bakeViewSelector_snapshotMeta aggVS

    handleSnapshotMeta :: SnapshotMeta -> Serializable (BakeView a)
    handleSnapshotMeta cfg = whenM (viewSelects () snapshotMetaVS) $ do
      pure $ mempty { _bakeView_snapshotMeta = toMaybeView snapshotMetaVS $ Just cfg }

    telegramConfigVS = _bakeViewSelector_telegramConfig aggVS

    handleTelegramConfig :: TelegramConfig -> Serializable (BakeView a)
    handleTelegramConfig cfg = whenM (viewSelects () telegramConfigVS) $ do
      pure $ mempty { _bakeView_telegramConfig = toMaybeView telegramConfigVS $ Just $ Just cfg }

    telegramRecipientsVS = _bakeViewSelector_telegramRecipients aggVS

    handleTelegramRecipient :: Id TelegramRecipient -> Maybe TelegramRecipient -> Serializable (BakeView a)
    handleTelegramRecipient rid recipient = whenM (viewSelects (Bounded rid) telegramRecipientsVS) $ do
      pure $ mempty
        { _bakeView_telegramRecipients = toRangeView1 telegramRecipientsVS (Bounded rid) (Just $ First recipient) }

    upgradeVS = _bakeViewSelector_upstreamVersion aggVS

    handleUpstreamVersion :: UpstreamVersion -> Serializable (BakeView a)
    handleUpstreamVersion ent = whenM (viewSelects () upgradeVS) $ do
      pure $ mempty { _bakeView_upstreamVersion = toMaybeView upgradeVS (Just ent) }

    rightNotificationSettingsVS = _bakeViewSelector_rightNotificationSettings aggVS
    handleRightNotificationSettings :: RightKind -> Maybe RightNotificationLimit -> Serializable (BakeView a)
    handleRightNotificationSettings rk mrnl
      | viewSelects rk rightNotificationSettingsVS = pure $ mempty
        { _bakeView_rightNotificationSettings = toRangeView1 rightNotificationSettingsVS rk $ Just $ First mrnl
        }
      | otherwise = pure mempty

    amendmentVS = _bakeViewSelector_amendment aggVS
    handleAmendment :: VotingPeriodKind -> Maybe Amendment -> Serializable (BakeView a)
    handleAmendment k ma = whenM (viewSelects k amendmentVS) $ do
      pure $ mempty { _bakeView_amendment = toRangeView1 amendmentVS k (Just $ First ma) }

    proposalsVS = _bakeViewSelector_proposals aggVS
    handleProposals :: Id PeriodProposal -> Maybe (PeriodProposal, Maybe Bool) -> Serializable (BakeView a)
    handleProposals pid mp
      | viewSelects (Bounded pid) proposalsVS = pure $ mempty
      { _bakeView_proposals = toRangeView1 proposalsVS (Bounded pid) $ Just $ First mp }
      | otherwise = pure mempty

    bakerVoteVS = _bakeViewSelector_bakerVote aggVS
    handleBakerVote :: Maybe BakerVote -> Serializable (BakeView a)
    handleBakerVote ma
      | viewSelects () bakerVoteVS = pure $ mempty { _bakeView_bakerVote = toMaybeView bakerVoteVS $ Just ma }
      | otherwise = pure mempty

    periodTestingVoteVS = _bakeViewSelector_periodTestingVote aggVS
    handlePeriodTestingVote :: Maybe PeriodTestingVote -> Serializable (BakeView a)
    handlePeriodTestingVote ma
      | viewSelects () periodTestingVoteVS = pure $ mempty { _bakeView_periodTestingVote = toMaybeView periodTestingVoteVS $ Just ma }
      | otherwise = pure mempty

    periodTestingVS = _bakeViewSelector_periodTesting aggVS
    handlePeriodTesting :: Maybe PeriodTesting -> Serializable (BakeView a)
    handlePeriodTesting ma
      | viewSelects () periodTestingVS = pure $ mempty { _bakeView_periodTesting = toMaybeView periodTestingVS $ Just ma }
      | otherwise = pure mempty

    periodPromotionVoteVS = _bakeViewSelector_periodPromotionVote aggVS
    handlePeriodPromotionVote :: Maybe PeriodPromotionVote -> Serializable (BakeView a)
    handlePeriodPromotionVote ma
      | viewSelects () periodPromotionVoteVS = pure $ mempty { _bakeView_periodPromotionVote = toMaybeView periodPromotionVoteVS $ Just ma }
      | otherwise = pure mempty

    periodAdoptionVS = _bakeViewSelector_periodAdoption aggVS
    handlePeriodAdoption :: Maybe PeriodAdoption -> Serializable (BakeView a)
    handlePeriodAdoption ma
      | viewSelects () periodAdoptionVS = pure $ mempty { _bakeView_periodAdoption = toMaybeView periodAdoptionVS $ Just ma }
      | otherwise = pure mempty

    bakerRegisteredVS = _bakeViewSelector_bakerRegistered aggVS
    handleBakerRegistered :: PublicKeyHash -> Bool -> Serializable (BakeView a)
    handleBakerRegistered pkh b
      | viewSelects (Bounded pkh) bakerRegisteredVS = pure $ mempty
        { _bakeView_bakerRegistered = toRangeView1 bakerRegisteredVS (Bounded pkh) (Just b)
        }
      | otherwise = pure mempty
