{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.NotifyHandler where

import Control.Lens
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger (MonadLogger)
import Control.Concurrent.STM (atomically)
import Data.Dependent.Map (DSum(..), Some (..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (sconcat)
import Data.Validation (liftError)
import Database.Groundhog.Postgresql (PersistBackend(..), Postgresql(..), get, (&&.), (==.), Cond(..))
import Database.Id.Class
import Database.Id.Groundhog
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (runDb, selectMap', selectSingle)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw(..))
import Rhyolite.Backend.Listen (DbNotification (..))
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Backend.Schema.Class (DefaultKeyUnique)

import Tezos.Types

import Backend.NodeRPC
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
  {- We run use runIdentity here to help the typechecker out. Otherwise we use a ton of memory.-}
  case _dbNotification_message notification of
    NotifyTag_Baker :=> args -> runIdentity $ uncurry handleBaker <$> args
    NotifyTag_BakerDetails :=> bakerDetails -> runIdentity $ handleBakerDetails <$> bakerDetails
    NotifyTag_BakerRightsProgress :=> args -> runIdentity $ (handleBakerAddress . _bakerRightsProgress_publicKeyHash . view _2) <$> args
    NotifyTag_ErrorLog tag :=> Identity eid ->
      logAssume tag $ handleErrorLog (errorLogIdForErrorLogView . (tag :=>) . Identity) tag eid
    NotifyTag_ProtocolIndex :=> eid -> runIdentity $ handleParameters <$> eid
    NotifyTag_MailServerConfig :=> args -> runIdentity $ handleMailServer . snd <$> args
    NotifyTag_NodeExternal :=> args -> runIdentity $ (liftA2 (flip (<>)) alsoEveryBakerSummary) . uncurry handleNodeExternal <$> args
    NotifyTag_NodeInternal :=> args -> runIdentity $ (liftA2 (flip (<>)) alsoEveryBakerSummary) . uncurry handleNodeInternal <$> args
    NotifyTag_NodeDetails :=> args -> runIdentity $ (liftA2 (flip (<>)) alsoEveryBakerSummary) . uncurry handleNodeDetails <$> args
    NotifyTag_Notificatee :=> _eid -> handleNotificatee
    NotifyTag_SnapshotMeta :=> arg -> runIdentity $ handleSnapshotMeta <$> arg
    NotifyTag_TelegramConfig :=> args -> runIdentity $ handleTelegramConfig . snd <$> args
    NotifyTag_TelegramRecipient :=> args -> runIdentity $ uncurry handleTelegramRecipient <$> args
    NotifyTag_UpstreamVersion :=> args -> runIdentity $ handleUpstreamVersion . snd <$> args
    NotifyTag_ConnectedLedger :=> args -> runIdentity $ handleConnectedLedger <$> args
    NotifyTag_ShowLedger :=> args -> runIdentity $ uncurry handleShowLedger <$> args
    NotifyTag_Prompting :=> args -> runIdentity $ uncurry handlePrompting <$> args
    NotifyTag_VotePrompting :=> args -> runIdentity $ uncurry handleVotePrompting <$> args
    NotifyTag_RightNotificationSettings :=> args -> runIdentity $ uncurry handleRightNotificationSettings <$> args
    NotifyTag_Amendment :=> args -> runIdentity $ uncurry handleAmendment <$> args
    NotifyTag_Proposals :=> args -> runIdentity $ uncurry handleProposals <$> args
    NotifyTag_PeriodTestingVote :=> arg -> runIdentity $ handlePeriodTestingVote <$> arg
    NotifyTag_PeriodTesting :=> arg -> runIdentity $ handlePeriodTesting <$> arg
    NotifyTag_PeriodPromotionVote :=> arg -> runIdentity $ handlePeriodPromotionVote <$> arg
    NotifyTag_PeriodAdoption :=> arg -> runIdentity $ handlePeriodAdoption <$> arg
    NotifyTag_BakerVote :=> arg -> runIdentity $ handleBakerVote <$> arg
    NotifyTag_BakerRegistered :=> args -> runIdentity $ uncurry handleBakerRegistered <$> args
    NotifyTag_NodeVersion :=> Identity (nid, mtzversion) -> handleTezosVersion mtzversion nid
    NotifyTag_LatestTezosRelease :=> arg -> runIdentity $ handleLatestTezosRelease <$> arg
  where

    latestTezosReleaseVS = _bakeViewSelector_latestTezosRelease aggVS

    handleLatestTezosRelease :: Applicative m' => Maybe MajorMinorVersion -> m' (BakeView a)
    handleLatestTezosRelease ver = whenM (viewSelects () latestTezosReleaseVS) $ do
        pure $ mempty { _bakeView_latestTezosRelease = toMaybeView latestTezosReleaseVS $ Just ver }

    nodeVersionsVS = _bakeViewSelector_nodeVersions aggVS

    handleTezosVersion :: Applicative m' => Maybe TezosVersion -> Id Node -> m' (BakeView a)
    handleTezosVersion tv nid = whenM (viewSelects (Bounded nid) nodeAddressesVS) $
            pure $ mempty { _bakeView_nodeVersions = toRangeView nodeVersionsVS [(Bounded nid, tv)] }

    latestHeadVS = _bakeViewSelector_latestHead aggVS

    connectedLedgerVS = _bakeViewSelector_connectedLedger aggVS

    handleConnectedLedger :: Applicative m' => Maybe ConnectedLedger -> m' (BakeView a)
    handleConnectedLedger mli
      | viewSelects () connectedLedgerVS = pure $ mempty
        { _bakeView_connectedLedger = toMaybeView connectedLedgerVS (Just mli)
        }
      | otherwise = pure mempty

    showLedgerVS = _bakeViewSelector_showLedger aggVS
    handleShowLedger :: Applicative m' => SecretKey -> Either Text (PublicKeyHash, Tez) -> m' (BakeView a)
    handleShowLedger sk epkh
      | viewSelects sk showLedgerVS = pure $ mempty
        { _bakeView_showLedger = toRangeView1 showLedgerVS sk $ Just $ liftError First epkh
        }
      | otherwise = pure mempty

    promptingVS = _bakeViewSelector_prompting aggVS
    handlePrompting :: Applicative m' => SecretKey -> Maybe SetupState -> m' (BakeView a)
    handlePrompting sk step
      | viewSelects sk promptingVS = pure $ mempty
        { _bakeView_prompting = toRangeView1 promptingVS sk $ Just $ First step
        }
      | otherwise = pure mempty

    votePromptingVS = _bakeViewSelector_votePrompting aggVS
    handleVotePrompting :: Applicative m' => SecretKey -> Maybe VoteState -> m' (BakeView a)
    handleVotePrompting sk step
      | viewSelects sk votePromptingVS = pure $ mempty
        { _bakeView_votePrompting = toRangeView1 votePromptingVS sk $ Just $ First step
        }
      | otherwise = pure mempty

    paramsVS = _bakeViewSelector_parameters aggVS

    handleParameters :: PersistBackend m' => Id ProtocolIndex -> m' (BakeView a)
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
    handleNodeExternal
      :: (Monad m', PostgresRaw m', MonadLogger m', PersistBackend m')
      => Id Node -> Maybe NodeExternalData -> m' (BakeView a)
    handleNodeExternal nid mNodeExternalData = whenM (viewSelects (Bounded nid) nodeAddressesVS) $ do
      nodeExternalV <- case mNodeExternalData of
        Nothing -> pure [(Bounded nid, First Nothing)]
        Just _ -> getNodeAddresses (Just nid)
      pure $ mempty { _bakeView_nodeAddresses = toRangeView nodeAddressesVS nodeExternalV }

    {-# INLINE handleNodeInternal #-}
    handleNodeInternal
      :: (Monad m', PostgresRaw m', MonadLogger m', PersistBackend m')
      => Id Node -> Maybe ProcessData -> m' (BakeView a)
    handleNodeInternal nid mProcessData = whenM (viewSelects (Bounded nid) nodeAddressesVS) $ do
      nodeInternalV <- case mProcessData of
        Nothing -> pure [(Bounded nid, First Nothing)]
        Just _ -> getNodeAddresses (Just nid)
      pure $ mempty { _bakeView_nodeAddresses = toRangeView nodeAddressesVS nodeInternalV }

    handleNodeDetails :: (MonadIO m') => Id Node -> Maybe NodeDetailsData -> m' (BakeView a)
    handleNodeDetails nid mNodeDetailsData = mconcat <$> sequence
      [ whenM (viewSelects (Bounded nid) nodeDetailsVS) $
        pure $ mempty
          { _bakeView_nodeDetails = toRangeView1 nodeDetailsVS (Bounded nid) mNodeDetailsData
          }
      , whenM (viewSelects () latestHeadVS) $ do
          -- Ideally we should use different bakeView for latest final head instead of
          -- reusing the existing view for latest head, but views update may cause
          -- unexpected error during the update from the previous Kiln versions.
          latestHead <- liftIO $ atomically $ dataSourceFinalHead nds
          pure mempty { _bakeView_latestHead = toMaybeView latestHeadVS latestHead }
      ]

    bakerAddressesVS = _bakeViewSelector_bakerAddresses aggVS
    bakerDetailsVS = _bakeViewSelector_bakerDetails aggVS
      -- TODO: shove PKH in the NotifyMessage body so we can sample the
      -- viewselector without making a trip to the database and this whole
      -- thing can live in a withM (viewSelects ...)

    handleBaker :: (Monad m', MonadIO m', MonadLogger m', PersistBackend m', PostgresRaw m', MonadMask m') => Id Baker -> Maybe BakerData -> m' (BakeView a)
    handleBaker (Id pkh) mBaker = whenM (viewSelects (Bounded pkh) bakerAddressesVS) $
      case mBaker of
        -- fast path
        Nothing -> pure mempty {
          _bakeView_bakerAddresses = toRangeView bakerAddressesVS [(Bounded pkh, First Nothing)]
          }
        Just _ -> handleBakerAddress pkh

    handleBakerAddress :: (Monad m', MonadIO m', MonadLogger m', PersistBackend m', PostgresRaw m', MonadMask m') => PublicKeyHash -> m' (BakeView a)
    handleBakerAddress pkh  = whenM (viewSelects (Bounded pkh) bakerAddressesVS) $ do
      bakerV <- getBakerAddresses nds (Just pkh)
      pure mempty { _bakeView_bakerAddresses = toRangeView bakerAddressesVS bakerV }

    -- this is a kludge; id really like a way to send only things that are "new information" to the frontend.
    alsoEveryBakerSummary :: (Monad m', MonadIO m', MonadLogger m', PersistBackend m', PostgresRaw m', MonadMask m') => m' (BakeView a)
    alsoEveryBakerSummary = do
      bakerAddresses :: RangeView' PublicKeyHash (Deletable BakerSummary) a <- whenM (not $ null bakerAddressesVS) $
        toRangeView bakerAddressesVS <$> getBakerAddresses nds Nothing
      whenM (not $ null bakerAddresses) $
        (\x -> mempty {_bakeView_bakerAddresses = x}) . toRangeView bakerAddressesVS <$> getBakerAddresses nds Nothing

    handleBakerDetails :: Monad m' => BakerDetails -> m' (BakeView a)
    handleBakerDetails bakerDetails = whenM (viewSelects (Bounded $ _bakerDetails_publicKeyHash bakerDetails) bakerDetailsVS) $
      pure $ mempty
        { _bakeView_bakerDetails = toRangeView1
            bakerDetailsVS
            (Bounded $ _bakerDetails_publicKeyHash bakerDetails)
            (Just $ First $ Just bakerDetails)
        }

    mailServerVS = _bakeViewSelector_mailServer aggVS

    handleNotificatee :: PersistBackend m' => m' (BakeView a)
    handleNotificatee = whenM (viewSelects () mailServerVS) $ do
      notificatees <- fmap _notificatee_email . toList <$> selectMap' NotificateeConstructor CondEmpty
      -- TODO: do something a little more reasonable that 'listToMaybe'  what happens if there *are* more than one serverConfig?
      mailServer :: Maybe MailServerConfig <- listToMaybe . toList <$> selectMap' MailServerConfigConstructor CondEmpty
      pure $ (mempty :: BakeView a)
        { _bakeView_mailServer = toMaybeView mailServerVS $ Just $ flip mailServerConfigToView notificatees <$> mailServer
        }

    handleMailServer :: PersistBackend m' => MailServerConfig -> m' (BakeView a)
    handleMailServer mailServer = whenM (viewSelects () mailServerVS) $ do
      notificatees <- fmap _notificatee_email . toList <$> selectMap' NotificateeConstructor CondEmpty
      pure $ (mempty :: BakeView a)
        { _bakeView_mailServer = toMaybeView mailServerVS $ Just $ Just $ mailServerConfigToView mailServer notificatees
        }

    handleErrorLog
      :: forall e m2. (EntityWithIdBy (DefaultKeyUnique e) e, MonadIO m2, MonadLogger m2, PersistBackend m2, PostgresRaw m2, MonadMask m2)
      => (e -> Id ErrorLog) -> LogTag e -> Id e -> m2 (BakeView a)
    handleErrorLog = handleErrorLog' (const $ pure mempty)

    alertCountVS = _bakeViewSelector_alertCount aggVS
    handleErrorLog'
      :: forall e m2
      . (EntityWithIdBy (DefaultKeyUnique e) e, MonadIO m2, MonadLogger m2, PersistBackend m2, PostgresRaw m2, MonadMask m2)
      => (e -> m2 (BakeView a))
      -> (e -> Id ErrorLog)
      -> LogTag e
      -> Id e
      -> m2 (BakeView a)
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

    snapshotMetaVS = _bakeViewSelector_snapshotMeta aggVS

    handleSnapshotMeta :: Applicative m' => SnapshotMeta -> m' (BakeView a)
    handleSnapshotMeta cfg = whenM (viewSelects () snapshotMetaVS) $ do
      pure $ mempty { _bakeView_snapshotMeta = toMaybeView snapshotMetaVS $ Just cfg }

    telegramConfigVS = _bakeViewSelector_telegramConfig aggVS

    handleTelegramConfig :: Applicative m' => TelegramConfig -> m' (BakeView a)
    handleTelegramConfig cfg = whenM (viewSelects () telegramConfigVS) $ do
      pure $ mempty { _bakeView_telegramConfig = toMaybeView telegramConfigVS $ Just $ Just cfg }

    telegramRecipientsVS = _bakeViewSelector_telegramRecipients aggVS

    handleTelegramRecipient :: Applicative m' => Id TelegramRecipient -> Maybe TelegramRecipient -> m' (BakeView a)
    handleTelegramRecipient rid recipient = whenM (viewSelects (Bounded rid) telegramRecipientsVS) $ do
      pure $ mempty
        { _bakeView_telegramRecipients = toRangeView1 telegramRecipientsVS (Bounded rid) (Just $ First recipient) }

    upgradeVS = _bakeViewSelector_upstreamVersion aggVS

    handleUpstreamVersion :: Applicative m' => UpstreamVersion -> m' (BakeView a)
    handleUpstreamVersion ent = whenM (viewSelects () upgradeVS) $ do
      pure $ mempty { _bakeView_upstreamVersion = toMaybeView upgradeVS (Just ent) }

    rightNotificationSettingsVS = _bakeViewSelector_rightNotificationSettings aggVS
    handleRightNotificationSettings :: Applicative m' => RightKind -> Maybe RightNotificationLimit -> m' (BakeView a)
    handleRightNotificationSettings rk mrnl
      | viewSelects rk rightNotificationSettingsVS = pure $ mempty
        { _bakeView_rightNotificationSettings = toRangeView1 rightNotificationSettingsVS rk $ Just $ First mrnl
        }
      | otherwise = pure mempty

    amendmentVS = _bakeViewSelector_amendment aggVS
    handleAmendment :: Applicative m' => VotingPeriodKind -> Maybe Amendment -> m' (BakeView a)
    handleAmendment k ma = whenM (viewSelects k amendmentVS) $ do
      pure $ mempty { _bakeView_amendment = toRangeView1 amendmentVS k (Just $ First ma) }

    proposalsVS = _bakeViewSelector_proposals aggVS
    handleProposals :: PersistBackend m' => Id PeriodProposal -> Maybe (PeriodProposal, Maybe Bool) -> m' (BakeView a)
    handleProposals pid mp
      | viewSelects (Bounded pid) proposalsVS = pure $ mempty
      { _bakeView_proposals = toRangeView1 proposalsVS (Bounded pid) $ Just $ First mp }
      | otherwise = pure mempty

    bakerVoteVS = _bakeViewSelector_bakerVote aggVS
    handleBakerVote :: PersistBackend m' => Maybe BakerVote -> m' (BakeView a)
    handleBakerVote ma
      | viewSelects () bakerVoteVS = pure $ mempty { _bakeView_bakerVote = toMaybeView bakerVoteVS $ Just ma }
      | otherwise = pure mempty

    periodTestingVoteVS = _bakeViewSelector_periodTestingVote aggVS
    handlePeriodTestingVote :: PersistBackend m' => Maybe PeriodTestingVote -> m' (BakeView a)
    handlePeriodTestingVote ma
      | viewSelects () periodTestingVoteVS = pure $ mempty { _bakeView_periodTestingVote = toMaybeView periodTestingVoteVS $ Just ma }
      | otherwise = pure mempty

    periodTestingVS = _bakeViewSelector_periodTesting aggVS
    handlePeriodTesting :: PersistBackend m' => Maybe PeriodTesting -> m' (BakeView a)
    handlePeriodTesting ma
      | viewSelects () periodTestingVS = pure $ mempty { _bakeView_periodTesting = toMaybeView periodTestingVS $ Just ma }
      | otherwise = pure mempty

    periodPromotionVoteVS = _bakeViewSelector_periodPromotionVote aggVS
    handlePeriodPromotionVote :: PersistBackend m' => Maybe PeriodPromotionVote -> m' (BakeView a)
    handlePeriodPromotionVote ma
      | viewSelects () periodPromotionVoteVS = pure $ mempty { _bakeView_periodPromotionVote = toMaybeView periodPromotionVoteVS $ Just ma }
      | otherwise = pure mempty

    periodAdoptionVS = _bakeViewSelector_periodAdoption aggVS
    handlePeriodAdoption :: PersistBackend m' => Maybe PeriodAdoption -> m' (BakeView a)
    handlePeriodAdoption ma
      | viewSelects () periodAdoptionVS = pure $ mempty { _bakeView_periodAdoption = toMaybeView periodAdoptionVS $ Just ma }
      | otherwise = pure mempty

    bakerRegisteredVS = _bakeViewSelector_bakerRegistered aggVS
    handleBakerRegistered :: Applicative m' => PublicKeyHash -> Bool -> m' (BakeView a)
    handleBakerRegistered pkh b
      | viewSelects (Bounded pkh) bakerRegisteredVS = pure $ mempty
        { _bakeView_bakerRegistered = toRangeView1 bakerRegisteredVS (Bounded pkh) (Just b)
        }
      | otherwise = pure mempty
