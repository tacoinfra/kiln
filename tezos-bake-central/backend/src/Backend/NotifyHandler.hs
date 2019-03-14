{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.NotifyHandler where

import Control.Lens
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Concurrent.STM (atomically)
import Data.Dependent.Sum (DSum(..))
import qualified Data.Map.Monoidal as MMap
import Database.Groundhog.Postgresql (PersistBackend, get, project, (==.), Cond(..))
import Rhyolite.Backend.DB (runDb, selectMap')
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw)
import Rhyolite.Backend.Listen (DbNotification (..))
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Backend.Schema.Class (DefaultKeyUnique)
import Rhyolite.Schema (Id (..))

import Tezos.Types

import Backend.BalanceTracking
import Backend.CachedNodeRPC
-- import Backend.Graphs
import Backend.Schema
import Backend.ViewSelectorHandler (getAlertCount, getNodeAddresses, getBakerAddresses)
import Common.App (BakeView (..), BakeViewSelector (..), Deletable,
                   NodeSummary (..), BakerSummary (..), SetupState (..),
                   nodeIdForNodeErrorLogView, nodeErrorViewOnly,
                   mailServerConfigToView, Deletable, BakerSummary)
import Common.App (bakerErrorViewOnly)
import Common.App (bakerIdForBakerErrorLogView)
import Common.App (errorLogIdForErrorLogView)
import Common.Alerts (alertsFilter)
import Common.Schema
import Common.Vassal
import ExtraPrelude

notifyHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a)
  => NodeDataSource
  -> DbNotification NotifyTag
  -> BakeViewSelector a
  -> m (BakeView a)
notifyHandler nds notification aggVS = runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity $ _nodeDataSource_pool nds) $
  --  $(logDebugS) "NotifyHandler" (T.decodeUtf8 $ LBS.toStrict $ Aeson.encode $ _notifyMessage_value notifyMessage) *>
  case _dbNotification_message notification of
    NotifyTag_BakerDaemonExternal :=> Identity (eid, mBaker) -> handleClient eid mBaker
    NotifyTag_Baker :=> Identity (bid, mBaker) -> handleBaker bid mBaker
    NotifyTag_BakerDetails :=> Identity bakerDetails -> handleBakerDetails bakerDetails
    NotifyTag_BakerRightsProgress :=> Identity (_x, y, _z) -> handleBakerAddress (_bakerRightsCycleProgress_publicKeyHash y)
    NotifyTag_ErrorLog tag :=> Identity eid ->
      logAssume tag $ handleErrorLog (errorLogIdForErrorLogView . (tag :=>) . Identity) tag eid
    NotifyTag_MailServerConfig :=> Identity (_eid, cfg) -> handleMailServer cfg
    NotifyTag_NodeExternal :=> Identity (eid, ent) -> (<>) <$> handleNodeExternal eid ent <*> alsoEveryBakerSummary
    NotifyTag_NodeInternal :=> Identity (eid, ent) -> (<>) <$> handleNodeInternal eid ent <*> alsoEveryBakerSummary
    NotifyTag_NodeDetails :=> Identity (eid, ent) -> (<>) <$> handleNodeDetails eid ent <*> alsoEveryBakerSummary
    NotifyTag_Notificatee :=> _eid -> handleNotificatee
    NotifyTag_Parameters :=> Identity (_eid, ent) -> handleParameters ent
    NotifyTag_PublicNodeConfig :=> Identity (_eid, ent) -> handlePublicNodeConfig ent
    NotifyTag_PublicNodeHead :=> Identity (eid, ent) -> handlePublicNodeHead eid ent
    NotifyTag_TelegramConfig :=> Identity (_eid, ent) -> handleTelegramConfig ent
    NotifyTag_TelegramRecipient :=> Identity (eid, ent) -> handleTelegramRecipient eid ent
    NotifyTag_UpstreamVersion :=> Identity (_eid, ent) -> handleUpstreamVersion ent
    NotifyTag_ConnectedLedger :=> Identity mli -> handleConnectedLedger mli
    NotifyTag_ShowLedger :=> Identity (sk, mpkh) -> handleShowLedger sk mpkh
    NotifyTag_Prompting :=> Identity (sk, step) -> handlePrompting sk step
  where
    clientsVS = _bakeViewSelector_clients aggVS
    clientAddressesVS = _bakeViewSelector_clientAddresses aggVS
    latestHeadVS = _bakeViewSelector_latestHead aggVS

    connectedLedgerVS = _bakeViewSelector_connectedLedger aggVS

    handleConnectedLedger :: Applicative m' => Maybe ConnectedLedger -> m' (BakeView a)
    handleConnectedLedger mli
      | viewSelects () connectedLedgerVS = pure $ mempty
        { _bakeView_connectedLedger = toMaybeView connectedLedgerVS (Just mli)
        }
      | otherwise = pure mempty

    showLedgerVS = _bakeViewSelector_showLedger aggVS
    handleShowLedger :: Applicative m' => SecretKey -> Maybe (PublicKeyHash, Tez) -> m' (BakeView a)
    handleShowLedger sk mpkh
      | viewSelects sk showLedgerVS = pure $ mempty
        { _bakeView_showLedger = toRangeView1 showLedgerVS sk $ Just $ First mpkh
        }
      | otherwise = pure mempty

    promptingVS = _bakeViewSelector_prompting aggVS
    handlePrompting :: Applicative m' => SecretKey -> Maybe SetupState -> m' (BakeView a)
    handlePrompting sk step
      | viewSelects sk promptingVS = pure $ mempty
        { _bakeView_prompting = toRangeView1 promptingVS sk $ Just $ First step
        }
      | otherwise = pure mempty

    summaryVS = _bakeViewSelector_summary aggVS

    handleClient :: PersistBackend m' => Id BakerDaemon -> Maybe BakerDaemonExternalData -> m' (BakeView a)
    handleClient cid client = whenM ( viewSelects cid clientsVS || viewSelects (Bounded cid) clientAddressesVS ) $ do
      infos :: Maybe BakerDaemonInfoData <- fmap listToMaybe $ project BakerDaemonInfo_dataField (BakerDaemonInfo_idField ==. cid)
      let
        clientsPatch = mempty
          { _bakeView_clients = toRangeView1 clientsVS cid $ Just $ First infos
          , _bakeView_clientAddresses = toRangeView1 clientAddressesVS (Bounded cid) $ Just $ First $ _bakerDaemonExternalData_address <$> client
          }
      summaryPatch <- whenM (viewSelects () summaryVS) $ do
        -- maxLevel <- getMaxLevel
        summaryReport <- getSummaryReport
        -- summaryGraph <- whenJust maxLevel $ \l -> do
        --   mGraph <- liftIO $ cumulativeRewardsGraph (fromIntegral l) (fmap (getFirst . fst) rewardMap)
        --   return $ single mGraph a
        return $ mempty
          { _bakeView_summary = toMaybeView (_bakeViewSelector_summary aggVS) summaryReport
          }
      return $ clientsPatch <> summaryPatch

    paramsVS = _bakeViewSelector_parameters aggVS

    handleParameters :: Applicative m' => Parameters -> m' (BakeView a)
    handleParameters params =
      -- bakerStatsV iew <- flip runReaderT nds $ withCache mempty $ \_protoInfo ->
      --   calculateBakerStats (_bakeViewSelector_bakerStats aggVS)
      whenM (viewSelects () paramsVS) $
        pure $ mempty
          { _bakeView_parameters = toMaybeView paramsVS $ Just $ _parameters_protoInfo params
          -- , _bakeView_bakerStats = bakerStatsView
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
        Just _ -> getNodeAddresses (Just $ nid)
      pure $ mempty { _bakeView_nodeAddresses = toRangeView nodeAddressesVS nodeExternalV }

    {-# INLINE handleNodeInternal #-}
    handleNodeInternal
      :: (Monad m', PostgresRaw m', MonadLogger m', PersistBackend m')
      => Id Node -> Maybe ProcessData -> m' (BakeView a)
    handleNodeInternal nid mProcessData = whenM (viewSelects (Bounded nid) nodeAddressesVS) $ do
      nodeInternalV <- case mProcessData of
        Nothing -> pure [(Bounded nid, First Nothing)]
        Just _ -> getNodeAddresses (Just $ nid)
      pure $ mempty { _bakeView_nodeAddresses = toRangeView nodeAddressesVS nodeInternalV }

    handleNodeDetails :: (MonadIO m') => Id Node -> Maybe NodeDetailsData -> m' (BakeView a)
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

    handleBaker :: (Monad m', MonadIO m', MonadLogger m', PersistBackend m', PostgresRaw m')
                => Id Baker -> Maybe BakerData -> m' (BakeView a)
    handleBaker (Id pkh) mBaker = whenM (viewSelects (Bounded pkh) bakerAddressesVS) $
      case mBaker of
        -- fast path
        Nothing -> pure mempty {
          _bakeView_bakerAddresses = toRangeView bakerAddressesVS [(Bounded pkh, First Nothing)]
          }
        Just _ -> handleBakerAddress pkh

    handleBakerAddress :: (Monad m', MonadIO m', MonadLogger m', PersistBackend m', PostgresRaw m')
                       => PublicKeyHash -> m' (BakeView a)
    handleBakerAddress pkh  = whenM (viewSelects (Bounded pkh) bakerAddressesVS) $ do
      bakerV <- getBakerAddresses nds (Just $ pkh)
      pure mempty { _bakeView_bakerAddresses = toRangeView bakerAddressesVS bakerV }

    -- this is a kludge; id really like a way to send only things that are "new information" to the frontend.
    alsoEveryBakerSummary :: (Monad m', MonadIO m', MonadLogger m', PersistBackend m', PostgresRaw m') => m' (BakeView a)
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
        { _bakeView_mailServer = toMaybeView mailServerVS $ Just $ Just $ flip mailServerConfigToView notificatees $ mailServer
        }

    handleErrorLog
      :: forall e m2. (EntityWithIdBy (DefaultKeyUnique e) e, MonadIO m2, MonadLogger m2, PersistBackend m2, PostgresRaw m2)
      => (e -> Id ErrorLog) -> LogTag e -> Id e -> m2 (BakeView a)
    handleErrorLog = handleErrorLog' (const $ pure mempty)

    alertCountVS = _bakeViewSelector_alertCount aggVS
    handleErrorLog'
      :: forall e m2
      . (EntityWithIdBy (DefaultKeyUnique e) e, MonadIO m2, MonadLogger m2, PersistBackend m2, PostgresRaw m2)
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
        alertCount <- getAlertCount
        pure mempty
          { _bakeView_alertCount = toMaybeView alertCountVS alertCount
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

          pure $ flip ifoldMap (_bakeViewSelector_errors aggVS)$ \flt errorsVS ->
            if viewSelects errorInterval errorsVS
            then mempty
              { _bakeView_errors = MMap.singleton flt $ IntervalView (unIntervalSelector errorsVS) $ -- see comment on instance Semigroup (IntervalView) for why this is "legit"
                  MMap.singleton logId $ First (First $ alertsFilter fst flt $ Just (errorLog, toView specificLog), errorInterval)
              }
            else mempty
      userSupplied <- maybe (pure mempty) k specificLog'
      return $ newCount <> newErrors <> fold logNodeSummary <> fold logBakerSummary <> userSupplied

    publicNodeConfigVS = _bakeViewSelector_publicNodeConfig aggVS

    handlePublicNodeConfig :: Applicative m' => PublicNodeConfig -> m' (BakeView a)
    handlePublicNodeConfig pnc =
      whenM (viewSelects (_publicNodeConfig_source pnc) publicNodeConfigVS) $
        pure $ mempty { _bakeView_publicNodeConfig = toRangeView1 publicNodeConfigVS (_publicNodeConfig_source pnc) (Just pnc) }

    publicNodeHeadsVS = _bakeViewSelector_publicNodeHeads aggVS

    handlePublicNodeHead :: (Monad m', MonadIO m') => Id PublicNodeHead -> Maybe PublicNodeHead -> m' (BakeView a)
    handlePublicNodeHead nid pnh = mconcat <$> sequence
      [ whenM (viewSelects (Bounded nid) publicNodeHeadsVS) $ do
          pure $ mempty { _bakeView_publicNodeHeads = toRangeView1 publicNodeHeadsVS (Bounded nid) pnh }
      , whenM (viewSelects () latestHeadVS) $ do
          latestHead <- liftIO $ atomically $ dataSourceHead nds
          pure $ mempty { _bakeView_latestHead = toMaybeView latestHeadVS latestHead }
      ]

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
