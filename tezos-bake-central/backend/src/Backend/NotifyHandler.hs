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
import Control.Monad.Logger (logWarn)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Concurrent.STM (atomically)
import Data.Aeson (fromJSON)
import qualified Data.Aeson as Aeson
import Data.Dependent.Sum (DSum(..))
import qualified Data.Map.Monoidal as MMap
import Database.Groundhog.Postgresql (AutoKeyField (..), PersistBackend, get, select, (&&.), (==.), Cond(..))
import Rhyolite.Backend.DB (runDb, selectMap')
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw)
import Rhyolite.Backend.Listen (NotifyMessage (..))
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Schema (Id (..))

import Tezos.Types

import Backend.BalanceTracking
import Backend.CachedNodeRPC
-- import Backend.Graphs
import Backend.Schema
import Backend.ViewSelectorHandler (getAlertCount, getNodeAddresses, getBakerAddresses)
import Common.App (BakeView (..), BakeViewSelector (..), Deletable,
                   NodeSummary (..), BakerSummary (..),
                   nodeIdForNodeErrorLogView, nodeErrorViewOnly,
                   mailServerConfigToView, Deletable, BakerSummary)
import Common.Alerts (alertsFilter)
import Common.Schema
import Common.Vassal
import ExtraPrelude

notifyHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a)
  => NodeDataSource
  -> NotifyMessage
  -> BakeViewSelector a
  -> m (BakeView a)
notifyHandler nds notifyMessage aggVS = runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity $ _nodeDataSource_pool nds) $
  --  $(logDebugS) "NotifyHandler" (T.decodeUtf8 $ LBS.toStrict $ Aeson.encode $ _notifyMessage_value notifyMessage) *>
  case fromJSON (_notifyMessage_value notifyMessage) of
    Aeson.Error e -> do
      $(logWarn) $ "Unable to parse NotifyMessage: " <> tshow (_notifyMessage_value notifyMessage) <> ": " <> tshow e
      pure mempty
    Aeson.Success notification -> case notification of
      Notify_Client eid -> handleClient eid
      Notify_Baker bid mBaker -> handleBaker bid mBaker
      Notify_BakerDetails bakerDetails -> handleBakerDetails bakerDetails
      Notify_BakerRightsProgress _x y _z -> handleBakerAddress (_bakerRightsCycleProgress_publicKeyHash y)
      Notify_ErrorLogBakerMissed eid -> handleErrorLog'
        (handleBakerAddress . unId . _errorLogBakerMissed_baker)
        _errorLogBakerMissed_log
        (LogTag_Baker BakerLogTag_BakerMissed)
        eid
      Notify_ErrorLogBadNodeHead eid -> handleErrorLog _errorLogBadNodeHead_log
        (LogTag_Node NodeLogTag_BadNodeHead)
        eid
      Notify_ErrorLogInaccessibleNode eid -> handleErrorLog _errorLogInaccessibleNode_log
        (LogTag_Node NodeLogTag_InaccessibleNode)
        eid
      Notify_ErrorLogNodeWrongChain eid -> handleErrorLog _errorLogNodeWrongChain_log
        (LogTag_Node NodeLogTag_NodeWrongChain)
        eid
      Notify_ErrorLogNodeInvalidPeerCount eid -> handleErrorLog _errorLogNodeInvalidPeerCount_log
        (LogTag_Node NodeLogTag_NodeInvalidPeerCount)
        eid
      Notify_ErrorLogMultipleBakersForSameBaker eid -> handleErrorLog _errorLogMultipleBakersForSameBaker_log
        (LogTag_Baker BakerLogTag_MultipleBakersForSameBaker)
        eid
      Notify_ErrorLogBakerDeactivated eid -> handleErrorLog _errorLogBakerDeactivated_log
        (LogTag_Baker BakerLogTag_BakerDeactivated)
        eid
      Notify_ErrorLogBakerDeactivationRisk eid -> handleErrorLog _errorLogBakerDeactivationRisk_log
        (LogTag_Baker BakerLogTag_BakerDeactivationRisk)
        eid
      Notify_ErrorLogBakerNoHeartbeat eid -> handleErrorLog _errorLogBakerNoHeartbeat_log LogTag_BakerNoHeartbeat eid
      Notify_ErrorLogNetworkUpdate eid -> handleErrorLog _errorLogNetworkUpdate_log LogTag_NetworkUpdate eid
      Notify_MailServerConfig _eid cfg -> handleMailServer cfg
      Notify_NodeExternal eid ent -> (<>) <$> handleNodeExternal eid ent <*> alsoEveryBakerSummary
      Notify_NodeInternal eid ent -> (<>) <$> handleNodeInternal eid ent <*> alsoEveryBakerSummary
      Notify_NodeDetails eid ent -> (<>) <$> handleNodeDetails eid ent <*> alsoEveryBakerSummary
      Notify_Notificatee eid -> handleNotificatee eid
      Notify_Parameters eid ent -> handleParameters eid ent
      Notify_PublicNodeConfig _eid ent -> handlePublicNodeConfig ent
      Notify_PublicNodeHead eid ent -> handlePublicNodeHead eid ent
      Notify_TelegramConfig _eid ent -> handleTelegramConfig ent
      Notify_TelegramRecipient eid ent -> handleTelegramRecipient eid ent
      Notify_UpstreamVersion _eid ent -> handleUpstreamVersion ent
  where
    clientsVS = _bakeViewSelector_clients aggVS
    clientAddressesVS = _bakeViewSelector_clientAddresses aggVS
    latestHeadVS = _bakeViewSelector_latestHead aggVS

    summaryVS = _bakeViewSelector_summary aggVS
    handleClient cid = whenM ( viewSelects cid clientsVS || viewSelects (Bounded cid) clientAddressesVS ) $ do
      client :: Maybe Client <- fmap listToMaybe $
        select $ AutoKeyField ==. fromId cid &&. Client_deletedField ==. False
      infos :: Maybe ClientInfo <- fmap listToMaybe $ select (ClientInfo_clientField ==. cid)
      let
        clientsPatch = mempty
          { _bakeView_clients = toRangeView1 clientsVS cid $ Just $ First infos
          , _bakeView_clientAddresses = toRangeView1 clientAddressesVS (Bounded cid) $ Just $ First $ _client_address <$> client
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
    handleParameters _eid params =
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
      :: (Monad m', PostgresRaw m')
      => Id Node -> Maybe NodeExternalData -> m' (BakeView a)
    handleNodeExternal nid mNodeExternalData = whenM (viewSelects (Bounded nid) nodeAddressesVS) $ do
      nodeExternalV <- case mNodeExternalData of
        Nothing -> pure [(Bounded nid, First Nothing)]
        Just _ -> getNodeAddresses (Just $ nid)
      pure $ mempty { _bakeView_nodeAddresses = toRangeView nodeAddressesVS nodeExternalV }

    {-# INLINE handleNodeInternal #-}
    handleNodeInternal
      :: (Monad m', PostgresRaw m')
      => Id Node -> Maybe NodeInternalData -> m' (BakeView a)
    handleNodeInternal nid mNodeInternalData = whenM (viewSelects (Bounded nid) nodeAddressesVS) $ do
      nodeInternalV <- case mNodeInternalData of
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

    handleBaker (Id pkh) mBaker = whenM (viewSelects (Bounded pkh) bakerAddressesVS) $
      case mBaker of
        -- fast path
        Nothing -> pure mempty {
          _bakeView_bakerAddresses = toRangeView bakerAddressesVS [(Bounded pkh, First Nothing)]
          }
        Just _ -> handleBakerAddress pkh

    handleBakerAddress pkh  = whenM (viewSelects (Bounded pkh) bakerAddressesVS) $ do
      bakerV <- getBakerAddresses nds (Just $ pkh)
      pure mempty { _bakeView_bakerAddresses = toRangeView bakerAddressesVS bakerV }

    -- this is a kludge; id really like a way to send only things that are "new information" to the frontend.
    alsoEveryBakerSummary = do
      bakerAddresses :: RangeView' PublicKeyHash (Deletable BakerSummary) a <- whenM (not $ null bakerAddressesVS) $
        toRangeView bakerAddressesVS <$> getBakerAddresses nds Nothing
      whenM (not $ null bakerAddresses) $
        (\x -> mempty {_bakeView_bakerAddresses = x}) . toRangeView bakerAddressesVS <$> getBakerAddresses nds Nothing


    handleBakerDetails bakerDetails = whenM (viewSelects (Bounded $ _bakerDetails_publicKeyHash bakerDetails) bakerDetailsVS) $
      pure $ mempty
        { _bakeView_bakerDetails = toRangeView1
            bakerDetailsVS
            (Bounded $ _bakerDetails_publicKeyHash bakerDetails)
            (Just $ First $ Just bakerDetails)
        }

    mailServerVS = _bakeViewSelector_mailServer aggVS
    handleNotificatee _nid = whenM (viewSelects () mailServerVS) $ do
      notificatees <- fmap _notificatee_email . toList <$> selectMap' NotificateeConstructor CondEmpty
      -- TODO: do something a little more reasonable that 'listToMaybe'  what happens if there *are* more than one serverConfig?
      mailServer :: Maybe MailServerConfig <- listToMaybe . toList <$> selectMap' MailServerConfigConstructor CondEmpty
      pure $ (mempty :: BakeView a)
        { _bakeView_mailServer = toMaybeView mailServerVS $ Just $ flip mailServerConfigToView notificatees <$> mailServer
        }
    handleMailServer mailServer = whenM (viewSelects () mailServerVS) $ do
      notificatees <- fmap _notificatee_email . toList <$> selectMap' NotificateeConstructor CondEmpty
      pure $ (mempty :: BakeView a)
        { _bakeView_mailServer = toMaybeView mailServerVS $ Just $ Just $ flip mailServerConfigToView notificatees $ mailServer
        }

    handleErrorLog
      :: forall e u m2. (EntityWithIdBy u e, PersistBackend m2, PostgresRaw m2)
      => (e -> Id ErrorLog) -> LogTag e -> Id e -> m2 (BakeView a)
    handleErrorLog = handleErrorLog' (const $ pure mempty)

    alertCountVS = _bakeViewSelector_alertCount aggVS
    handleErrorLog'
      :: forall e u m2
      . (EntityWithIdBy u e, PersistBackend m2, PostgresRaw m2)
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
      return $ newCount <> newErrors <> fold logNodeSummary <> userSupplied

    publicNodeConfigVS = _bakeViewSelector_publicNodeConfig aggVS
    handlePublicNodeConfig pnc =
      whenM (viewSelects (_publicNodeConfig_source pnc) publicNodeConfigVS) $
        pure $ mempty { _bakeView_publicNodeConfig = toRangeView1 publicNodeConfigVS (_publicNodeConfig_source pnc) (Just pnc) }

    publicNodeHeadsVS = _bakeViewSelector_publicNodeHeads aggVS
    handlePublicNodeHead nid pnh = mconcat <$> sequence
      [ whenM (viewSelects (Bounded nid) publicNodeHeadsVS) $ do
          pure $ mempty { _bakeView_publicNodeHeads = toRangeView1 publicNodeHeadsVS (Bounded nid) pnh }
      , whenM (viewSelects () latestHeadVS) $ do
          latestHead <- liftIO $ atomically $ dataSourceHead nds
          pure $ mempty { _bakeView_latestHead = toMaybeView latestHeadVS latestHead }
      ]

    telegramConfigVS = _bakeViewSelector_telegramConfig aggVS
    handleTelegramConfig cfg = whenM (viewSelects () telegramConfigVS) $ do
      pure $ mempty { _bakeView_telegramConfig = toMaybeView telegramConfigVS $ Just $ Just cfg }

    telegramRecipientsVS = _bakeViewSelector_telegramRecipients aggVS
    handleTelegramRecipient rid recipient = whenM (viewSelects (Bounded rid) telegramRecipientsVS) $ do
      pure $ mempty
        { _bakeView_telegramRecipients = toRangeView1 telegramRecipientsVS (Bounded rid) (Just $ First recipient) }

    upgradeVS = _bakeViewSelector_upstreamVersion aggVS
    handleUpstreamVersion ent = whenM (viewSelects () upgradeVS) $ do
      pure $ mempty { _bakeView_upstreamVersion = toMaybeView upgradeVS (Just ent) }
