{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.NotifyHandler where

import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import Control.Arrow ((&&&))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.AppendMap as Map
import Data.Bool (bool)
import Data.Functor.Identity (Identity (..))
import Data.Maybe (listToMaybe)
import Data.Semigroup (First (..), (<>))
import Database.Groundhog.Postgresql (AutoKeyField (..), PersistBackend, get, select, (&&.), (==.))
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.Listen (NotifyMessage (..))
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Schema (Id)
import Say (sayErr)

import Backend.BalanceTracking
import Backend.CachedNodeRPC
-- import Backend.Graphs
import Backend.Schema
import Backend.ViewSelectorHandler (getUpgradeNotice)
import Common (tshow, whenJust, whenM)
import Common.App (BakeView (..), BakeViewSelector (..), ErrorLogView (..), mailServerConfigToView)
import Common.Schema

import Common.Vassal

notifyHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a)
  => NodeDataSource
  -> NotifyMessage
  -> BakeViewSelector a
  -> m (BakeView a)
notifyHandler nds notifyMessage aggVS = runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity $ _nodeDataSource_pool nds) $
  case fromJSON (_notifyMessage_value notifyMessage) of
    Aeson.Error e -> do
      sayErr $ "Unable to parse NotifyMessage: " <> tshow (_notifyMessage_value notifyMessage) <> ": " <> tshow e
      pure mempty
    Aeson.Success notification -> case notification of
      Notify_Client eid -> handleClient eid
      Notify_Delegate eid -> handleDelegate eid
      Notify_ErrorLogBadNodeHead eid -> handleErrorLog _errorLogBadNodeHead_log ErrorLogView_BadNodeHead eid
      Notify_ErrorLogBakerNoHeartbeat eid -> handleErrorLog _errorLogBakerNoHeartbeat_log ErrorLogView_BakerNoHeartbeat eid
      Notify_ErrorLogInaccessibleNode eid -> handleErrorLog _errorLogInaccessibleNode_log ErrorLogView_InaccessibleNode eid
      Notify_ErrorLogMultipleBakersForSameDelegate eid -> handleErrorLog _errorLogMultipleBakersForSameDelegate_log ErrorLogView_MultipleBakersForSameDelegate eid
      Notify_ErrorLogNodeWrongChain eid -> handleErrorLog _errorLogNodeWrongChain_log ErrorLogView_NodeWrongChain eid
      Notify_ErrorLogUpgradeNotice eid -> handleUpgradeNotice eid
      Notify_MailServerConfig eid -> handleMailServer eid
      Notify_Node eid ent -> handleNode eid ent
      Notify_Notificatee eid -> handleNotificatee eid
      Notify_Parameters eid ent -> handleParameters eid ent
      Notify_PublicNodeConfig eid ent -> handlePublicNodeConfig eid ent
      Notify_PublicNodeHead eid -> handlePublicNodeHead eid
  where
    clientsVS = _bakeViewSelector_clients aggVS
    clientAddressesVS = _bakeViewSelector_clientAddresses aggVS

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
      -- delegateStatsV iew <- flip runReaderT nds $ withCache mempty $ \_protoInfo ->
      --   calculateDelegateStats (_bakeViewSelector_delegateStats aggVS)
      whenM (viewSelects () paramsVS) $
        pure $ mempty
          { _bakeView_parameters = toMaybeView paramsVS $ Just $ _parameters_protoInfo params
          -- , _bakeView_delegateStats = delegateStatsView
          }

    nodesVS = _bakeViewSelector_nodes aggVS
    nodeAddressesVS = _bakeViewSelector_nodeAddresses aggVS
    handleNode nid node' = whenM (viewSelects (Bounded nid) nodesVS || viewSelects (Bounded nid) nodeAddressesVS) $
      let node = if _node_deleted node' then Nothing else Just node' in
      pure mempty
        { _bakeView_nodes = toRangeView1 nodesVS (Bounded nid) (Just (First node))
        , _bakeView_nodeAddresses = toRangeView1 nodeAddressesVS (Bounded nid) $ Just $ First $ (_node_address &&& _node_alias) <$> node
        }

    delegateVS = _bakeViewSelector_delegates aggVS
    handleDelegate dId = do
      -- TODO: shove PKH in the NotifyMessage body so we can sample the
      -- viewselector without making a trip to the database and this whole
      -- thing can live in a withM (viewSelects ...)
      delegate :: Maybe Delegate <- get $ fromId dId
      pure $ mempty
        { _bakeView_delegates = foldMap (\d -> toRangeView1 delegateVS (Bounded $ _delegate_publicKeyHash d) (Just $ First $ bool Nothing (Just ()) $ _delegate_deleted d)) delegate
        }

    notificateesVS = _bakeViewSelector_notificatees aggVS
    handleNotificatee nid = whenM (viewSelects (Bounded nid) notificateesVS) $ do
      notificatee :: Maybe Notificatee <- get $ fromId nid
      pure $ (mempty :: BakeView a)
        { _bakeView_notificatees = toRangeView1 notificateesVS (Bounded nid) $ Just $ First $ _notificatee_email <$> notificatee
        }

    mailServerVS = _bakeViewSelector_mailServer aggVS
    handleMailServer nid = whenM (viewSelects () mailServerVS) $ do
      mailServer :: Maybe MailServerConfig <- get $ fromId nid
      pure $ (mempty :: BakeView a)
        { _bakeView_mailServer = toMaybeView mailServerVS $ Just $ mailServerConfigToView <$> mailServer
        }

    errorsVS = _bakeViewSelector_errors aggVS
    handleErrorLog
      :: forall e m2. (EntityWithId e, PersistBackend m2)
      => (e -> Id ErrorLog) -> (e -> ErrorLogView) -> Id e -> m2 (BakeView a)
    handleErrorLog getLogId toView specificLogId = do
      -- TODO: shove a time range, or perhaps an (Id ErrorLog) in the
      -- message body so that we can avoid doing some of the work if it
      -- won't be observed
      specificLog' :: Maybe e <- getId specificLogId
      whenJust specificLog' $ \specificLog -> do
        let logId = getLogId specificLog
        errorLog' :: Maybe ErrorLog <- get $ fromId logId
        whenJust errorLog' $ \errorLog -> do
          let
            -- todo: Common.App.getErrorInterval does this already
            errorInterval = ClosedInterval
                  (Bounded $ _errorLog_started errorLog)
                  (maybe UpperInfinity Bounded $ _errorLog_stopped errorLog)
          whenM (viewSelects errorInterval errorsVS) $ pure mempty
              { _bakeView_errors = IntervalView (unIntervalSelector errorsVS) $ -- see comment on instance Semigroup (IntervalView) for why this is "legit"
                  Map.singleton logId $ First ((errorLog, toView specificLog), errorInterval)
              }

    publicNodeConfigVS = _bakeViewSelector_publicNodeConfig aggVS
    handlePublicNodeConfig _cid pnc =
      whenM (viewSelects (_publicNodeConfig_source pnc) publicNodeConfigVS) $
        pure $ mempty { _bakeView_publicNodeConfig = toRangeView1 publicNodeConfigVS (_publicNodeConfig_source pnc) (Just pnc) }

    publicNodeHeadsVS = _bakeViewSelector_publicNodeHeads aggVS
    handlePublicNodeHead nid = whenM (viewSelects (Bounded nid) publicNodeHeadsVS) $ do
      node <- get $ fromId nid
      pure $ mempty { _bakeView_publicNodeHeads = toRangeView1 publicNodeHeadsVS (Bounded nid) node }

    upgradeVS = _bakeViewSelector_upgrade aggVS
    handleUpgradeNotice _specificLogId = whenM (viewSelects () upgradeVS) $ do
      n <- getUpgradeNotice
      pure $ mempty { _bakeView_upgrade = toMaybeView upgradeVS n }
