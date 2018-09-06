{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.NotifyHandler where

import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import qualified Common.AppendIntervalMap as AppendIMap
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.AppendMap as Map
import Data.Bifunctor (first)
import Data.Foldable (fold, toList)
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup (First (..), Semigroup, (<>))
import qualified Data.Set as Set
import Data.Time (UTCTime)
import Database.Groundhog.Postgresql (AutoKeyField (..), PersistBackend, Postgresql, get, select, (&&.),
                                      (==.))
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.Listen (NotifyMessage (..))
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Backend.Schema.Class (DefaultKeyId)
import Rhyolite.Schema (Id, IdData)
import Say

import Tezos.Types

import Backend.BalanceTracking
import Backend.CachedNodeRPC
import Backend.Graphs
import Backend.Schema
import Backend.ViewSelectorHandler (getErrorLogs, getUpgradeNotice)
import Common (tshow, whenJust, whenM)
import Common.App (BakeView (..), BakeViewSelector (..), ErrorLogView (..), mailServerConfigToView)
import Common.Schema

import Common.Vassal

notifyHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a, Semigroup a, Show a)
  => NodeDataSource
  -> NotifyMessage
  -> BakeViewSelector a
  -> m (BakeView a)
notifyHandler nds notifyMessage aggVS = runNoLoggingT $ runDb (Identity $ _nodeDataSource_pool nds) $ do
  -- sayShow ("notified", notifyMessage)

  let clientsVS = _bakeViewSelector_clients aggVS
      clientAddressesVS = _bakeViewSelector_clientAddresses aggVS
      summaryVS = _bakeViewSelector_summary aggVS
      handleClient = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success cid -> whenM ( viewSelects cid clientsVS || viewSelects (Bounded cid) clientAddressesVS ) $ do
          client :: Maybe Client <- fmap listToMaybe $
            select $ AutoKeyField ==. fromId cid &&. Client_deletedField ==. False
          infos :: Maybe ClientInfo <- fmap listToMaybe $ select (ClientInfo_clientField ==. cid)
          let
            clientsPatch = mempty
                { _bakeView_clients = toRangeView1 clientsVS cid infos
                , _bakeView_clientAddresses = toRangeView1 clientAddressesVS (Bounded cid) $ Just $ _client_address <$> client
                }
          summaryPatch <- whenM (viewSelects () summaryVS) $ do
            maxLevel <- getMaxLevel
            summaryReport <- getSummaryReport
            -- summaryGraph <- whenJust maxLevel $ \l -> do
            --   mGraph <- liftIO $ cumulativeRewardsGraph (fromIntegral l) (fmap (getFirst . fst) rewardMap)
            --   return $ single mGraph a
            return $ mempty
              { _bakeView_summary = toMaybeView (_bakeViewSelector_summary aggVS) summaryReport
              }
          return $ clientsPatch <> summaryPatch

      paramsVS = _bakeViewSelector_parameters aggVS
      handleParameters = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success (nid :: Id Parameters) -> do
          -- delegateStatsView <- flip runReaderT nds $ withCache mempty $ \_protoInfo ->
          --   calculateDelegateStats (_bakeViewSelector_delegateStats aggVS)
          whenM (viewSelects () paramsVS) $ do
            params :: Maybe Parameters <- listToMaybe <$> select (AutoKeyField ==. fromId nid)
            pure $ mempty
              { _bakeView_parameters = toMaybeView paramsVS $ _parameters_protoInfo <$> params
              -- , _bakeView_delegateStats = delegateStatsView
              }

      nodesVS = _bakeViewSelector_nodes aggVS
      nodeAddressesVS = _bakeViewSelector_nodeAddresses aggVS

      handleNode = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success nid -> whenM (viewSelects (Bounded nid) nodesVS || viewSelects (Bounded nid) nodeAddressesVS) $ do
          node :: Maybe Node <- fmap listToMaybe $
            select $ AutoKeyField ==. fromId nid &&. Node_deletedField ==. False
          return mempty
                  { _bakeView_nodes = toRangeView1 nodesVS (Bounded nid) node
                  , _bakeView_nodeAddresses = toRangeView1 nodeAddressesVS (Bounded nid) $ _node_address <$> node
                  }

      delegateVS = _bakeViewSelector_delegates aggVS

      handleDelegate = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success (dId :: Id Delegate) -> do
          -- TODO: shove PKH in the NotifyMessage body so we can sample the
          -- viewselector without making a trip to the database and this whole
          -- thing can live in a withM (viewSelects ...)
          delegate :: Maybe Delegate <- get $ fromId dId
          pure $ mempty
            { _bakeView_delegates = foldMap (\pkh -> toRangeView1 delegateVS (Bounded pkh) $ Just ()) $ _delegate_publicKeyHash <$> delegate
            }

      notificateesVS = _bakeViewSelector_notificatees aggVS
      handleNotificatee = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success nid -> whenM (viewSelects (Bounded nid) notificateesVS) $ do
          notificatee :: Maybe Notificatee <- get $ fromId nid
          pure $ (mempty :: BakeView a)
            { _bakeView_notificatees = toRangeView1 notificateesVS (Bounded nid) $ _notificatee_email <$> notificatee
            }

      mailServerVS = _bakeViewSelector_mailServer aggVS
      handleMailServer = case fromJSON (_notifyMessage_value notifyMessage) :: Aeson.Result (Id MailServerConfig) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success nid -> whenM (viewSelects () mailServerVS) $ do
          mailServer :: Maybe MailServerConfig <- get $ fromId nid
          pure $ (mempty :: BakeView a)
            { _bakeView_mailServer = toMaybeView mailServerVS $ Just $ mailServerConfigToView <$> mailServer
            }

      errorsVS = _bakeViewSelector_errors aggVS
      handleErrorLog
        :: forall e m2. (EntityWithId e, FromJSON (IdData e), PersistBackend m2, MonadIO m2)
        => (e -> Id ErrorLog) -> (e -> ErrorLogView) -> m2 (BakeView a)
      handleErrorLog getLogId toView = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success (specificLogId :: Id e) -> do
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

      publicNodeHeadsVS = _bakeViewSelector_publicNodeHeads aggVS
      handlePublicNodeHead = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success nid -> whenM (viewSelects (Bounded nid) publicNodeHeadsVS) $ do
          node <- get $ fromId nid
          pure $ mempty { _bakeView_publicNodeHeads = toRangeView1 publicNodeHeadsVS (Bounded nid) node }

      upgradeVS = _bakeViewSelector_upgrade aggVS
      handleUpgradeNotice = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success (specificLogId :: Id ErrorLogUpgradeNotice) ->
          whenM (viewSelects () upgradeVS) $ do
            n <- getUpgradeNotice
            pure $ mempty { _bakeView_upgrade = toMaybeView upgradeVS n }

  case _notifyMessage_entityName notifyMessage of
    "Client" -> handleClient
    "Delegate" -> handleDelegate
    "ErrorLogBadNodeHead" -> handleErrorLog _errorLogBadNodeHead_log ErrorLogView_BadNodeHead
    "ErrorLogBakerNoHeartbeat" -> handleErrorLog _errorLogBakerNoHeartbeat_log ErrorLogView_BakerNoHeartbeat
    "ErrorLogInaccessibleEndpoint" -> handleErrorLog _errorLogInaccessibleEndpoint_log ErrorLogView_InaccessibleEndpoint
    "ErrorLogMultipleBakersForSameDelegate" -> handleErrorLog _errorLogMultipleBakersForSameDelegate_log ErrorLogView_MultipleBakersForSameDelegate
    "ErrorLogNodeWrongChain" -> handleErrorLog _errorLogNodeWrongChain_log ErrorLogView_NodeWrongChain
    "ErrorLogUpgradeNotice" -> handleUpgradeNotice
    "MailServerConfig" -> handleMailServer
    "Node" -> handleNode
    "Notificatee" -> handleNotificatee
    "Parameters" -> handleParameters
    "PublicNodeHead" -> handlePublicNodeHead
    _ -> do
      sayErr $ "Unhandled NotifyMessage: " <> tshow notifyMessage
      return mempty

parseErr :: (MonadIO m, Show nm, Show err, Monoid r) => nm -> err -> m r
parseErr nm err = do
  sayErr $ "Unable to parse NotifyMessage: " <> tshow nm <> ": " <> tshow err
  return mempty
