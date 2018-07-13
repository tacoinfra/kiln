{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.NotifyHandler where

import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import qualified Common.AppendIntervalMap as AppendIMap
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.AppendMap as Map
import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup (First (..), Semigroup, (<>))
import qualified Data.Set as Set
import Database.Groundhog.Postgresql (PersistBackend, Postgresql, get, select, (==.))
import Rhyolite.App (single)
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.Listen (NotifyMessage (..))
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Backend.Schema.Class (DefaultKeyId)
import Rhyolite.Schema (Id, IdData)
import Say (say)

import Backend.BalanceTracking
import Backend.Graphs
import Backend.Schema
import Backend.ViewSelectorHandler (getErrorLogs)
import Common (tshow, whenJust)
import Common.App (BakeView (..), BakeViewSelector (..), ErrorLogView (..), TimeWindow,
                   mailServerConfigToView)
import Common.Schema

notifyHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a, Semigroup a, Show a)
  => Pool Postgresql
  -> NotifyMessage
  -> BakeViewSelector a
  -> m (BakeView a)
notifyHandler db notifyMessage aggVS = runNoLoggingT $ runDb (Identity db) $ do
  let handleClient = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success cid -> do
          (client :: Maybe Client) <- get $ fromId (cid :: Id Client)
          (infos :: [ClientInfo]) <- select (ClientInfo_clientField ==. cid)
          let emptyV :: BakeView a
              emptyV = mempty -- avoid writing type signatures
              clientsPatch = case Map.lookup cid (_bakeViewSelector_clients aggVS) of
                Nothing -> emptyV
                Just a -> emptyV
                  { _bakeView_clients = Map.singleton cid (First (listToMaybe infos), a)
                  }
              clientAddressPatch = case _bakeViewSelector_clientAddresses aggVS of
                Nothing -> emptyV
                Just a -> emptyV
                  { _bakeView_clientAddresses = Map.singleton cid (First (_client_address <$> client), a)
                  }
          summaryPatch <- whenJust (_bakeViewSelector_summary aggVS) $ \a -> do
            rewardMap <- getAllRewards a
            maxLevel <- getMaxLevel
            summaryReport <- getSummaryReport
            summaryGraph <- whenJust maxLevel $ \l -> do
              mGraph <- liftIO $ cumulativeRewardsGraph (fromIntegral l) (fmap (getFirst . fst) rewardMap)
              return $ single mGraph a
            return $ emptyV
              { _bakeView_summaryGraph = summaryGraph
              , _bakeView_summary = single summaryReport a
              }
          return $ clientsPatch <> clientAddressPatch <> summaryPatch

      handleParameters = case fromJSON (_notifyMessage_value notifyMessage) :: Aeson.Result (Id Node) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success nid -> do
          whenJust (_bakeViewSelector_parameters aggVS) $ \a -> do
            params :: Maybe Parameters <- listToMaybe <$> select (Parameters_nodeField ==. nid)
            pure $ (mempty :: BakeView a)
              { _bakeView_parameters = single (_parameters_protoInfo <$> params) a
              }

      handleNode = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success nid -> do
          (node :: Maybe Node) <- get $ fromId nid
          let nodes = case Map.lookup nid (_bakeViewSelector_nodes aggVS) of
                Nothing -> mempty
                Just a -> (mempty :: BakeView a)
                  { _bakeView_nodes = Map.singleton nid (First node, a)
                  }
          let nodeAddresses = case _bakeViewSelector_nodeAddresses aggVS of
                Nothing -> mempty
                Just a -> (mempty :: BakeView a)
                  { _bakeView_nodeAddresses = Map.singleton nid (First $ _node_address <$> node, a)
                  }
          return $ nodeAddresses <> nodes

      handleDelegate = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success (dId :: Id Delegate) -> do
          say $ "Handling delegate id: " <> tshow dId
          whenJust (_bakeViewSelector_delegates aggVS) $ \a -> do
            delegate :: Maybe Delegate <- get $ fromId dId
            pure $ mempty
              { _bakeView_delegates = single (Set.singleton . _delegate_publicKeyHash <$> delegate) a
              }

      handleDelegateStats = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success (dsId :: Id DelegateStats) -> do
          delegateStats :: Maybe DelegateStats <- get $ fromId dsId
          whenJust delegateStats $ \stats -> do
            delegate :: Delegate <- fmap (fromMaybe $ error "Bad Foreign Key Delegate->DelegateStats") $ get $ fromId $ _delegateStats_delegate stats
            let publicKeyHash = _delegate_publicKeyHash delegate
            whenJust (Map.lookup publicKeyHash (_bakeViewSelector_delegateStats aggVS)) $ \a ->
              return $ mempty { _bakeView_delegateStats = Map.singleton publicKeyHash (First $ unDelegateStats publicKeyHash stats, a) }

      handleNotificatee = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success nid -> whenJust (_bakeViewSelector_notificatees aggVS) $ \a -> do
          notificatee :: Maybe Notificatee <- get $ fromId nid
          pure $ (mempty :: BakeView a)
            { _bakeView_notificatees = Map.singleton nid (First $ _notificatee_email <$> notificatee, a)
            }

      handleMailServer = case fromJSON (_notifyMessage_value notifyMessage) :: Aeson.Result (Id MailServerConfig) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success nid -> whenJust (_bakeViewSelector_mailServer aggVS) $ \a -> do
          mailServer :: Maybe MailServerConfig <- get $ fromId nid
          pure $ (mempty :: BakeView a)
            { _bakeView_mailServer = single (mailServerConfigToView <$> mailServer) a
            }

      handleErrorLog
        :: forall e m2. (EntityWithId e, FromJSON (IdData e), PersistBackend m2, MonadIO m2)
        => (e -> Id ErrorLog) -> (e -> ErrorLogView) -> m2 (BakeView a)
      handleErrorLog getLogId toView = case fromJSON (_notifyMessage_value notifyMessage) of
        Aeson.Error e -> parseErr notifyMessage e
        Aeson.Success (specificLogId :: Id e) -> do
          specificLog' :: Maybe e <- getId specificLogId
          whenJust specificLog' $ \specificLog -> do
            let logId = getLogId specificLog
            errorLog' :: Maybe ErrorLog <- get $ fromId logId
            whenJust errorLog' $ \errorLog -> do
              let
                relevantIntervals :: AppendIMap.AppendIntervalMap TimeWindow a =
                  _bakeViewSelector_errors aggVS `AppendIMap.intersecting`
                    ClosedInterval
                      (Bounded $ _errorLog_started errorLog)
                      (maybe UpperInfinity Bounded $ _errorLog_stopped errorLog)

              pure $ if null relevantIntervals
                then mempty :: BakeView a
                else mempty
                  { _bakeView_errors = (,) (Set.singleton logId) <$> relevantIntervals
                  , _bakeView_errorsById =
                      Map.singleton logId (First (Just (errorLog, toView specificLog)))
                  }

  case _notifyMessage_entityName notifyMessage of
    "Client" -> handleClient
    "Parameters" -> handleParameters
    "Node" -> handleNode
    "Delegate" -> handleDelegate
    "DelegateStats" -> handleDelegateStats
    "Notificatee" -> handleNotificatee
    "MailServerConfig" -> handleMailServer
    "ErrorLogInaccessibleEndpoint" -> handleErrorLog _errorLogInaccessibleEndpoint_log ErrorLogView_InaccessibleEndpoint
    "ErrorLogBakerNoHeartbeat" -> handleErrorLog _errorLogBakerNoHeartbeat_log ErrorLogView_BakerNoHeartbeat
    "ErrorLogNodeOnFork" -> handleErrorLog _errorLogNodeOnFork_log ErrorLogView_NodeOnFork
    "ErrorLogMultipleBakersForSameDelegate" -> handleErrorLog _errorLogMultipleBakersForSameDelegate_log ErrorLogView_MultipleBakersForSameDelegate
    _ -> do
      say $ "Unhandled NotifyMessage: " <> tshow notifyMessage
      return mempty

parseErr :: (MonadIO m, Show nm, Show err, Monoid r) => nm -> err -> m r
parseErr nm err = do
  say $ "Unable to parse NotifyMessage: " <> tshow nm <> ": " <> tshow err
  return mempty
