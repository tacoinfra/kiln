{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.NotifyHandler where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (Result (Error, Success), fromJSON)
import qualified Data.AppendMap as Map
import Data.Functor.Identity (Identity (..))
import Data.Maybe (listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup (First (..), Semigroup, (<>))
import Database.Groundhog.Postgresql (Postgresql, get, select, (==.))
import Rhyolite.App (single)
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.Listen (NotifyMessage (..))
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Schema (Id)
import Say (say)

import Backend.BalanceTracking
import Backend.Graphs
import Backend.Schema
import Common (tshow, whenJust)
import Common.App (BakeView (..), BakeViewSelector (..), mailServerConfigToView)
import Common.Schema (Account (..))
import Common.Schema (AccountDelegate (..))
import Common.Schema (Client (..))
import Common.Schema (ClientInfo)
import Common.Schema (Delegate (..))
import Common.Schema (DelegateStats (..), unDelegateStats)
import Common.Schema (MailServerConfig (..))
import Common.Schema (Node (..))
import Common.Schema (Notificatee (..))
import Common.Schema (Parameters (..))

notifyHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a, Semigroup a)
  => Pool Postgresql
  -> NotifyMessage
  -> BakeViewSelector a
  -> m (BakeView a)
notifyHandler db notifyMessage aggVS = runNoLoggingT . runDb (Identity db) $ do
  let handleClient = case fromJSON (_notifyMessage_value notifyMessage) of
        Success cid -> do
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
          summaryPatch <- case _bakeViewSelector_summary aggVS of
            Nothing -> return emptyV
            Just a -> do
              rewardMap <- getAllRewards a
              maxLevel <- getMaxLevel
              summaryReport <- getSummaryReport
              summaryGraph <- case maxLevel of
                Just l -> do
                  mGraph <- liftIO $ cumulativeRewardsGraph (fromIntegral l) (fmap (getFirst . fst) rewardMap)
                  return $ single mGraph a
                _ -> return mempty
              return $ emptyV
                  { _bakeView_summaryGraph = summaryGraph
                  , _bakeView_summary = single summaryReport a
                  }
          return $ clientsPatch <> clientAddressPatch <> summaryPatch
        Error e -> parseErr notifyMessage e
      handleParameters = case fromJSON (_notifyMessage_value notifyMessage) :: Result (Id Node) of
        Success nid -> do
          (params :: [Parameters]) <- select (Parameters_nodeField ==. nid)
          return $ case _bakeViewSelector_parameters aggVS of
            Nothing -> mempty :: BakeView a
            Just a -> (mempty :: BakeView a)
              { _bakeView_parameters = single (_parameters_protoInfo <$> listToMaybe params) a
              }
        Error e -> parseErr notifyMessage e
      handleNode = case fromJSON (_notifyMessage_value notifyMessage) of
        Success nid -> do
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
        Error e -> parseErr notifyMessage e
      handleDelegate = case fromJSON (_notifyMessage_value notifyMessage) of
        Success (dId :: Id Delegate) -> do
          delegate :: Maybe Delegate <- get $ fromId dId
          let v d a = mempty {_bakeView_delegates = Map.singleton (_delegate_publicKeyHash d) a}
          return $ maybe mempty id $ (v <$> delegate <*> _bakeViewSelector_delegates aggVS)
      handleDelegateStats = case fromJSON (_notifyMessage_value notifyMessage) of
        Success (dsId :: Id DelegateStats) -> do
          delegateStats :: Maybe DelegateStats <- get $ fromId dsId
          whenJust delegateStats $ \stats -> do
            delegate :: Delegate <- fmap (maybe (error "Bad Foreign Key Delegate->DelegateStats") id) $ get $ fromId $ _delegateStats_delegate stats
            let publicKeyHash = _delegate_publicKeyHash delegate
            whenJust (Map.lookup publicKeyHash (_bakeViewSelector_delegateStats aggVS)) $ \a ->
              return $ mempty { _bakeView_delegateStats = Map.singleton publicKeyHash (First $ unDelegateStats publicKeyHash stats, a) }
        Error e -> parseErr notifyMessage e
      handleNotificatee = case fromJSON (_notifyMessage_value notifyMessage) of
        Success nid -> do
          (notificatee :: Maybe Notificatee) <- get $ fromId nid
          return $ case _bakeViewSelector_notificatees aggVS of
            Nothing -> mempty
            Just a -> (mempty :: BakeView a)
              { _bakeView_notificatees = Map.singleton nid (First $ _notificatee_email <$> notificatee, a)
              }
        Error e -> parseErr notifyMessage e
      handleMailServer = case fromJSON (_notifyMessage_value notifyMessage) :: Result (Id MailServerConfig) of
        Success nid -> case _bakeViewSelector_mailServer aggVS of
            Nothing -> return mempty
            Just a -> do
              (mailServer :: Maybe MailServerConfig) <- get $ fromId nid
              return $ (mempty :: BakeView a)
                { _bakeView_mailServer = single (mailServerConfigToView <$> mailServer) a
                }
        Error e -> parseErr notifyMessage e
  case _notifyMessage_entityName notifyMessage of
    "Client" -> handleClient
    "Parameters" -> handleParameters
    "Node" -> handleNode
    "Delegate" -> handleDelegate
    "DelegateStats" -> handleDelegateStats
    "Notificatee" -> handleNotificatee
    "MailServerConfig" -> handleMailServer
    _ -> do
      say $ "Unhandled NotifyMessage: " <> tshow notifyMessage
      return mempty

parseErr :: (MonadIO m, Show nm, Show err, Monoid r) => nm -> err -> m r
parseErr nm err = do
  say $ "Unable to parse NotifyMessage: " <> tshow nm <> ": " <> tshow err
  return mempty
