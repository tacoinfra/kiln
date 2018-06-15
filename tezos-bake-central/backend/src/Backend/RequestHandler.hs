{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Backend.RequestHandler where

import Control.Monad
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Pool (Pool)
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Client as Http
import Rhyolite.Api
import Rhyolite.Backend.App
import Rhyolite.Backend.DB (getTime, runDb, selectMap)
import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.Backend.Listen
import Rhyolite.Schema
import qualified Web.ClientSession as CS

import Backend.ChainHealth (obtainNode)
import Backend.NodeRPC
import Backend.Schema
import Common.Api
import Common.App
import Common.Schema


requestHandler
  :: (MonadBaseControl IO m, MonadIO m)
  => CS.Key
  -> Http.Manager
  -> Pool Postgresql
  -> RequestHandler Bake m
requestHandler csk httpMgr db = RequestHandler $ \req -> runNoLoggingT . runDb (Identity db) $
  case req of
    ApiRequest_Public r ->
      case r of
        PublicRequest_AddNode addr -> do
          let ctx = NodeRPCContext httpMgr addr
          (_, node) <- runNodeRPCT ctx obtainNode
          void $ insertAndNotify node
        PublicRequest_RemoveNode addr -> do
          nodeIds <- [queryQ| SELECT id FROM "Node" where address = ?addr |]
          let inNodeIds = In (fromOnly <$> nodeIds)
          -- delete parameters
          void $ [executeQ| DELETE FROM "Parameters" where node in ?inNodeIds |]
          -- delete node
          void $ [executeQ| DELETE FROM "Node" where id in ?inNodeIds |]
          -- notify
          void $ forM_ nodeIds $ \(Only nodeId) -> notifyEntityId NotificationType_Delete (nodeId :: Id Node)
        PublicRequest_AddClient addr -> do
          void $ insertAndNotify $ Client { _client_address = addr, _client_updated = Nothing }
        PublicRequest_RemoveClient addr -> do
          _ <- [executeQ| DELETE FROM "PendingReward" p USING "Client" c WHERE p.client = c.id AND c.address = ?addr |]
          cids <- [queryQ| SELECT id FROM "Client" WHERE "address" = ?addr |]
          let inCids = In (map fromOnly cids)
          _ <- [executeQ| DELETE FROM "Client" c WHERE c.id IN ?inCids |]
          forM_ cids $ \(Only cid) -> notifyEntityId NotificationType_Delete (cid :: Id Client)
          return ()
        PublicRequest_AddNotificatee email -> do
          void $ insertAndNotify $ Notificatee { _notificatee_email = email }
        PublicRequest_RemoveNotificatee email -> do
          nids <- [queryQ| SELECT n.id FROM "Notificatee" n WHERE n.email = ?email |]
          _ <- [executeQ| DELETE FROM "Notificatee" n WHERE n.email = ?email |]
          forM_ nids $ \(Only nid) -> notifyEntityId NotificationType_Delete (nid :: Id Notificatee)
          return ()
        PublicRequest_SetMailServerConfig mailServerView password -> do
          now <- getTime
          let updatedMailServer = MailServerConfig
                { _mailServerConfig_hostName = _mailServerView_hostName mailServerView
                , _mailServerConfig_portNumber  = _mailServerView_portNumber  mailServerView
                , _mailServerConfig_smtpProtocol = _mailServerView_smtpProtocol mailServerView
                , _mailServerConfig_userName = _mailServerView_userName mailServerView
                , _mailServerConfig_password = password
                , _mailServerConfig_madeDefaultAt = now
                }
          defaultMailServer <- getDefaultMailServer
          case defaultMailServer of
            Nothing -> void $ insertAndNotify updatedMailServer
            Just (id_, _) -> updateAndNotify id_
              [ MailServerConfig_hostNameField =. _mailServerConfig_hostName updatedMailServer
              , MailServerConfig_portNumberField =. _mailServerConfig_portNumber updatedMailServer
              , MailServerConfig_smtpProtocolField =. _mailServerConfig_smtpProtocol updatedMailServer
              , MailServerConfig_userNameField =. _mailServerConfig_userName updatedMailServer
              , MailServerConfig_passwordField =. _mailServerConfig_password updatedMailServer
              , MailServerConfig_madeDefaultAtField =. _mailServerConfig_madeDefaultAt updatedMailServer
              ]

    ApiRequest_Private key r ->
      case r of
        PrivateRequest_NoOp -> return ()

getDefaultMailServer :: PersistBackend m => m (Maybe (Id MailServerConfig, MailServerConfig))
getDefaultMailServer =
  fmap (listToMaybe . Map.toList) $
    selectMap MailServerConfigConstructor $ CondEmpty `orderBy` [Desc MailServerConfig_madeDefaultAtField] `limitTo` 1
