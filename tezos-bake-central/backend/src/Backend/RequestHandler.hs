{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Backend.RequestHandler where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Pool (Pool)
import Data.Traversable (for)
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Client as Http
import Network.Mail.Mime (Address (..), simpleMail')
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))
import Rhyolite.Backend.DB (getTime, runDb, selectMap)
import Rhyolite.Backend.DB.PsqlSimple (In (..), Only (..), executeQ, queryQ)
import Rhyolite.Backend.EmailWorker (queueEmail)
import Rhyolite.Backend.Listen (NotificationType (..), insertAndNotify_, notifyEntityId, updateAndNotify)
import Rhyolite.Backend.Schema (toId)
import Rhyolite.Schema (Id (..))

import Backend.ChainHealth (obtainNode)
import Backend.NodeRPC (NodeRPCContext (..))
import Backend.Schema
import Common.Api (PrivateRequest (..), PublicRequest (..))
import Common.App
import Common.Schema


requestHandler
  :: (MonadBaseControl IO m, MonadIO m)
  => Address
  -> Http.Manager
  -> Pool Postgresql
  -> RequestHandler Bake m
requestHandler emailFromAddr httpMgr db = RequestHandler $ \req -> runNoLoggingT $ runDb (Identity db) $
  case req of
    ApiRequest_Public r ->
      case r of
        PublicRequest_AddNode addr nodeIdent -> do
          existingIds :: [Id Node] <- fmap toId <$> project AutoKeyField (Node_addressField ==. addr)
          case nonEmpty existingIds of
            Nothing -> do
              let ctx = NodeRPCContext httpMgr addr
              (_, node) <- runReaderT obtainNode ctx
              insertAndNotify_ node
            Just nids -> for_ nids $ \nid -> updateAndNotify nid [Node_deletedField =. False]

        PublicRequest_RemoveNode addr -> do
          nids :: [Id Node] <- fmap toId <$> project AutoKeyField (Node_addressField ==. addr)
          let inIds = In nids
          _ <- [executeQ| DELETE FROM "Parameters" where node in ?inIds |]
          for_ nids $ \nid -> updateAndNotify nid [Node_deletedField =. True]

        PublicRequest_AddClient addr -> do
          existingIds :: [Id Client] <- fmap toId <$> project AutoKeyField (Client_addressField ==. addr)
          case nonEmpty existingIds of
            Nothing -> insertAndNotify_ $ Client
              { _client_address = addr
              , _client_updated = Nothing
              , _client_deleted = False
              }
            Just cids -> for_ cids $ \cid -> updateAndNotify cid [Client_deletedField =. False]

        PublicRequest_RemoveClient addr -> do
          cids :: [Id Client] <- fmap toId <$> project AutoKeyField (Client_addressField ==. addr)
          let inCids = In cids
          _ <- [executeQ| DELETE FROM "Client" c WHERE c.id IN ?inCids |]
          notifyEntitiesDeleted cids

        PublicRequest_AddDelegate pkh -> do
          existingIds :: [Id Delegate] <- fmap toId <$> project AutoKeyField (Delegate_publicKeyHashField ==. pkh)
          case nonEmpty existingIds of
            Nothing -> insertAndNotify_ $ Delegate { _delegate_publicKeyHash = pkh, _delegate_deleted = False }
            Just dids -> for_ dids $ \did -> updateAndNotify did [Delegate_deletedField =. False]

        PublicRequest_RemoveDelegate pkh -> do
          dids :: [Id Delegate] <- fmap toId <$> project AutoKeyField (Delegate_publicKeyHashField ==. pkh)
          let inIds = In dids
          _ <- [executeQ| DELETE FROM "PendingReward" pr WHERE pr.delegate IN ?inIds |]
          _ <- [executeQ| DELETE FROM "DelegateStats" ds WHERE ds.delegate IN ?inIds |]
          for_ dids $ \did -> updateAndNotify did [Delegate_deletedField =. True]

        PublicRequest_AddNotificatee email -> do
          insertAndNotify_ $ Notificatee { _notificatee_email = email }

        PublicRequest_RemoveNotificatee email -> do
          nids :: [Id Notificatee] <- fmap toId <$> project AutoKeyField (Notificatee_emailField ==. email)
          let inIds = In nids
          _ <- [executeQ| DELETE FROM "Notificatee" n WHERE n.id IN inIds |]
          notifyEntitiesDeleted nids

        PublicRequest_SendTestEmail email -> void $ queueEmail
          (simpleMail'
            (Address Nothing email)
            emailFromAddr
            "Tezos Bake Monitor - Test"
            "This is a test email!"
          )
          Nothing

        PublicRequest_SetMailServerConfig mailServerView password -> do
          now <- getTime
          let updatedMailServer = MailServerConfig
                { _mailServerConfig_hostName = _mailServerView_hostName mailServerView
                , _mailServerConfig_portNumber = _mailServerView_portNumber mailServerView
                , _mailServerConfig_smtpProtocol = _mailServerView_smtpProtocol mailServerView
                , _mailServerConfig_userName = _mailServerView_userName mailServerView
                , _mailServerConfig_password = password
                , _mailServerConfig_madeDefaultAt = now
                }
          defaultMailServer <- getDefaultMailServer
          case defaultMailServer of
            Nothing -> insertAndNotify_ updatedMailServer
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

    where
      notifyEntitiesDeleted ids = for_ ids $ void . notifyEntityId NotificationType_Delete

getDefaultMailServer :: PersistBackend m => m (Maybe (Id MailServerConfig, MailServerConfig))
getDefaultMailServer =
  fmap (listToMaybe . Map.toList) $
    selectMap MailServerConfigConstructor $ CondEmpty `orderBy` [Desc MailServerConfig_madeDefaultAtField] `limitTo` 1
