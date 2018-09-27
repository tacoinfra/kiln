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
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Client as Http
import Network.Mail.Mime (Address (..), simpleMail')
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))
import Rhyolite.Backend.DB (getTime, runDb, selectMap)
import Rhyolite.Backend.DB.PsqlSimple (In (..), executeQ)
import Rhyolite.Backend.EmailWorker (queueEmail)
import Rhyolite.Backend.Schema (toId)
import Rhyolite.Schema (Id (..))

import Backend.Config (AppConfig)
import Backend.Schema
import Backend.Upgrade (checkForUpgrade)
import Common.Api (PrivateRequest (..), PublicRequest (..))
import Common.App
import Common.Schema

requestHandler
  :: (MonadBaseControl IO m, MonadIO m)
  => Text
  -> Address
  -> Http.Manager
  -> Pool Postgresql
  -> AppConfig
  -> RequestHandler Bake m
requestHandler upgradeBranch emailFromAddr httpMgr db appConfig =
  RequestHandler $ \req -> runNoLoggingT $ runDb (Identity db) $ case req of
    ApiRequest_Public r ->
      case r of
        PublicRequest_AddNode addr alias nodeIdent -> do
          existingIds :: [Id Node] <- fmap toId <$> project AutoKeyField (Node_addressField ==. addr)
          case nonEmpty existingIds of
            Nothing -> let node = mkNode addr alias in notify . flip Notify_Node node =<< insert' node
            Just nids -> for_ nids $ \nid -> do
              updateId nid [Node_deletedField =. False, Node_aliasField =. alias]
              getId nid >>= traverse_ (notify . Notify_Node nid)

        PublicRequest_RemoveNode addr -> do
          nids :: [Id Node] <- fmap toId <$> project AutoKeyField (Node_addressField ==. addr)
          for_ nids $ \nid -> do
            updateId nid [Node_deletedField =. True]
            getId nid >>= traverse_ (notify . Notify_Node nid)

        PublicRequest_AddClient addr alias -> do
          existingIds :: [Id Client] <- fmap toId <$> project AutoKeyField (Client_addressField ==. addr)
          case nonEmpty existingIds of
            Nothing -> insertNotify Client
              { _client_address = addr
              , _client_alias = alias
              , _client_updated = Nothing
              , _client_deleted = False
              }
            Just cids -> for_ cids $ \cid ->
              updateIdNotify cid [Client_deletedField =. False, Client_aliasField =. alias]

        PublicRequest_RemoveClient addr -> do
          cids :: [Id Client] <- fmap toId <$> project AutoKeyField (Client_addressField ==. addr)
          let inCids = In cids
          _ <- [executeQ| DELETE FROM "Client" c WHERE c.id IN ?inCids |]
          for_ cids $ notify . mkDefaultNotify

        PublicRequest_AddDelegate pkh alias -> do
          existingIds :: [Id Delegate] <- fmap toId <$> project AutoKeyField (Delegate_publicKeyHashField ==. pkh)
          case nonEmpty existingIds of
            Nothing -> insertNotify Delegate { _delegate_publicKeyHash = pkh, _delegate_alias = alias, _delegate_deleted = False }
            Just dids -> for_ dids $ \did ->
              updateIdNotify (did :: Id Delegate) [Delegate_deletedField =. False, Delegate_aliasField =. alias]

        PublicRequest_RemoveDelegate pkh -> do
          dids :: [Id Delegate] <- fmap toId <$> project AutoKeyField (Delegate_publicKeyHashField ==. pkh)
          let inIds = In dids
          _ <- [executeQ| DELETE FROM "PendingReward" pr WHERE pr.delegate IN ?inIds |]
          _ <- [executeQ| DELETE FROM "DelegateStats" ds WHERE ds.delegate IN ?inIds |]
          for_ dids $ \did ->
            updateIdNotify did [Delegate_deletedField =. True]

        PublicRequest_AddNotificatee email ->
          insertNotify Notificatee { _notificatee_email = email }

        PublicRequest_RemoveNotificatee email -> do
          nids :: [Id Notificatee] <- fmap toId <$> project AutoKeyField (Notificatee_emailField ==. email)
          let inIds = In nids
          _ <- [executeQ| DELETE FROM "Notificatee" n WHERE n.id IN ?inIds |]
          for_ nids $ notify . mkDefaultNotify

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
            Nothing -> insertNotify updatedMailServer
            Just (id_, _) -> updateIdNotify id_
              [ MailServerConfig_hostNameField =. _mailServerConfig_hostName updatedMailServer
              , MailServerConfig_portNumberField =. _mailServerConfig_portNumber updatedMailServer
              , MailServerConfig_smtpProtocolField =. _mailServerConfig_smtpProtocol updatedMailServer
              , MailServerConfig_userNameField =. _mailServerConfig_userName updatedMailServer
              , MailServerConfig_passwordField =. _mailServerConfig_password updatedMailServer
              , MailServerConfig_madeDefaultAtField =. _mailServerConfig_madeDefaultAt updatedMailServer
              ]

        PublicRequest_CheckForUpgrade ->
          checkForUpgrade upgradeBranch httpMgr appConfig id

        PublicRequest_SetPublicNodeConfig publicNode enabled -> do
          cid' :: Maybe (Id PublicNodeConfig) <- fmap toId . listToMaybe <$>
            project AutoKeyField (PublicNodeConfig_sourceField ==. publicNode)
          now <- getTime
          case cid' of
            Nothing ->
              let
                pnc = PublicNodeConfig
                  { _publicNodeConfig_source = publicNode
                  , _publicNodeConfig_enabled = enabled
                  , _publicNodeConfig_updated = now
                  }
              in notify . flip Notify_PublicNodeConfig pnc =<< insert' pnc
            Just cid -> do
              updateId cid
                [ PublicNodeConfig_sourceField =. publicNode
                , PublicNodeConfig_enabledField =. enabled
                , PublicNodeConfig_updatedField =. now
                ]
              getId cid >>= traverse_ (notify . Notify_PublicNodeConfig cid)

    ApiRequest_Private _key r -> case r of
      PrivateRequest_NoOp -> return ()

getDefaultMailServer :: PersistBackend m => m (Maybe (Id MailServerConfig, MailServerConfig))
getDefaultMailServer =
  fmap (listToMaybe . Map.toList) $
    selectMap MailServerConfigConstructor $ CondEmpty `orderBy` [Desc MailServerConfig_madeDefaultAtField] `limitTo` 1
