{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Backend.RequestHandler where

import Control.Concurrent.Async (async)
import Control.Exception.Safe (SomeException, try)
import Control.Monad.Logger (MonadLogger, logError, logInfo)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Database.Groundhog.Postgresql
import Network.Mail.Mime (Address (..), simpleMail')
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))
import Rhyolite.Backend.DB (getTime, runDb, selectMap)
import Rhyolite.Backend.DB.PsqlSimple (In (..), executeQ)
import Rhyolite.Backend.EmailWorker (queueEmail)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Backend.Schema (toId)
import Rhyolite.Schema (Id (..))

import Backend.CachedNodeRPC (NodeDataSource (..))
import Backend.Config (AppConfig)
import Backend.Http (runHttpT)
import Backend.Schema
import qualified Backend.Telegram as Telegram
import Backend.Upgrade (checkForUpgrade)
import Backend.Workers.Node (DataSource, updateDataSource)
import Common.Api (PrivateRequest (..), PublicRequest (..))
import Common.App
import Common.Schema
import ExtraPrelude

requestHandler
  :: forall m. (MonadBaseControl IO m, MonadIO m)
  => Text
  -> Address
  -> NodeDataSource
  -> [DataSource]
  -> AppConfig
  -> RequestHandler Bake m
requestHandler upgradeBranch emailFromAddr nds publicNodeSources appConfig =
  RequestHandler $ \case
    ApiRequest_Public r -> runLoggingEnv (_nodeDataSource_logger nds) $ case r of
      PublicRequest_AddNode addr alias -> inDb $ do
        existingIds :: [Id Node] <- fmap toId <$> project AutoKeyField (Node_addressField ==. addr)
        case nonEmpty existingIds of
          Nothing -> let node = mkNode addr alias in notify . flip Notify_Node node =<< insert' node
          Just nids -> for_ nids $ \nid -> do
            updateId nid [Node_deletedField =. False, Node_aliasField =. alias]
            getId nid >>= traverse_ (notify . Notify_Node nid)

      PublicRequest_RemoveNode addr -> inDb $ do
        nids :: [Id Node] <- fmap toId <$> project AutoKeyField (Node_addressField ==. addr)
        for_ nids $ \nid -> do
          updateId nid [Node_deletedField =. True]
          getId nid >>= traverse_ (notify . Notify_Node nid)

      PublicRequest_AddClient addr alias -> inDb $ do
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

      PublicRequest_RemoveClient addr -> inDb $ do
        cids :: [Id Client] <- fmap toId <$> project AutoKeyField (Client_addressField ==. addr)
        let inCids = In cids
        _ <- [executeQ| DELETE FROM "Client" c WHERE c.id IN ?inCids |]
        for_ cids $ notify . mkDefaultNotify

      PublicRequest_AddDelegate pkh alias -> inDb $ do
        existingIds :: [Id Delegate] <- fmap toId <$> project AutoKeyField (Delegate_publicKeyHashField ==. pkh)
        case nonEmpty existingIds of
          Nothing -> insertNotify Delegate { _delegate_publicKeyHash = pkh, _delegate_alias = alias, _delegate_deleted = False }
          Just dids -> for_ dids $ \did ->
            updateIdNotify (did :: Id Delegate) [Delegate_deletedField =. False, Delegate_aliasField =. alias]

      PublicRequest_RemoveDelegate pkh -> inDb $ do
        dids :: [Id Delegate] <- fmap toId <$> project AutoKeyField (Delegate_publicKeyHashField ==. pkh)
        let inIds = In dids
        _ <- [executeQ| DELETE FROM "PendingReward" pr WHERE pr.delegate IN ?inIds |]
        _ <- [executeQ| DELETE FROM "DelegateStats" ds WHERE ds.delegate IN ?inIds |]
        for_ dids $ \did ->
          updateIdNotify did [Delegate_deletedField =. True]

      PublicRequest_AddNotificatee email -> inDb $
        insertNotify Notificatee { _notificatee_email = email }

      PublicRequest_RemoveNotificatee email -> inDb $ do
        nids :: [Id Notificatee] <- fmap toId <$> project AutoKeyField (Notificatee_emailField ==. email)
        let inIds = In nids
        _ <- [executeQ| DELETE FROM "Notificatee" n WHERE n.id IN ?inIds |]
        for_ nids $ notify . mkDefaultNotify

      PublicRequest_SendTestEmail email -> inDb $ void $ queueEmail
        (simpleMail'
          (Address Nothing email)
          emailFromAddr
          "Tezos Bake Monitor - Test"
          "This is a test email!"
        )
        Nothing

      PublicRequest_SetMailServerConfig mailServerView password -> inDb $ do
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
        void $ liftIO $ async $ runLoggingEnv (_nodeDataSource_logger nds) $ inDb $
          void $ checkForUpgrade upgradeBranch (_nodeDataSource_httpMgr nds) appConfig id

      PublicRequest_SetPublicNodeConfig publicNode enabled -> do
        inDb $ do
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

        -- When turning something "on" immediately update the data source.
        when enabled $
          for_ (filter (\(pn, _, _) -> pn == publicNode) publicNodeSources) $
            updateDataSource nds

      PublicRequest_AddTelegramConfig apiKey -> do
        -- Initialize the config to have NULL bot name and NULL enabled.
        -- NULL enabled means the bot is not yet validated.
        $(logInfo) "Adding a Telegram configuration"
        inDb $ void $ updateTelegramCfg apiKey Nothing True Nothing

        -- Fork a thread to collect meta info about this bot.
        void $ liftIO $ async $ runLoggingEnv (_nodeDataSource_logger nds) $
          connectTelegram apiKey

        where
          connectTelegram botApiKey = do
            result' <- try @_ @SomeException $ runHttpT (_nodeDataSource_httpMgr nds) $
              Telegram.getBotAndLastSender botApiKey
            inDb $ case result' of
              Left e -> do
                $(logError) $ "Failed to connect Telegram: " <> tshow e
                void $ updateTelegramCfg botApiKey Nothing True (Just False)
              Right Nothing -> do
                $(logError) "Failed to connect Telegram: no bot or no senders"
                void $ updateTelegramCfg botApiKey Nothing True (Just False)
              Right (Just (botMeta, chat, sender)) -> do
                let
                  botName = Telegram._botGetMe_firstName botMeta
                $(logInfo) $ "Telegram Bot found: " <> botName
                cid <- updateTelegramCfg botApiKey (Just botName) True (Just True)
                rid <- updateRecipient cid chat sender
                now <- getTime
                void $ insert' TelegramMessageQueue
                  { _telegramMessageQueue_recipient = rid
                  , _telegramMessageQueue_message = "Great! You'll receive alerts like this."
                  , _telegramMessageQueue_created = now
                  }

          updateTelegramCfg botApiKey (botName :: Maybe Text) enabled validated = do
            cid' :: Maybe (Id TelegramConfig) <-
              fmap toId . listToMaybe <$> project AutoKeyField
                (TelegramConfig_enabledField ==. TelegramConfig_enabledField) -- Silliness to help types infer
            now <- getTime
            case cid' of
              Nothing -> do
                let
                  new = TelegramConfig
                    { _telegramConfig_botApiKey = botApiKey
                    , _telegramConfig_botName = botName
                    , _telegramConfig_created = now
                    , _telegramConfig_updated = now
                    , _telegramConfig_enabled = enabled
                    , _telegramConfig_validated = validated
                    }
                cid <- insert' new
                notify $ Notify_TelegramConfig cid new
                pure cid

              Just cid -> do
                updateId cid
                  [ TelegramConfig_botNameField =. botName
                  , TelegramConfig_botApiKeyField =. botApiKey
                  , TelegramConfig_updatedField =. now
                  , TelegramConfig_enabledField =. enabled
                  , TelegramConfig_validatedField =. validated
                  ]
                getId cid >>= traverse_ (notify . Notify_TelegramConfig cid)
                pure cid

          updateRecipient cid chat sender = inDb $ do
            rid' :: Maybe (Id TelegramRecipient) <-
              fmap toId . listToMaybe <$> project AutoKeyField
                (TelegramRecipient_deletedField ==. False)
            now <- getTime
            case rid' of
              Nothing -> do
                let
                  new = TelegramRecipient
                    { _telegramRecipient_config = cid
                    , _telegramRecipient_userId = Telegram._sender_id sender
                    , _telegramRecipient_chatId = Telegram._chat_id chat
                    , _telegramRecipient_firstName = Telegram._sender_firstName sender
                    , _telegramRecipient_lastName = Telegram._sender_lastName sender
                    , _telegramRecipient_username = Telegram._sender_username sender
                    , _telegramRecipient_created = now
                    , _telegramRecipient_deleted = False
                    }
                rid <- insert' new
                notify $ Notify_TelegramRecipient rid (Just new)
                pure rid

              Just rid -> do
                updateId rid
                  [ TelegramRecipient_configField =. cid
                  , TelegramRecipient_userIdField =. Telegram._sender_id sender
                  , TelegramRecipient_chatIdField =. Telegram._chat_id chat
                  , TelegramRecipient_firstNameField =. Telegram._sender_firstName sender
                  , TelegramRecipient_lastNameField =. Telegram._sender_lastName sender
                  , TelegramRecipient_usernameField =. Telegram._sender_username sender
                  , TelegramRecipient_createdField =. now
                  , TelegramRecipient_deletedField =. False
                  ]
                notify . Notify_TelegramRecipient rid =<< getId rid
                pure rid

    ApiRequest_Private _key r -> case r of
      PrivateRequest_NoOp -> return ()

  where
    inDb :: forall m' a. (MonadLogger m', MonadIO m', MonadBaseControl IO m') => DbPersist Postgresql m' a -> m' a
    inDb = runDb (Identity $ _nodeDataSource_pool nds)

getDefaultMailServer :: PersistBackend m => m (Maybe (Id MailServerConfig, MailServerConfig))
getDefaultMailServer =
  fmap (listToMaybe . Map.toList) $
    selectMap MailServerConfigConstructor $ CondEmpty `orderBy` [Desc MailServerConfig_madeDefaultAtField] `limitTo` 1
