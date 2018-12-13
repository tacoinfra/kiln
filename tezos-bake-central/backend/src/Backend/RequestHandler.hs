{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Backend.RequestHandler where

import Control.Concurrent.Async (async)
import Control.Exception.Safe (SomeException, try)
import Control.Monad.Logger (MonadLogger, logError, logInfo)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Foldable (toList)
import Data.Functor.Infix
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Monoidal as MMap
import qualified Data.Set as Set
import Data.Dependent.Sum (DSum ((:=>)))
import Database.Groundhog.Core (Field)
import Database.Groundhog.Postgresql
import Network.Mail.Mime (Address (..), simpleMail')
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))
import Rhyolite.Backend.DB (getTime, runDb, selectMap')
import Rhyolite.Backend.DB.PsqlSimple (In (..), executeQ)
import Rhyolite.Backend.EmailWorker (queueEmail)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Schema (Email, Id (..))

import Backend.CachedNodeRPC (NodeDataSource (..))
import Backend.Http (runHttpT)
import Backend.Schema
import qualified Backend.Telegram as Telegram
import Backend.Upgrade (updateUpstreamVersion)
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
  -> RequestHandler Bake m
requestHandler upgradeBranch emailFromAddr nds publicNodeSources =
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

          elin <- selectMap' ErrorLogInaccessibleNodeConstructor (ErrorLogInaccessibleNode_nodeField ==. nid)
          elnwc <- selectMap' ErrorLogNodeWrongChainConstructor (ErrorLogNodeWrongChain_nodeField ==. nid)
          elbnh <- selectMap' ErrorLogBadNodeHeadConstructor (ErrorLogBadNodeHead_nodeField ==. nid)

          now <- getTime
          let
            logIds :: [Id ErrorLog] = mconcat
              [ _errorLogInaccessibleNode_log <$> toList elin
              , _errorLogNodeWrongChain_log <$> toList elnwc
              , _errorLogBadNodeHead_log <$> toList elbnh
              ]
          update [ErrorLog_stoppedField =. Just now] (AutoKeyField `in_` fmap fromId logIds)

          for_ (MMap.keys elin) $ notify . mkDefaultNotify
          for_ (MMap.keys elnwc) $ notify . mkDefaultNotify
          for_ (MMap.keys elbnh) $ notify . mkDefaultNotify

      PublicRequest_AddClient addr alias -> inDb $ do
        existingIds :: [Id Client] <- fmap toId <$> project AutoKeyField (Client_addressField ==. addr)
        case nonEmpty existingIds of
          Nothing -> void $ insertNotify Client
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

      PublicRequest_AddBaker pkh alias -> inDb $ do
        existingIds :: [Id Baker] <- fmap toId <$> project AutoKeyField (Baker_publicKeyHashField ==. pkh)
        let newVal = Baker
              { _baker_publicKeyHash = pkh
              , _baker_alias = alias
              , _baker_deleted = False
              }
        case nonEmpty existingIds of
          Nothing -> void $ insert newVal
          Just bIds -> for_ bIds $ \bId ->
            updateId (bId :: Id Baker) [Baker_deletedField =. False, Baker_aliasField =. alias]
        notify $ mkDefaultNotify newVal

      PublicRequest_RemoveBaker pkh -> inDb $ do
        bIds :: [Id Baker] <- fmap toId <$> project AutoKeyField (Baker_publicKeyHashField ==. pkh)
        let inIds = In bIds
        _ <- [executeQ| DELETE FROM "PendingReward" pr WHERE pr.baker IN ?inIds |]
        _ <- [executeQ| DELETE FROM "BakerDetails" ds WHERE ds."publicKeyHash" = ?pkh |]
        for_ bIds $ \bId -> do
          updateId bId [Baker_deletedField =. True]
          notify $ mkDefaultNotify $ Baker
            { _baker_publicKeyHash = pkh
            , _baker_alias = Nothing
            , _baker_deleted = True
            }

      PublicRequest_SendTestEmail email -> inDb $ void $ queueEmail
        (simpleMail'
          (Address Nothing email)
          emailFromAddr
          "Tezos Bake Monitor - Test"
          "This is a test email!"
        )
        Nothing

      -- TODO think harder about update versus initial set
      PublicRequest_SetMailServerConfig mailServerView recipients mPassword -> inDb $ do
        now <- getTime
        getDefaultMailServer >>= \case
          Nothing -> do
            let updatedMailServer = MailServerConfig
                  { _mailServerConfig_hostName = _mailServerView_hostName mailServerView
                  , _mailServerConfig_portNumber = _mailServerView_portNumber mailServerView
                  , _mailServerConfig_smtpProtocol = _mailServerView_smtpProtocol mailServerView
                  , _mailServerConfig_userName = _mailServerView_userName mailServerView
                  , _mailServerConfig_password = maybe "" id mPassword
                  , _mailServerConfig_madeDefaultAt = now
                  , _mailServerConfig_enabled = _mailServerView_enabled mailServerView
                  }
            void $ insertNotifyUnique updatedMailServer
          Just (id_, _) -> updateIdNotifyUnique id_ $
            [ MailServerConfig_hostNameField =. _mailServerView_hostName mailServerView
            , MailServerConfig_portNumberField =. _mailServerView_portNumber mailServerView
            , MailServerConfig_smtpProtocolField =. _mailServerView_smtpProtocol mailServerView
            , MailServerConfig_userNameField =. _mailServerView_userName mailServerView
            , MailServerConfig_madeDefaultAtField =. now
            , MailServerConfig_enabledField =. _mailServerView_enabled mailServerView
            ] ++
            [ MailServerConfig_passwordField =. password
            | password <- toList mPassword
            ]
        delete $ Notificatee_emailField `notIn_` recipients
        keep :: [Email] <- project Notificatee_emailField (Notificatee_emailField `in_` recipients)
        for_ ((Set.difference `on` Set.fromList) recipients keep) $ insertNotify . Notificatee

      PublicRequest_CheckForUpgrade ->
        void $ liftIO $ async $ runLoggingEnv (_nodeDataSource_logger nds) $
          void $ updateUpstreamVersion upgradeBranch (_nodeDataSource_httpMgr nds) inDb

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
          void $ liftIO $ async $ for_ (filter (\(pn, _, _) -> pn == publicNode) publicNodeSources) $
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
            cid' :: Maybe (Id TelegramConfig) <- getTelegramCfgId
            now <- getTime
            case cid' of
              Nothing -> insertNotifyUnique $ TelegramConfig
                { _telegramConfig_botApiKey = botApiKey
                , _telegramConfig_botName = botName
                , _telegramConfig_created = now
                , _telegramConfig_updated = now
                , _telegramConfig_enabled = enabled
                , _telegramConfig_validated = validated
                }

              Just cid -> do
                updateIdNotifyUnique cid
                  [ TelegramConfig_botNameField =. botName
                  , TelegramConfig_botApiKeyField =. botApiKey
                  , TelegramConfig_updatedField =. now
                  , TelegramConfig_enabledField =. enabled
                  , TelegramConfig_validatedField =. validated
                  ]
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

      PublicRequest_SetAlertNotificationMethodEnabled method enabled -> inDb $ do
        let f :: (PersistBackend m', MonadLogger m', _)
              => Text -> Field cfg cstr Bool -> Maybe (Id cfg) -> m' Bool
            f name enabledField = \case
              Just cid -> do
                updateIdNotifyUnique cid [enabledField =. enabled]
                pure True
              Nothing -> do
                $(logInfo) $ "Requested to " <> bool "enable" "disable" enabled <> " "
                  <> name <> " notifications, but no configuration set, so doing nothing."
                -- disabling the non existent config is trivially successful
                pure $ not enabled
        case method of
          AlertNotificationMethod_Email ->
            f "email" MailServerConfig_enabledField =<< (fst <$$> getDefaultMailServer)
          AlertNotificationMethod_Telegram ->
            f "Telegram" TelegramConfig_enabledField =<< getTelegramCfgId

      PublicRequest_ResolveAlert (tag :=> lid) -> inDb $ do
        elid_notifier' :: Maybe (Id ErrorLog, Notify) <- case tag of
          LogTag_InaccessibleNode -> pure Nothing
          LogTag_NodeWrongChain -> pure Nothing
          LogTag_BakerNoHeartbeat -> pure Nothing
          LogTag_BadNodeHead -> pure Nothing
          LogTag_MultipleBakersForSameBaker -> pure Nothing
          LogTag_BakerDeactivated -> pure Nothing
          LogTag_BakerDeactivationRisk -> pure Nothing

        for_ elid_notifier' $ \(elid, notifier) -> do
          now <- getTime
          updateId elid [ErrorLog_stoppedField =. Just now]
          notify notifier

    ApiRequest_Private _key r -> case r of
      PrivateRequest_NoOp -> return ()

  where
    inDb :: forall m' a. (MonadLogger m', MonadIO m', MonadBaseControl IO m') => DbPersist Postgresql m' a -> m' a
    inDb = runDb (Identity $ _nodeDataSource_pool nds)

getDefaultMailServer :: PersistBackend m => m (Maybe (Id MailServerConfig, MailServerConfig))
getDefaultMailServer =
  fmap (listToMaybe . MMap.toList) $
    selectMap' MailServerConfigConstructor $ CondEmpty `orderBy` [Desc MailServerConfig_madeDefaultAtField] `limitTo` 1

getTelegramCfgId :: PersistBackend m => m (Maybe (Id TelegramConfig))
getTelegramCfgId = toId <$$> listToMaybe <$> project AutoKeyField
  -- Silliness to help type inference:
  (TelegramConfig_enabledField ==. TelegramConfig_enabledField)
