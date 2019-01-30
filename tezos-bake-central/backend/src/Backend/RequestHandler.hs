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

module Backend.RequestHandler where

import Control.Concurrent.Async (async)
import Control.Exception.Safe (SomeException, try)
import Control.Monad.Logger (MonadLogger, logError, logInfo)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Foldable (toList)
import Data.Functor.Infix hiding ((<&>))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Monoidal as MMap
import qualified Data.Set as Set
import Data.Dependent.Sum (DSum ((:=>)))
import Database.Groundhog.Core (Field)
import Database.Groundhog.Postgresql
import Network.Mail.Mime (Address (..), simpleMail')
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))
import Rhyolite.Backend.DB (getTime, project1, runDb, selectMap')
import Rhyolite.Backend.DB.PsqlSimple (executeQ)
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

      PublicRequest_AddInternalNode -> inDb $ do
        getInternalNode >>= \case
          Nothing -> do
            let nodeData = NodeInternalData
                  { _nodeInternalData_running = True
                  , _nodeInternalData_state = NodeInternalState_Stopped
                  , _nodeInternalData_stateUpdated = Nothing
                  , _nodeInternalData_backend = Nothing
                  }

            nid <- insert' Node
            insert $ NodeInternal
              { _nodeInternal_id = nid
              , _nodeInternal_data = DeletableRow
                { _deletableRow_data = nodeData
                , _deletableRow_deleted = False
                }
              }
            notify $ Notify_NodeInternal nid $ Just $ nodeData

          Just (nid, nodeData) -> do
            when (_deletableRow_deleted nodeData || (not $ nodeData ^. deletableRow_data . nodeInternalData_running)) $ do
              update
                [ NodeInternal_dataField ~> DeletableRow_deletedSelector =. False
                , NodeInternal_dataField ~> DeletableRow_dataSelector ~> ProcessState_runningSelector =. True
                ]
                CondEmpty
              notify $ Notify_NodeInternal nid $ Just $ _deletableRow_data nodeData

      PublicRequest_AddExternalNode addr alias minPeerConn -> inDb $ do

        existingIds :: [Id Node] <- project NodeExternal_idField (NodeExternal_dataField ~> DeletableRow_dataSelector ~> NodeExternalData_addressSelector ==. addr)
        case nonEmpty existingIds of
          Nothing -> do
            nid <- insert' Node
            let nodeData = NodeExternalData
                    { _nodeExternalData_address = addr
                    , _nodeExternalData_alias = alias
                    , _nodeExternalData_minPeerConnections = minPeerConn
                    }
                node = NodeExternal
                  { _nodeExternal_id = nid
                  , _nodeExternal_data = DeletableRow
                    { _deletableRow_data = nodeData
                    , _deletableRow_deleted = False
                    }
                  }
            insert node
            notify $ Notify_NodeExternal nid $ Just nodeData
          Just nids -> for_ nids $ \nid -> do
            update
              [ NodeExternal_dataField ~> DeletableRow_deletedSelector =. False
              , NodeExternal_dataField ~> DeletableRow_dataSelector ~> NodeExternalData_aliasSelector =. alias
              , NodeExternal_dataField ~> DeletableRow_dataSelector ~> NodeExternalData_minPeerConnectionsSelector =. minPeerConn
              ]
              (NodeExternal_idField ==. nid)
            project (NodeExternal_dataField ~> DeletableRow_dataSelector)
                    (NodeExternal_idField ==. nid)
              >>= traverse_ (notify . Notify_NodeExternal nid . Just)

      PublicRequest_UpdateInternalNode shouldRun -> inDb $ do
        update
          [ NodeInternal_dataField ~> DeletableRow_dataSelector ~> NodeInternalData_runningSelector =. shouldRun ]
          CondEmpty

        (getInternalNode >>=) $ traverse_ $ \(nid, nodeData) ->
          notify $ Notify_NodeInternal nid $ Just $ _deletableRow_data nodeData

      PublicRequest_RemoveNode node -> inDb $ case node of
        Left addr -> do
          nids :: [Id Node] <- project NodeExternal_idField (NodeExternal_dataField ~> DeletableRow_dataSelector ~> NodeExternalData_addressSelector ==. addr)
          for_ nids $ \nid -> do
            update [NodeExternal_dataField ~> DeletableRow_deletedSelector =. True] (NodeExternal_idField ==. nid)
            notify $ Notify_NodeExternal nid Nothing
            clearErrors nid
        Right () -> do
          getInternalNode >>= \case
            Nothing -> pure ()
            Just (nid, _nodeData) -> do
              update
                [ NodeInternal_dataField ~> DeletableRow_deletedSelector =. True
                , NodeInternal_dataField ~> DeletableRow_dataSelector ~> NodeInternalData_runningSelector =. False
                ]
                CondEmpty
              clearErrors nid
              notify $ Notify_NodeInternal nid Nothing
        where
          clearErrors nid = do
            elin <- select (ErrorLogInaccessibleNode_nodeField ==. nid)
            elnwc <- select (ErrorLogNodeWrongChain_nodeField ==. nid)
            elbnh <- select (ErrorLogBadNodeHead_nodeField ==. nid)

            now <- getTime
            let
              logIds :: [Id ErrorLog] = mconcat
                [ _errorLogInaccessibleNode_log <$> elin
                , _errorLogNodeWrongChain_log <$> elnwc
                , _errorLogBadNodeHead_log <$> elbnh
                ]
            update [ErrorLog_stoppedField =. Just now] (AutoKeyField `in_` fmap fromId logIds)

            for_ elin $ notify . mkDefaultNotify  . (Id @ErrorLogInaccessibleNode) . _errorLogInaccessibleNode_log
            for_ elnwc $ notify . mkDefaultNotify . (Id @ErrorLogNodeWrongChain) . _errorLogNodeWrongChain_log
            for_ elbnh $ notify . mkDefaultNotify . (Id @ErrorLogBadNodeHead) . _errorLogBadNodeHead_log

      PublicRequest_AddClient addr alias -> inDb $ do

        existingIds :: [Id BakerDaemon] <- project BakerDaemonExternal_idField (BakerDaemonExternal_dataField ~> DeletableRow_dataSelector ~> BakerDaemonExternalData_addressSelector ==. addr)
        case nonEmpty existingIds of
          Nothing -> do
            nid <- insert' BakerDaemon
            let bakerDaemonData = BakerDaemonExternalData
                    { _bakerDaemonExternalData_address = addr
                    , _bakerDaemonExternalData_alias = alias
                    , _bakerDaemonExternalData_updated = Nothing
                    }
                bakerDaemon = BakerDaemonExternal
                  { _bakerDaemonExternal_id = nid
                  , _bakerDaemonExternal_data = DeletableRow
                    { _deletableRow_data = bakerDaemonData
                    , _deletableRow_deleted = False
                    }
                  }
            insert bakerDaemon
            notify $ Notify_BakerDaemonExternal nid $ Just bakerDaemonData
          Just nids -> for_ nids $ \nid -> do
            update
              [ BakerDaemonExternal_dataField ~> DeletableRow_deletedSelector =. False
              , BakerDaemonExternal_dataField ~> DeletableRow_dataSelector ~> BakerDaemonExternalData_aliasSelector =. alias
              -- Skip updated?
              ]
              (BakerDaemonExternal_idField ==. nid)
            project (BakerDaemonExternal_dataField ~> DeletableRow_dataSelector)
                    (BakerDaemonExternal_idField ==. nid)
              >>= traverse_ (notify . Notify_BakerDaemonExternal nid . Just)

      PublicRequest_RemoveClient addr -> inDb $ do
        nids :: [Id BakerDaemon] <- project BakerDaemonExternal_idField (BakerDaemonExternal_dataField ~> DeletableRow_dataSelector ~> BakerDaemonExternalData_addressSelector ==. addr)
        for_ nids $ \nid -> do
          update [BakerDaemonExternal_dataField ~> DeletableRow_deletedSelector =. True] (BakerDaemonExternal_idField ==. nid)
          notify $ Notify_BakerDaemonExternal nid Nothing

      -- TODO: use BakerRightsCycleProgress to fast-path update rights we already have in cache.
      PublicRequest_AddBaker pkh alias -> inDb $ do
        existingIds :: [Id Baker] <- fmap toId <$> project BakerKey (Baker_publicKeyHashField ==. pkh)
        let newVal = BakerData
              { _bakerData_alias = alias
              }
        case nonEmpty existingIds of
          Nothing -> void $ insert $ Baker
            { _baker_publicKeyHash = pkh
            , _baker_data = DeletableRow
              { _deletableRow_data = newVal
              , _deletableRow_deleted = False
              }
            }
          Just bIds -> for_ bIds $ \bId ->
            update [ Baker_dataField ~> DeletableRow_deletedSelector =. False
                   , Baker_dataField ~> DeletableRow_dataSelector ~> BakerData_aliasSelector =. alias
                   ]
                   (BakerKey ==. fromId bId)
        notify $ Notify_Baker (Id pkh) (Just newVal)

      PublicRequest_RemoveBaker pkh -> inDb $ do
        bIds :: [Id Baker] <- fmap toId <$> project BakerKey (Baker_publicKeyHashField ==. pkh)
        _ <- [executeQ| DELETE FROM "BakerDetails" ds WHERE ds."publicKeyHash" = ?pkh |]
        for_ bIds $ \bId -> do
          update
            [Baker_dataField ~> DeletableRow_deletedSelector =. True]
            (BakerKey ==. fromId bId)
          notify $ Notify_Baker (Id pkh) Nothing

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

      PublicRequest_ResolveAlert (tag :=> (Identity specificLog)) -> inDb $ do
        -- TODO: this is not the only place we encode knowledge of which alert types can be manually resolved
        elid_notifier' :: Maybe (Id ErrorLog, Notify) <- case tag of
          LogTag_InaccessibleNode -> pure Nothing
          LogTag_NodeWrongChain -> pure Nothing
          LogTag_BakerNoHeartbeat -> pure Nothing
          LogTag_BadNodeHead -> pure Nothing
          LogTag_NodeInvalidPeerCount -> pure Nothing
          LogTag_MultipleBakersForSameBaker -> pure Nothing
          LogTag_NetworkUpdate -> do
            let eid = _errorLogNetworkUpdate_log specificLog
            n <- fmap (Notify_ErrorLogNetworkUpdate . Id) . listToMaybe <$> project ErrorLogNetworkUpdate_logField (ErrorLogNetworkUpdate_logField `in_` [eid])
            return $ (,) <$> pure eid <*> n
          LogTag_BakerDeactivated -> pure Nothing
          LogTag_BakerDeactivationRisk -> pure Nothing
          LogTag_BakerMissed -> do
            let eid = _errorLogBakerMissed_log specificLog
            n <- fmap (Notify_ErrorLogBakerMissed . Id) . listToMaybe <$> project ErrorLogBakerMissed_logField (ErrorLogBakerMissed_logField `in_` [eid])
            return $ (,) <$> pure eid <*> n

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

getInternalNode :: PersistBackend m => m (Maybe (Id Node, DeletableRow NodeInternalData))
getInternalNode = project1 (NodeInternal_idField, NodeInternal_dataField) CondEmpty
