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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Backend.RequestHandler where

import Control.Concurrent.Async (async)
import Control.Exception.Safe (SomeException, try)
import Control.Monad.Logger (MonadLogger, LoggingT, logError, logInfo)
import Data.Foldable (toList)
import Data.Functor.Infix hiding ((<&>))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Monoidal as MMap
import qualified Data.Set as Set
import Data.Some (Some(This))
import Data.Universe
import Database.Groundhog.Core (EntityConstr, Field)
import Database.Groundhog.Postgresql
import Network.Mail.Mime (Address (..), simpleMail')
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (getTime, project1, runDb, selectMap', selectSingle)
import Rhyolite.Backend.DB.PsqlSimple (executeQ)
import Rhyolite.Backend.EmailWorker (queueEmail)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Schema (Email, Id (..), IdData)
import Tezos.Types (Tez, PublicKeyHash)

import Backend.CachedNodeRPC (NodeDataSource (..))
import Backend.Http (runHttpT)
import Backend.Alerts (resolveAlert, resolveAlerts)
import Backend.Schema
import qualified Backend.Telegram as Telegram
import Backend.Upgrade (updateUpstreamVersion)
import Backend.Workers.Node (DataSource, updateDataSource)
import Backend.Workers.TezosClient (addBakerImpl)
import Backend.Common
import Common.Api (PrivateRequest (..), PublicRequest (..))
import Common.App
import Common.Schema
import ExtraPrelude

requestHandler
  :: forall m. (MonadBaseNoPureAborts IO m, MonadIO m)
  => Text
  -> Address
  -> NodeDataSource
  -> [DataSource]
  -> RequestHandler Bake m
requestHandler upgradeBranch emailFromAddr nds publicNodeSources =
  RequestHandler $ \case
    ApiRequest_Public r -> runLoggingEnv (_nodeDataSource_logger nds) $ case r of

      PublicRequest_PollLedgerDevice -> inDb $ do
        deleteAll' @ConnectedLedger Proxy
        -- Deliberately don't notify here: let the worker pick it up and notify
        -- as required
        insert $ ConnectedLedger
          { _connectedLedger_bakingAppVersion = Nothing
          , _connectedLedger_ledgerIdentifier = Nothing
          , _connectedLedger_updated = Nothing
          , _connectedLedger_walletAppVersion = Nothing
          }
      PublicRequest_ShowLedger sk -> inDb $ do
        existing <- selectSingle $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
        case existing of
          Just _ -> update [LedgerAccount_balanceField =. (Nothing :: Maybe Tez)] $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
          Nothing -> insert $ LedgerAccount
            { _ledgerAccount_secretKey = sk
            , _ledgerAccount_publicKeyHash = Nothing
            , _ledgerAccount_balance = Nothing
            , _ledgerAccount_shouldImport = False
            , _ledgerAccount_imported = False
            , _ledgerAccount_shouldSetupToBake = False
            , _ledgerAccount_shouldRegisterFee = Nothing
            , _ledgerAccount_shouldSetHWM = Nothing
            , _ledgerAccount_shouldDoVoteProtocol = Nothing
            , _ledgerAccount_shouldDoVoteBallot = Nothing
            }
      PublicRequest_ImportSecretKey sk -> inDb $ do
        update [LedgerAccount_shouldImportField =. True] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
      PublicRequest_SetupLedgerToBake sk -> inDb $ do
        update [LedgerAccount_shouldSetupToBakeField =. True] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
      PublicRequest_RegisterKeyAsDelegate sk fee -> inDb $ do
        update [LedgerAccount_shouldRegisterFeeField =. Just fee] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
      PublicRequest_SetHWM sk bl -> inDb $ do
        update [LedgerAccount_shouldSetHWMField =. Just bl] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)

      PublicRequest_AddInternalNode -> inDb $ do
        getInternalNode >>= \case
          Nothing -> do
            let processData = ProcessData
                  { _processData_control = ProcessControl_Stop
                  , _processData_state = ProcessState_Stopped
                  , _processData_updated = Nothing
                  , _processData_backend = Nothing
                  }

            pdid <- insert' processData
            nid <- insert' Node
            insert $ NodeInternal
              { _nodeInternal_id = nid
              , _nodeInternal_data = DeletableRow
                { _deletableRow_data = pdid
                , _deletableRow_deleted = False
                }
              }
            notify NotifyTag_NodeInternal (nid, Just processData)

          Just (nid, nodeData) -> do
            processData <- do
              getId (nodeData ^. deletableRow_data) >>= \case
                Nothing -> error "NodeInternal ProcessData not found"
                (Just v) -> pure v
            when (_deletableRow_deleted nodeData || (ProcessControl_Stop == _processData_control processData)) $ do
              update
                [ NodeInternal_dataField ~> DeletableRow_deletedSelector =. False
                ]
                (NodeInternal_idField ==. nid)
              update [ProcessData_controlField =. ProcessControl_Run]
                (AutoKeyField ==. fromId (nodeData ^. deletableRow_data))
              notify NotifyTag_NodeInternal (nid, Just processData)

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
            notify NotifyTag_NodeExternal (nid, Just nodeData)
          Just nids -> for_ nids $ \nid -> do
            update
              [ NodeExternal_dataField ~> DeletableRow_deletedSelector =. False
              , NodeExternal_dataField ~> DeletableRow_dataSelector ~> NodeExternalData_aliasSelector =. alias
              , NodeExternal_dataField ~> DeletableRow_dataSelector ~> NodeExternalData_minPeerConnectionsSelector =. minPeerConn
              ]
              (NodeExternal_idField ==. nid)
            project (NodeExternal_dataField ~> DeletableRow_dataSelector)
                    (NodeExternal_idField ==. nid)
              >>= traverse_ (notify NotifyTag_NodeExternal . (nid,) . Just)

      PublicRequest_UpdateInternalWorker workerType shouldRun -> case workerType of
        WorkerType_Node
          | shouldRun -> void $ updateNode True -- Only start node
          | otherwise -> do -- On stopping node, stop the baker also (if running)
          updateBaker False (Nothing :: Maybe (Id ProcessData))
          void $ updateNode False
        WorkerType_Baker
          | not shouldRun -> updateBaker False (Nothing :: Maybe (Id ProcessData)) -- Only stop baker
          | otherwise -> do -- On starting baker, start the node also (if stopped)
          updateNode True >>= updateBaker True
        where
          updateBaker shouldRun' mPid = if shouldRun'
            then mapM_ waitForNodeToStart mPid
            else updateBakerDaemon shouldRun'
            where
              waitForNodeToStart pid =
                inDb (project1 ProcessData_stateField
                  (AutoKeyField ==. fromId pid)) >>= \case
                Nothing -> return ()
                Just ProcessState_Failed -> return ()
                Just ProcessState_Running -> updateBakerDaemon shouldRun'
                _ -> threadDelay' 1 *> waitForNodeToStart pid

          updateBakerDaemon shouldRun' = inDb $
            project1 (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty
              >>= traverse_ (\bdid -> do
                let bPid = _bakerDaemonInternalData_bakerProcessData bdid
                    ePid = _bakerDaemonInternalData_endorserProcessData bdid
                    c = if shouldRun' then ProcessControl_Run else ProcessControl_Stop
                update [ProcessData_controlField =. c]
                  (AutoKeyField `in_` map fromId [bPid, ePid]))

          updateNode shouldRun' = inDb $
            (getInternalNode >>=) $ traverse $ \(nid, nodeData) -> do
              let pid = _deletableRow_data nodeData
                  c = if shouldRun' then ProcessControl_Run else ProcessControl_Stop
              update [ProcessData_controlField =. c] (AutoKeyField ==. fromId pid)
              processData <- getId $ _deletableRow_data nodeData
              notify NotifyTag_NodeInternal (nid, processData)
              return pid

      PublicRequest_RemoveNode node -> inDb $ case node of
        Left addr -> do
          nids :: [Id Node] <- project NodeExternal_idField (NodeExternal_dataField ~> DeletableRow_dataSelector ~> NodeExternalData_addressSelector ==. addr)
          for_ nids $ \nid -> do
            update [NodeExternal_dataField ~> DeletableRow_deletedSelector =. True] (NodeExternal_idField ==. nid)
            notify NotifyTag_NodeExternal (nid, Nothing)
            clearErrors nid
        Right () -> do
          getInternalNode >>= \case
            Nothing -> pure ()
            Just (nid, nodeData) -> do
              update
                [ NodeInternal_dataField ~> DeletableRow_deletedSelector =. True
                ]
                CondEmpty
              let pid = _deletableRow_data nodeData
              update [ProcessData_controlField =. ProcessControl_Stop] (AutoKeyField ==. fromId pid)
              clearErrors nid
              notify NotifyTag_NodeInternal (nid, Nothing)
        where
          clearErrors nid = do
            let
              deleteLogs :: forall cstr m' t.
                            ( Monad m', PersistBackend m'
                            , IdData t ~ Id ErrorLog, HasDefaultNotify (Id t), EntityConstr t cstr)
                         => NodeLogTag t
                         -> Field t cstr (Id Node)
                         -> m' [Id ErrorLog]
              deleteLogs tag field = do
                ids <- errorLogIdForNodeLogTag tag <$$> select (field ==. nid)
                for_ ids $ notifyDefault . Id @t
                pure ids

              onTag :: Some NodeLogTag -> DbPersist Postgresql (LoggingT m) [Id ErrorLog]
              onTag (This tag) = case tag of
                NodeLogTag_InaccessibleNode -> deleteLogs tag ErrorLogInaccessibleNode_nodeField
                NodeLogTag_NodeWrongChain -> deleteLogs tag ErrorLogNodeWrongChain_nodeField
                NodeLogTag_BadNodeHead -> deleteLogs tag ErrorLogBadNodeHead_nodeField
                NodeLogTag_NodeInvalidPeerCount -> deleteLogs tag ErrorLogNodeInvalidPeerCount_nodeField

            ids <- fmap concat $ for universe onTag
            now <- getTime
            update [ErrorLog_stoppedField =. Just now] (AutoKeyField `in_` fmap fromId ids)

      -- TODO: use BakerRightsCycleProgress to fast-path update rights we already have in cache.
      PublicRequest_AddBaker pkh alias -> inDb $ addBakerImpl pkh alias

      PublicRequest_RemoveBaker pkh -> inDb $ do
        bIds :: [Id Baker] <- fmap toId <$> project BakerKey (Baker_publicKeyHashField ==. pkh)
        _ <- [executeQ| DELETE FROM "BakerDetails" ds WHERE ds."publicKeyHash" = ?pkh |]
        for_ bIds $ \bId -> do
          update
            [Baker_dataField ~> DeletableRow_deletedSelector =. True]
            (BakerKey ==. fromId bId)
          let data' = BakerDaemonInternal_dataField ~> DeletableRow_dataSelector
          selectSingle (data' ~> BakerDaemonInternalData_publicKeyHashSelector ==. Just pkh) >>= \m -> for_ m $ \bdi -> do
            let bdid = _deletableRow_data $ _bakerDaemonInternal_data bdi
                bakerProcess = fromId $ _bakerDaemonInternalData_bakerProcessData bdid
                endorserProcess = fromId $ _bakerDaemonInternalData_endorserProcessData bdid
            update [ProcessData_controlField =. ProcessControl_Stop] $ AutoKeyField `in_` [bakerProcess, endorserProcess]
          update
            [BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector =. True]
            (data' ~> BakerDaemonInternalData_publicKeyHashSelector ==. Just pkh)
          clearErrors bId
          notify NotifyTag_Baker (Id pkh, Nothing)
        where
          clearErrors bid = do
            let
              -- TODO: Unify the types of the baker alert columns so this duplication isn't needed.
              deleteLogsId
                :: forall cstr m' t.
                 ( Monad m', PersistBackend m'
                 , IdData t ~ Id ErrorLog, HasDefaultNotify (Id t), EntityConstr t cstr
                 )
                => BakerLogTag t
                -> Field t cstr (Id Baker)
                -> m' [Id ErrorLog]
              deleteLogsId tag field = do
                ids <- errorLogIdForBakerLogTag tag <$$> select (field ==. bid)
                for_ ids $ notifyDefault . Id @t
                pure ids

              deleteLogsPkh
                :: forall cstr m' t.
                 ( Monad m', PersistBackend m'
                 , IdData t ~ Id ErrorLog, HasDefaultNotify (Id t), EntityConstr t cstr
                 )
                => BakerLogTag t
                -> Field t cstr PublicKeyHash
                -> m' [Id ErrorLog]
              deleteLogsPkh tag field = do
                ids <- errorLogIdForBakerLogTag tag <$$> select (field ==. unId bid)
                for_ ids $ notifyDefault . Id @t
                pure ids

              onTag :: Some BakerLogTag -> DbPersist Postgresql (LoggingT m) [Id ErrorLog]
              onTag (This tag) = case tag of
                BakerLogTag_MultipleBakersForSameBaker -> deleteLogsPkh tag ErrorLogMultipleBakersForSameBaker_publicKeyHashField
                BakerLogTag_BakerMissed -> deleteLogsId tag ErrorLogBakerMissed_bakerField
                BakerLogTag_BakerDeactivated -> deleteLogsPkh tag ErrorLogBakerDeactivated_publicKeyHashField
                BakerLogTag_BakerDeactivationRisk -> deleteLogsPkh tag ErrorLogBakerDeactivationRisk_publicKeyHashField
                BakerLogTag_BakerAccused -> deleteLogsId tag ErrorLogBakerAccused_bakerField
                BakerLogTag_InsufficientFunds -> deleteLogsId tag ErrorLogInsufficientFunds_bakerField

            ids <- fmap concat $ for universe onTag
            now <- getTime
            update [ErrorLog_stoppedField =. Just now] (AutoKeyField `in_` fmap fromId ids)

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
                  , _mailServerConfig_password = fromMaybe "" mPassword
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
              in notify NotifyTag_PublicNodeConfig . (,pnc) =<< insert' pnc
            Just cid -> do
              updateId cid
                [ PublicNodeConfig_sourceField =. publicNode
                , PublicNodeConfig_enabledField =. enabled
                , PublicNodeConfig_updatedField =. now
                ]
              getId cid >>= traverse_ (notify NotifyTag_PublicNodeConfig . (cid,))

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

          updateRecipient cid chat sender = do
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
                notify NotifyTag_TelegramRecipient (rid, Just new)
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
                notify NotifyTag_TelegramRecipient . (rid,) =<< getId rid
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

      PublicRequest_ResolveAlert elv -> inDb $ resolveAlert elv
      PublicRequest_ResolveAlerts dm -> inDb $ resolveAlerts dm

      PublicRequest_SetRightNotificationSettings rk mLimit -> inDb $ do
        let pk = RightNotificationSettings_rightKindField ==. rk
        case mLimit of
          Nothing -> delete pk
          Just limit -> selectSingle pk >>= \case --upsert
            Nothing -> insert $ RightNotificationSettings
              { _rightNotificationSettings_rightKind = rk
              , _rightNotificationSettings_limit = limit
              }
            Just _ -> update [RightNotificationSettings_limitField =. limit] pk
        notify NotifyTag_RightNotificationSettings (rk, mLimit)

      PublicRequest_DoVote sk p b -> inDb $
        update
          [ LedgerAccount_shouldDoVoteProtocolField =. Just p
          , LedgerAccount_shouldDoVoteBallotField =. b
          ]
          (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)

    ApiRequest_Private _key r -> case r of
      PrivateRequest_NoOp -> return ()

  where
    inDb :: forall m' a. (MonadLogger m', MonadIO m', MonadBaseNoPureAborts IO m') => DbPersist Postgresql m' a -> m' a
    inDb = runDb (Identity $ _nodeDataSource_pool nds)

getDefaultMailServer :: PersistBackend m => m (Maybe (Id MailServerConfig, MailServerConfig))
getDefaultMailServer =
  fmap (listToMaybe . MMap.toList) $
    selectMap' MailServerConfigConstructor $ CondEmpty `orderBy` [Desc MailServerConfig_madeDefaultAtField] `limitTo` 1

getTelegramCfgId :: PersistBackend m => m (Maybe (Id TelegramConfig))
getTelegramCfgId = toId <$$> listToMaybe <$> project AutoKeyField
  -- Silliness to help type inference:
  (TelegramConfig_enabledField ==. TelegramConfig_enabledField)

getInternalNode :: PersistBackend m => m (Maybe (Id Node, DeletableRow (Id ProcessData)))
getInternalNode = project1 (NodeInternal_idField, NodeInternal_dataField) CondEmpty
