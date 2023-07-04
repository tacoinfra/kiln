{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
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
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (tryPeekTQueue, writeTQueue)
import Control.Exception.Safe (MonadMask, SomeException, try)
import Control.Monad.Logger (LoggingT, MonadLoggerIO, MonadLogger, logError, logInfo, logDebug)
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Data.Foldable (toList)
import Data.Functor.Infix hiding ((<&>))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Monoidal as MMap
import qualified Data.Set as Set
import Data.Some (Some(..))
import Data.Universe
import Database.Groundhog.Core (EntityConstr, Field)
import Database.Groundhog.Postgresql
import Database.Id.Class
import Database.Id.Groundhog
import Network.Mail.Mime (Address (..), simpleMail')
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))
import Rhyolite.Backend.DB (MonadBaseNoPureAborts, getTime, project1, runDb, selectMap', selectSingle)
import Rhyolite.Backend.DB.PsqlSimple (executeQ)
import Rhyolite.Backend.DB.Serializable
import Rhyolite.Backend.EmailWorker (queueEmail)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Schema (Email)
import Safe
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import Tezos.Types (PublicKeyHash)

import Backend.Alerts (resolveAlert, resolveAlerts)
import Backend.Common
import Backend.Config (AppConfig (..), nodeDataDir)
import Backend.Http (runHttpT)
import Backend.NodeRPC (NodeDataSource (..))
import Backend.Schema
import Backend.Snapshot
import qualified Backend.Telegram as Telegram
import Backend.Upgrade (updateUpstreamVersion)
import Backend.Process.Common (updateProcessState)
import Backend.Workers.TezosClient
  (importSecretKey, isKnownLedgerPkh, registerKeyAsDelegate, setHighWaterMark, setupLedgerToBake,
  showLedger, submitVote, updateConnectedLedgerViaGetConnectedLedger)
import Common.Api (PrivateRequest (..), PublicRequest (..))
import Common.App
import Common.Schema
import ExtraPrelude

requestHandler
  :: forall m. (MonadBaseNoPureAborts IO m, MonadIO m, MonadMask m, MonadUnliftIO m)
  => AppConfig
  -> NodeDataSource
  -> RequestHandler (ApiRequest () PublicRequest PrivateRequest) m
requestHandler appConfig nds =
  RequestHandler $ \case
    ApiRequest_Public r -> runLoggingEnv (_nodeDataSource_logger nds) $ case r of

      PublicRequest_PollLedgerDevice ->
        liftIO $ atomically $ do
          nextQuery <- tryPeekTQueue ledgerIOQueue
          -- If the next query is ledger connectivity, avoid adding another check to the queue
          case nextQuery of
            Just (LedgerQuery LedgerQueryType_PollLedger _) -> pure ()
            _ -> writeTQueue ledgerIOQueue $ updateConnectedLedgerViaGetConnectedLedger appConfig db
      PublicRequest_ShowLedgerBatch sks -> showLedgers sks
      PublicRequest_ShowLedger sk -> showLedgers [sk]
      PublicRequest_SetLiquidityBakingToggle pkh shouldRestartBaker lqdtyToggle -> inDb $ do
        let
          chainId = _appConfig_chainId appConfig
          lqdtyBakingExtraArg = toBakerExtraArgs lqdtyToggle pkh chainId
          cond =
            BakerExtraArgs_publicKeyHashField ==. pkh &&.
            BakerExtraArgs_chainIdField ==. chainId &&.
            BakerExtraArgs_optionField ==. _bakerExtraArgs_option lqdtyBakingExtraArg
        existingArg <- selectSingle cond
        case existingArg of
          Nothing -> insert lqdtyBakingExtraArg
          Just _ -> update
            [ BakerExtraArgs_valueField =. _bakerExtraArgs_value lqdtyBakingExtraArg
            ] cond
        when shouldRestartBaker restartBakerDaemon
      PublicRequest_ImportSecretKey sk ->
        queryLedger $ importSecretKey appConfig db sk
      PublicRequest_SetupLedgerToBake sk ->
        queryLedger $ setupLedgerToBake appConfig db nds sk
      PublicRequest_RegisterKeyAsDelegate sk ->
        queryLedger $ registerKeyAsDelegate db nds sk appConfig
      PublicRequest_SetHWM sk bl ->
        queryLedger $ setHighWaterMark appConfig db sk bl

      req@(PublicRequest_AddInternalNode mNodeProcessState) -> validateAddInternalNodeRequest req appConfig $ do
        inDb $ do
          let ps = maybe ProcessState_Stopped (ProcessState_Node . fst) mNodeProcessState
              pc = maybe ProcessControl_Run (const ProcessControl_Stop) mNodeProcessState
          getInternalNode >>= \case
            Nothing -> do
              let processData = ProcessData
                    { _processData_control = pc
                    , _processData_state = ps
                    , _processData_updated = Nothing
                    , _processData_backend = Nothing
                    , _processData_errorLog = Nothing
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
                update
                  [ ProcessData_controlField =. pc
                  , ProcessData_stateField =. ps
                  , ProcessData_errorLogField =. (Nothing :: Maybe Text)
                  ] (AutoKeyField ==. fromId (nodeData ^. deletableRow_data))
                notify NotifyTag_NodeInternal (nid, Just processData)
        case mNodeProcessState of
          Just (NodeProcessState_DownloadingSnapshot, SnapshotImportSource_UriSource u) ->
            handleDownloadSnapshotByUrlOverloaded appConfig nds u
          Just (NodeProcessState_DownloadingSnapshot, SnapshotImportSource_KnownSnapshotProviderSource p) ->
            handleDownloadSnapshotFromProviderAsync appConfig nds p
          Just (NodeProcessState_ImportingSnapshot, SnapshotImportSource_FilePathSource fp) ->
            handleSnapshotFilePathImport appConfig nds fp
          Nothing ->
            liftIO $ createDirectoryIfMissing True (nodeDataDir appConfig)
          _ -> return ()

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

      PublicRequest_CancelSnapshotImport -> inDb $ do
        procControl <- project SnapshotMeta_controlField CondEmpty
        case headMay procControl of
          -- If the value is already Stop, then the last command was
          -- not completed succesfully, so do a force cleanup of the node
          Just ProcessControl_Stop -> removeNodeDbImpl (Right ())
          _ -> update [ SnapshotMeta_controlField =. ProcessControl_Stop ] CondEmpty

      PublicRequest_CancelSnapshotDownload -> inDb $ do
        getInternalNode >>= \case
          Nothing -> return ()
          Just (nid, nodeData) -> do
            let pdid = nodeData ^. deletableRow_data
            updateProcessState pdid
              (Just (\pd -> (NotifyTag_NodeInternal, (nid, pd))))
              (ProcessState_Node NodeProcessState_DownloadCanceled)

      PublicRequest_UpdateInternalWorker workerType shouldRun -> inDb $ case workerType of
        WorkerType_Node
          | shouldRun -> startNodeDaemon -- Only start node
          | otherwise -> do -- On stopping node, stop the baker also (if running)
              stopBakerDaemon
              stopNodeDaemon
        WorkerType_Baker
          | not shouldRun -> stopBakerDaemon -- Only stop baker
          | otherwise -> do -- On starting baker, start the node also (if stopped)
              startNodeDaemon
              startBakerDaemon
      PublicRequest_RemoveNode node -> do
        inDb $ removeNodeDbImpl node
        when (isRight node) $
          void $ liftIO $ async $ runLoggingEnv (_nodeDataSource_logger nds) removeDataDir
        where
          removeDataDir = do
            let dataDir = nodeDataDir appConfig
            $(logDebug) ("Removing Kiln node's data dir: " <> tshow dataDir)
            liftIO $ removeDirectoryRecursive dataDir

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
            update
              [ ProcessData_controlField =. ProcessControl_Stop
              , ProcessData_errorLogField =. (Nothing :: Maybe Text)
              ] $ AutoKeyField ==. bakerProcess
          update
            [BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector =. True]
            (data' ~> BakerDaemonInternalData_publicKeyHashSelector ==. Just pkh)
          clearErrors bId
          notify NotifyTag_Baker (Id pkh, Nothing)
        where
          clearErrors :: Id Baker -> Serializable ()
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

              onTag :: Some BakerLogTag -> Serializable [Id ErrorLog]
              onTag (Some tag) = case tag of
                BakerLogTag_BakerLedgerDisconnected -> deleteLogsId tag ErrorLogBakerLedgerDisconnected_bakerField
                BakerLogTag_BakerMissed -> deleteLogsId tag ErrorLogBakerMissed_bakerField
                BakerLogTag_BakerDeactivated -> deleteLogsPkh tag ErrorLogBakerDeactivated_publicKeyHashField
                BakerLogTag_BakerDeactivationRisk -> deleteLogsPkh tag ErrorLogBakerDeactivationRisk_publicKeyHashField
                BakerLogTag_BakerAccused -> deleteLogsId tag ErrorLogBakerAccused_bakerField
                BakerLogTag_InsufficientFunds -> deleteLogsId tag ErrorLogInsufficientFunds_bakerField
                BakerLogTag_VotingReminder -> deleteLogsId tag ErrorLogVotingReminder_bakerField
                BakerLogTag_MissedEndorsementBonus -> deleteLogsId tag ErrorLogBakerMissedEndorsementBonus_bakerField
                BakerLogTag_NeedToResetHWM -> deleteLogsId tag ErrorLogBakerNeedToResetHWM_bakerField
            ids <- fmap concat $ for universe onTag
            now <- getTime
            update [ErrorLog_stoppedField =. Just now] (AutoKeyField `in_` fmap fromId ids)

      PublicRequest_SendTestEmail email ->
        let fromAddr = _appConfig_emailFromAddress appConfig ?: Address Nothing email
        in inDb $ void $ queueEmail
        (simpleMail'
          (Address Nothing email)
          fromAddr
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
          void $ updateUpstreamVersion (_nodeDataSource_httpMgr nds) inDb

      PublicRequest_DismissUpgradeAlert -> inDb $ do
        update [ UpstreamVersion_dismissedField =. True ] CondEmpty
        mId <- project1 AutoKeyField (UpstreamVersion_dismissedField ==. UpstreamVersion_dismissedField)
        for_ mId $ \i -> do
          get i >>= traverse_ (notify NotifyTag_UpstreamVersion . (toId i,))

      PublicRequest_AddTelegramConfig apiKey -> do
        -- Initialize the config to have NULL bot name and NULL enabled.
        -- NULL enabled means the bot is not yet validated.
        $(logInfo) "Adding a Telegram configuration"
        cid <- inDb $ updateTelegramCfg apiKey Nothing True Nothing

        -- Fork a thread to collect meta info about this bot.
        void $ liftIO $ async $ runLoggingEnv (_nodeDataSource_logger nds) $
          connectTelegram apiKey cid

        where
          connectTelegram botApiKey cid = do
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
                -- Since we sample recipient in frontend based on update on telegram config
                -- update this before updating the telegram config
                rid <- updateRecipient cid chat sender
                _ <- updateTelegramCfg botApiKey (Just botName) True (Just True)
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

      PublicRequest_DoVote sk p b ->
        queryLedger $ submitVote appConfig db nds sk p b

      PublicRequest_RestartKilnBaker -> inDb restartBakerDaemon

    ApiRequest_Private _key r -> case r of
      PrivateRequest_NoOp -> return ()

  where
    ledgerIOQueue = _nodeDataSource_ledgerIOQueue nds
    db = _nodeDataSource_pool nds
    queryLedger :: LedgerQuery (LoggingT IO) -> LoggingT m ()
    queryLedger action = liftIO $ atomically $ writeTQueue ledgerIOQueue action
    inDb :: forall m' a. (MonadLoggerIO m', MonadLogger m', MonadIO m', MonadBaseNoPureAborts IO m') => Serializable a -> m' a
    inDb = runDb (Identity $ _nodeDataSource_pool nds)
    showLedgers sks = do
      for_ (reverse sks) $ \sk -> do
        isKnown <- isKnownLedgerPkh appConfig db sk
        unless isKnown $ queryLedger $ showLedger appConfig db nds sk

getDefaultMailServer :: PersistBackend m => m (Maybe (Id MailServerConfig, MailServerConfig))
getDefaultMailServer =
  fmap (listToMaybe . MMap.toList) $
    selectMap' MailServerConfigConstructor $ CondEmpty `orderBy` [Desc MailServerConfig_madeDefaultAtField] `limitTo` 1

getTelegramCfgId :: PersistBackend m => m (Maybe (Id TelegramConfig))
getTelegramCfgId = toId <$$> listToMaybe <$> project AutoKeyField
  -- Silliness to help type inference:
  (TelegramConfig_enabledField ==. TelegramConfig_enabledField)

validateAddInternalNodeRequest
  :: (MonadBaseNoPureAborts IO m, MonadIO m, MonadMask m, MonadUnliftIO m, MonadLogger m)
  => PublicRequest (Either AddInternalNodeError ())
  -> AppConfig
  -> m ()
  -> m (Either AddInternalNodeError ())
validateAddInternalNodeRequest req appConfig reqHandler =
  case req of
    PublicRequest_AddInternalNode (Just (_, SnapshotImportSource_FilePathSource fp)) -> do
      validationRes <- validateSnapshotFilePath appConfig fp
      case validationRes of
        Left e -> pure $ Left $ AddInternalNodeError_SnapshotImportError e
        Right () -> Right <$> reqHandler
    _ -> Right <$> reqHandler
