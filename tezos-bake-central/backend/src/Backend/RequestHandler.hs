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
import Control.Exception.Safe (SomeException, try)
import Control.Monad.Logger (MonadLogger, LoggingT, logError, logInfo, logDebug, logDebugNS, logErrorNS)
import Control.Monad.Trans.Except
import Control.Retry
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (toList)
import Data.Functor.Compose
import Data.Functor.Infix hiding ((<&>))
import Data.List (partition, dropWhileEnd)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Monoidal as MMap
import qualified Data.Set as Set
import Data.Some (Some(..))
import qualified Data.Text as T
import Data.Universe
import Database.Groundhog.Core (EntityConstr, Field)
import Database.Groundhog.Postgresql
import Database.Id.Class
import Database.Id.Groundhog
import Network.Mail.Mime (Address (..), simpleMail')
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (getTime, project1, runDb, selectMap', selectSingle)
import Rhyolite.Backend.DB.PsqlSimple (executeQ)
import Rhyolite.Backend.EmailWorker (queueEmail)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Schema (Email)
import Safe
import System.Directory (removeDirectoryRecursive)
import Text.Printf
import Text.URI (render)
import Tezos.Types (Tez, PublicKeyHash, LedgerIdentifier, toPublicKeyHashText)

import Backend.CachedNodeRPC (NodeDataSource (..))
import Backend.Common
import Backend.Config (AppConfig (..), nodeDataDir)
import Backend.Http (runHttpT)
import Backend.Alerts (resolveAlert, resolveAlerts)
import Backend.Schema
import qualified Backend.Telegram as Telegram
import Backend.Upgrade (updateUpstreamVersion)
import Backend.Workers.TezosClient (showLedger)
import Backend.Workers.Node (DataSource, updateDataSource)
import Common.Api (PrivateRequest (..), PublicRequest (..))
import Common.App
import Common.Schema
import ExtraPrelude

requestHandler
  :: forall m. (MonadBaseNoPureAborts IO m, MonadIO m)
  => AppConfig
  -> Address
  -> NodeDataSource
  -> [DataSource]
  -> RequestHandler (ApiRequest () PublicRequest PrivateRequest) m
requestHandler appConfig emailFromAddr nds publicNodeSources =
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
          , _connectedLedger_forceConnectivityCheck = True
          , _connectedLedger_walletAppVersion = Nothing
          }
      PublicRequest_ShowLedgerBatch sks -> inDb $ do

        existent <- select (foldr1 Or (map (embeddedSecretKeyEquals LedgerAccount_secretKeyField) sks))

        let insertAccount (sk, pkh, tez) = insert $ LedgerAccount
                { _ledgerAccount_secretKey = sk
                , _ledgerAccount_publicKeyHash = Just pkh
                , _ledgerAccount_balance = Just tez
                , _ledgerAccount_shouldImport = False
                , _ledgerAccount_imported = False
                , _ledgerAccount_shouldSetupToBake = False
                , _ledgerAccount_shouldRegisterFee = Nothing
                , _ledgerAccount_shouldSetHWM = Nothing
                , _ledgerAccount_shouldDoVoteProtocol = Nothing
                , _ledgerAccount_shouldDoVoteBallot = Nothing
                }

            updateAccount (sk, pkh, tez) =
                update
                    [LedgerAccount_balanceField =. Just tez, LedgerAccount_publicKeyHashField =. Just pkh]
                    $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk

            internalURI = T.unpack $ render $ _nodeDataSource_kilnNodeUri nds

            balancePath pkh = printf "/chains/main/blocks/head/context/contracts/%s/balance" (T.unpack $ toPublicKeyHashText pkh)

            balanceUrl pkh = dropWhileEnd (== '/') internalURI <> balancePath pkh

            fastGetBalanceFor pkh = do
                let mgr = _nodeDataSource_httpMgr nds

                tezResp :: Either Http.HttpException (Http.Response LB.ByteString) <-
                        liftIO $ try $ Http.httpLBS =<< (Http.setRequestManager mgr <$> Http.parseRequest (balanceUrl pkh))

                pure $ case tezResp of
                    Left err -> Left err
                    Right resp -> Right $ decode' @Tez $ Http.getResponseBody $ resp

            insertOrUpdateAccounts f sks' = do
              res <- runExceptT $ for sks' $ \sk -> do
                  mPkh <- withExceptT ((,) sk . Right) $ ExceptT $ showLedger appConfig (_appConfig_binaryPaths appConfig) sk
                  case mPkh of
                      Nothing -> do
                        notify NotifyTag_ShowLedger (sk, Left $ "tezosClientWorker:showLedger: public key hash unavailable")
                        return Nothing
                      Just pkh -> do

                          mTez <- withExceptT ((,) sk . Left) $ ExceptT $ do
                              let oneSecond :: Int
                                  oneSecond = 1e6
                                  defaultPolicy = limitRetriesByCumulativeDelay (5 * oneSecond) $ exponentialBackoff oneSecond

                                  toRetry mcd rs rr = do

                                    let doRetry =
                                          case rr of
                                            Left (Http.InvalidUrlException _ _) -> False
                                            Left (Http.HttpExceptionRequest _ hc) -> case hc of
                                                 Http.StatusCodeException _ _ -> True
                                                 Http.ResponseTimeout -> True

                                                 Http.ConnectionTimeout -> True
                                                 _ -> False
                                            _ -> False

                                    if not doRetry
                                        then logDebugNS "kiln-node" $ fold
                                         ["RPC call ("
                                         , T.pack $ balanceUrl pkh
                                         ,") succeeded after "
                                         , tshow (succ $ rsIterNumber rs)
                                         , " retries!"
                                         ]
                                        else do
                                            let isFinal = mcd <= rsCumulativeDelay rs + 2 * fromMaybe 0 (rsPreviousDelay rs)

                                            if isFinal
                                              then logDebugNS "kiln-node" $ fold
                                                   [ "For RPC call ("
                                                   , T.pack $ balanceUrl pkh
                                                   , ") this will be the final retry after "
                                                   , tshow (succ $ rsIterNumber rs)
                                                   , " retries."
                                                    ]
                                              else logDebugNS "kiln-node" $ fold
                                                   [ "Retrying rpc call ("
                                                    , T.pack $ balanceUrl pkh
                                                    , "). Attempt number: {"
                                                    , tshow (succ $ rsIterNumber rs)
                                                    , "}."
                                                   ]

                                    return doRetry

                              retrying defaultPolicy (toRetry (5 * oneSecond)) (const $ fastGetBalanceFor pkh)

                          case mTez of
                              Nothing -> do
                                logErrorNS "kiln-node" $ "Failed to get balance of account " <> toPublicKeyHashText pkh
                                return Nothing
                              Just tez -> do
                                  notify NotifyTag_ShowLedger (sk, Right (pkh, tez))
                                  return $ Just (sk, pkh, tez)
              case res of
                  Right rs -> mapM_ f (Compose rs)
                  Left (_, Right ClientError_LedgerDisconnected) -> do
                      now <- getTime
                      -- Mark ledger as disconnected
                      update
                          [ ConnectedLedger_ledgerIdentifierField =. (Nothing :: Maybe LedgerIdentifier)
                          , ConnectedLedger_bakingAppVersionField =. (Nothing :: Maybe Text)
                          , ConnectedLedger_updatedField =. Just now
                          ] CondEmpty
                  Left (sk, err) -> do
                      delete $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
                      notify NotifyTag_ShowLedger (sk, Left $ T.pack $ show err)
                      $(logError) (either tshow tshow err)

        case existent of
          [] -> insertOrUpdateAccounts insertAccount sks
          found ->  case partition (`elem` (_ledgerAccount_secretKey <$> found)) sks of
            (alreadyFound, notInDb) -> do
                insertOrUpdateAccounts updateAccount alreadyFound
                insertOrUpdateAccounts insertAccount notInDb

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

      PublicRequest_AddInternalNode mNodeProcessState -> inDb $ do
        let ps = maybe ProcessState_Stopped ProcessState_Node mNodeProcessState
            pc = maybe ProcessControl_Run (const ProcessControl_Stop) mNodeProcessState
        getInternalNode >>= \case
          Nothing -> do
            let processData = ProcessData
                  { _processData_control = pc
                  , _processData_state = ps
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
              update [ProcessData_controlField =. pc, ProcessData_stateField =. ps]
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

      PublicRequest_CancelSnapshotImport -> inDb $ do
        procControl <- project SnapshotMeta_controlField CondEmpty
        case headMay procControl of
          -- If the value is already Stop, then the last command was
          -- not completed succesfully, so do a force cleanup of the node
          Just ProcessControl_Stop -> removeNodeDbImpl (Right ())
          _ -> update [ SnapshotMeta_controlField =. ProcessControl_Stop ] CondEmpty

      PublicRequest_UpdateInternalWorker workerType shouldRun -> inDb $ case workerType of
        WorkerType_Node
          | shouldRun -> updateNode -- Only start node
          | otherwise -> do -- On stopping node, stop the baker also (if running)
              updateBakerDaemon
              updateNode
        WorkerType_Baker
          | not shouldRun -> updateBakerDaemon -- Only stop baker
          | otherwise -> do -- On starting baker, start the node also (if stopped)
              updateNode
              updateBakerDaemon
        where
          c = if shouldRun then ProcessControl_Run else ProcessControl_Stop
          updateBakerDaemon = do
            project1 (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty
              >>= traverse_ (\bdid -> do
                let bPid = _bakerDaemonInternalData_bakerProcessData bdid
                    ePid = _bakerDaemonInternalData_endorserProcessData bdid
                update [ProcessData_controlField =. c]
                  (AutoKeyField `in_` map fromId [bPid, ePid]))

          updateNode = do
            (getInternalNode >>=) $ traverse_ $ \(nid, nodeData) -> do
              let pid = _deletableRow_data nodeData
              update [ProcessData_controlField =. c] (AutoKeyField ==. fromId pid)
              processData <- getId $ _deletableRow_data nodeData
              notify NotifyTag_NodeInternal (nid, processData)

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
              onTag (Some tag) = case tag of
                BakerLogTag_BakerLedgerDisconnected -> deleteLogsId tag ErrorLogBakerLedgerDisconnected_bakerField
                BakerLogTag_BakerMissed -> deleteLogsId tag ErrorLogBakerMissed_bakerField
                BakerLogTag_BakerDeactivated -> deleteLogsPkh tag ErrorLogBakerDeactivated_publicKeyHashField
                BakerLogTag_BakerDeactivationRisk -> deleteLogsPkh tag ErrorLogBakerDeactivationRisk_publicKeyHashField
                BakerLogTag_BakerAccused -> deleteLogsId tag ErrorLogBakerAccused_bakerField
                BakerLogTag_InsufficientFunds -> deleteLogsId tag ErrorLogInsufficientFunds_bakerField
                BakerLogTag_VotingReminder -> deleteLogsId tag ErrorLogVotingReminder_bakerField

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
          void $ updateUpstreamVersion (_nodeDataSource_httpMgr nds) inDb

      PublicRequest_DismissUpgradeAlert -> inDb $ do
        update [ UpstreamVersion_dismissedField =. True ] CondEmpty
        mId <- project1 AutoKeyField (UpstreamVersion_dismissedField ==. UpstreamVersion_dismissedField)
        for_ mId $ \i -> do
          get i >>= traverse_ (notify NotifyTag_UpstreamVersion . (toId i,))

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
