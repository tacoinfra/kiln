{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Backend.Errors where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Either.Combinators (leftToMaybe, rightToMaybe)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Maybe (listToMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime)
import Data.Version (Version)
import Database.Groundhog
import Database.Groundhog.Core (PersistEntity)
import Database.Groundhog.Postgresql (PersistBackend)
import Network.Mail.Mime (Address (..), Mail, simpleMail')
import Rhyolite.Backend.DB (getTime)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (Only (..), PostgresRaw, executeQ, queryQ)
import Rhyolite.Backend.EmailWorker (queueEmail)
import Rhyolite.Backend.Listen (NotificationType (..), insertAndNotify, insertAndNotify_, notifyEntityId,
                                updateAndNotify)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Schema (Id)

import Tezos.Types

import Backend.Config (AppConfig (..), HasAppConfig, askAppConfig)
import Backend.Schema
import Common.Schema
import Common.Verification (ForkInfo (..), ForkStatus (..), showBadFork)

mailFor :: Address -> Text -> [Error] -> Mail
mailFor fromAddr toAddr errs =
  let
    toA = Address Nothing toAddr
    body = TL.fromStrict . T.unlines $ [T.pack (show t) <> ": " <> e | Error t e <- errs]
  in simpleMail' toA fromAddr "Error from Tezos bake monitor" body

queueAllEmails ::
  ( PersistBackend m, PostgresLargeObject m, MonadIO m
  , MonadReader a m, HasAppConfig a) => [Error] -> m ()
queueAllEmails message = do
  ns <- select CondEmpty
  fromAddr <- _appConfig_emailFromAddress <$> askAppConfig
  for_ ns $ \n ->
    queueEmail (mailFor fromAddr (_notificatee_email n) message) Nothing


reportNoBakerHeartbeatError
  :: (Monad m, PersistBackend m, PostgresRaw m, PostgresLargeObject m, MonadIO m
     , MonadReader a m, HasAppConfig a)
  => Id Client -> SeenEvent -> m ()
reportNoBakerHeartbeatError cid eventDetail = do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogBakerNoHeartbeat) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.id
    FROM "ErrorLog" el
    JOIN "ErrorLogBakerNoHeartbeat" t ON t.log = el.id
    WHERE t.cid = ?cid AND el.stopped IS NULL
    ORDER BY el."lastSeen" DESC, el.started DESC
    LIMIT 1
  |]
  let
    seenLevel = _seenEvent_level eventDetail
    seenHash = _seenEvent_hash eventDetail
  case existingLog of
    Nothing -> do
      insertErrorLog $ \logId -> ErrorLogBakerNoHeartbeat
        { _errorLogBakerNoHeartbeat_log = logId
        , _errorLogBakerNoHeartbeat_lastLevel = seenLevel
        , _errorLogBakerNoHeartbeat_lastBlockHash = seenHash
        , _errorLogBakerNoHeartbeat_client = cid
        }

      client :: Maybe Client <- get $ fromId cid
      now <- getTime
      queueAllEmails [Error
        { _error_time = now
        , _error_text = "Baker at " <> maybe "?" _client_address client <> " has not seen a block for while!"
        }]
    Just (logId, specificLogId) -> do
      updateErrorLogBy logId specificLogId
        [ ErrorLogBakerNoHeartbeat_lastLevelField =. seenLevel
        , ErrorLogBakerNoHeartbeat_lastBlockHashField =. seenHash
        ]


clearNoBakerHeartbeatError :: (Monad m, PostgresRaw m, PersistBackend m) => Id Client -> m ()
clearNoBakerHeartbeatError cid = do
  lids :: [Id ErrorLogBakerNoHeartbeat] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogBakerNoHeartbeat" t
    WHERE t.log = el.id AND t.client = ?cid AND el.stopped IS NULL
    RETURNING t.id |]
  for_ lids $ notifyEntityId NotificationType_Update

reportInaccessibleEndpointError
  :: (Monad m, PostgresRaw m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig a, MonadReader a m)
  => EndpointType -> ClientAddress -> m ()
reportInaccessibleEndpointError endpointType addr = do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogInaccessibleEndpoint) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.id
      FROM "ErrorLog" el
      JOIN "ErrorLogInaccessibleEndpoint" t ON t.log = el.id
     WHERE t.type = ?endpointType AND t.address = ?addr AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Nothing -> do
      insertErrorLog $ \logId -> ErrorLogInaccessibleEndpoint logId endpointType addr
      let typeName = case endpointType of
            EndpointType_Node -> "node"
            EndpointType_Client -> "client"
      now <- getTime
      queueAllEmails [Error
        { _error_time = now
        , _error_text = "Unable to connect to " <> typeName <> " at " <> addr
        }]
    Just (logId, specificLogId) -> updateErrorLog logId specificLogId

clearInaccessibleEndpointError
  :: (Monad m, PostgresRaw m, PersistBackend m) => EndpointType -> ClientAddress -> m ()
clearInaccessibleEndpointError endpointType addr = do
  lids :: [Id ErrorLogInaccessibleEndpoint] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogInaccessibleEndpoint" t
    WHERE t.log = el.id AND t.type = ?endpointType AND t.address = ?addr AND el.stopped IS NULL
    RETURNING t.id |]
  for_ lids $ notifyEntityId NotificationType_Update


reportNodeOnForkError
  :: (Monad m, PostgresRaw m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig a, MonadReader a m)
  => Id Node -> Bool -> BlockHash -> UTCTime -> m ()
reportNodeOnForkError nodeId tooOld bakedBlock bakedBlockTime = do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogNodeOnFork) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.id
      FROM "ErrorLog" el
      JOIN "ErrorLogNodeOnFork" t ON t.log = el.id
     WHERE t.node = ?nodeId AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Nothing -> do
      insertErrorLog $ \logId -> ErrorLogNodeOnFork logId nodeId tooOld bakedBlock bakedBlockTime
      node <- get $ fromId nodeId
      for_ node $ \n ->
        queueAllEmails
          [showBadFork n $ ForkInfo (Left $ if tooOld then ForkStatus_TooOld else ForkStatus_Forked) bakedBlockTime bakedBlock]

    Just (logId, specificLogId) -> do
      updateErrorLogBy logId specificLogId
        [ ErrorLogNodeOnFork_tooOldField =. tooOld
        , ErrorLogNodeOnFork_bakedBlockField =. bakedBlock
        , ErrorLogNodeOnFork_bakedBlockTimeField =. bakedBlockTime
        ]

clearNodeOnForkError :: (Monad m, PostgresRaw m, PersistBackend m) => Id Node -> m ()
clearNodeOnForkError nodeId = do
  lids :: [Id ErrorLogNodeOnFork] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogNodeOnFork" t
    WHERE t.log = el.id AND t.node = ?nodeId AND el.stopped IS NULL
    RETURNING t.id |]
  for_ lids $ notifyEntityId NotificationType_Update

reportUpgradeNotice
  :: (PostgresRaw m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig r, MonadReader r m)
  => Either UpgradeCheckError Version -> m ()
reportUpgradeNotice errorOrNewVersion = do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogUpgradeNotice) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.id
      FROM "ErrorLog" el
      JOIN "ErrorLogUpgradeNotice" t ON t.log = el.id
     WHERE el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Nothing -> do
      insertErrorLog $ \logId -> ErrorLogUpgradeNotice logId (leftToMaybe errorOrNewVersion) (rightToMaybe errorOrNewVersion)
      now <- getTime
      queueAllEmails [Error
        { _error_time = now
        , _error_text = case errorOrNewVersion of
            Left _ -> "We were not able to contact the upgrade check endpoint. If this issue persists please check for an upgrade manually."
            Right _ -> "We found a newer version of the monitor. Please consider upgrading."
        }]

    Just (logId, specificLogId) -> do
      updateErrorLogBy logId specificLogId
        [ ErrorLogUpgradeNotice_errorField =. leftToMaybe errorOrNewVersion
        , ErrorLogUpgradeNotice_newVersionField =. rightToMaybe errorOrNewVersion
        ]

clearUpgradeNotice :: (Monad m, PostgresRaw m, PersistBackend m) => m ()
clearUpgradeNotice = do
  lids :: [Id ErrorLogUpgradeNotice] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogUpgradeNotice" t
    WHERE t.log = el.id AND el.stopped IS NULL
    RETURNING t.id |]
  for_ lids $ notifyEntityId NotificationType_Update

insertErrorLog mkErrorLog = do
  now <- getTime
  logId <- toId <$> insert ErrorLog
    { _errorLog_started = now
    , _errorLog_stopped = Nothing
    , _errorLog_lastSeen = now
    , _errorLog_noticeSentAt = Just now
    }
  insertAndNotify_ $ mkErrorLog logId

updateErrorLog logId specificLogId = do
  updateErrorLogLastSeen logId
  notifyEntityId NotificationType_Update specificLogId

updateErrorLogBy logId specificLogId updates = do
  updateErrorLogLastSeen logId
  updateAndNotify specificLogId updates

updateErrorLogLastSeen :: PersistBackend m => Id ErrorLog -> m ()
updateErrorLogLastSeen logId = do
  now <- getTime
  update [ErrorLog_lastSeenField =. now] (AutoKeyField ==. fromId logId)
