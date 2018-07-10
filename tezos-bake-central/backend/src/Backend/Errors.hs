{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Backend.Errors where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Maybe (listToMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime)
import Database.Groundhog
import Database.Groundhog.Core (PersistEntity)
import Database.Groundhog.Postgresql (PersistBackend)
import Network.Mail.Mime (Address (..), Mail, simpleMail')
import Rhyolite.Backend.DB (getTime)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (Only (..), PostgresRaw, executeQ, queryQ)
import Rhyolite.Backend.EmailWorker (queueEmail)
import Rhyolite.Backend.Listen (insertAndNotify, insertAndNotify_, updateAndNotify)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Schema (Id)

import Backend.Config (AppConfig (..), HasAppConfig, getAppConfig)
import Backend.Schema
import Common.Schema
import Common.TaggedHash (BlockHash)
import Common.Verification (ForkInfoF (..), ForkStatusF (..), showBadFork)

mailFor :: Address -> Text -> [Error] -> Mail
mailFor fromAddr toAddr errs =
  let
    toA = Address Nothing toAddr
    body = TL.fromStrict . T.unlines $ [T.pack (show t) <> ": " <> e | Error t e <- errs]
  in simpleMail' toA fromAddr "Error from Tezos bake monitor" body

queueAllEmails :: (PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig m) => [Error] -> m ()
queueAllEmails message = do
  ns <- select CondEmpty
  fromAddr <- _appConfig_emailFromAddress <$> getAppConfig
  for_ ns $ \n ->
    queueEmail (mailFor fromAddr (_notificatee_email n) message) Nothing


reportNoBakerHeartbeatError
  :: (Monad m, PersistBackend m, PostgresRaw m, PostgresLargeObject m, MonadIO m, HasAppConfig m)
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
      updateErrorLogWith logId specificLogId
        [ ErrorLogBakerNoHeartbeat_lastLevelField =. seenLevel
        , ErrorLogBakerNoHeartbeat_lastBlockHashField =. seenHash
        ]


clearNoBakerHeartbeatError :: (Monad m, PostgresRaw m) => Id Client -> m ()
clearNoBakerHeartbeatError cid = void $ [executeQ|
  UPDATE "ErrorLog" el SET el.stopped = NOW()
    FROM "ErrorLogBakerNoHeartbeat" t
   WHERE t.log = el.id AND t.client = ?cid AND el.stopped IS NULL
  |]

reportInaccessibleEndpointError
  :: (Monad m, PostgresRaw m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig m)
  => EndpointType -> ClientAddress -> m ()
reportInaccessibleEndpointError endpointType addr = do
  existingLog :: Maybe (Id ErrorLog) <- listToMaybe . stripOnly <$> [queryQ|
    SELECT el.id
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
    Just logId -> updateErrorLog logId

clearInaccessibleEndpointError :: (Monad m, PostgresRaw m) => EndpointType -> ClientAddress -> m ()
clearInaccessibleEndpointError endpointType addr = void $ [executeQ|
  UPDATE "ErrorLog" el SET stopped = NOW()
    FROM "ErrorLogInaccessibleEndpoint" t
   WHERE t.log = el.id AND t.type = ?endpointType AND t.address = ?addr AND el.stopped IS NULL
  |]


reportNodeOnForkError
  :: (Monad m, PostgresRaw m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig m)
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
          [showBadFork $ ForkInfo n (if tooOld then ForkStatus_TooOld else ForkStatus_Forked) bakedBlockTime bakedBlock]

    Just (logId, specificLogId) -> do
      updateErrorLogWith logId specificLogId
        [ ErrorLogNodeOnFork_tooOldField =. tooOld
        , ErrorLogNodeOnFork_bakedBlockField =. bakedBlock
        , ErrorLogNodeOnFork_bakedBlockTimeField =. bakedBlockTime
        ]

clearNodeOnForkError :: (Monad m, PostgresRaw m) => Id Node -> m ()
clearNodeOnForkError nodeId = void $ [executeQ|
  UPDATE "ErrorLog" el SET stopped = NOW()
    FROM "ErrorLogNodeOnFork" t
   WHERE t.log = el.id AND t.node = ?nodeId AND el.stopped IS NULL
  |]



insertErrorLog :: (PersistBackend m, PersistEntity a) => (Id ErrorLog -> a) -> m ()
insertErrorLog mkErrorLog = do
  now <- getTime
  logId <- insertAndNotify ErrorLog
    { _errorLog_started = now
    , _errorLog_stopped = Nothing
    , _errorLog_lastSeen = now
    , _errorLog_noticeSentAt = Nothing
    }
  insert_ $ mkErrorLog logId

updateErrorLog :: (PersistBackend m) => Id ErrorLog -> m ()
updateErrorLog logId = do
  now <- getTime
  updateAndNotify logId [ErrorLog_lastSeenField =. now]

-- TODO: The type for this is not easy to write down.
updateErrorLogWith logId specificLogId updates = do
  update updates (AutoKeyField ==. fromId specificLogId)
  updateErrorLog logId
