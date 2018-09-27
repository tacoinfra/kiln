{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Backend.Alerts where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Either.Combinators (leftToMaybe, rightToMaybe)
import Data.Foldable (for_)
import Data.Functor.Const (Const (..))
import Data.Maybe (listToMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Version (Version)
import Database.Groundhog
import Database.Groundhog.Core
import qualified Database.Groundhog.Expression as GH
import Database.Groundhog.Postgresql (PersistBackend)
import Network.Mail.Mime (Address (..), Mail, simpleMail')
import Rhyolite.Backend.DB (getTime)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (Only (..), PostgresRaw, queryQ)
import Rhyolite.Backend.EmailWorker (queueEmail)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Schema (Id, Json (..))
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.Types

import Backend.Config (AppConfig (..), HasAppConfig, askAppConfig)
import Backend.Schema
import Common.Alerts (badNodeHeadMessage)
import Common.Schema

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
  :: ( Monad m, PersistBackend m, PostgresRaw m, PostgresLargeObject m, MonadIO m
     , MonadReader a m, HasAppConfig a
     )
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
      _ <- insertErrorLog $ \logId -> ErrorLogBakerNoHeartbeat
        { _errorLogBakerNoHeartbeat_log = logId
        , _errorLogBakerNoHeartbeat_lastLevel = seenLevel
        , _errorLogBakerNoHeartbeat_lastBlockHash = seenHash
        , _errorLogBakerNoHeartbeat_client = cid
        }

      client :: Maybe Client <- get $ fromId cid
      now <- getTime
      queueAllEmails [Error
        { _error_time = now
        , _error_text = "Baker" <> maybe "" (" " <>) (client >>= _client_alias) <> " at " <> maybe "?" (Uri.render . _client_address) client <> " has not seen a block for while!"
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
  for_ lids $ notify . mkDefaultNotify

reportInaccessibleNodeError
  :: (Monad m, PostgresRaw m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig a, MonadReader a m)
  => Id Node -> m ()
reportInaccessibleNodeError nodeId = do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogInaccessibleNode) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.id
      FROM "ErrorLog" el
      JOIN "ErrorLogInaccessibleNode" t ON t.log = el.id
     WHERE t.node = ?nodeId AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Nothing -> do
      node' <- get (fromId nodeId)
      for_ node' $ \node -> do
        _ <- insertErrorLog $ \logId -> ErrorLogInaccessibleNode logId nodeId (_node_address node) (_node_alias node)
        now <- getTime
        queueAllEmails [Error
          { _error_time = now
          , _error_text = "Unable to connect to node" <> maybe "" (" " <>) (_node_alias node) <> " at " <> Uri.render (_node_address node)
          }]
    Just (logId, specificLogId) -> updateErrorLog logId specificLogId

clearInaccessibleNodeError
  :: (Monad m, PostgresRaw m, PersistBackend m) => Id Node -> m ()
clearInaccessibleNodeError nodeId = do
  lids :: [Id ErrorLogInaccessibleNode] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogInaccessibleNode" t
    WHERE t.log = el.id AND t.node = ?nodeId AND el.stopped IS NULL
    RETURNING t.id |]
  for_ lids $ notify . mkDefaultNotify

reportNodeWrongChainError
  :: (Monad m, PostgresRaw m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig a, MonadReader a m)
  => Id Node -> ChainId -> ChainId -> m ()
reportNodeWrongChainError nodeId expectedChainId actualChainId = do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogNodeWrongChain) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.id
      FROM "ErrorLog" el
      JOIN "ErrorLogNodeWrongChain" t ON t.log = el.id
     WHERE t."expectedChainId" = ?expectedChainId
       AND t."actualChainId" = ?actualChainId
       AND t.node = ?nodeId
       AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Nothing -> do
      node' <- get $ fromId nodeId
      for_ node' $ \node -> do
        _ <- insertErrorLog $ \logId -> ErrorLogNodeWrongChain logId nodeId (_node_address node) (_node_alias node) expectedChainId actualChainId
        now <- getTime
        queueAllEmails [Error
          { _error_time = now
          , _error_text = "Node" <> maybe "" (" " <>) (_node_alias node) <> " at " <> Uri.render (_node_address node) <> " is on network " <> toBase58Text actualChainId <> " but is expected to be on " <> toBase58Text expectedChainId
          }]
    Just (logId, specificLogId) -> updateErrorLog logId specificLogId

clearNodeWrongChainError
  :: (Monad m, PostgresRaw m, PersistBackend m) => Id Node -> m ()
clearNodeWrongChainError nodeId = do
  lids :: [Id ErrorLogNodeWrongChain] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogNodeWrongChain" t
    WHERE t.log = el.id
      AND t.node = ?nodeId
      AND el.stopped IS NULL
    RETURNING t.id |]
  for_ lids $ notify . Notify_ErrorLogNodeWrongChain

reportBadNodeHeadError
  :: ( Monad m, PostgresRaw m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig a, MonadReader a m
     , BlockLike latestHead, BlockLike nodeHead, BlockLike lca)
  => Id Node -> latestHead -> nodeHead -> Maybe lca -> m ()
reportBadNodeHeadError nodeId latestHead nodeHead lca = do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogBadNodeHead) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.id
      FROM "ErrorLog" el
      JOIN "ErrorLogBadNodeHead" t ON t.log = el.id
     WHERE t.node = ?nodeId AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Nothing -> do
      l <- insertErrorLog $ \logId -> ErrorLogBadNodeHead
        { _errorLogBadNodeHead_log = logId
        , _errorLogBadNodeHead_node = nodeId
        , _errorLogBadNodeHead_lca = Json . mkVeryBlockLike <$> lca
        , _errorLogBadNodeHead_nodeHead = Json $ mkVeryBlockLike nodeHead
        , _errorLogBadNodeHead_latestHead = Json $ mkVeryBlockLike latestHead
        }
      node <- get $ fromId nodeId
      for_ node $ \n -> do
        let (mkSubject, Const message) = badNodeHeadMessage Const (Const . toBase58Text) l
        now <- getTime
        queueAllEmails [Error now $
          mkSubject (maybe "" (\x -> "Node " <> x <> " at ") $ _node_alias n) <> (Uri.render $ _node_address n) <> "\n\n" <> message]
      pure ()

    Just (logId, specificLogId) -> do
      updateErrorLogBy logId specificLogId
        [ ErrorLogBadNodeHead_lcaField =. (Json . mkVeryBlockLike <$> lca)
        , ErrorLogBadNodeHead_nodeHeadField =. Json (mkVeryBlockLike nodeHead)
        , ErrorLogBadNodeHead_latestHeadField =. Json (mkVeryBlockLike latestHead)
        ]

clearBadNodeHeadError :: (Monad m, PostgresRaw m, PersistBackend m) => Id Node -> m ()
clearBadNodeHeadError nodeId = do
  lids :: [Id ErrorLogBadNodeHead] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogBadNodeHead" t
    WHERE t.log = el.id AND t.node = ?nodeId AND el.stopped IS NULL
    RETURNING t.id |]
  for_ lids $ notify . mkDefaultNotify

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
      _ <- insertErrorLog $ \logId -> ErrorLogUpgradeNotice logId (leftToMaybe errorOrNewVersion) (rightToMaybe errorOrNewVersion)
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
  for_ lids $ notify . mkDefaultNotify

insertErrorLog :: (EntityWithId a, HasDefaultNotify (Id a), AutoKey a ~ DefaultKey a, PersistBackend m) => (Id ErrorLog -> a) -> m a
insertErrorLog mkErrorLog = do
  now <- getTime
  logId <- insert' ErrorLog
    { _errorLog_started = now
    , _errorLog_stopped = Nothing
    , _errorLog_lastSeen = now
    , _errorLog_noticeSentAt = Just now
    }
  let errLog = mkErrorLog logId
  notify . mkDefaultNotify =<< insert' errLog
  pure errLog

updateErrorLog :: (EntityWithId a, HasDefaultNotify (Id a), PersistBackend m) => Id ErrorLog -> Id a -> m ()
updateErrorLog logId specificLogId = do
  updateErrorLogLastSeen logId
  notify $ mkDefaultNotify specificLogId

updateErrorLogBy
  :: (EntityWithId a, GH.Expression (PhantomDb m) (RestrictionHolder v c) (DefaultKey a), PersistEntity v, PersistBackend m, GH.Unifiable (AutoKeyField v c) (DefaultKey a), _)
  => Id ErrorLog
  -> Id a
  -> [Update (PhantomDb m) (RestrictionHolder v c)]
  -> m ()
updateErrorLogBy logId specificLogId updates = do
  updateErrorLogLastSeen logId
  updateId specificLogId updates
  notify $ mkDefaultNotify specificLogId

updateErrorLogLastSeen :: PersistBackend m => Id ErrorLog -> m ()
updateErrorLogLastSeen logId = do
  now <- getTime
  updateId logId [ErrorLog_lastSeenField =. now]
