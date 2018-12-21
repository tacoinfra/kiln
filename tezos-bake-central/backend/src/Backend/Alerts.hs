{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-partial-type-signatures -Werror #-}

module Backend.Alerts where

import Control.Monad.Logger (MonadLogger, logDebugSH)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, addUTCTime)
import Database.Groundhog
import Database.Groundhog.Core
import qualified Database.Groundhog.Expression as GH
import Database.Groundhog.Postgresql (PersistBackend)
import Rhyolite.Backend.DB (getTime)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (Only (..), queryQ)
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Schema (Id, Json (..))
import qualified Text.URI as Uri

import Tezos.Types

import Backend.Alerts.Common (Alert (..), queueAlert, AlertType(..))
import Backend.Config (HasAppConfig)
import Backend.Schema
import Common.Alerts (BakerErrorDescriptions(..), badNodeHeadMessage
                     , bakerDeactivatedDescriptions, bakerDeactivationRiskDescriptions)
import Common.Schema
import ExtraPrelude
import Prelude hiding (log)

reportNoBakerHeartbeatError
  :: ( Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m
     , MonadReader a m, HasAppConfig a, MonadLogger m
     )
  => Id Client -> SeenEvent -> m ()
reportNoBakerHeartbeatError cid eventDetail = do -- TODO: Only on non-deleted bakers
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
      (logId, _) <- insertErrorLog $ \logId -> ErrorLogBakerNoHeartbeat
        { _errorLogBakerNoHeartbeat_log = logId
        , _errorLogBakerNoHeartbeat_lastLevel = seenLevel
        , _errorLogBakerNoHeartbeat_lastBlockHash = seenHash
        , _errorLogBakerNoHeartbeat_client = cid
        }

      client :: Maybe Client <- get $ fromId cid
      queueAlert (Just logId) $
        Alert Unresolved "Baker has not seen block for a while" $
        "Baker" <> maybe "" (" " <>) (client >>= _client_alias) <> " at " <> maybe "?" (Uri.render . _client_address) client <> " has not seen a block for while!"
    Just (logId, specificLogId) -> do
      updateErrorLogBy logId specificLogId
        [ ErrorLogBakerNoHeartbeat_lastLevelField =. seenLevel
        , ErrorLogBakerNoHeartbeat_lastBlockHashField =. seenHash
        ]


clearNoBakerHeartbeatError :: (Monad m, PersistBackend m,
                               PostgresLargeObject m, MonadIO m, MonadReader a m, MonadLogger m,
                               HasAppConfig a) => Id Client -> m ()
clearNoBakerHeartbeatError cid = do -- TODO: Only on non-deleted bakers
  lids :: [Id ErrorLogBakerNoHeartbeat] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogBakerNoHeartbeat" t
      JOIN "Client" c ON t.client = c.id
    WHERE t.log = el.id
      AND t.client = ?cid
      AND NOT c.deleted
      AND el.stopped IS NULL
    RETURNING t.id |]
  for_ lids $ notify . mkDefaultNotify
  client :: Maybe Client <- get $ fromId cid
  when (not $ null lids) $ queueAlert Nothing $
    Alert Resolved "Resolved: Baker has now seen a block" $
    "Baker" <> maybe "" (" " <>) (client >>= _client_alias) <> " at " <> maybe "?" (Uri.render . _client_address) client <> " has now seen a block again"

unresolvedBakerAlert :: BakerErrorDescriptions -> Alert
unresolvedBakerAlert dsc = Alert Unresolved (_bakerErrorDescriptions_title dsc) $ T.unlines $ catMaybes
  [ Just $ _bakerErrorDescriptions_problem dsc
  , _bakerErrorDescriptions_warning dsc
  ]

resolvedBakerAlert :: BakerErrorDescriptions -> Baker -> Alert
resolvedBakerAlert dsc = uncurry (Alert Resolved) . _bakerErrorDescriptions_resolved dsc

reportBakerDeactivated
  :: ( Monad m, MonadIO m, MonadReader a m, MonadLogger m
     , PersistBackend m, PostgresLargeObject m, HasAppConfig a
     )
  => Id Baker -> ProtoInfo -> Fitness -> m ()
reportBakerDeactivated bakerId protoInfo newFit = do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogBakerDeactivated, Fitness) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.id, t.fitness
      FROM "ErrorLog" el
      JOIN "ErrorLogBakerDeactivated" t ON t.log = el.id
      JOIN "Baker" b ON b.id = t.node
     WHERE t.baker = ?bakerId
       AND NOT b.deleted
       AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Just (logId, specificLogId, oldFit) -> updateErrorLogBy logId specificLogId
      [ ErrorLogBakerDeactivated_fitnessField =. max newFit oldFit ]
    Nothing -> do
      baker' <- get $ fromId bakerId
      for_ baker' $ \baker -> do
        (logId, log) <- insertErrorLog $ \logId ->
          ErrorLogBakerDeactivated logId (_baker_publicKeyHash baker) (_protoInfo_preservedCycles protoInfo) newFit
        queueAlert (Just logId) $ unresolvedBakerAlert $ bakerDeactivatedDescriptions log

clearBakerDeactivated
  :: ( Monad m, MonadIO m, MonadReader a m, MonadLogger m
     , PersistBackend m, PostgresLargeObject m, HasAppConfig a
     )
  => Id Baker -> Fitness -> m ()
clearBakerDeactivated bakerId newFit = do
  lids :: [Id ErrorLogBakerDeactivated] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogBakerDeactivated" t
    WHERE t.log = el.id
      AND t.baker = ?bakerId
      AND el.stopped IS NULL
      AND t.fitness < ?newFit
    RETURNING t.id |]
  for_ lids $ notify . mkDefaultNotify
  $(logDebugSH) ("LIDs we've supposedly blanked out"::String, lids)
  baker' <- get (fromId bakerId)
  log' <- for (listToMaybe lids) $ get . fromId
  for_ (liftA2 (,) baker' (join log')) $ \(baker, log) ->
    queueAlert Nothing $ resolvedBakerAlert (bakerDeactivatedDescriptions log) baker

reportBakerDeactivationRisk
  :: ( Monad m, MonadIO m, MonadReader a m, MonadLogger m
     , PersistBackend m, PostgresLargeObject m, HasAppConfig a
     )
  => Id Baker -> Cycle -> Cycle -> ProtoInfo -> Fitness -> m ()
reportBakerDeactivationRisk bakerId gracePeriod latestCycle protoInfo newFit = do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogBakerDeactivationRisk, Fitness) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.id, t.fitness
      FROM "ErrorLog" el
      JOIN "ErrorLogBakerDeactivationRisk" t ON t.log = el.id
      JOIN "Baker" b ON b.id = t.node
     WHERE t.baker = ?bakerId
       AND NOT b.deleted
       AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Just (logId, specificLogId, oldFit) -> updateErrorLogBy logId specificLogId
      [ ErrorLogBakerDeactivationRisk_fitnessField =. max newFit oldFit ]
    Nothing -> do
      baker' <- get $ fromId bakerId
      for_ baker' $ \baker -> do
        (logId, log) <- insertErrorLog $ \logId ->
          ErrorLogBakerDeactivationRisk logId (_baker_publicKeyHash baker) gracePeriod latestCycle (_protoInfo_preservedCycles protoInfo) newFit
        queueAlert (Just logId) $ unresolvedBakerAlert $ bakerDeactivationRiskDescriptions log

clearBakerDeactivationRisk
  :: ( Monad m, MonadIO m, MonadReader a m, MonadLogger m
     , PersistBackend m, PostgresLargeObject m, HasAppConfig a
     )
  => Id Baker -> Fitness -> m ()
clearBakerDeactivationRisk bakerId newFit = do
  lids :: [Id ErrorLogBakerDeactivationRisk] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogBakerDeactivationRisk" t
    WHERE t.log = el.id
      AND t.baker = ?bakerId
      AND el.stopped IS NULL
      AND t.fitness < ?newFit
    RETURNING t.id |]
  for_ lids $ notify . mkDefaultNotify
  $(logDebugSH) ("LIDs we've supposedly blanked out"::String, lids)
  baker' <- get (fromId bakerId)
  log' <- for (listToMaybe lids) $ get . fromId
  for_ (liftA2 (,) baker' (join log')) $ \(baker, log) ->
    queueAlert Nothing $ resolvedBakerAlert (bakerDeactivationRiskDescriptions log) baker

reportInaccessibleNodeError
  :: (Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig a, MonadReader a m,
      MonadLogger m)
  => Id Node -> m ()
reportInaccessibleNodeError nodeId = when' (nodeNotDeleted nodeId) $ do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogInaccessibleNode) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.id
      FROM "ErrorLog" el
      JOIN "ErrorLogInaccessibleNode" t ON t.log = el.id
      JOIN "Node" n ON n.id = t.node
     WHERE t.node = ?nodeId
       AND NOT n.deleted
       AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Nothing -> do
      node' <- get (fromId nodeId)
      for_ node' $ \node -> do
        (logId, _) <- insertErrorLog $ \logId -> ErrorLogInaccessibleNode logId nodeId (_node_address node) (_node_alias node)
        queueAlert (Just logId) $ Alert Unresolved "Unable to connect to node" $
          "Unable to connect to node, " <> maybe "" (" " <>) (_node_alias node) <> " at " <> Uri.render (_node_address node)
    Just (logId, specificLogId) -> updateErrorLog logId specificLogId

clearInaccessibleNodeError
  :: (Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m, MonadLogger m,
      MonadReader a m, HasAppConfig a) => Id Node -> m ()
clearInaccessibleNodeError nodeId = when' (nodeNotDeleted nodeId) $ do
  lids :: [Id ErrorLogInaccessibleNode] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogInaccessibleNode" t
    WHERE t.log = el.id AND t.node = ?nodeId AND el.stopped IS NULL
    RETURNING t.id |]
  for_ lids $ notify . mkDefaultNotify
  node' <- get (fromId nodeId)
  $(logDebugSH) ("LIDs we've supposedly blanked out"::String, lids)
  when (not $ null lids) $ for_ node' $ \node -> do
    queueAlert Nothing $ Alert Resolved "Resolved: Now able to connect to node" $
        "Able to again connect to node" <> maybe "" (" " <>) (_node_alias node) <> " at " <> Uri.render (_node_address node)

reportNodeWrongChainError
  :: (Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig a, MonadReader a m,
      MonadLogger m)
  => Id Node -> ChainId -> ChainId -> m ()
reportNodeWrongChainError nodeId expectedChainId actualChainId = when' (nodeNotDeleted nodeId) $ do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogNodeWrongChain) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.id
      FROM "ErrorLog" el
      JOIN "ErrorLogNodeWrongChain" t ON t.log = el.id
      JOIN "Node" n ON n.id = t.node
     WHERE t."expectedChainId" = ?expectedChainId
       AND t."actualChainId" = ?actualChainId
       AND t.node = ?nodeId
       AND NOT n.deleted
       AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Nothing -> do
      node' <- get $ fromId nodeId
      for_ node' $ \node -> do
        (logId, _) <- insertErrorLog $ \logId -> ErrorLogNodeWrongChain logId nodeId (_node_address node) (_node_alias node) expectedChainId actualChainId
        queueAlert (Just logId) $ Alert Unresolved "Node on wrong network" $
          "Node" <> maybe "" (" " <>) (_node_alias node) <> " at " <> Uri.render (_node_address node) <> " is on network " <> toBase58Text actualChainId <> " but is expected to be on " <> toBase58Text expectedChainId
    Just (logId, specificLogId) -> updateErrorLog logId specificLogId

clearNodeWrongChainError
  :: (Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m, MonadLogger m,
      MonadReader a m, HasAppConfig a) => Id Node -> m ()
clearNodeWrongChainError nodeId = when' (nodeNotDeleted nodeId) $ do
  lids :: [Id ErrorLogNodeWrongChain] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogNodeWrongChain" t
    WHERE t.log = el.id
      AND t.node = ?nodeId
      AND el.stopped IS NULL
    RETURNING t.id |]
  for_ lids $ notify . Notify_ErrorLogNodeWrongChain
  for_ lids $ notify . mkDefaultNotify
  node' <- get $ fromId nodeId
  when (not $ null lids) $ for_ node' $ \node -> do
    queueAlert Nothing $ Alert Resolved "Resolved: Node on right network" $
       "Node" <> maybe "" (" " <>) (_node_alias node) <> " at " <> Uri.render (_node_address node) <> " is on correct network"

badNodeHeadErrorDelaySeconds :: NominalDiffTime
badNodeHeadErrorDelaySeconds = 125

reportBadNodeHeadError
  :: ( Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig a, MonadReader a m
     , BlockLike latestHead, BlockLike nodeHead, BlockLike lca, MonadLogger m)
  => Id Node -> latestHead -> nodeHead -> Maybe lca -> m ()
reportBadNodeHeadError nodeId latestHead nodeHead lca = when' (nodeNotDeleted nodeId) $ do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogBadNodeHead) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.id
      FROM "ErrorLog" el
      JOIN "ErrorLogBadNodeHead" t ON t.log = el.id
      JOIN "Node" n ON n.id = t.node
     WHERE t.node = ?nodeId
       AND NOT n.deleted
       AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Nothing -> do
      void $ insertErrorLog $ \logId -> ErrorLogBadNodeHead
        { _errorLogBadNodeHead_log = logId
        , _errorLogBadNodeHead_node = nodeId
        , _errorLogBadNodeHead_lca = Json . mkVeryBlockLike <$> lca
        , _errorLogBadNodeHead_nodeHead = Json $ mkVeryBlockLike nodeHead
        , _errorLogBadNodeHead_latestHead = Json $ mkVeryBlockLike latestHead
        }

    Just (logId, specificLogId) -> do
      (g,l) <- returnUpdateErrorLogBy logId specificLogId
        [ ErrorLogBadNodeHead_lcaField =. (Json . mkVeryBlockLike <$> lca)
        , ErrorLogBadNodeHead_nodeHeadField =. Json (mkVeryBlockLike nodeHead)
        , ErrorLogBadNodeHead_latestHeadField =. Json (mkVeryBlockLike latestHead)
        ]
      when (_errorLog_lastSeen g >= addUTCTime badNodeHeadErrorDelaySeconds (_errorLog_started g) && isNothing (_errorLog_noticeSentAt g)) $ do
        node <- getId nodeId
        for_ node $ \n -> do
          let (heading, Const message) = badNodeHeadMessage Const (Const . toBase58Text) l
          queueAlert (Just logId) $ Alert Unresolved heading $
            heading <> ": " <> maybe "" (\x -> "Node " <> x <> " at ") (_node_alias n) <> Uri.render (_node_address n) <> "\n\n" <> message

clearBadNodeHeadError :: (Monad m, PersistBackend m, PostgresLargeObject m, MonadLogger m,
                          MonadIO m, MonadReader a m, HasAppConfig a) => Id Node -> m ()
clearBadNodeHeadError nodeId = when' (nodeNotDeleted nodeId) $ do
  lids :: [Id ErrorLogBadNodeHead] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogBadNodeHead" t
    WHERE t.log = el.id AND t.node = ?nodeId AND el.stopped IS NULL
    RETURNING t.id |]
  for_ lids $ notify . mkDefaultNotify
  node <- get $ fromId nodeId
  specErrs <- catMaybes <$> for lids getId
  errs <- catMaybes <$> traverse getId (_errorLogBadNodeHead_log <$> specErrs)
  when (any (\e -> isJust $ _errorLog_noticeSentAt e) errs) $ for_ node $ \n -> do
    queueAlert Nothing $ Alert Resolved "Resolved: Node is in sync" $
        "Resolved: " <> maybe "" (\x -> "Node " <> x <> " at ") (_node_alias n) <> Uri.render (_node_address n) <> " is now in sync."

nodeNotDeleted :: (PersistBackend m) => Id Node -> m Bool
nodeNotDeleted nodeId = all not <$> project Node_deletedField ((AutoKeyField ==. fromId nodeId) `limitTo` 1)

insertErrorLog :: (EntityWithId a, HasDefaultNotify (Id a), AutoKey a ~ DefaultKey a, PersistBackend m) => (Id ErrorLog -> a) -> m (Id ErrorLog, a)
insertErrorLog mkErrorLog = do
  now <- getTime
  logId <- insert' ErrorLog
    { _errorLog_started = now
    , _errorLog_stopped = Nothing
    , _errorLog_lastSeen = now
    , _errorLog_noticeSentAt = Nothing
    }
  let errLog = mkErrorLog logId
  notify . mkDefaultNotify =<< insert' errLog
  pure (logId, errLog)

updateErrorLog :: (HasDefaultNotify (Id a), PersistBackend m) => Id ErrorLog -> Id a -> m ()
updateErrorLog logId specificLogId = do
  updateErrorLogLastSeen logId
  notify $ mkDefaultNotify specificLogId

updateErrorLogBy
  :: (EntityWithId a, HasDefaultNotify (Id a), GH.Expression (PhantomDb m) (RestrictionHolder v c) (DefaultKey a), PersistEntity v, PersistBackend m, GH.Unifiable (AutoKeyField v c) (DefaultKey a), _)
  => Id ErrorLog
  -> Id a
  -> [Update (PhantomDb m) (RestrictionHolder v c)]
  -> m ()
updateErrorLogBy logId specificLogId updates = do
  updateErrorLogLastSeen logId
  updateId specificLogId updates
  notify $ mkDefaultNotify specificLogId

returnUpdateErrorLogBy
  :: (EntityWithId a, HasDefaultNotify (Id a), GH.Expression (PhantomDb m) (RestrictionHolder v c) (DefaultKey a), PersistEntity v, PersistBackend m, GH.Unifiable (AutoKeyField v c) (DefaultKey a), _)
  => Id ErrorLog
  -> Id a
  -> [Update (PhantomDb m) (RestrictionHolder v c)]
  -> m (ErrorLog, a)
returnUpdateErrorLogBy logId specificLogId updates = do
  g <- returnUpdateErrorLogLastSeen logId
  updateId specificLogId updates
  notify $ mkDefaultNotify specificLogId
  getId specificLogId >>= \case
    Nothing -> fail $ "returnUpdateErrorLogBy called on nonexistent specific record " <> show specificLogId
    Just l -> return (g,l)

updateErrorLogLastSeen :: PersistBackend m => Id ErrorLog -> m ()
updateErrorLogLastSeen logId = do
  now <- getTime
  updateId logId [ErrorLog_lastSeenField =. now]

returnUpdateErrorLogLastSeen :: PersistBackend m => Id ErrorLog -> m ErrorLog
returnUpdateErrorLogLastSeen logId = do
  updateErrorLogLastSeen logId
  getId logId >>= \case
    Nothing -> fail $ "returnUpdateErrorLogLastSeen called on nonexistent record " <> show logId
    Just l -> return l
