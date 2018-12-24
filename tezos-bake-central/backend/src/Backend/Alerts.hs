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
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -fno-warn-partial-type-signatures -Werror #-}

module Backend.Alerts where

import Control.Lens ((<&>))
import Control.Monad.Logger (MonadLogger, logDebugSH)
import Data.Map (Map())
import qualified Data.Map as Map
import Data.List.NonEmpty (nonEmpty)
import Data.Time (NominalDiffTime, addUTCTime)
import Database.Groundhog
import Database.Groundhog.Core
import qualified Database.Groundhog.Expression as GH
import Database.Groundhog.Postgresql (PersistBackend)
import Rhyolite.Backend.DB (getTime)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (Only (..), queryQ, PostgresRaw)
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Schema (Id, Json (..))
import qualified Text.URI as Uri

import Tezos.Types

import Backend.Alerts.Common (Alert (..), queueAlert, AlertType(..))
import Backend.Config (HasAppConfig)
import Backend.Schema
import Common.Alerts (badNodeHeadMessage)
import Common.Schema
import ExtraPrelude

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


missedBakeLog :: forall m. (PersistBackend m, PostgresRaw m) => RightKind -> PublicKeyHash -> RawLevel -> m (Map (Id Baker) [(Id ErrorLog, Id ErrorLogBakerMissed, Fitness)])
missedBakeLog right pkh lvl =
  ([queryQ|
    SELECT b."publicKeyHash", el.id, elbm.id, elbm.fitness
    FROM "Baker" b
    LEFT OUTER JOIN "ErrorLogBakerMissed" elbm
      ON b."publicKeyHash" = elbm."baker#baker#publicKeyHash"
      AND elbm.right = ?right
      AND elbm.level = ?lvl
    JOIN "ErrorLog" el
      ON el.id = elbm.log
      AND el.stopped IS NULL
    WHERE NOT b.deleted
      AND b."publicKeyHash" = ?pkh
  |] :: m [(Id Baker, Maybe (Id ErrorLog), Maybe (Id ErrorLogBakerMissed), Maybe Fitness)]) <&> Map.fromList . fmap (\(bid, elid, elbmid, f) -> (bid, toList $ (,,) <$> elid <*> elbmid <*> f))

bakerNotDeleted :: PersistBackend m => PublicKeyHash -> m Bool
bakerNotDeleted pkh = all not <$> project Baker_deletedField ((Baker_publicKeyHashField ==. pkh) `limitTo` 1)

reportMissedBake :: (MonadReader r m, HasAppConfig r, PostgresLargeObject m, MonadIO m, PersistBackend m, MonadLogger m) => Fitness -> RightKind -> PublicKeyHash -> RawLevel -> m ()
reportMissedBake f right pkh lvl = when' (bakerNotDeleted pkh) $ (missedBakeLog right pkh lvl >>=) $ itraverse_  $ \bid eids -> case nonEmpty eids of
  Nothing -> do
    (eid, _elbm) <- insertErrorLog $ \eid -> ErrorLogBakerMissed
      { _errorLogBakerMissed_log = eid
      , _errorLogBakerMissed_baker = ErrorLogBaker
        { _errorLogBaker_log = eid
        , _errorLogBaker_baker = bid
        }
      , _errorLogBakerMissed_right = right
      , _errorLogBakerMissed_level = lvl
      , _errorLogBakerMissed_fitness = f
      }
    queueAlert (Just eid) alert
  Just xs -> for_ xs $ \(eid, elbmid, f') -> when (f' <= f) $ do
    updateErrorLogBy eid elbmid [ ErrorLogBakerMissed_fitnessField =. f ]
    queueAlert (Just eid) alert
  where
    alert = Alert Unresolved
      ("Missed " <> rightTxt <> " opportunity")
      ("Baker with address:" <> toPublicKeyHashText pkh <> " Missed " <> rightTxt <> " opportunity at level " <> tshow (unRawLevel lvl))
    rightTxt = case right of
      RightKind_Baking -> "bake"
      RightKind_Endorsing -> "endorsement"


clearMissedBake :: (MonadLogger m, MonadReader r m, HasAppConfig r, MonadIO m, PostgresLargeObject m, PersistBackend m) => Fitness -> RightKind -> PublicKeyHash -> RawLevel -> m ()
clearMissedBake f right pkh lvl = do
  lids :: [Id ErrorLogBakerMissed] <- stripOnly <$> [queryQ|
      UPDATE "ErrorLog" el SET stopped = NOW()
        FROM "ErrorLogBakerMissed" elbm
        JOIN "Baker" b
          ON b."publicKeyHash" = elbm."baker#baker#publicKeyHash"
      WHERE elbm.log = el.id
        AND NOT b.deleted
        AND el.stopped IS NULL
        AND elbm.fitness < ?f :: VARCHAR[] -- because groundhog
        AND elbm.right = ?right
        AND b."publicKeyHash" = ?pkh
        AND elbm.level = ?lvl
      RETURNING elbm.id |]
  for_ lids $ notify . mkDefaultNotify
  when (not $ null lids) $ queueAlert Nothing $
    Alert Resolved
      ("Resolved: Missed " <> rightTxt <> " opportunity")
      ("Resolved: Baker with address:" <> toPublicKeyHashText pkh <> " " <> rightTxt <> " opportunity at level " <> tshow (unRawLevel lvl) <> " included due to branch reorganization")
    where
      rightTxt = case right of
        RightKind_Baking -> "bake"
        RightKind_Endorsing -> "endorsement"

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
