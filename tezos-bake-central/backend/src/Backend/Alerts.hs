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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Alerts where

import Control.Lens ((<&>))
import Control.Monad.Logger (MonadLogger)
import Data.Map (Map())
import qualified Data.Map as Map
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, addUTCTime)
import Data.Word
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (PersistBackend, SqlDb, in_)
import Database.PostgreSQL.Simple.Types (Identifier(..))
import Rhyolite.Backend.DB (getTime, selectSingle, project1)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (Only (..), queryQ, PostgresRaw)
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Schema (Id(..), Json (..), IdData)
import qualified Text.URI as Uri

import Tezos.Types

import Backend.Alerts.Common (Alert (..), queueAlert, AlertType(..))
import Backend.Config (HasAppConfig)
import Backend.Schema
import Common.Alerts (badNodeHeadMessage , bakerDeactivatedDescriptions, bakerDeactivationRiskDescriptions)
import Common.Schema
import ExtraPrelude
import Prelude hiding (log)


reportNoBakerHeartbeatError
  :: ( Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m
     , MonadReader a m, HasAppConfig a, MonadLogger m
     , SqlDb (PhantomDb m)
     )
  => Id BakerDaemon -> SeenEvent -> m ()
reportNoBakerHeartbeatError cid eventDetail = do -- TODO: Only on non-deleted bakers
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogBakerNoHeartbeat) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.log
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

      -- Use a simple "get" primitive with Beam
      client :: Maybe BakerDaemonExternalData <- do
        cs <- project (BakerDaemonExternal_dataField ~> DeletableRow_dataSelector) $ (BakerDaemonExternal_idField `in_` [cid]) `limitTo` 1
        pure $ case cs of
          [] -> Nothing
          [c] -> Just c
          (_:_:_) -> error "BakerDaemonExternal primary key constraint invalidated"
      queueAlert (Just logId) $
        Alert Unresolved "Baker has not seen block for a while" $
        "Baker" <> maybe "" (" " <>) (client >>= _bakerDaemonExternalData_alias) <> " at " <> maybe "?" (Uri.render . _bakerDaemonExternalData_address) client <> " has not seen a block for while!"
    Just (logId, _specificLogId) -> do
      updateErrorLogBy logId ErrorLogBakerNoHeartbeat_logField
        [ ErrorLogBakerNoHeartbeat_lastLevelField =. seenLevel
        , ErrorLogBakerNoHeartbeat_lastBlockHashField =. seenHash
        ]


clearNoBakerHeartbeatError
  :: ( Monad m, PersistBackend m
     , PostgresLargeObject m, MonadIO m, MonadReader a m, MonadLogger m
     , SqlDb (PhantomDb m)
     , HasAppConfig a)
  => Id BakerDaemon
  -> m ()
clearNoBakerHeartbeatError cid = do -- TODO: Only on non-deleted bakers
  lids :: [Id ErrorLogBakerNoHeartbeat] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogBakerNoHeartbeat" t
      JOIN "BakerDaemon" c ON t.client = c.id
    WHERE t.log = el.id
      AND t.client = ?cid
      AND NOT c.deleted
      AND el.stopped IS NULL
    RETURNING t.log |]
  for_ lids $ notify . mkDefaultNotify
  -- Use a simple "get" primitive with Beam
  client :: Maybe BakerDaemonExternalData <- do
    cs <- project (BakerDaemonExternal_dataField ~> DeletableRow_dataSelector) $ (BakerDaemonExternal_idField `in_` [cid]) `limitTo` 1
    pure $ case cs of
      [] -> Nothing
      [c] -> Just c
      (_:_:_) -> error "BakerDaemonExternal primary key constraint invalidated"
  when (not $ null lids) $ queueAlert Nothing $
    Alert Resolved "Resolved: Baker has now seen a block" $
    "Baker" <> maybe "" (" " <>) (client >>= _bakerDaemonExternalData_alias) <> " at " <> maybe "?" (Uri.render . _bakerDaemonExternalData_address) client <> " has now seen a block again"

clearUnrelatedNetworkUpdateError :: (PersistBackend m, PostgresRaw m) => NamedChain -> m ()
clearUnrelatedNetworkUpdateError namedChain = do
  lids :: [Id ErrorLogNetworkUpdate] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
    FROM "ErrorLogNetworkUpdate" elnu
    WHERE elnu.log = el.id
    AND elnu."namedChain" <> ?namedChain
    AND el.stopped IS NULL
    RETURNING elnu.log
    |]
  for_ lids $ notify . mkDefaultNotify

unresolvedBakerAlert :: BakerErrorDescriptions -> Alert
unresolvedBakerAlert dsc = Alert Unresolved (_bakerErrorDescriptions_title dsc) $ T.unlines $ catMaybes
  [ Just $ _bakerErrorDescriptions_problem dsc
  , _bakerErrorDescriptions_warning dsc
  ]

resolvedBakerAlert :: BakerErrorDescriptions -> Baker -> Alert
resolvedBakerAlert dsc = uncurry (Alert Resolved) . _bakerErrorDescriptions_resolved dsc

getBaker :: (PersistBackend m, SqlDb (PhantomDb m)) => PublicKeyHash -> m (Maybe Baker)
getBaker pkh = selectSingle $ Baker_publicKeyHashField `in_` [pkh]

reportBakerDeactivated
  :: ( Monad m, MonadIO m, MonadReader a m, MonadLogger m, SqlDb (PhantomDb m)
     , PersistBackend m, PostgresLargeObject m, HasAppConfig a
     )
  => PublicKeyHash -> ProtoInfo -> Fitness -> m ()
reportBakerDeactivated pkh protoInfo newFit = do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogBakerDeactivated, Fitness) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.log, t.fitness
      FROM "ErrorLog" el
      JOIN "ErrorLogBakerDeactivated" t ON t.log = el.id
      JOIN "Baker" b ON b."publicKeyHash" = t."publicKeyHash"
       AND NOT b."data#deleted"
       AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Just (logId, _specificLogId, oldFit) -> updateErrorLogBy logId ErrorLogBakerDeactivated_logField
      [ ErrorLogBakerDeactivated_fitnessField =. max newFit oldFit ]
    Nothing -> do
      baker' <- getBaker pkh
      for_ baker' $ \baker -> do
        (logId, log) <- insertErrorLog $ \logId ->
          ErrorLogBakerDeactivated logId (_baker_publicKeyHash baker) (_protoInfo_preservedCycles protoInfo) newFit
        queueAlert (Just logId) $ unresolvedBakerAlert $ bakerDeactivatedDescriptions log

clearBakerDeactivated
  :: ( Monad m, MonadIO m, MonadReader a m, MonadLogger m, SqlDb (PhantomDb m)
     , PersistBackend m, PostgresLargeObject m, HasAppConfig a
     )
  => PublicKeyHash -> Fitness -> m ()
clearBakerDeactivated pkh newFit = do
  lids :: [Id ErrorLogBakerDeactivated] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogBakerDeactivated" t
    WHERE t.log = el.id
      AND t."publicKeyHash" = ?pkh
      AND el.stopped IS NULL
      AND t.fitness < ?newFit :: VARCHAR[]
    RETURNING t.log |]
  for_ lids $ notify . mkDefaultNotify
  --  $(logDebugSH) ("LIDs we've supposedly blanked out"::String, lids)
  baker' <- getBaker pkh
  log' <- for (listToMaybe lids) $ getBy . fromId
  for_ (liftA2 (,) baker' (join log')) $ \(baker, log) ->
    queueAlert Nothing $ resolvedBakerAlert (bakerDeactivatedDescriptions log) baker

reportBakerDeactivationRisk
  :: ( Monad m, MonadIO m, MonadReader a m, MonadLogger m, SqlDb (PhantomDb m)
     , PersistBackend m, PostgresLargeObject m, HasAppConfig a
     )
  => PublicKeyHash -> Cycle -> Cycle -> ProtoInfo -> Fitness -> m ()
reportBakerDeactivationRisk pkh gracePeriod latestCycle protoInfo newFit = do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogBakerDeactivationRisk, Fitness) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.log, t.fitness
      FROM "ErrorLog" el
      JOIN "ErrorLogBakerDeactivationRisk" t ON t.log = el.id
      JOIN "Baker" b ON b.publicKeyHash = t."publicKeyHash"
     WHERE NOT b."data#deleted"
       AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Just (logId, _specificLogId, oldFit) -> updateErrorLogBy logId ErrorLogBakerDeactivationRisk_logField
      [ ErrorLogBakerDeactivationRisk_fitnessField =. max newFit oldFit ]
    Nothing -> do
      baker' <- getBaker pkh
      for_ baker' $ \baker -> do
        (logId, log) <- insertErrorLog $ \logId ->
          ErrorLogBakerDeactivationRisk logId (_baker_publicKeyHash baker) gracePeriod latestCycle (_protoInfo_preservedCycles protoInfo) newFit
        queueAlert (Just logId) $ unresolvedBakerAlert $ bakerDeactivationRiskDescriptions log

clearBakerDeactivationRisk
  :: ( Monad m, MonadIO m, MonadReader a m, MonadLogger m, SqlDb (PhantomDb m)
     , PersistBackend m, PostgresLargeObject m, HasAppConfig a
     )
  => PublicKeyHash -> Fitness -> m ()
clearBakerDeactivationRisk pkh newFit = do
  lids :: [Id ErrorLogBakerDeactivationRisk] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogBakerDeactivationRisk" t
    WHERE t.log = el.id
      AND t."publicKeyHash" = ?pkh
      AND el.stopped IS NULL
      AND t.fitness < ?newFit :: VARCHAR[]
    RETURNING t.log |]
  for_ lids $ notify . mkDefaultNotify
  --  $(logDebugSH) ("LIDs we've supposedly blanked out"::String, lids)
  baker' <- getBaker pkh
  log' <- for (listToMaybe lids) $ getBy . fromId
  for_ (liftA2 (,) baker' (join log')) $ \(baker, log) ->
    queueAlert Nothing $ resolvedBakerAlert (bakerDeactivationRiskDescriptions log) baker

reportInaccessibleNodeError
  :: ( Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m
     , HasAppConfig a, MonadReader a m, SqlDb (PhantomDb m)
     , MonadLogger m)
  => Id Node -> m ()
reportInaccessibleNodeError nodeId = when' (nodeNotDeleted nodeId) $ do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogInaccessibleNode) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.log
      FROM "ErrorLog" el
      JOIN "ErrorLogInaccessibleNode" t ON t.log = el.id
      JOIN "NodeExternal" n ON n.id = t.node
     WHERE t.node = ?nodeId
       AND NOT n."data#deleted"
       AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Nothing -> do
      node' <- project (NodeExternal_dataField ~> DeletableRow_dataSelector) $ (NodeExternal_idField `in_` [nodeId]) `limitTo` 1
      for_ node' $ \node -> do
        (logId, _) <- insertErrorLog $ \logId -> ErrorLogInaccessibleNode logId nodeId
        queueAlert (Just logId) $ Alert Unresolved "Unable to connect to node" $
          "Unable to connect to node, " <> maybe "" (" " <>) (_nodeExternalData_alias node) <> " at " <> Uri.render (_nodeExternalData_address node)
    Just (logId, specificLogId) -> updateErrorLog logId specificLogId

clearInaccessibleNodeError
  :: ( Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m, MonadLogger m
     , SqlDb (PhantomDb m)
     , MonadReader a m, HasAppConfig a)
  => Id Node -> m ()
clearInaccessibleNodeError nodeId = when' (nodeNotDeleted nodeId) $ do
  lids :: [Id ErrorLogInaccessibleNode] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogInaccessibleNode" t
    WHERE t.log = el.id AND t.node = ?nodeId AND el.stopped IS NULL
    RETURNING t.log |]
  for_ lids $ notify . mkDefaultNotify
  node' <- project (NodeExternal_dataField ~> DeletableRow_dataSelector) $ (NodeExternal_idField `in_` [nodeId]) `limitTo` 1
  --  $(logDebugSH) ("LIDs we've supposedly blanked out"::String, lids)
  when (not $ null lids) $ for_ node' $ \node -> do
    queueAlert Nothing $ Alert Resolved "Resolved: Now able to connect to node" $
        "Able to again connect to node" <> maybe "" (" " <>) (_nodeExternalData_alias node) <> " at " <> Uri.render (_nodeExternalData_address node)

reportNodeWrongChainError
  :: ( Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig a, MonadReader a m
     , SqlDb (PhantomDb m)
     , MonadLogger m)
  => Id Node -> ChainId -> ChainId -> m ()
reportNodeWrongChainError nodeId expectedChainId actualChainId = when' (nodeNotDeleted nodeId) $ do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogNodeWrongChain) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.log
      FROM "ErrorLog" el
      JOIN "ErrorLogNodeWrongChain" t ON t.log = el.id
      JOIN "NodeExternal" n ON n.id = t.node
     WHERE t."expectedChainId" = ?expectedChainId
       AND t."actualChainId" = ?actualChainId
       AND t.node = ?nodeId
       AND NOT n."data#deleted"
       AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  let formatExtNodeName alias address = "Node" <> maybe "" (" " <>) alias <> " at " <> address
  case existingLog of
    Nothing -> (getNodeName nodeId formatExtNodeName >>=) $ mapM_ $ \nodeName -> do
      (logId, _) <- insertErrorLog $ \logId -> ErrorLogNodeWrongChain logId nodeId expectedChainId actualChainId
      queueAlert (Just logId) $ Alert Unresolved "Node on wrong network" $
        nodeName <> " is on network " <> toBase58Text actualChainId <> " but is expected to be on " <> toBase58Text expectedChainId
    Just (logId, specificLogId) -> updateErrorLog logId specificLogId

clearNodeWrongChainError
  :: ( Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m, MonadLogger m
     , SqlDb (PhantomDb m)
     , MonadReader a m, HasAppConfig a) => Id Node -> m ()
clearNodeWrongChainError nodeId = when' (nodeNotDeleted nodeId) $ do
  lids :: [Id ErrorLogNodeWrongChain] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogNodeWrongChain" t
    WHERE t.log = el.id
      AND t.node = ?nodeId
      AND el.stopped IS NULL
    RETURNING t.log |]
  for_ lids $ notify . mkDefaultNotify
  let formatExtNodeName alias address = "Node" <> maybe "" (" " <>) alias <> " at " <> address
  when (not $ null lids) $ (getNodeName nodeId formatExtNodeName >>=) $ mapM_ $ \nodeName -> do
    queueAlert Nothing $ Alert Resolved "Resolved: Node on right network" $
      nodeName <> " is on correct network"

reportNodeInvalidPeerCountError
  :: (Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig a, MonadReader a m,
      MonadLogger m, SqlDb (PhantomDb m))
  => Id Node -> Int -> Word64 -> m ()
reportNodeInvalidPeerCountError nodeId minPeerCount actualPeerCount = when' (nodeNotDeleted nodeId) $ do
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogNodeInvalidPeerCount) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.log
      FROM "ErrorLog" el
      JOIN "ErrorLogNodeInvalidPeerCount" t ON t.log = el.id
      JOIN "NodeExternal" n ON n.id = t.node
     WHERE t.node = ?nodeId
       AND NOT n."data#deleted"
       AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  let formatExtNodeName alias address = "Node" <> maybe "" (" " <>) alias <> " at " <> address
  case existingLog of
    Nothing -> (getNodeName nodeId formatExtNodeName >>=) $ mapM_ $ \nodeName -> do
      (logId, _) <- insertErrorLog $ \logId ->
        ErrorLogNodeInvalidPeerCount logId nodeId minPeerCount actualPeerCount
      queueAlert (Just logId) $ Alert Unresolved "Node has too few peers." $
        nodeName <> " has fewer peers than the configured minimum of " <> tshow minPeerCount <> "."
    Just (logId, specificLogId) -> updateErrorLog logId specificLogId

clearNodeInvalidPeerCountError
  :: (Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m, MonadLogger m,
      MonadReader a m, HasAppConfig a, SqlDb (PhantomDb m)) => Id Node -> m ()
clearNodeInvalidPeerCountError nodeId = when' (nodeNotDeleted nodeId) $ do
  lids :: [Id ErrorLogNodeInvalidPeerCount] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogNodeInvalidPeerCount" t
    WHERE t.log = el.id
      AND t.node = ?nodeId
      AND el.stopped IS NULL
    RETURNING t.log |]
  for_ lids $ notify . mkDefaultNotify
  let formatExtNodeName alias address = "Node" <> maybe "" (" " <>) alias <> " at " <> address
  when (not $ null lids) $ (getNodeName nodeId formatExtNodeName >>=) $ mapM_ $ \nodeName -> do
    queueAlert Nothing $ Alert Resolved "Resolved: Node has enough peers." $
      nodeName <> " now meets or exceeds the required minimum number of connected peers."


badNodeHeadErrorDelaySeconds :: NominalDiffTime
badNodeHeadErrorDelaySeconds = 125

firstSuccess :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstSuccess = \case
  []     -> pure Nothing
  (x:xs) -> x >>= maybe (firstSuccess xs) (pure . Just)

queryNodeTables :: Monad m => (Identifier -> m (Maybe a)) -> m (Maybe a)
queryNodeTables q = firstSuccess $ fmap q ["NodeExternal", "NodeInternal"]

reportBadNodeHeadError
  :: forall m a latestHead nodeHead lca.
     ( Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig a, MonadReader a m
     , SqlDb (PhantomDb m)
     , BlockLike latestHead, BlockLike nodeHead, BlockLike lca, MonadLogger m)
  => Id Node -> latestHead -> nodeHead -> Maybe lca -> m ()
reportBadNodeHeadError nodeId latestHead nodeHead lca = when' (nodeNotDeleted nodeId) $ do
  let existingLog :: Identifier -> m (Maybe (Id ErrorLog, Id ErrorLogBadNodeHead))
      existingLog nodeTable = listToMaybe <$> [queryQ|
    SELECT el.id, t.log
      FROM "ErrorLog" el
      JOIN "ErrorLogBadNodeHead" t ON t.log = el.id
      JOIN ?nodeTable n ON n.id = t.node
     WHERE t.node = ?nodeId
       AND NOT n."data#deleted"
       AND el.stopped IS NULL
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  queryNodeTables existingLog >>= \case
    Nothing -> do
      void $ insertErrorLog $ \logId -> ErrorLogBadNodeHead
        { _errorLogBadNodeHead_log = logId
        , _errorLogBadNodeHead_node = nodeId
        , _errorLogBadNodeHead_lca = Json . mkVeryBlockLike <$> lca
        , _errorLogBadNodeHead_nodeHead = Json $ mkVeryBlockLike nodeHead
        , _errorLogBadNodeHead_latestHead = Json $ mkVeryBlockLike latestHead
        }

    Just (logId, _specificLogId) -> do
      (g,l) <- returnUpdateErrorLogBy logId ErrorLogBadNodeHead_logField
        [ ErrorLogBadNodeHead_lcaField =. (Json . mkVeryBlockLike <$> lca)
        , ErrorLogBadNodeHead_nodeHeadField =. Json (mkVeryBlockLike nodeHead)
        , ErrorLogBadNodeHead_latestHeadField =. Json (mkVeryBlockLike latestHead)
        ]
      when (_errorLog_lastSeen g >= addUTCTime badNodeHeadErrorDelaySeconds (_errorLog_started g) && isNothing (_errorLog_noticeSentAt g)) $ do
        let (heading, Const message) = badNodeHeadMessage Const (Const . toBase58Text) l
            formatExtNodeName alias address = (maybe "" (\x -> "Node " <> x <> " at ") alias) <> address
        (getNodeName nodeId formatExtNodeName >>=) $ mapM_ $ \nodeName -> do
          queueAlert (Just logId) $ Alert Unresolved heading $
            heading <> ": " <> nodeName <> "\n\n" <> message

clearBadNodeHeadError
  :: ( Monad m, PersistBackend m, PostgresLargeObject m, MonadLogger m
     , SqlDb (PhantomDb m)
     , MonadIO m, MonadReader a m, HasAppConfig a)
  => Id Node -> m ()
clearBadNodeHeadError nodeId = when' (nodeNotDeleted nodeId) $ do
  lids :: [Id ErrorLogBadNodeHead] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogBadNodeHead" t
    WHERE t.log = el.id AND t.node = ?nodeId AND el.stopped IS NULL
    RETURNING t.log |]
  for_ lids $ notify . mkDefaultNotify
  specErrs <- catMaybes <$> for lids getIdBy
  errs <- catMaybes <$> traverse getId (_errorLogBadNodeHead_log <$> specErrs)
  let formatExtNodeName alias address = (maybe "" (\x -> "Node " <> x <> " at ") alias) <> address
  when (any (\e -> isJust $ _errorLog_noticeSentAt e) errs) $
    (getNodeName nodeId formatExtNodeName >>=) $ mapM_ $ \nodeName -> do
      queueAlert Nothing $ Alert Resolved "Resolved: Node is in sync" $
        "Resolved: " <> nodeName <> " is now in sync."

missedBakeLog :: forall m. (PersistBackend m, PostgresRaw m) => RightKind -> PublicKeyHash -> RawLevel -> m (Map (Id Baker) [(Id ErrorLog, Id ErrorLogBakerMissed, Fitness)])
missedBakeLog right pkh lvl =
  ([queryQ|
    SELECT b."publicKeyHash", el.id, elbm.log, elbm.fitness
    FROM "Baker" b
    LEFT OUTER JOIN "ErrorLogBakerMissed" elbm
      ON b."publicKeyHash" = elbm."baker#publicKeyHash"
      AND elbm.right = ?right
      AND elbm.level = ?lvl
    LEFT OUTER JOIN "ErrorLog" el
      ON el.id = elbm.log
      AND el.stopped IS NULL
    WHERE NOT b."data#deleted"
      AND b."publicKeyHash" = ?pkh
  |] :: m [(Id Baker, Maybe (Id ErrorLog), Maybe (Id ErrorLogBakerMissed), Maybe Fitness)]) <&> Map.fromList . fmap (\(bid, elid, elbmid, f) -> (bid, toList $ (,,) <$> elid <*> elbmid <*> f))

bakerNotDeleted :: (PersistBackend m, SqlDb (PhantomDb m)) => PublicKeyHash -> m Bool
bakerNotDeleted pkh = all not <$> project
  (Baker_dataField ~> DeletableRow_deletedSelector)
  ((Baker_publicKeyHashField `in_` [pkh]) `limitTo` 1)

reportMissedBake
  :: ( MonadReader r m, HasAppConfig r, PostgresLargeObject m, MonadIO m, PersistBackend m
     , SqlDb (PhantomDb m)
     , MonadLogger m)
  => Fitness -> RightKind -> PublicKeyHash -> RawLevel -> m ()
reportMissedBake f right pkh lvl = when' (bakerNotDeleted pkh) $ (missedBakeLog right pkh lvl >>=) $ itraverse_ $ \bid eids -> case nonEmpty eids of
  Nothing -> do
    (eid, _elbm) <- insertErrorLog $ \eid -> ErrorLogBakerMissed
      { _errorLogBakerMissed_log = eid
      , _errorLogBakerMissed_baker = bid
      , _errorLogBakerMissed_right = right
      , _errorLogBakerMissed_level = lvl
      , _errorLogBakerMissed_fitness = f
      }
    queueAlert (Just eid) alert
  Just xs -> for_ xs $ \(eid, _elbmid, f') -> when (f' <= f) $ do
    updateErrorLogBy eid ErrorLogBakerMissed_logField [ ErrorLogBakerMissed_fitnessField =. f ]
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
          ON b."publicKeyHash" = elbm."baker#publicKeyHash"
      WHERE elbm.log = el.id
        AND NOT b."data#deleted"
        AND el.stopped IS NULL
        AND elbm.fitness < ?f :: VARCHAR[] -- because groundhog
        AND elbm.right = ?right
        AND b."publicKeyHash" = ?pkh
        AND elbm.level = ?lvl
      RETURNING elbm.log |]
  for_ lids $ notify . mkDefaultNotify
  when (not $ null lids) $ queueAlert Nothing $
    Alert Resolved
      ("Resolved: Missed " <> rightTxt <> " opportunity")
      ("Resolved: Baker with address:" <> toPublicKeyHashText pkh <> " " <> rightTxt <> " opportunity at level " <> tshow (unRawLevel lvl) <> " included due to branch reorganization")
    where
      rightTxt = case right of
        RightKind_Baking -> "bake"
        RightKind_Endorsing -> "endorsement"

getNodeName
  :: (PersistBackend m, SqlDb (PhantomDb m))
  => Id Node
  -> (Maybe Text -> Text -> Text)
  -> m (Maybe Text)
getNodeName nodeId formatExtNodeName = do
  project1 (NodeExternal_dataField ~> DeletableRow_dataSelector) (NodeExternal_idField `in_` [nodeId]) >>= \case
    Just n -> return $ Just $ formatExtNodeName (_nodeExternalData_alias n) (Uri.render (_nodeExternalData_address n))
    Nothing -> project1 NodeInternal_idField (NodeInternal_idField `in_` [nodeId]) >>= \case
      Nothing -> return Nothing
      Just _ -> return $ Just "Kiln Node"

nodeNotDeleted :: (PersistBackend m, SqlDb (PhantomDb m)) => Id Node -> m Bool
nodeNotDeleted nodeId = fmap (all not)
  $ project (NodeExternal_dataField ~> DeletableRow_deletedSelector)
  $ (NodeExternal_idField `in_` [nodeId]) `limitTo` 1

insertErrorLog :: forall a m. (IdData a ~ Id ErrorLog, PersistEntity a, HasDefaultNotify (Id a), PersistBackend m) => (Id ErrorLog -> a) -> m (Id ErrorLog, a)
insertErrorLog mkErrorLog = do
  now <- getTime
  logId <- insert' ErrorLog
    { _errorLog_started = now
    , _errorLog_stopped = Nothing
    , _errorLog_lastSeen = now
    , _errorLog_noticeSentAt = Nothing
    }
  let errLog = mkErrorLog logId
  insert_ errLog
  notify $ mkDefaultNotify (Id logId :: Id a)
  pure (logId, errLog)

updateErrorLog :: (HasDefaultNotify (Id a), PersistBackend m) => Id ErrorLog -> Id a -> m ()
updateErrorLog logId specificLogId = do
  updateErrorLogLastSeen logId
  notify $ mkDefaultNotify specificLogId

updateErrorLogBy
  :: forall a ctor m.
    ( PersistBackend m
    , PersistEntity a
    , IdData a ~ Id ErrorLog
    , IsSumType a ~ HFalse
    , HasDefaultNotify (Id a)
    )
  => Id ErrorLog
  -> Field a ctor (IdData a)
  -> [Update (PhantomDb m) (RestrictionHolder a ctor)]
  -> m ()
updateErrorLogBy logId idField updates = do
  updateErrorLogLastSeen logId
  update updates (idField ==. logId)
  notify $ mkDefaultNotify (Id logId :: Id a)

returnUpdateErrorLogBy
  :: forall a u m ctor.
  ( IdData a ~ Id ErrorLog
  , EntityWithIdBy u a
  , PersistBackend m
  , IsSumType a ~ HFalse
  , HasDefaultNotify (Id a)
  )
  => Id ErrorLog
  -> Field a ctor (IdData a)
  -> [Update (PhantomDb m) (RestrictionHolder a ctor)]
  -> m (ErrorLog, a)
returnUpdateErrorLogBy logId idField updates = do
  g <- returnUpdateErrorLogLastSeen logId
  update updates (idField ==. logId)
  notify $ mkDefaultNotify (Id logId :: Id a)
  getIdBy (Id logId :: Id a) >>= \case
    Nothing -> fail $ "returnUpdateErrorLogBy called on nonexistent specific record " <> show logId
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
