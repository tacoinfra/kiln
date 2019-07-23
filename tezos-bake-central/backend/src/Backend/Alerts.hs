{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Alerts where

import Prelude hiding (log, cycle)
import Data.Dependent.Sum
import Control.Lens ((<&>))
import Control.Monad.Logger (MonadLogger)
import Data.Map (Map())
import qualified Data.Map as Map
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
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
import Backend.Config (AppConfig(..), HasAppConfig, askAppConfig)
import Backend.Schema
import Common.Alerts (
    BakerErrorDescriptions(..),
    ErrorLogMessage(..),
    badNodeHeadMessage,
    bakerDeactivatedDescriptions,
    bakerDeactivationRiskDescriptions,
    bakerVotingReminderDescriptions,
    plaintextErrorDescription,
  )
import Common.App (errorLogIdForErrorLogView)
import Common.Schema
import ExtraPrelude


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
  for_ lids notifyDefault

unresolvedBakerAlert :: BakerErrorDescriptions -> Alert
unresolvedBakerAlert dsc = Alert Unresolved (_bakerErrorDescriptions_title dsc) $ T.unlines $ catMaybes
  [ Just $ plaintextErrorDescription $ foldl (\t x -> t <> "\n\n" <> x) "" (_bakerErrorDescriptions_problem dsc)
  , _bakerErrorDescriptions_warning dsc
  ]

resolvedBakerAlert :: BakerErrorDescriptions -> Baker -> Alert
resolvedBakerAlert dsc = uncurry (Alert Resolved) . _bakerErrorDescriptions_resolved dsc

mkErrorLogAlert :: ErrorLogMessage -> Alert
mkErrorLogAlert = pure Alert
  <*> bool Unresolved Resolved . _errorLogMessage_resolved
  <*> _errorLogMessage_subject
  <*> _errorLogMessage_content

getBaker :: (PersistBackend m, SqlDb (PhantomDb m)) => PublicKeyHash -> m (Maybe Baker)
getBaker pkh = selectSingle $ Baker_publicKeyHashField `in_` [pkh]

reportBakerDeactivated
  :: ( Monad m, MonadIO m, MonadReader a m, MonadLogger m, SqlDb (PhantomDb m)
     , PersistBackend m, PostgresLargeObject m, HasAppConfig a
     )
  => PublicKeyHash -> ProtoInfo -> Fitness -> m ()
reportBakerDeactivated pkh protoInfo newFit = do
  chainId <- _appConfig_chainId <$> askAppConfig
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogBakerDeactivated, Fitness) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.log, t.fitness
      FROM "ErrorLog" el
      JOIN "ErrorLogBakerDeactivated" t ON t.log = el.id
      JOIN "Baker" b ON b."publicKeyHash" = t."publicKeyHash"
       AND NOT b."data#deleted"
       AND el.stopped IS NULL
       AND el."chainId" = ?chainId
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
  chainId <- _appConfig_chainId <$> askAppConfig
  lids :: [Id ErrorLogBakerDeactivated] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogBakerDeactivated" t
    WHERE t.log = el.id
      AND t."publicKeyHash" = ?pkh
      AND el.stopped IS NULL
      AND el."chainId" = ?chainId
      AND t.fitness < ?newFit :: VARCHAR[]
    RETURNING t.log |]
  for_ lids notifyDefault
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
  chainId <- _appConfig_chainId <$> askAppConfig
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogBakerDeactivationRisk, Fitness) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.log, t.fitness
      FROM "ErrorLog" el
      JOIN "ErrorLogBakerDeactivationRisk" t ON t.log = el.id
      JOIN "Baker" b ON b."publicKeyHash" = t."publicKeyHash"
     WHERE NOT b."data#deleted"
       AND el.stopped IS NULL
       AND el."chainId" = ?chainId

     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Just (logId, _specificLogId, oldFit) -> updateErrorLogBy logId ErrorLogBakerDeactivationRisk_logField
      [ ErrorLogBakerDeactivationRisk_fitnessField =. max newFit oldFit ]
    Nothing -> do
      baker' <- getBaker pkh
      for_ baker' $ \baker -> do
        (logId, log) <- insertErrorLog $ \logId -> ErrorLogBakerDeactivationRisk
          logId
          (_baker_publicKeyHash baker)
          gracePeriod
          latestCycle
          (_protoInfo_preservedCycles protoInfo)
          newFit
        queueAlert (Just logId) $ unresolvedBakerAlert $ bakerDeactivationRiskDescriptions log

clearBakerDeactivationRisk
  :: ( Monad m, MonadIO m, MonadReader a m, MonadLogger m, SqlDb (PhantomDb m)
     , PersistBackend m, PostgresLargeObject m, HasAppConfig a
     )
  => PublicKeyHash -> Fitness -> m ()
clearBakerDeactivationRisk pkh newFit = do
  chainId <- _appConfig_chainId <$> askAppConfig
  lids :: [Id ErrorLogBakerDeactivationRisk] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogBakerDeactivationRisk" t
    WHERE t.log = el.id
      AND t."publicKeyHash" = ?pkh
      AND el.stopped IS NULL
      AND el."chainId" = ?chainId
      AND t.fitness < ?newFit :: VARCHAR[]
    RETURNING t.log |]
  for_ lids notifyDefault
  --  $(logDebugSH) ("LIDs we've supposedly blanked out"::String, lids)
  baker' <- getBaker pkh
  log' <- for (listToMaybe lids) $ getBy . fromId
  for_ (liftA2 (,) baker' (join log')) $ \(baker, log) ->
    queueAlert Nothing $ resolvedBakerAlert (bakerDeactivationRiskDescriptions log) baker

reportInsufficientFunds
  :: ( Monad m, MonadIO m, MonadReader a m
     , PersistBackend m, PostgresLargeObject m, HasAppConfig a
     )
  => Baker -> m ()
reportInsufficientFunds baker = do
  let pkh = _baker_publicKeyHash baker
  chainId <- _appConfig_chainId <$> askAppConfig
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogInsufficientFunds) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.log
      FROM "ErrorLog" el
      JOIN "ErrorLogInsufficientFunds" t ON t.log = el.id
      JOIN "Baker" b ON b."publicKeyHash" = t."baker#publicKeyHash"
     WHERE NOT b."data#deleted"
       AND el.stopped IS NULL
       AND el."chainId" = ?chainId
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  now <- getTime
  case existingLog of
    Just (logId, _specificLogId) -> updateErrorLogBy logId ErrorLogInsufficientFunds_logField
      [ ErrorLogInsufficientFunds_detectedField =. now ]
    Nothing -> do
      void $ insertErrorLog $ \logId ->
        ErrorLogInsufficientFunds logId (Id pkh) now

clearInsufficientFunds
  :: ( Monad m, MonadIO m, MonadReader a m
     , PersistBackend m, PostgresLargeObject m, HasAppConfig a
     )
  => Baker -> m ()
clearInsufficientFunds baker = do
  let pkh = _baker_publicKeyHash baker
  chainId <- _appConfig_chainId <$> askAppConfig
  lids :: [Id ErrorLogInsufficientFunds] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogInsufficientFunds" t
      WHERE t.log = el.id
      AND t."baker#publicKeyHash" = ?pkh
      AND el.stopped IS NULL
      AND el."chainId" = ?chainId
    RETURNING t.log |]
  for_ lids notifyDefault

reportInaccessibleNodeError
  :: ( Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m
     , HasAppConfig a, MonadReader a m, SqlDb (PhantomDb m)
     , MonadLogger m)
  => Id Node -> m ()
reportInaccessibleNodeError nodeId = when' (nodeNotDeleted nodeId) $ do
  chainId <- _appConfig_chainId <$> askAppConfig
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogInaccessibleNode) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.log
      FROM "ErrorLog" el
      JOIN "ErrorLogInaccessibleNode" t ON t.log = el.id
      JOIN "NodeExternal" n ON n.id = t.node
     WHERE t.node = ?nodeId
       AND NOT n."data#deleted"
       AND el.stopped IS NULL
       AND el."chainId" = ?chainId
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
  chainId <- _appConfig_chainId <$> askAppConfig
  lids :: [Id ErrorLogInaccessibleNode] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogInaccessibleNode" t
     WHERE t.log = el.id
       AND t.node = ?nodeId
       AND el.stopped IS NULL
       AND el."chainId" = ?chainId
    RETURNING t.log |]
  for_ lids notifyDefault
  node' <- project (NodeExternal_dataField ~> DeletableRow_dataSelector) $ (NodeExternal_idField `in_` [nodeId]) `limitTo` 1
  --  $(logDebugSH) ("LIDs we've supposedly blanked out"::String, lids)
  unless (null lids) $ for_ node' $ \node -> do
    queueAlert Nothing $ Alert Resolved "Resolved: Now able to connect to node" $
        "Able to again connect to node" <> maybe "" (" " <>) (_nodeExternalData_alias node) <> " at " <> Uri.render (_nodeExternalData_address node)

reportNodeWrongChainError
  :: ( Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig a, MonadReader a m
     , SqlDb (PhantomDb m)
     , MonadLogger m)
  => Id Node -> ChainId -> ChainId -> m ()
reportNodeWrongChainError nodeId expectedChainId actualChainId = when' (nodeNotDeleted nodeId) $ do
  chainId <- _appConfig_chainId <$> askAppConfig
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
       AND el."chainId" = ?chainId
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
  chainId <- _appConfig_chainId <$> askAppConfig
  lids :: [Id ErrorLogNodeWrongChain] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogNodeWrongChain" t
    WHERE t.log = el.id
      AND t.node = ?nodeId
      AND el.stopped IS NULL
      AND el."chainId" = ?chainId
    RETURNING t.log |]
  for_ lids notifyDefault
  let formatExtNodeName alias address = "Node" <> maybe "" (" " <>) alias <> " at " <> address
  unless (null lids) $ (getNodeName nodeId formatExtNodeName >>=) $ mapM_ $ \nodeName -> do
    queueAlert Nothing $ Alert Resolved "Resolved: Node on right network" $
      nodeName <> " is on correct network"

reportNodeInvalidPeerCountError
  :: (Monad m, PersistBackend m, PostgresLargeObject m, MonadIO m, HasAppConfig a, MonadReader a m,
      MonadLogger m, SqlDb (PhantomDb m))
  => Id Node -> Int -> Word64 -> m ()
reportNodeInvalidPeerCountError nodeId minPeerCount actualPeerCount = when' (nodeNotDeleted nodeId) $ do
  chainId <- _appConfig_chainId <$> askAppConfig
  existingLog :: Maybe (Id ErrorLog, Id ErrorLogNodeInvalidPeerCount) <- listToMaybe <$> [queryQ|
    SELECT el.id, t.log
      FROM "ErrorLog" el
      JOIN "ErrorLogNodeInvalidPeerCount" t ON t.log = el.id
      JOIN "NodeExternal" n ON n.id = t.node
     WHERE t.node = ?nodeId
       AND NOT n."data#deleted"
       AND el.stopped IS NULL
       AND el."chainId" = ?chainId
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
  chainId <- _appConfig_chainId <$> askAppConfig
  lids :: [Id ErrorLogNodeInvalidPeerCount] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogNodeInvalidPeerCount" t
    WHERE t.log = el.id
      AND t.node = ?nodeId
      AND el.stopped IS NULL
      AND el."chainId" = ?chainId
    RETURNING t.log |]
  for_ lids notifyDefault
  let formatExtNodeName alias address = "Node" <> maybe "" (" " <>) alias <> " at " <> address
  unless (null lids) $ (getNodeName nodeId formatExtNodeName >>=) $ mapM_ $ \nodeName -> do
    queueAlert Nothing $ Alert Resolved "Resolved: Node has enough peers." $
      nodeName <> " now meets or exceeds the required minimum number of connected peers."

reportVotingReminderError
  :: ( Monad m, MonadIO m, MonadReader a m, MonadLogger m
     , PersistBackend m, PostgresLargeObject m, HasAppConfig a
     )
  => ChainId
  -> Id Baker
  -> RawLevel
  -> VotingPeriodKind
  -> Bool
  -> Int
  -> UTCTime
  -> m ()
reportVotingReminderError
  chainId
  bid
  votingPeriod
  votingPeriodKind
  previouslyVoted
  rangeMax
  periodEndsAt
  = do
  existingLog :: Maybe (Id ErrorLog, Bool) <- listToMaybe <$> [queryQ|
    SELECT el.id, el.stopped IS NULL
      FROM "ErrorLog" el
      JOIN "ErrorLogVotingReminder" t ON t.log = el.id
      JOIN "Baker" b ON b."publicKeyHash" = t."baker#publicKeyHash"
     WHERE NOT b."data#deleted"
       AND el."chainId" = ?chainId
       AND t."baker#publicKeyHash" = ?bid
       AND t."periodKind" = ?votingPeriodKind
       AND t."previouslyVoted" = ?previouslyVoted
       AND t."votingPeriod" = ?votingPeriod
       AND t."rangeMax" = ?rangeMax
     ORDER BY el."lastSeen" DESC, el.started DESC
     LIMIT 1
    |]
  case existingLog of
    Just (_, False) -> pure () -- We already issued this alert but it was manually resolved so don't issue it again.
    Just (logId, True) -> updateErrorLogBy logId ErrorLogVotingReminder_logField
      [ ErrorLogVotingReminder_previouslyVotedField =. previouslyVoted
      , ErrorLogVotingReminder_periodEndsAtField =. periodEndsAt
      ]
    Nothing -> do
      (logId, log) <- insertErrorLog $ \logId ->
        ErrorLogVotingReminder
          { _errorLogVotingReminder_log = logId
          , _errorLogVotingReminder_baker = bid
          , _errorLogVotingReminder_periodKind = votingPeriodKind
          , _errorLogVotingReminder_votingPeriod = votingPeriod
          , _errorLogVotingReminder_previouslyVoted = previouslyVoted
          , _errorLogVotingReminder_rangeMax = rangeMax
          , _errorLogVotingReminder_periodEndsAt = periodEndsAt
          }
      now <- getTime
      let desc = bakerVotingReminderDescriptions log (periodEndsAt `diffUTCTime` now)
      queueAlert (Just logId) $ Alert Unresolved
        (_bakerErrorDescriptions_title desc)
        ""

clearPastVotingPeriodErrors
  :: ( Monad m, MonadIO m
     , PersistBackend m, PostgresLargeObject m
     )
  => ChainId -> Id Baker -> Maybe Bool -> Int -> m ()
clearPastVotingPeriodErrors chainId bid previouslyVoted rangeMax = do
  lids :: [Id ErrorLogVotingReminder] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogVotingReminder" t
    WHERE t.log = el.id
      AND el.stopped IS NULL
      AND el."chainId" = ?chainId
      AND t."baker#publicKeyHash" = ?bid
      AND (t."rangeMax" <> ?rangeMax OR ?previouslyVoted IS NULL OR t."previouslyVoted" <> ?previouslyVoted)
    RETURNING t.log |]
  for_ lids notifyDefault

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
  chainId <- _appConfig_chainId <$> askAppConfig
  let existingLog :: Identifier -> m (Maybe (Id ErrorLog, Id ErrorLogBadNodeHead))
      existingLog nodeTable = listToMaybe <$> [queryQ|
    SELECT el.id, t.log
      FROM "ErrorLog" el
      JOIN "ErrorLogBadNodeHead" t ON t.log = el.id
      JOIN ?nodeTable n ON n.id = t.node
     WHERE t.node = ?nodeId
       AND NOT n."data#deleted"
       AND el.stopped IS NULL
       AND el."chainId" = ?chainId
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
            formatExtNodeName alias address = maybe "" (\x -> "Node " <> x <> " at ") alias <> address
        (getNodeName nodeId formatExtNodeName >>=) $ mapM_ $ \nodeName -> do
          queueAlert (Just logId) $ Alert Unresolved heading $
            heading <> ": " <> nodeName <> "\n\n" <> message

clearBadNodeHeadError
  :: ( Monad m, PersistBackend m, PostgresLargeObject m, MonadLogger m
     , SqlDb (PhantomDb m)
     , MonadIO m, MonadReader a m, HasAppConfig a)
  => Id Node -> m ()
clearBadNodeHeadError nodeId = when' (nodeNotDeleted nodeId) $ do
  chainId <- _appConfig_chainId <$> askAppConfig
  lids :: [Id ErrorLogBadNodeHead] <- stripOnly <$> [queryQ|
    UPDATE "ErrorLog" el SET stopped = NOW()
      FROM "ErrorLogBadNodeHead" t
     WHERE t.log = el.id
       AND t.node = ?nodeId
       AND el.stopped IS NULL
       AND el."chainId" = ?chainId
 RETURNING t.log |]
  for_ lids notifyDefault
  specErrs <- catMaybes <$> for lids getIdBy
  errs <- catMaybes <$> traverse getId (_errorLogBadNodeHead_log <$> specErrs)
  let formatExtNodeName alias address = maybe "" (\x -> "Node " <> x <> " at ") alias <> address
  when (any (isJust . _errorLog_noticeSentAt) errs) $
    (getNodeName nodeId formatExtNodeName >>=) $ traverse_ $ \nodeName -> do
      queueAlert Nothing $ Alert Resolved "Resolved: Node is in sync" $
        "Resolved: " <> nodeName <> " is now in sync."

missedBakeLog
  :: forall m a. (PersistBackend m, PostgresRaw m, MonadReader a m, HasAppConfig a)
  => RightKind
  -> PublicKeyHash
  -> RawLevel
  -> m (Map (Id Baker) [(Id ErrorLog, Id ErrorLogBakerMissed, Fitness)])
missedBakeLog right pkh lvl = do
  chainId <- _appConfig_chainId <$> askAppConfig
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
      AND el."chainId" = ?chainId
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
reportMissedBake f right pkh lvl = when' (bakerNotDeleted pkh) $ do
  chainId <- _appConfig_chainId <$> askAppConfig
  (missedBakeLog right pkh lvl >>=) $ itraverse_ $ \bid eids -> case nonEmpty eids of
    Nothing -> do
      (eid, _elbm) <- insertErrorLog $ \eid -> ErrorLogBakerMissed
        { _errorLogBakerMissed_log = eid
        , _errorLogBakerMissed_baker = bid
        , _errorLogBakerMissed_right = right
        , _errorLogBakerMissed_level = lvl
        , _errorLogBakerMissed_fitness = f
        }
      project1 RightNotificationSettings_limitField (RightNotificationSettings_rightKindField ==. right) >>= \case
        Nothing -> queueAlert (Just eid) $ alert lvl
        Just rnl -> do
          let mins = _rightNotificationLimit_withinMinutes rnl
          elIds :: [(Id ErrorLog, RawLevel)] <- [queryQ|
            SELECT el.id, elbm.level
            FROM "Baker" b
            JOIN "ErrorLogBakerMissed" elbm
              ON b."publicKeyHash" = elbm."baker#publicKeyHash"
              AND elbm.right = ?right
            JOIN "ErrorLog" el
              ON el.id = elbm.log
              AND el.stopped IS NULL
              AND el.chainId = ?chainId
            WHERE NOT b."data#deleted"
              AND b."publicKeyHash" = ?pkh
              AND el.started > NOW() AT TIME ZONE 'UTC' - ?mins * INTERVAL '1 minute'
              AND el."noticeSentAt" IS NULL
          |]
          when (length elIds >= _rightNotificationLimit_amount rnl) $ for_ elIds $ \(eid', lvl') -> queueAlert (Just eid') $ alert lvl'
    Just xs -> for_ xs $ \(eid, _elbmid, f') -> when (f' <= f) $ do
      updateErrorLogBy eid ErrorLogBakerMissed_logField [ ErrorLogBakerMissed_fitnessField =. f ]
      queueAlert (Just eid) $ alert lvl
    where
      alert lvl' = Alert Unresolved
        ("Missed " <> rightTxt <> " opportunity")
        ("Baker with address:" <> toPublicKeyHashText pkh <> " Missed " <> rightTxt <> " opportunity at level " <> tshow (unRawLevel lvl'))
      rightTxt = case right of
        RightKind_Baking -> "bake"
        RightKind_Endorsing -> "endorsement"

-- we care only to inform the baker of each accusation against them, and no other provenance matters.
accusedBakeLog
  :: forall m. (PersistBackend m, PostgresRaw m)
  => PublicKeyHash
  -> ChainId
  -> OperationHash
  -> BlockHash
  -> m (Map (Id Baker) [(Id ErrorLog, Id ErrorLogBakerAccused)])
accusedBakeLog pkh chainId opHash blkHash =
  ([queryQ|
    SELECT b."publicKeyHash", el.id, elbm.log
    FROM "Baker" b
    LEFT OUTER JOIN "ErrorLogBakerAccused" elbm
      ON b."publicKeyHash" = elbm."baker#publicKeyHash"
      AND elbm."op#hash" = ?opHash
      AND elbm."op#blockHash" = ?blkHash
     LEFT OUTER JOIN "ErrorLog" el
       ON el.id = elbm.log
      AND el."chainId" = ?chainId
    WHERE NOT b."data#deleted"
      AND b."publicKeyHash" = ?pkh
  |] :: m [(Id Baker, Maybe (Id ErrorLog), Maybe (Id ErrorLogBakerAccused))]) <&> Map.fromList . fmap (\(bid, elid, elbmid) -> (bid, toList $ (,) <$> elid <*> elbmid))

-- there is no corresponding 'clear accusation'.  you cannot become unaccused by branch reorg
reportAccusation
  :: ( MonadReader r m, HasAppConfig r, PostgresLargeObject m, MonadIO m, PersistBackend m
     , SqlDb (PhantomDb m)
     , MonadLogger m)
  => OperationHash -> BlockHash -> RightKind -> PublicKeyHash -> RawLevel -> Cycle -> RawLevel -> Cycle -> m ()
reportAccusation opHash blkHash right pkh lvl cycle aLvl aCycle = when' (bakerNotDeleted pkh) $ do
  chainId <- _appConfig_chainId <$> askAppConfig
  (accusedBakeLog pkh chainId opHash blkHash >>=) $ itraverse_ $ \bid eids -> case nonEmpty eids of
    Nothing -> do
      (eid, _elbm) <- insertErrorLog $ \eid -> ErrorLogBakerAccused
        { _errorLogBakerAccused_log = eid
        , _errorLogBakerAccused_op = Id (opHash, blkHash)
        , _errorLogBakerAccused_baker = bid
        , _errorLogBakerAccused_right = right
        , _errorLogBakerAccused_level = lvl
        , _errorLogBakerAccused_cycle = cycle
        , _errorLogBakerAccused_accusedLevel = aLvl
        , _errorLogBakerAccused_accusedCycle = aCycle
        }
      queueAlert (Just eid) alert
    Just xs -> for_ xs $ \(_eid, _elbmid) ->
      pure ()
    where
      alert = Alert Unresolved
        ("Double " <> rightTxt)
        ("Baker with address:" <> toPublicKeyHashText pkh <> " Double " <> rightTxt <> " at level " <> tshow (unRawLevel lvl))
      rightTxt = case right of
        RightKind_Baking -> "baked"
        RightKind_Endorsing -> "endorsed"

clearMissedBake
  :: (MonadLogger m, MonadReader r m, HasAppConfig r, MonadIO m, PostgresLargeObject m, PersistBackend m)
  => Fitness -> RightKind -> PublicKeyHash -> RawLevel -> m ()
clearMissedBake f right pkh lvl = do
  chainId <- _appConfig_chainId <$> askAppConfig
  lids :: [Id ErrorLogBakerMissed] <- stripOnly <$> [queryQ|
      UPDATE "ErrorLog" el SET stopped = NOW()
        FROM "ErrorLogBakerMissed" elbm
        JOIN "Baker" b
          ON b."publicKeyHash" = elbm."baker#publicKeyHash"
      WHERE elbm.log = el.id
        AND NOT b."data#deleted"
        AND el.stopped IS NULL
        AND el."chainId" = ?chainId
        AND elbm.fitness < ?f :: VARCHAR[] -- because groundhog
        AND elbm.right = ?right
        AND b."publicKeyHash" = ?pkh
        AND elbm.level = ?lvl
      RETURNING elbm.log |]
  for_ lids notifyDefault
  unless (null lids) $ queueAlert Nothing $
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

insertErrorLog
  :: forall a r m
  . (IdData a ~ Id ErrorLog, PersistEntity a, HasDefaultNotify (Id a), PersistBackend m, MonadReader r m, HasAppConfig r)
  => (Id ErrorLog -> a) -> m (Id ErrorLog, a)
insertErrorLog mkErrorLog = do
  now <- getTime
  chainId <- _appConfig_chainId <$> askAppConfig
  logId <- insert' ErrorLog
    { _errorLog_started = now
    , _errorLog_stopped = Nothing
    , _errorLog_lastSeen = now
    , _errorLog_noticeSentAt = Nothing
    , _errorLog_chainId = chainId
    }
  let errLog = mkErrorLog logId
  insert_ errLog
  notifyDefault (Id logId :: Id a)
  pure (logId, errLog)

updateErrorLog :: (HasDefaultNotify (Id a), PersistBackend m) => Id ErrorLog -> Id a -> m ()
updateErrorLog logId specificLogId = do
  updateErrorLogLastSeen logId
  notifyDefault specificLogId

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
  notifyDefault (Id logId :: Id a)

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
  notifyDefault (Id logId :: Id a)
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

resolveAlert :: PersistBackend m => DSum LogTag Identity -> m ()
resolveAlert elv@(tag :=> _) = logAssume tag $ do
  let eid = mkId tag (errorLogIdForErrorLogView elv)
  now <- getTime
  updateId (unId eid) [ErrorLog_stoppedField =. Just now]
  notifyDefault (tag :=> eid)
  where
    mkId :: proxy e -> IdData e -> Id e
    mkId _ = Id

resolveAlerts :: (PersistBackend m, SqlDb (PhantomDb m)) => [DSum LogTag (Const (Id ErrorLog))] -> m ()
resolveAlerts tags = do
  eids :: [Key ErrorLog BackendSpecific] <- for tags $ \(tag :=> Const eid) -> logAssume tag $ do
    notifyDefault (tag :=> Id eid)
    pure $ fromId eid
  now <- getTime
  update [ErrorLog_stoppedField =. Just now] $ AutoKeyField `in_` eids
