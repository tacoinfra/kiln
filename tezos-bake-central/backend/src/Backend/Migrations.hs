{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Migrations where

import Backend.Schema (migrateSchema)
import Control.Monad (forM_)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Logger (MonadLogger, logInfoS)
import Data.String (fromString)
import qualified Data.Text as T
import Database.Groundhog.Core
import Database.Groundhog.Generic (runMigration)
import Database.Groundhog.Generic.Migration hiding (migrateSchema)
import Database.PostgreSQL.Simple.Types (Identifier (..), QualifiedIdentifier (..))
import Rhyolite.Backend.Account (migrateAccount)
import Rhyolite.Backend.DB.PsqlSimple (Only (..), PostgresRaw, execute_, queryQ, traceExecuteQ)
import Rhyolite.Backend.EmailWorker (migrateQueuedEmail)
import Safe
import Tezos.Types (ChainId)

import Common.Schema (ErrorLog, Id, TezosVersion(..))
import ExtraPrelude

type Migrate m = (PersistBackend m, SchemaAnalyzer m, PostgresRaw m, MonadLogger m, MonadIO m)

convQN :: QualifiedIdentifier -> QualifiedName
convQN (QualifiedIdentifier a b) = (T.unpack <$> a, T.unpack b)

migrateKiln :: Migrate m => MonadFail m => ChainId -> m ()
migrateKiln chainId = (getTableAnalysis >>= preMigrate chainId >>= autoMigrate) *> extraIndexes

autoMigrate :: Migrate m => TableAnalysis m -> m ()
autoMigrate tableAnalysis = runMigration $ do
  migrateAccount tableAnalysis
  migrateQueuedEmail tableAnalysis
  migrateSchema tableAnalysis

preMigrate :: Migrate m => ChainId -> TableAnalysis m -> m (TableAnalysis m)
preMigrate chainId =
      migrateParameters
  >=> migratePublicNodeHead
  >=> dropTableIfExists False (QualifiedIdentifier Nothing "ErrorLogUpgradeNotice")
  >=> dropTableIfExists False (QualifiedIdentifier Nothing "PendingReward")
  >=> dropTableIfExists False (QualifiedIdentifier Nothing "ClientInfo")
  >=> dropTableIfExists True (QualifiedIdentifier Nothing "Client")
  >=> dropTableIfExists False (QualifiedIdentifier Nothing "GenericCacheEntry")
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "Delegate") "id" -- No, it's not possible to promote the existing unique key to the primary key.  oh well.
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerDeactivated") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerDeactivationRisk") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerMissed") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerNoHeartbeat") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogInaccessibleNode") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogInaccessibleNode") "alias"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogInaccessibleNode") "address"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogMultipleBakersForSameBaker") "id"
  >=> migrateErrorLogNetworkUpdateCommitHash
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogNetworkUpdate") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogNodeInvalidPeerCount") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogNodeWrongChain") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogNodeWrongChain") "alias"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogNodeWrongChain") "address"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBadNodeHead") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "LedgerAccount") "checkIfRegistered"
  >=> migrateLedgerAccountTable
  >=> renameColumnIfExists (QualifiedIdentifier Nothing "Delegate") "deleted" "data#deleted"
  >=> renameColumnIfExists (QualifiedIdentifier Nothing "Delegate") "alias" "data#data#alias"
  >=> renameTableIfExists (QualifiedIdentifier Nothing "Delegate") "Baker"
  >=> migrateNodesToSplitTable
  >=> migrateProcessDataToSplitTable
  >=> migrateErrorLogBadNodeHeadTable
  >=> migrateBakerRightsCycleProgressTable
  >=> removeArchivalNodeFromTables
  >=> migrateRawCacheEntryTable
  >=> migrateNodeDetailsAddSynchronisationThreshold
  >=> migratePeriodTestingTable
  >=> migrateErrorLogBakerLedgerDisconnected
  >=> removeUnusedProtocolIndexColumns
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "BakerRight") "slots"
  >=> removeNodeSavePointInfo
  >=> migrateAccusationTable
  >=> migrateErrorLogBakerAccusedTable
  >=> createSequence (QualifiedIdentifier Nothing "NodeInternal_pid")
  >=> createSequence (QualifiedIdentifier Nothing "ProcessLockUniqueId")
  >=> migrateBakerDaemonInternalTable
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "BakerDaemonInternal") "data#data#insufficientFunds"
  >=> migrateProcessDataTable
  >=> migrateProcessDataTable2
  >=> migrateProcessDataTableAddErrorLog
  >=> migrateUpstreamVersionTable
  >=> dropTableIf (QualifiedIdentifier Nothing "PeriodTesting") (ColumnExists "votingPeriod") False
  >=> dropTableIf (QualifiedIdentifier Nothing "PeriodTestingVote") (ColumnExists "periodVote#votingPeriod") False
  >=> dropTableIf (QualifiedIdentifier Nothing "PeriodPromotionVote") (ColumnExists "periodVote#votingPeriod") False
  >=> dropTableIf (QualifiedIdentifier Nothing "PeriodProposal") (ColumnMissing "id") False
  >=> dropTableIf (QualifiedIdentifier Nothing "ProtocolIndex") (ColumnMissing "jsonConstants") False
  >=> migrateChainIdToErrorLog chainId
  >=> dropTableIfExists False (QualifiedIdentifier Nothing "CachedProtocolConstants")
  >=> dropTableIfExists False (QualifiedIdentifier Nothing "Parameters")
  >=> migrateErrorLogBakerMissedTimestamp
  >=> migrateProtocolIndexKey
  >=> migrateSnapshotMetaControl
  >=> deleteObsidianPublicNodeConfigs
  >=> deleteObsidianPublicNodeHeads
  >=> deleteTzScanPublicNodeConfigs
  >=> deleteTzScanPublicNodeHeads
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "NodeExternal") "data#data#commitHash"
  >=> updateAmendment
  >=> dropTableIfExists False (QualifiedIdentifier Nothing "BlockTodo")
  >=> dropTableIfExists False (QualifiedIdentifier Nothing "PublicNodeConfig")
  >=> dropTableIfExists False (QualifiedIdentifier Nothing "PublicNodeHead")
  >=> dropTableIfExists False (QualifiedIdentifier Nothing "RawCacheEntry")
  >=> migrateBakerDetailsAddMissedRigtsInRow
  >=> migrateCacheBakingRights
  >=> migrateCacheEndorsingRightsDropContext
  >=> removeErrorLogBakerAccusedForeignKey
  >=> renameColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerAccused") "op#hash" "opHash"
  >=> renameColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerAccused") "op#blockHash" "blockHash"
  >=> dropTableIfExists False (QualifiedIdentifier Nothing "Accusation")

migrateErrorLogNetworkUpdateCommitHash :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateErrorLogNetworkUpdateCommitHash ta = do
  let table = QualifiedIdentifier Nothing "ErrorLogNetworkUpdate"
  analyzedTable' <- analyzeTable ta (convQN table)
  let hasCommit = any ((== "commit") . colName)  . tableColumns
      hasVersion = any ((== "version") . colName) . tableColumns
  case analyzedTable' of
    Nothing -> pure ta
    Just analyzedTable -> case hasCommit analyzedTable && not (hasVersion analyzedTable) of
        False -> pure ta
        True -> do
            void [traceExecuteQ|ALTER TABLE "ErrorLogNetworkUpdate" ADD COLUMN "version" VARCHAR;|]

            (idsHashes :: [(Id ErrorLog, Text)]) <-
                [queryQ|SELECT "log", "commit" FROM "ErrorLogNetworkUpdate";|]

            let idsVersions = fmap (fmap (TezosVersion . Left)) idsHashes

            for_ idsVersions $ \(logg, version) -> do
                [traceExecuteQ|
                    UPDATE "ErrorLogNetworkUpdate"
                    SET "version" = ?version
                    WHERE "log" = ?logg;
                |]

            void [traceExecuteQ|ALTER TABLE "ErrorLogNetworkUpdate" DROP COLUMN "commit";|]
            void [traceExecuteQ|ALTER TABLE "ErrorLogNetworkUpdate" ALTER COLUMN "version" SET NOT NULL;|]
            getTableAnalysis

migrateParameters :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateParameters ta = do
  let table = QualifiedIdentifier Nothing "Parameters"
  analyzedTable' <- analyzeTable ta (convQN table)
  let
    hasHeadTimestamp = any ((== "headTimestamp") . colName) . tableColumns
    hasOriginationSize = any ((== "protoInfo#originationSize") . colName) . tableColumns
  case analyzedTable' of
    Nothing -> pure ta
    Just analyzedTable -> if hasHeadTimestamp analyzedTable || not (hasOriginationSize analyzedTable)
      then dropTable table False *> getTableAnalysis
      else pure ta

migratePublicNodeHead :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migratePublicNodeHead ta = do
  let table = QualifiedIdentifier Nothing "PublicNodeHead"
  hasHeadBlockHash <- fmap (any ((== "headBlock#hash") . colName) . tableColumns) <$> analyzeTable ta (convQN table)
  hasProtocolHash <- fmap (any ((== "protocolHash") . colName) . tableColumns) <$> analyzeTable ta (convQN table)
  if hasProtocolHash == Just False || hasHeadBlockHash == Just False
    then dropTable table False *> getTableAnalysis
    else pure ta

renameColumnIfExists :: Migrate m => QualifiedIdentifier -> Identifier -> Identifier -> TableAnalysis m -> m (TableAnalysis m)
renameColumnIfExists table columnFrom columnTo ta = do
  maybeTableInfo <- analyzeTable ta (convQN table)
  let columnExists = do
        tableInfo <- maybeTableInfo
        return $ columnFrom `elem` fmap (Identifier . T.pack . colName) (tableColumns tableInfo)
  case columnExists of
    Just True -> renameColumn table columnFrom columnTo *> getTableAnalysis
    _ -> pure ta

data DropTableCondition
  = ColumnExists String
  | ColumnMissing String
  deriving (Eq, Ord, Show)

dropTableIf :: Migrate m => QualifiedIdentifier -> DropTableCondition -> Bool -> TableAnalysis m -> m (TableAnalysis m)
dropTableIf table cond cascade ta = do
  let
    hasColumn col = fmap (any ((== col) . colName) . tableColumns) <$> analyzeTable ta (convQN table)
    shouldDrop = case cond of
      ColumnExists col -> hasColumn col
      ColumnMissing col -> fmap not <$> hasColumn col

  shouldDrop >>= \case
    Just True -> dropTable table cascade *> getTableAnalysis
    _ -> pure ta

renameColumn :: Migrate m => QualifiedIdentifier -> Identifier -> Identifier -> m ()
renameColumn tableName columnNameFrom columnNameTo = void [traceExecuteQ|
    ALTER TABLE ?tableName RENAME COLUMN ?columnNameFrom TO ?columnNameTo
  |]

dropColumnIfExists :: Migrate m => QualifiedIdentifier -> Identifier -> TableAnalysis m -> m (TableAnalysis m)
dropColumnIfExists table columnFrom ta = do
  maybeTableInfo <- analyzeTable ta (convQN table)
  let columnExists = do
        tableInfo <- maybeTableInfo
        return $ columnFrom `elem` fmap (Identifier . T.pack . colName) (tableColumns tableInfo)
  case columnExists of
    Just True -> dropColumn table columnFrom *> getTableAnalysis
    _ -> pure ta

dropColumn :: Migrate m => QualifiedIdentifier -> Identifier -> m ()
dropColumn tableName columnNameFrom = void [traceExecuteQ|
    ALTER TABLE ?tableName DROP COLUMN ?columnNameFrom
  |]

createSequence :: Migrate m => QualifiedIdentifier -> TableAnalysis m -> m (TableAnalysis m)
createSequence sequenceName ta = do
  void [traceExecuteQ|
      CREATE SEQUENCE IF NOT EXISTS ?sequenceName
    |]
  return ta

renameTableIfExists :: Migrate m => QualifiedIdentifier -> Identifier -> TableAnalysis m -> m (TableAnalysis m)
renameTableIfExists tableFrom tableTo ta = do
  analyzeTable ta (convQN tableFrom) >>= \case
    Nothing -> pure ta
    Just _ -> renameTable tableFrom tableTo *> getTableAnalysis

renameTable :: Migrate m => QualifiedIdentifier -> Identifier -> m ()
renameTable tableNameFrom tableNameTo = void [traceExecuteQ|
    ALTER TABLE ?tableNameFrom RENAME TO ?tableNameTo
  |]

dropTableIfExists :: Migrate m => Bool -> QualifiedIdentifier -> TableAnalysis m -> m (TableAnalysis m)
dropTableIfExists cascade table ta = do
  analyzeTable ta (convQN table) >>= \case
    Nothing -> pure ta
    Just _ -> dropTable table cascade *> getTableAnalysis

extraIndexes :: Migrate m => MonadFail m => m ()
extraIndexes = do
  createIndex (QualifiedIdentifier Nothing "ErrorLog") [Right "started"] "_errorLog_started_idx" Nothing
  createIndex (QualifiedIdentifier Nothing "ErrorLog") [Right "id"] "_errorLog_idWhereStarted_idx" (Just "\"stopped\" IS NULL")
  createIndex (QualifiedIdentifier Nothing "Baker") [Right "publicKeyHash"] "_baker_publicKeyHashWhereNotDeleted_idx" (Just "NOT \"data#deleted\"")

createIndex
  :: Migrate m
  => MonadFail m
  => QualifiedIdentifier
  -> [Either Text Identifier]
  -> Identifier
  -> Maybe Text
  -> m ()
createIndex table@(QualifiedIdentifier tableSchema _tableName) columns indexIdent condition = do
  let indexName = fromIdentifier indexIdent
  -- TODO: this only verifies that the index exists, not that it uses the right columns in the right order.
  -- JOIN pg_catalog.pg_attribute a  ON a.attrelid = t.oid
  --   where a.attnum = ANY(ix.indkey)
  Only needIndex:_ <- [queryQ|
    SELECT count(ix.indexrelid) = 0
    FROM pg_catalog.pg_class t
    JOIN pg_catalog.pg_index ix     ON t.oid = ix.indrelid
    JOIN pg_catalog.pg_class i      ON i.oid = ix.indexrelid
    JOIN pg_catalog.pg_namespace c  ON c.oid = i.relnamespace

    WHERE t.relkind = 'r'
      AND i.relname = ?indexName
      AND c.nspname = COALESCE(?tableSchema, 'public') |]
  case needIndex of
    True -> do
      let sqlCode = "CREATE INDEX " <> quoteNameSql indexIdent
            <> " ON " <> tableSql table
            <> " (" <> T.intercalate ", " (either id quoteNameSql <$> columns) <> ")"
            <> maybe "" (" WHERE " <>) condition
      $(logInfoS) "SQL" (tshow sqlCode) *> void (execute_ $ fromString $ T.unpack sqlCode)
    False -> return ()

quoteNameSql :: Identifier -> Text
quoteNameSql x = "\"" <> fromIdentifier x <> "\""

tableSql :: QualifiedIdentifier -> Text
tableSql (QualifiedIdentifier schema tableName) =
  maybe "" ((<> ".") . quoteNameSql . Identifier) schema
  <> quoteNameSql (Identifier tableName)

dropTable :: Migrate m => QualifiedIdentifier -> Bool -> m ()
dropTable tableName cascade = if cascade
  then void [traceExecuteQ|DROP TABLE ?tableName CASCADE|]
  else void [traceExecuteQ|DROP TABLE ?tableName|]


-- | Move the data into the new tables and then do the "unsafe" column drop.
migrateNodesToSplitTable :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateNodesToSplitTable ta = do
  let table = (Nothing, "Node")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | any ((== "address") . colName) $ tableColumns analyzedTable
      -> do
          void [traceExecuteQ|
              CREATE TABLE "NodeExternal"
                ( "id" INT8 NOT NULL
                , "data#data#address" VARCHAR NOT NULL
                , "data#data#alias" VARCHAR NULL
                , "data#deleted" BOOLEAN NOT NULL
                );
              ALTER TABLE "NodeExternal" ADD CONSTRAINT "NodeExternalId" PRIMARY KEY("id");
              ALTER TABLE "NodeExternal" ADD FOREIGN KEY("id") REFERENCES "Node"("id");
              CREATE TABLE "NodeDetails"
                ( "id" INT8 NOT NULL
                , "data#identity" BYTEA NULL
                , "data#headLevel" INT8 NULL
                , "data#headBlockHash" BYTEA NULL
                , "data#headBlockPred" BYTEA NULL
                , "data#headBlockBakedAt" TIMESTAMP NULL
                , "data#peerCount" INT8 NULL
                , "data#networkStat#totalSent" INT8 NOT NULL
                , "data#networkStat#totalRecv" INT8 NOT NULL
                , "data#networkStat#currentInflow" INT4 NOT NULL
                , "data#networkStat#currentOutflow" INT4 NOT NULL
                , "data#fitness" VARCHAR[] NULL
                , "data#updated" TIMESTAMP NULL
                );
              ALTER TABLE "NodeDetails" ADD CONSTRAINT "NodeDetailsId" PRIMARY KEY("id");
              ALTER TABLE "NodeDetails" ADD FOREIGN KEY("id") REFERENCES "Node"("id");
              INSERT INTO "NodeExternal"
                  ( "id"
                  , "data#data#address"
                  , "data#data#alias"
                  , "data#deleted"
                  )
                  SELECT "id"
                       , "address"
                       , "alias"
                       , "deleted"
                  FROM "Node";
              INSERT INTO "NodeDetails"
                  ( "id"
                  , "data#identity"
                  , "data#headLevel"
                  , "data#headBlockHash"
                  , "data#headBlockPred"
                  , "data#headBlockBakedAt"
                  , "data#peerCount"
                  , "data#networkStat#totalSent"
                  , "data#networkStat#totalRecv"
                  , "data#networkStat#currentInflow"
                  , "data#networkStat#currentOutflow"
                  , "data#fitness"
                  , "data#updated"
                  )
                  SELECT "id"
                       , "identity"
                       , "headLevel"
                       , "headBlockHash"
                       , "headBlockPred"
                       , "headBlockBakedAt"
                       , "peerCount"
                       , "networkStat#totalSent"
                       , "networkStat#totalRecv"
                       , "networkStat#currentInflow"
                       , "networkStat#currentOutflow"
                       , "fitness"
                       , "updated"
                  FROM "Node";
              ALTER TABLE "Node" DROP COLUMN "updated";
              ALTER TABLE "Node" DROP COLUMN "deleted";
              ALTER TABLE "Node" DROP COLUMN "fitness";
              ALTER TABLE "Node" DROP COLUMN "networkStat#currentOutflow";
              ALTER TABLE "Node" DROP COLUMN "networkStat#currentInflow";
              ALTER TABLE "Node" DROP COLUMN "networkStat#totalRecv";
              ALTER TABLE "Node" DROP COLUMN "networkStat#totalSent";
              ALTER TABLE "Node" DROP COLUMN "peerCount";
              ALTER TABLE "Node" DROP COLUMN "headBlockBakedAt";
              ALTER TABLE "Node" DROP COLUMN "headBlockPred";
              ALTER TABLE "Node" DROP COLUMN "headBlockHash";
              ALTER TABLE "Node" DROP COLUMN "headLevel";
              ALTER TABLE "Node" DROP COLUMN "identity";
              ALTER TABLE "Node" DROP COLUMN "alias";
              ALTER TABLE "Node" DROP COLUMN "address";
            |]
          getTableAnalysis
    _ -> pure ta

-- | Move the data into the new tables and then do the "unsafe" column drop.
migrateProcessDataToSplitTable :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateProcessDataToSplitTable ta = do
  let table = (Nothing, "NodeInternal")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | any ((== "data#data#backend") . colName) $ tableColumns analyzedTable
      -> do
          void [traceExecuteQ|
              CREATE TABLE "ProcessData"
                ( "id" INT8 PRIMARY KEY UNIQUE
                , "running" BOOLEAN NOT NULL
                , "state" VARCHAR NOT NULL
                , "updated" TIMESTAMP NULL
                , "backend" INT8 NULL);
              CREATE SEQUENCE "ProcessData_id_seq";
              ALTER TABLE "ProcessData" ALTER COLUMN "id" SET DEFAULT nextval('"ProcessData_id_seq"');
              ALTER SEQUENCE "ProcessData_id_seq" OWNED BY "ProcessData"."id";
              INSERT INTO "ProcessData" ("running", "state", "updated", "backend")
                SELECT FALSE, 'ProcessState_Stopped', NULL, NULL
                FROM "NodeInternal";
              ALTER TABLE "NodeInternal" DROP COLUMN "data#data#backend";
              ALTER TABLE "NodeInternal" DROP COLUMN "data#data#stateUpdated";
              ALTER TABLE "NodeInternal" DROP COLUMN "data#data#state";
              ALTER TABLE "NodeInternal" DROP COLUMN "data#data#running";
              ALTER TABLE "NodeInternal" ADD COLUMN "data#data" INT8 NULL;
              UPDATE "NodeInternal" n SET "data#data" = p."id" FROM "ProcessData" p;
              ALTER TABLE "NodeInternal" ALTER COLUMN "data#data" SET NOT NULL;
              ALTER TABLE "NodeInternal" ADD FOREIGN KEY("data#data") REFERENCES "ProcessData"("id");
            |]
          getTableAnalysis
    _ -> pure ta

migrateNodeDetailsAddSynchronisationThreshold :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateNodeDetailsAddSynchronisationThreshold ta = do
  let table = (Nothing, "NodeDetails")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | all ((/= "data#synchronisationThreshold") . colName) $ tableColumns analyzedTable
      -> do
        void [traceExecuteQ|
          ALTER TABLE "NodeDetails" ADD COLUMN "data#synchronisationThreshold" INT8 NOT NULL DEFAULT 4;
        |]
        getTableAnalysis
    _ -> pure ta

migrateBakerDaemonInternalTable :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateBakerDaemonInternalTable ta = do
  let table = (Nothing, "BakerDaemonInternal")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | not . any ((== "data#data#protocol") . colName) $ tableColumns analyzedTable
      -> do
          void [traceExecuteQ|
              ALTER TABLE "BakerDaemonInternal" ADD COLUMN "data#data#protocol" BYTEA NULL;
              ALTER TABLE "BakerDaemonInternal" ADD COLUMN "data#data#altProtocol" BYTEA NULL;
              ALTER TABLE "BakerDaemonInternal" ADD COLUMN "data#data#altBakerProcessData" INT8 NULL;
              ALTER TABLE "BakerDaemonInternal" ADD COLUMN "data#data#altEndorserProcessData" INT8 NULL;
              CREATE FUNCTION addcols() RETURNS VOID AS $$
              DECLARE
                      abpid integer;
                      aepid integer;
                      bid integer;
              BEGIN
              FOR bid IN SELECT "id" FROM "BakerDaemonInternal" LOOP
                      INSERT INTO "ProcessData" ("running", "state", "updated", "backend")
                        VALUES (FALSE, 'ProcessState_Stopped', NULL, NULL) RETURNING "id" INTO abpid;
                      INSERT INTO "ProcessData" ("running", "state", "updated", "backend")
                        VALUES(FALSE, 'ProcessState_Stopped', NULL, NULL) RETURNING "id" INTO aepid;
                      UPDATE "BakerDaemonInternal"
                        -- Hex value for 'PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP' :: ProtocolHash
                        SET "data#data#protocol" = E'\\x782f0e56d71e26cfe09e2e33eb44b32ad6d467b742bb79d461db2d86af22adad',
                            "data#data#altBakerProcessData" = abpid,
                            "data#data#altEndorserProcessData" = aepid
                        WHERE "id" = bid;
              END LOOP;
              RETURN;
              END;
              $$ LANGUAGE plpgsql;
              SELECT addcols();
              DROP FUNCTION addcols();
              ALTER TABLE "BakerDaemonInternal" ALTER COLUMN "data#data#protocol" SET NOT NULL;
              ALTER TABLE "BakerDaemonInternal" ALTER COLUMN "data#data#altBakerProcessData" SET NOT NULL;
              ALTER TABLE "BakerDaemonInternal" ALTER COLUMN "data#data#altEndorserProcessData" SET NOT NULL;
              ALTER TABLE "BakerDaemonInternal" ADD FOREIGN KEY("data#data#altBakerProcessData") REFERENCES "ProcessData"("id");
              ALTER TABLE "BakerDaemonInternal" ADD FOREIGN KEY("data#data#altEndorserProcessData") REFERENCES "ProcessData"("id");
            |]
          getTableAnalysis
    _ -> pure ta
migrateProcessDataTable :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateProcessDataTable ta = do
  let table = (Nothing, "ProcessData")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | any ((== "running") . colName) $ tableColumns analyzedTable
      -> do
          void [traceExecuteQ|
              ALTER TABLE "ProcessData" ADD COLUMN "control" VARCHAR NULL;
              UPDATE "ProcessData" SET "control" = 'ProcessControl_Stop' WHERE "running" = FALSE;
              UPDATE "ProcessData" SET "control" = 'ProcessControl_Run' WHERE "running" = TRUE;
              ALTER TABLE "ProcessData" DROP COLUMN "running";
              ALTER TABLE "ProcessData" ALTER COLUMN "control" SET NOT NULL;
            |]
          getTableAnalysis
    _ -> pure ta

-- Fix for enhancement to ProcessState
migrateProcessDataTable2 :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
migrateProcessDataTable2 ta = do
  let table = (Nothing, "ProcessData")
  analyzeTable ta table >>= \case
    Just _ -> do
      void [traceExecuteQ|
          UPDATE "ProcessData" SET "state" = 'ProcessState_Stopped' WHERE "state" = 'ProcessState_GeneratingIdentity';
        |]
      getTableAnalysis
    _ -> pure ta


migrateProcessDataTableAddErrorLog :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
migrateProcessDataTableAddErrorLog ta = do
  let table = (Nothing, "ProcessData")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | all ((/= "errorLog") . colName) $ tableColumns analyzedTable -> do
      void [traceExecuteQ|
        ALTER TABLE "ProcessData" ADD COLUMN "errorLog" VARCHAR NULL;
      |]
      getTableAnalysis
    _ -> pure ta

migrateUpstreamVersionTable :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
migrateUpstreamVersionTable ta = do
  let table = (Nothing, "UpstreamVersion")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | all ((/= "dismissed") . colName) $ tableColumns analyzedTable
      -> do
          void [traceExecuteQ|
              ALTER TABLE "UpstreamVersion" ADD COLUMN "dismissed" BOOLEAN NULL;
              UPDATE "UpstreamVersion" SET "dismissed" = FALSE;
              ALTER TABLE "UpstreamVersion" ALTER COLUMN "dismissed" SET NOT NULL;
            |]
          getTableAnalysis
    _ -> pure ta

migrateChainIdToErrorLog :: Migrate m => ChainId -> TableAnalysis m -> m (TableAnalysis m)
migrateChainIdToErrorLog currentChainId ta = do
  let table = (Nothing, "ErrorLogVotingReminder")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | any ((== "chainId") . colName) $ tableColumns analyzedTable
      -> do
          void [traceExecuteQ|
              ALTER TABLE "ErrorLog" ADD COLUMN "chainId" BYTEA NULL;

              UPDATE "ErrorLog"
              SET "chainId" = vr."chainId"
              FROM "ErrorLogVotingReminder" vr
              WHERE id = vr.log;

              UPDATE "ErrorLog"
              SET "chainId" = ?currentChainId
              WHERE "chainId" IS NULL;

              ALTER TABLE "ErrorLog" ALTER COLUMN "chainId" SET NOT NULL;
              ALTER TABLE "ErrorLogVotingReminder" DROP COLUMN "chainId";
            |]
          getTableAnalysis
    _ -> pure ta

migrateErrorLogBakerMissedTimestamp :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateErrorLogBakerMissedTimestamp ta = do
  let table = (Nothing, "ErrorLogBakerMissed")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | not . any ((== "bakeTime") . colName) $ tableColumns analyzedTable
      -> do
          -- Using the ErrorLog.started as Block's timestamp is not correct, but mostly a good approximation
          void [traceExecuteQ|
              ALTER TABLE "ErrorLogBakerMissed" ADD COLUMN "bakeTime" TIMESTAMP WITHOUT TIME ZONE NULL;
              UPDATE "ErrorLogBakerMissed" e SET "bakeTime" = (SELECT started FROM "ErrorLog" l WHERE l.id = e.log);
              ALTER TABLE "ErrorLogBakerMissed" ALTER COLUMN "bakeTime" SET NOT NULL;
            |]
          getTableAnalysis
    _ -> pure ta

-- This is needed to remove the firstBlockHash from the unique constraints
-- Without this the auto migration fails to make the firstBlockHash field 'Maybe'
-- The constraint will be added back by automigrate after altering the columns
migrateProtocolIndexKey :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateProtocolIndexKey ta = do
  let table = (Nothing, "ProtocolIndex")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | maybe False ((==) 3 . length . uniqueDefFields) $ headMay $ tableUniques analyzedTable
      -> do
          void [traceExecuteQ|
              ALTER TABLE "ProtocolIndex" DROP CONSTRAINT "ProtocolIndexKey";
            |]
          getTableAnalysis
    _ -> pure ta

migrateSnapshotMetaControl :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateSnapshotMetaControl ta = do
  let table = (Nothing, "SnapshotMeta")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | not . any ((== "control") . colName) $ tableColumns analyzedTable
      -> do
          void [traceExecuteQ|
              TRUNCATE TABLE "SnapshotMeta";
            |]
          getTableAnalysis
    _ -> pure ta

-- See note below at deleteTzScanPublicNodeConfigs. Similar issue.
deleteObsidianPublicNodeConfigs :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
deleteObsidianPublicNodeConfigs ta = do
  let table = (Nothing, "PublicNodeConfig")
  analyzeTable ta table >>= \case
    Just _ -> do
        void [traceExecuteQ| DELETE FROM "PublicNodeConfig" WHERE "source" = 'PublicNode_Obsidian' |]
        pure ta
    _ -> pure ta


-- See note below deleteTzScanPublicNodeConfigs. Similar issue.
deleteObsidianPublicNodeHeads :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
deleteObsidianPublicNodeHeads ta = do
  let table = (Nothing, "PublicNodeHead")
  analyzeTable ta table >>= \case
    Just _ -> do
        void [traceExecuteQ| DELETE FROM "PublicNodeHead" WHERE "source" = 'PublicNode_Obsidian' |]
        pure ta
    _ -> pure ta

-- Without deleting these entries with TzScan entries, you get a really nasty crash
-- of the ViewSelectorHandler with Prelude.read: no parse
deleteTzScanPublicNodeConfigs :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
deleteTzScanPublicNodeConfigs ta = do
  let table = (Nothing, "PublicNodeConfig")
  analyzeTable ta table >>= \case
    Just _ -> do
      void [traceExecuteQ|
          DELETE FROM "PublicNodeConfig" WHERE "source" = 'PublicNode_TzScan'
        |]
      pure ta
    _ -> pure ta

deleteTzScanPublicNodeHeads :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
deleteTzScanPublicNodeHeads ta = do
  let table = (Nothing, "PublicNodeHead")
  analyzeTable ta table >>= \case
    Just _ -> do
      void [traceExecuteQ|
          DELETE FROM "PublicNodeHead" WHERE "source" = 'PublicNode_TzScan'
        |]
      pure ta
    _ -> pure ta


updateAmendment :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
updateAmendment ta = do
  let table = (Nothing, "Amendment")
  analyzeTable ta table >>= \case
    Just _ -> do
      void [traceExecuteQ|
          UPDATE "Amendment"
          SET "period" = 'VotingPeriodKind_Exploration'
          WHERE "period" = 'VotingPeriodKind_TestingVote';
          UPDATE "Amendment"
          SET "period" = 'VotingPeriodKind_Cooldown'
          WHERE "period" = 'VotingPeriodKind_Testing';
          UPDATE "Amendment"
          SET "period" = 'VotingPeriodKind_Promotion'
          WHERE "period" = 'VotingPeriodKind_PromotionVote';
        |]
      pure ta
    _ -> pure ta

migrateLedgerAccountTable :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateLedgerAccountTable ta = do
  let table = (Nothing, "LedgerAccount")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | any ((== "shouldRegisterFee") . colName) $ tableColumns analyzedTable
      -> do
        void [traceExecuteQ|
            ALTER TABLE "LedgerAccount"
            ADD COLUMN "shouldRegister" BOOLEAN;
            UPDATE "LedgerAccount" SET "shouldRegister" =
              CASE
                WHEN "shouldRegisterFee" IS NULL THEN false
                ELSE true
              END;
            ALTER TABLE "LedgerAccount"
            ALTER COLUMN "shouldRegister" SET NOT NULL,
            DROP COLUMN "shouldRegisterFee";
          |]
        pure ta
    _ -> pure ta

migrateErrorLogBadNodeHeadTable :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateErrorLogBadNodeHeadTable ta = do
  let table = (Nothing, "ErrorLogBadNodeHead")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | any ((== "lca") . colName) $ tableColumns analyzedTable
      -> do
        -- It should be safe to drop existing bad node heads reports.
        -- If the bad node head error was resolved we no longer interested
        -- in its details. If the error still persists, it will be reported
        -- again after the Kiln is restarted and migration is applied.
        -- Additionally, some of the errors' reports can be false positive.
        void
          [traceExecuteQ|
            DELETE FROM "ErrorLogBadNodeHead";
            ALTER TABLE "ErrorLogBadNodeHead"
            DROP COLUMN "lca",
            ADD COLUMN "bootstrapped" BOOLEAN NOT NULL,
            ADD COLUMN "chainStatus" INT8 NOT NULL;
          |]
        pure ta
    _ -> pure ta

migrateBakerRightsCycleProgressTable :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateBakerRightsCycleProgressTable ta = do
  let table = (Nothing, "BakerRightsCycleProgress")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | any ((== "branch") . colName) $ tableColumns analyzedTable
      -> do
        void
          [traceExecuteQ|
            -- It's easier to drop all the existing data than
            -- adapt it to the changed constraint that doesn't have 'branch' column.
            --
            -- Purge data from table that depend on the 'BakerRightsCycleProgress'.
            DELETE FROM "BakerRight";
            -- The progress stored in 'BakerRightsCycleProgress' affects the data
            -- that will be added to the tables below, so cleaning them as well
            -- to prevent constraints violations.
            DELETE FROM "CacheBakingRights";
            DELETE FROM "CacheEndorsingRights";
            -- Drop the data from the table itself.
            DELETE FROM "BakerRightsCycleProgress";
          |]
        void
          [traceExecuteQ|
            ALTER TABLE "BakerRightsCycleProgress"
            DROP COLUMN "branch";
          |]
        pure ta
    _ -> pure ta

removeArchivalNodeFromTables :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
removeArchivalNodeFromTables ta = do
  analyzeTable ta (Nothing, "PublicNodeConfig") >>= \case
    Just _
      -> do
        void
          [traceExecuteQ|
            DELETE FROM "PublicNodeConfig" where source = 'PublicNode_Archival';
          |]
    _ -> pure ()
  analyzeTable ta (Nothing, "PublicNodeHead") >>= \case
    Just _
      -> do
        void
          [traceExecuteQ|
            DELETE FROM "PublicNodeHead" where source = 'PublicNode_Archival';
          |]
    _ -> pure ()
  pure ta

migrateRawCacheEntryTable :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateRawCacheEntryTable ta = do
  let table = (Nothing, "RawCacheEntry")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | not $ any ((== "addedAt") . colName) $ tableColumns analyzedTable
      -> do
        void
          [traceExecuteQ|
            ALTER TABLE "RawCacheEntry"
            ADD COLUMN "addedAt" timestamp NOT NULL DEFAULT now();
          |]
        pure ta
    _ -> pure ta

migratePeriodTestingTable :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migratePeriodTestingTable ta = do
  let table = (Nothing, "PeriodTesting")
  analyzeTable ta table >>= \case
    Just analyzedTable | any ((== "status") . colName) $ tableColumns analyzedTable
      -> do
        void
          [traceExecuteQ|
            ALTER TABLE "PeriodTesting"
            DROP COLUMN "status",
            DROP COLUMN "startingLevel",
            DROP COLUMN "testChainId";
          |]
        pure ta
    _ -> pure ta

migrateErrorLogBakerLedgerDisconnected :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateErrorLogBakerLedgerDisconnected ta = do
  let table = (Nothing, "ErrorLogBakerLedgerDisconnected")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | not . any ((== "isWrongApp") . colName) $ tableColumns analyzedTable
      -> do
          void [traceExecuteQ|
              ALTER TABLE "ErrorLogBakerLedgerDisconnected" ADD COLUMN "isWrongApp" BOOLEAN NULL;
              UPDATE "ErrorLogBakerLedgerDisconnected" SET "isWrongApp" = false;
              ALTER TABLE "ErrorLogBakerLedgerDisconnected" ALTER COLUMN "isWrongApp" SET NOT NULL;
            |]
          getTableAnalysis
    _ -> pure ta

removeUnusedProtocolIndexColumns :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
removeUnusedProtocolIndexColumns ta = do
  let columns =
        [ "constants#minProposalQuorum", "constants#quorumMax", "constants#quorumMin"
        , "constants#initialEndorsers", "constants#delayPerMissingEndorsement"
        , "constants#hardStorageLimitPerOperation", "constants#costPerByte"
        , "constants#endorsementSecurityDeposit", "constants#blockSecurityDeposit"
        , "constants#originationBurn", "constants#seedNonceRevelationTip"
        , "constants#michelsonMaximumTypeSize", "constants#proofOfWorkThreshold"
        , "constants#hardGasLimitPerBlock", "constants#hardGasLimitPerOperation"
        , "constants#endorsersPerBlock", "constants#blocksPerRollSnapshot"
        , "constants#blocksPerCommitment", "constants#maxOperationDataLength"
        , "constants#maxRevelationsPerBlock", "constants#nonceLength"
        , "constants#proofOfWorkNonceSize", "constants#originationSize"
        ]
  forM_ columns $ \column -> do
    dropColumnIfExists (QualifiedIdentifier Nothing "ProtocolIndex") column ta
  getTableAnalysis

removeNodeSavePointInfo :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
removeNodeSavePointInfo ta = do
  let columns = ["data#savePointUpdated", "data#savePoint"]
  forM_ columns $ \column -> do
    dropColumnIfExists (QualifiedIdentifier Nothing "NodeDetails") column ta
  getTableAnalysis

migrateAccusationTable :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateAccusationTable ta = do
  let table = (Nothing, "Accusation")
  analyzeTable ta table >>= \case
    Just analyzedTable | any ((== "isBake") . colName) $ tableColumns analyzedTable -> do
      void [traceExecuteQ|
        ALTER TABLE "Accusation" ADD COLUMN "accusationType" VARCHAR NULL;
        UPDATE "Accusation" SET "accusationType" =
          CASE
            WHEN "isBake" THEN 'AccusationType_DoubleBake' ELSE 'AccusationType_DoubleEndorsement'
          END;
        ALTER TABLE "Accusation" ALTER COLUMN "accusationType" SET NOT NULL;
        ALTER TABLE "Accusation" DROP COLUMN "isBake";
      |]
      getTableAnalysis
    _ -> pure ta

migrateErrorLogBakerAccusedTable :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateErrorLogBakerAccusedTable ta = do
  let table = (Nothing, "ErrorLogBakerAccused")
  analyzeTable ta table >>= \case
    Just analyzedTable | any ((== "right") . colName) $ tableColumns analyzedTable -> do
      void [traceExecuteQ|
        ALTER TABLE "ErrorLogBakerAccused" ADD COLUMN "accusationType" VARCHAR NULL;
        UPDATE "ErrorLogBakerAccused" SET "accusationType" = 'AccusationType_DoubleBake' where "right" = 'RightKind_Baking';
        UPDATE "ErrorLogBakerAccused" SET "accusationType" = 'AccusationType_DoubleEndorsement' where "right" = 'RightKind_Endorsing';
        ALTER TABLE "ErrorLogBakerAccused" ALTER COLUMN "accusationType" SET NOT NULL;
        ALTER TABLE "ErrorLogBakerAccused" DROP COLUMN "right";
      |]
      getTableAnalysis
    _ -> pure ta

migrateBakerDetailsAddMissedRigtsInRow :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateBakerDetailsAddMissedRigtsInRow ta = do
  let table = (Nothing, "BakerDetails")
  analyzeTable ta table >>= \case
    Just analyzedTable | all ((/= "missedRightsInRow") . colName) $ tableColumns analyzedTable -> do
      void [traceExecuteQ|
        ALTER TABLE "BakerDetails" ADD COLUMN "missedRightsInRow" INT NOT NULL DEFAULT 0;
      |]
      getTableAnalysis
    _ -> pure ta

migrateCacheBakingRights :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateCacheBakingRights ta = do
  let table = (Nothing,  "CacheBakingRights")
  analyzeTable ta table >>= \case
    Just analyzedTable | all ((/= "round") . colName) $ tableColumns analyzedTable -> do
      void [traceExecuteQ|
        DELETE FROM "CacheBakingRights";
        ALTER TABLE "CacheBakingRights" DROP CONSTRAINT "CacheBakingRights_context";
        ALTER TABLE "CacheBakingRights" ADD COLUMN "round" INT NOT NULL;
        ALTER TABLE "CacheBakingRights" DROP COLUMN "result";
        ALTER TABLE "CacheBakingRights" DROP COLUMN "context";
        ALTER TABLE "CacheBakingRights" ADD COLUMN "delegate" VARCHAR NOT NULL;
        ALTER TABLE "CacheBakingRights" ADD COLUMN "estimatedTime" TIMESTAMP WITHOUT TIME ZONE NULL;
      |]
      getTableAnalysis
    _ -> pure ta

migrateCacheEndorsingRightsDropContext :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
migrateCacheEndorsingRightsDropContext ta = do
  let table = (Nothing,  "CacheEndorsingRights")
  analyzeTable ta table >>= \case
    Just analyzedTable | any ((== "context") . colName) $ tableColumns analyzedTable -> do
      void [traceExecuteQ|
        DELETE FROM "CacheEndorsingRights";
        ALTER TABLE "CacheEndorsingRights" DROP CONSTRAINT "CacheEndorsingRights_context";
        ALTER TABLE "CacheEndorsingRights" DROP COLUMN "context";
      |]
      getTableAnalysis
    _ -> pure ta

removeErrorLogBakerAccusedForeignKey :: Migrate m => TableAnalysis m -> m (TableAnalysis m)
removeErrorLogBakerAccusedForeignKey ta = do
  let table = (Nothing, "ErrorLogBakerAccused")
  analyzeTable ta table >>= \case
    Just analyzedTable | any ((== "op#hash") . colName) $ tableColumns analyzedTable -> do
      void [traceExecuteQ|
          ALTER TABLE "ErrorLogBakerAccused" DROP CONSTRAINT "ErrorLogBakerAccused_op#hash_fkey";
        |]
      getTableAnalysis
    _ -> pure ta
