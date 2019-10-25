{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Migrations where

import Backend.Schema (migrateSchema)
import Control.Monad.Logger (MonadLogger, logInfoS)
import qualified Data.Text as T
import Data.String (fromString)
import Database.Groundhog.Core
import Database.Groundhog.Generic (runMigration)
import Database.Groundhog.Generic.Migration hiding (migrateSchema)
import Database.PostgreSQL.Simple.Types (Identifier (..), QualifiedIdentifier (..))
import Rhyolite.Backend.Account (migrateAccount)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, execute_, queryQ, traceExecuteQ, Only(..))
import Rhyolite.Backend.EmailWorker (migrateQueuedEmail)
import Tezos.Types (ChainId)

import ExtraPrelude

type Migrate m = (PersistBackend m, SchemaAnalyzer m, PostgresRaw m, MonadLogger m, MonadIO m)

convQN :: QualifiedIdentifier -> QualifiedName
convQN (QualifiedIdentifier a b) = (T.unpack <$> a, T.unpack b)

migrateKiln :: Migrate m => ChainId -> m ()
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
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "Delegate") "id" -- No, it's not possible to promote the existing unique key to the primary key.  oh well.
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerDeactivated") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerDeactivationRisk") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerMissed") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerNoHeartbeat") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogInaccessibleNode") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogInaccessibleNode") "alias"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogInaccessibleNode") "address"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogMultipleBakersForSameBaker") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogNetworkUpdate") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogNodeInvalidPeerCount") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogNodeWrongChain") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogNodeWrongChain") "alias"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogNodeWrongChain") "address"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBadNodeHead") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "LedgerAccount") "checkIfRegistered"
  >=> renameColumnIfExists (QualifiedIdentifier Nothing "Delegate") "deleted" "data#deleted"
  >=> renameColumnIfExists (QualifiedIdentifier Nothing "Delegate") "alias" "data#data#alias"
  >=> renameTableIfExists (QualifiedIdentifier Nothing "Delegate") "Baker"
  >=> migrateNodesToSplitTable
  >=> migrateProcessDataToSplitTable
  >=> createSequence (QualifiedIdentifier Nothing "NodeInternal_pid")
  >=> createSequence (QualifiedIdentifier Nothing "ProcessLockUniqueId")
  >=> migrateBakerDaemonInternalTable
  >=> migrateProcessDataTable
  >=> migrateProcessDataTable2
  >=> migrateUpstreamVersionTable
  >=> dropTableIf (QualifiedIdentifier Nothing "PeriodTesting") (ColumnExists "votingPeriod") False
  >=> dropTableIf (QualifiedIdentifier Nothing "PeriodTestingVote") (ColumnExists "periodVote#votingPeriod") False
  >=> dropTableIf (QualifiedIdentifier Nothing "PeriodPromotionVote") (ColumnExists "periodVote#votingPeriod") False
  >=> dropTableIf (QualifiedIdentifier Nothing "PeriodProposal") (ColumnMissing "id") False
  >=> migrateChainIdToErrorLog chainId
  >=> dropTableIfExists False (QualifiedIdentifier Nothing "CachedProtocolConstants")
  >=> dropTableIfExists False (QualifiedIdentifier Nothing "Parameters")
  >=> migrateErrorLogBakerMissedTimestamp

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

extraIndexes :: Migrate m => m ()
extraIndexes = do
  createIndex (QualifiedIdentifier Nothing "ErrorLog") [Right "started"] "_errorLog_started_idx" Nothing
  createIndex (QualifiedIdentifier Nothing "ErrorLog") [Right "id"] "_errorLog_idWhereStarted_idx" (Just "\"stopped\" IS NULL")
  createIndex (QualifiedIdentifier Nothing "Baker") [Right "publicKeyHash"] "_baker_publicKeyHashWhereNotDeleted_idx" (Just "NOT \"data#deleted\"")
  createIndex (QualifiedIdentifier Nothing "BlockTodo") [Right "level"] "_blockTodo_levelWhereNotParsed_idx" (Just "NOT \"parsedParent\" OR NOT \"parsedAccusations\"")

createIndex
  :: Migrate m
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
