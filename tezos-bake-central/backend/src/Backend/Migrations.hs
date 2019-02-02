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

import ExtraPrelude

type Migrate m = (PersistBackend m, SchemaAnalyzer m, PostgresRaw m, MonadLogger m, MonadIO m)

convQN :: QualifiedIdentifier -> QualifiedName
convQN (QualifiedIdentifier a b) = (T.unpack <$> a, T.unpack b)

migrateKiln :: (Migrate m) => m ()
migrateKiln = (getTableAnalysis >>= preMigrate >>= autoMigrate) *> extraIndexes

autoMigrate :: (Migrate m) => TableAnalysis m -> m ()
autoMigrate tableAnalysis = runMigration $ do
  migrateAccount tableAnalysis
  migrateQueuedEmail tableAnalysis
  migrateSchema tableAnalysis

preMigrate :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
preMigrate =
      migrateParameters
  >=> migratePublicNodeHead
  >=> dropTableIfExists (QualifiedIdentifier Nothing "ErrorLogUpgradeNotice")
  >=> dropTableIfExists (QualifiedIdentifier Nothing "PendingReward")
  >=> dropTableIfExists (QualifiedIdentifier Nothing "ClientInfo")
  >=> dropTableIfExists (QualifiedIdentifier Nothing "Client")
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "Delegate") "id" -- No, it's not possible to promote the existing unique key to the primary key.  oh well.
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerDeactivated") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerDeactivationRisk") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerMissed") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBakerNoHeartbeat") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogInaccessibleNode") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogMultipleBakersForSameBaker") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogNetworkUpdate") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogNodeInvalidPeerCount") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogNodeWrongChain") "id"
  >=> dropColumnIfExists (QualifiedIdentifier Nothing "ErrorLogBadNodeHead") "id"
  >=> renameColumnIfExists (QualifiedIdentifier Nothing "Delegate") "deleted" "data#deleted"
  >=> renameColumnIfExists (QualifiedIdentifier Nothing "Delegate") "alias" "data#data#alias"
  >=> renameTableIfExists (QualifiedIdentifier Nothing "Delegate") "Baker"
  >=> migrateNodesToSplitTable
  >=> createSequence (QualifiedIdentifier Nothing "NodeInternal_pid")

migrateParameters :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
migrateParameters ta = do
  let table = QualifiedIdentifier Nothing "Parameters"
  analyzedTable' <- analyzeTable ta (convQN table)
  let
    hasHeadTimestamp = any ((== "headTimestamp") . colName) . tableColumns
    hasOriginationSize = any ((== "protoInfo#originationSize") . colName) . tableColumns
  case analyzedTable' of
    Nothing -> pure ta
    Just analyzedTable -> if hasHeadTimestamp analyzedTable || not (hasOriginationSize analyzedTable)
      then dropTable table *> getTableAnalysis
      else pure ta

migratePublicNodeHead :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
migratePublicNodeHead ta = do
  let table = QualifiedIdentifier Nothing "PublicNodeHead"
  hasHeadBlockHash <- fmap (any ((== "headBlock#hash") . colName) . tableColumns) <$> analyzeTable ta (convQN table)
  case hasHeadBlockHash of
    Nothing -> pure ta
    Just False -> dropTable table *> getTableAnalysis
    Just True -> pure ta

renameColumnIfExists :: (Migrate m) => QualifiedIdentifier -> Identifier -> Identifier -> TableAnalysis m -> m (TableAnalysis m)
renameColumnIfExists table columnFrom columnTo ta = do
  maybeTableInfo <- analyzeTable ta (convQN table)
  let columnExists = do
        tableInfo <- maybeTableInfo
        return $ columnFrom `elem` fmap (Identifier . T.pack . colName) (tableColumns tableInfo)
  case columnExists of
    Just True -> renameColumn table columnFrom columnTo *> getTableAnalysis
    _ -> pure ta

renameColumn :: (Migrate m) => QualifiedIdentifier -> Identifier -> Identifier -> m ()
renameColumn tableName columnNameFrom columnNameTo = void [traceExecuteQ|
    ALTER TABLE ?tableName RENAME COLUMN ?columnNameFrom TO ?columnNameTo
  |]

dropColumnIfExists :: (Migrate m) => QualifiedIdentifier -> Identifier -> TableAnalysis m -> m (TableAnalysis m)
dropColumnIfExists table columnFrom ta = do
  maybeTableInfo <- analyzeTable ta (convQN table)
  let columnExists = do
        tableInfo <- maybeTableInfo
        return $ columnFrom `elem` fmap (Identifier . T.pack . colName) (tableColumns tableInfo)
  case columnExists of
    Just True -> dropColumn table columnFrom *> getTableAnalysis
    _ -> pure ta

dropColumn :: (Migrate m) => QualifiedIdentifier -> Identifier -> m ()
dropColumn tableName columnNameFrom = void [traceExecuteQ|
    ALTER TABLE ?tableName DROP COLUMN ?columnNameFrom
  |]

createSequence :: (Migrate m) => QualifiedIdentifier -> TableAnalysis m -> m (TableAnalysis m)
createSequence sequenceName ta = do
  void [traceExecuteQ|
      CREATE SEQUENCE IF NOT EXISTS ?sequenceName
    |]
  return ta

renameTableIfExists :: (Migrate m) => QualifiedIdentifier -> Identifier -> TableAnalysis m -> m (TableAnalysis m)
renameTableIfExists tableFrom tableTo ta = do
  analyzeTable ta (convQN tableFrom) >>= \case
    Nothing -> pure ta
    Just _ -> renameTable tableFrom tableTo *> getTableAnalysis

renameTable :: (Migrate m) => QualifiedIdentifier -> Identifier -> m ()
renameTable tableNameFrom tableNameTo = void [traceExecuteQ|
    ALTER TABLE ?tableNameFrom RENAME TO ?tableNameTo
  |]

dropTableIfExists :: (Migrate m) => QualifiedIdentifier -> TableAnalysis m -> m (TableAnalysis m)
dropTableIfExists table ta = do
  analyzeTable ta (convQN table) >>= \case
    Nothing -> pure ta
    Just _ -> dropTable table *> getTableAnalysis

extraIndexes :: Migrate m => m ()
extraIndexes = do
  createIndex (QualifiedIdentifier Nothing "ErrorLog") [Right "started"] "_errorLog_started_idx" Nothing
  createIndex (QualifiedIdentifier Nothing "ErrorLog") [Right "id"] "_errorLog_idWhereStarted_idx" (Just "\"stopped\" IS NULL")
  createIndex (QualifiedIdentifier Nothing "Baker") [Right "publicKeyHash"] "_baker_publicKeyHashWhereNotDeleted_idx" (Just "NOT \"data#deleted\"")

createIndex
  :: (Migrate m)
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

dropTable :: (Migrate m) => QualifiedIdentifier -> m ()
dropTable tableName = void [traceExecuteQ|DROP TABLE ?tableName|]


-- | Move the data into the new tables and then do the "unsafe" column drop.
migrateNodesToSplitTable :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
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
