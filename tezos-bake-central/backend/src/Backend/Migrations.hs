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
import Data.List (intercalate)
import Data.String (fromString)
import Database.Groundhog.Core
import Database.Groundhog.Generic (runMigration)
import Database.Groundhog.Generic.Migration hiding (migrateSchema)
import Rhyolite.Backend.Account (migrateAccount)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, execute_, queryQ, sql, Only(..))
import Rhyolite.Backend.EmailWorker (migrateQueuedEmail)

import ExtraPrelude

type Migrate m = (PersistBackend m, SchemaAnalyzer m, PostgresRaw m, MonadLogger m, MonadIO m)

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
  >=> dropTableIfExists (Nothing, "ErrorLogUpgradeNotice")
  >=> dropTableIfExists (Nothing, "PendingReward")
  >=> dropColumnIfExists (Nothing, "Delegate") "id" -- No, it's not possible to promote the existing unique key to the primary key.  oh well.
  >=> renameColumnIfExists (Nothing, "Delegate") "deleted" "data#deleted"
  >=> renameColumnIfExists (Nothing, "Delegate") "alias" "data#data#alias"
  >=> renameTableIfExists (Nothing, "Delegate") "Baker"
  >=> createNodeDetailsTable
  >=> createNodeExternalTable
  >=> migrateNodesToSplitTable

migrateParameters :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
migrateParameters ta = do
  let table = (Nothing, "Parameters")
  analyzedTable' <- analyzeTable ta table
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
  let table = (Nothing, "PublicNodeHead")
  hasHeadBlockHash <- fmap (any ((== "headBlock#hash") . colName) . tableColumns) <$> analyzeTable ta table
  case hasHeadBlockHash of
    Nothing -> pure ta
    Just False -> dropTable table *> getTableAnalysis
    Just True -> pure ta

renameColumnIfExists :: (Migrate m) => QualifiedName -> String -> String -> TableAnalysis m -> m (TableAnalysis m)
renameColumnIfExists table columnFrom columnTo ta = do
  maybeTableInfo <- analyzeTable ta table
  let columnExists = do
        tableInfo <- maybeTableInfo
        return $ columnFrom `elem` fmap colName (tableColumns tableInfo)
  case columnExists of
    Just True -> renameColumn table columnFrom columnTo *> getTableAnalysis
    _ -> pure ta

renameColumn :: (Migrate m) => QualifiedName -> String -> String -> m ()
renameColumn (schema, tableName) columnNameFrom columnNameTo = do
  let sqlCode = "ALTER TABLE " <> maybe "" (\x -> "\"" <> x <> "\".") schema <> "\"" <> tableName <> "\" RENAME COLUMN \"" <> columnNameFrom <> "\" TO \"" <> columnNameTo <> "\""
  $(logInfoS) "SQL" (tshow sqlCode) *> void (execute_ $ fromString sqlCode)

dropColumnIfExists :: (Migrate m) => QualifiedName -> String -> TableAnalysis m -> m (TableAnalysis m)
dropColumnIfExists table columnFrom ta = do
  maybeTableInfo <- analyzeTable ta table
  let columnExists = do
        tableInfo <- maybeTableInfo
        return $ columnFrom `elem` fmap colName (tableColumns tableInfo)
  case columnExists of
    Just True -> dropColumn table columnFrom *> getTableAnalysis
    _ -> pure ta

dropColumn :: (Migrate m) => QualifiedName -> String -> m ()
dropColumn (schema, tableName) columnNameFrom = do
  let sqlCode = "ALTER TABLE " <> maybe "" (\x -> "\"" <> x <> "\".") schema <> "\"" <> tableName <> "\" DROP COLUMN \"" <> columnNameFrom <> "\""
  $(logInfoS) "SQL" (tshow sqlCode) *> void (execute_ $ fromString sqlCode)

renameTableIfExists :: (Migrate m) => QualifiedName -> String -> TableAnalysis m -> m (TableAnalysis m)
renameTableIfExists tableFrom tableTo ta = do
  analyzeTable ta tableFrom >>= \case
    Nothing -> pure ta
    Just _ -> renameTable tableFrom tableTo *> getTableAnalysis

renameTable :: (Migrate m) => QualifiedName -> String -> m ()
renameTable (schema, tableNameFrom) tableNameTo = do
  let sqlCode = "ALTER TABLE " <> maybe "" (\x -> "\"" <> x <> "\".") schema <> "\"" <> tableNameFrom <> "\" RENAME TO \"" <> tableNameTo <> "\""
  $(logInfoS) "SQL" (tshow sqlCode) *> void (execute_ $ fromString sqlCode)

dropTableIfExists :: (Migrate m) => QualifiedName -> TableAnalysis m -> m (TableAnalysis m)
dropTableIfExists table ta = do
  analyzeTable ta table >>= \case
    Nothing -> pure ta
    Just _ -> dropTable table *> getTableAnalysis



extraIndexes :: Migrate m => m ()
extraIndexes = do
  createIndex (Nothing, "ErrorLog") ["started"] "_errorLog_started_idx"


createIndex :: (Migrate m) => QualifiedName -> [String] -> String -> m ()
createIndex table@(tableSchema, _tableName) columns indexName = do
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
          let sqlCode = "CREATE INDEX " <> quoteNameSql indexName
                 <> " ON " <> tableSql table
                 <> " (" <> intercalate ", " (quoteNameSql <$> columns) <> ")"
          $(logInfoS) "SQL" (tshow sqlCode) *> void (execute_ $ fromString sqlCode)
    False -> return ()

quoteNameSql :: String -> String
quoteNameSql x = "\"" <> x <> "\""

tableSql :: QualifiedName -> String
tableSql (schema, tableName) = maybe "" ((<> ".") . quoteNameSql) schema <> "\"" <> tableName <> "\""

dropTable :: (Migrate m) => QualifiedName -> m ()
dropTable table = do
  let sqlCode = "DROP TABLE " <> tableSql table
  $(logInfoS) "SQL" (tshow sqlCode) *> void (execute_ $ fromString sqlCode)

-- | The auto migrate does this find, but we need this to happen first, so I cut
-- and pasted.
createNodeDetailsTable :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
createNodeDetailsTable ta = do
  let table = (Nothing, "NodeDetails")
  analyzeTable ta table >>= \case
    Just _analyzedTable -> pure ta
    Nothing -> do
      let sqlCode = [sql|
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
          |]
      $(logInfoS) "SQL" "" {-(tshow sql)-} *> void (execute_ sqlCode)
      getTableAnalysis

createNodeExternalTable :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
createNodeExternalTable ta = do
  let table = (Nothing, "NodeExternal")
  analyzeTable ta table >>= \case
    Just _analyzedTable -> pure ta
    Nothing -> do
      let sqlCode = [sql|
            CREATE TABLE "NodeExternal"
              ( "id" INT8 NOT NULL
              , "data#data#address" VARCHAR NOT NULL
              , "data#data#alias" VARCHAR NULL
              , "data#deleted" BOOLEAN NOT NULL
              );
            ALTER TABLE "NodeExternal" ADD CONSTRAINT "NodeExternalId" PRIMARY KEY("id");
            ALTER TABLE "NodeExternal" ADD FOREIGN KEY("id") REFERENCES "Node"("id");
            |]
      $(logInfoS) "SQL" "" {-(tshow sql)-} *> void (execute_ sqlCode)
      getTableAnalysis

-- | Move the data into the new tables and then do the "unsafe" column drop.
migrateNodesToSplitTable :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
migrateNodesToSplitTable ta = do
  let table = (Nothing, "Node")
  analyzeTable ta table >>= \case
    Just analyzedTable
      | any ((== "address") . colName) $ tableColumns analyzedTable
      -> do
          let
            sqlCode = [sql|
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
          $(logInfoS) "SQL" "" {-(tshow sql)-} *> void (execute_ sqlCode)
          getTableAnalysis
    _ -> pure ta
