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
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, execute_, queryQ, Only(..))
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
  >=> renameTableIfExists (Nothing, "Delegate") "Baker"
  >=> renameColumnIfExists (Nothing, "PendingReward") "delegate" "baker"

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
  let sql = "ALTER TABLE " <> maybe "" (\x -> "\"" <> x <> "\".") schema <> "\"" <> tableName <> "\" RENAME COLUMN \"" <> columnNameFrom <> "\" TO \"" <> columnNameTo <> "\""
  $(logInfoS) "SQL" (tshow sql) *> void (execute_ $ fromString sql)

renameTableIfExists :: (Migrate m) => QualifiedName -> String -> TableAnalysis m -> m (TableAnalysis m)
renameTableIfExists tableFrom tableTo ta = do
  analyzeTable ta tableFrom >>= \case
    Nothing -> pure ta
    Just _ -> renameTable tableFrom tableTo *> getTableAnalysis

renameTable :: (Migrate m) => QualifiedName -> String -> m ()
renameTable (schema, tableNameFrom) tableNameTo = do
  let sql = "ALTER TABLE " <> maybe "" (\x -> "\"" <> x <> "\".") schema <> "\"" <> tableNameFrom <> "\" RENAME TO \"" <> tableNameTo <> "\""
  $(logInfoS) "SQL" (tshow sql) *> void (execute_ $ fromString sql)

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
          let sql = "CREATE INDEX " <> quoteNameSql indexName
                 <> " ON " <> tableSql table
                 <> " (" <> intercalate ", " (quoteNameSql <$> columns) <> ")"
          $(logInfoS) "SQL" (tshow sql) *> void (execute_ $ fromString sql)
    False -> return ()

quoteNameSql :: String -> String
quoteNameSql x = "\"" <> x <> "\""

tableSql :: QualifiedName -> String
tableSql (schema, tableName) = maybe "" ((<> ".") . quoteNameSql) schema <> "\"" <> tableName <> "\""

dropTable :: (Migrate m) => QualifiedName -> m ()
dropTable table = do
  let sql = "DROP TABLE " <> tableSql table
  $(logInfoS) "SQL" (tshow sql) *> void (execute_ $ fromString sql)
