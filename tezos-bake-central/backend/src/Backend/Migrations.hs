{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Migrations where

import Backend.Schema (migrateSchema)
import Control.Monad.Logger (MonadLogger, logInfoS)
import Data.String (fromString)
import Database.Groundhog.Core
import Database.Groundhog.Generic (runMigration)
import Database.Groundhog.Generic.Migration hiding (migrateSchema)
import Rhyolite.Backend.Account (migrateAccount)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, execute_)
import Rhyolite.Backend.EmailWorker (migrateQueuedEmail)

import ExtraPrelude

type Migrate m = (PersistBackend m, SchemaAnalyzer m, PostgresRaw m, MonadLogger m, MonadIO m)

migrateKiln :: (Migrate m) => m ()
migrateKiln = autoMigrate =<< preMigrate =<< getTableAnalysis

autoMigrate :: (Migrate m) => TableAnalysis m -> m ()
autoMigrate tableAnalysis = runMigration $ do
  migrateAccount tableAnalysis
  migrateQueuedEmail tableAnalysis
  migrateSchema tableAnalysis

preMigrate :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
preMigrate ta = do
  migrateParameters ta

migrateParameters :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
migrateParameters ta = do
  let table = (Nothing, "Parameters")
  hasHeadTimestamp <- fmap (any ((== "headTimestamp") . colName) . tableColumns) <$> analyzeTable ta table
  case hasHeadTimestamp of
    Nothing -> pure ta
    Just False -> pure ta
    Just True -> dropTable table *> getTableAnalysis

migratePublicNodeHead :: (Migrate m) => TableAnalysis m -> m (TableAnalysis m)
migratePublicNodeHead ta = do
  let table = (Nothing, "PublicNodeHead")
  hasHeadBlockHash <- fmap (any ((== "headBlock#hash") . colName) . tableColumns) <$> analyzeTable ta table
  case hasHeadBlockHash of
    Nothing -> pure ta
    Just False -> dropTable table *> getTableAnalysis
    Just True -> pure ta

dropTable :: (Migrate m) => QualifiedName -> m ()
dropTable (schema, tableName) = do
  let sql = "DROP TABLE " <> maybe "" (\x -> "\"" <> x <> "\".") schema <> "\"" <> tableName <> "\""
  $(logInfoS) "SQL" (tshow sql) *> void (execute_ $ fromString sql)
