{-# LANGUAGE QuasiQuotes #-}

module Backend.BalanceTracking where

import Control.Monad
import Data.List
import Data.Ord
import Data.Word
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.Schema

import Backend.Schema
import Common.Schema

getMaxLevel :: (Monad m, PostgresRaw m) => m (Maybe Word64)
getMaxLevel = do
  rs <- [queryQ| SELECT max(n."headLevel") FROM "Node" n WHERE n."headLevel" IS NOT NULL |]
  return $ case rs of
    [] -> Nothing
    (Only l:_) -> l

getSummaryReport :: (PersistBackend m) => m (Maybe (Report, Int))
getSummaryReport = do
  reports <- project (BakerDaemonInfo_dataField ~> BakerDaemonInfoData_reportSelector) CondEmpty
  waiting <- count (BakerDaemonExternal_dataField ~> DeletableRow_deletedSelector ==. False)
  let
      aggReport = case map (cropBaked . dropSeen . unJson) reports of
        [] -> Nothing
        (x:xs) -> Just $ foldr (<>) x xs
      cropBaked r = r { _report_baked = take 20 (sortBy (flip (comparing _event_time)) (_report_baked r)) }
      dropSeen r = r { _report_seen = [] }
  return (liftM2 (,) (fmap (cropBaked . dropSeen) aggReport) (pure waiting))
