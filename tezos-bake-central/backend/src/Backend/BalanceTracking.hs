{-# LANGUAGE QuasiQuotes #-}
module Backend.BalanceTracking where

import Control.Monad
import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as Map
import Data.Fixed
import Data.List
import Data.Ord
import Data.Semigroup
import Data.Word
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.Schema

import Backend.Schema ()
import Common.Schema

-- NB: This eventually needs to change, we can't really be getting an unbounded amount of information. Our viewselector needs to become more specific.
getAllRewards :: (PersistBackend m) => a -> m (AppendMap (Id Client) (First (AppendMap Word64 Micro), a))
getAllRewards a = do
  rewards <- selectAll -- PendingReward
  let rewardMap' = Map.fromListWith (Map.unionWith (+))
        [(_pendingReward_client r, Map.singleton (_pendingReward_level r) (_pendingReward_amount r)) | (_,r) <- rewards]
      rewardMap = fmap (\x -> (First x, a)) rewardMap'
  return rewardMap

getMaxLevel :: (Monad m, PostgresRaw m) => m (Maybe Word64)
getMaxLevel = do
  rs <- [queryQ| SELECT max(n."headLevel") FROM "Node" n WHERE n."headLevel" IS NOT NULL |]
  return $ case rs of
    [] -> Nothing
    (Only l:_) -> l

getSummaryReport :: (PersistBackend m, PostgresRaw m) => m (Maybe (Report, Int))
getSummaryReport = do
  cis <- selectAll
  ns <- [queryQ| SELECT count(c.id) FROM "Client" c LEFT JOIN "ClientInfo" i ON c.id = i.client WHERE i.id IS NULL |]
  let waiting = case ns of
        (Only n:_) -> Just n
        _ -> Nothing
      aggReport = case map (\(_, ci) -> cropBaked . dropSeen $ unJson (_clientInfo_report ci)) cis of
        [] -> Nothing
        (x:xs) -> Just $ foldr (<>) x xs
      cropBaked r = r { _report_baked = take 20 (sortBy (flip (comparing _event_time)) (_report_baked r)) }
      dropSeen r = r { _report_seen = [] }
  return (liftM2 (,) (fmap (cropBaked . dropSeen) aggReport) waiting)
