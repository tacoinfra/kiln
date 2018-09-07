{-# LANGUAGE QuasiQuotes #-}

module Backend.BalanceTracking where

import Control.Monad
import Data.Fixed
import Data.List
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Ord
import Data.Semigroup
import Data.Word
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.Schema

import Tezos.Json (TezosWord64 (..))
import Tezos.PublicKeyHash

import Backend.Schema ()
import Common.Schema

-- NB: This eventually needs to change, we can't really be getting an unbounded amount of information. Our viewselector needs to become more specific.
getAllRewards :: (PostgresRaw m, PersistBackend m) => a -> m (MonoidalMap PublicKeyHash (First (MonoidalMap Word64 Micro), a))
getAllRewards a = do
  rewards <- [queryQ|
    SELECT d."publicKeyHash", COALESCE(pr.level, 0), COALESCE(pr.amount, 0)
    FROM "Delegate" d
    LEFT OUTER JOIN "PendingReward" pr
      ON d.id = pr.delegate
    |] -- selectAll -- PendingReward
  let rewardMap' = MMap.fromListWith (MMap.unionWith (+))
        [(delegate, MMap.singleton (unTezosWord64 level) amount) | (delegate, level, amount) <- rewards]
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
