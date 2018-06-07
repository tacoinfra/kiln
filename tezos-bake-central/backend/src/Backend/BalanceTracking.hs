module Backend.BalanceTracking where

import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as Map
import Data.Fixed
import Data.Semigroup
import Data.Word
import Database.Groundhog.Postgresql
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
