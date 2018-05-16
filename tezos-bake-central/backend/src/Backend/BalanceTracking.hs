module Backend.BalanceTracking where

import Control.Monad.IO.Class
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Control
import Data.Aeson
import qualified Data.AppendMap as Map
import Data.AppendMap (AppendMap)
import Data.Fixed
import Data.Functor.Identity
import Data.Maybe
import Data.Pool (Pool)
import Data.Semigroup
import Data.Word
import Database.Groundhog.Postgresql
import Focus.Backend.DB (runDb)
import Focus.Backend.Listen
import Focus.Backend.Schema.TH
import Focus.Schema

import Backend.Schema
import Common.App
import Common.Schema

-- NB: This eventually needs to change, we can't really be getting an unbounded amount of information. Our viewselector needs to become more specific.
getAllRewards :: (PersistBackend m) => a -> m (AppendMap (Id Client) (First (AppendMap Word64 Micro), a))
getAllRewards a = do
  rewards <- selectAll -- PendingReward
  let rewardMap' = Map.fromListWith (Map.unionWith (+))
        [(_pendingReward_client r, Map.singleton (_pendingReward_level r) (_pendingReward_amount r)) | (_,r) <- rewards]
      rewardMap = fmap (\x -> (First x, a)) rewardMap'
  return rewardMap