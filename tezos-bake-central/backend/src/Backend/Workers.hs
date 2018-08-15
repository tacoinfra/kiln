{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Workers where
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Database.Groundhog.Postgresql
import Backend.Schema
import Common.Schema
import Rhyolite.Backend.DB.PsqlSimple (In (..), Only (..), PostgresRaw, Values (..), executeQ, queryQ)
import Rhyolite.Schema (Id (..), Json (..))
import Tezos.Types
import Data.Traversable (for)
import Rhyolite.Backend.Schema (fromId, toId)
import Data.Set (Set)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Data.Set as Set
import Data.Bifunctor (first)
import Data.Foldable (fold, foldl', for_, toList, traverse_)
import Rhyolite.Backend.Listen (NotificationType (..), insertAndNotify, insertAndNotify_, notifyEntityId,
                                updateAndNotify)



insertClientDelegates :: (Monad m, PersistBackend m, PostgresRaw m) => Set PublicKeyHash -> m ()
insertClientDelegates pkhs = do
  let inPkhs = Pg.In $ Set.toList pkhs
  (existingIds :: [Id Delegate], existingPkhs :: [PublicKeyHash]) <-
    first (map toId) . unzip <$> project (AutoKeyField, Delegate_publicKeyHashField) CondEmpty

  let newPkhs = pkhs `Set.difference` Set.fromList existingPkhs
  for_ newPkhs $ \pkh -> insertAndNotify $ Delegate pkh False


