{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Workers where
import Backend.Schema
import Common.Schema
import Data.Bifunctor (first)
import Data.Foldable (fold, foldl', for_, toList, traverse_)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple as Pg
import Rhyolite.Backend.DB.PsqlSimple (In (..), Only (..), PostgresRaw, Values (..), executeQ, queryQ)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Schema (Id (..), Json (..))
import Tezos.Types


insertClientDelegates :: (Monad m, PersistBackend m, PostgresRaw m) => Set PublicKeyHash -> m ()
insertClientDelegates pkhs = do
  let inPkhs = Pg.In $ Set.toList pkhs
  (existingIds :: [Id Delegate], existingPkhs :: [PublicKeyHash]) <-
    first (map toId) . unzip <$> project (AutoKeyField, Delegate_publicKeyHashField) CondEmpty

  let newPkhs = pkhs `Set.difference` Set.fromList existingPkhs
  for_ newPkhs $ \pkh -> insertNotify $ Delegate pkh Nothing False


