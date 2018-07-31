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

-- I'm fairly sure this is not 100% correct, but I'm also not 100% sure what the correct thing is. Which block's protocol constants should be
-- inspected when determining the rewards for a block which is baked? I'm basically assuming that the constants are sufficiently constant for now.
queryBestNode :: (Monad m, PersistBackend m, PostgresRaw m) => m (Maybe (Id Node, Node, ProtoInfo))
queryBestNode = do
  nodeIds :: Maybe (Id Node, Id Parameters) <- listToMaybe <$> [queryQ|
    SELECT n.id, p.id
      FROM "Node" n JOIN "Parameters" p ON n.id = p.node
     WHERE n."headLevel" IS NOT NULL AND NOT n.deleted
     ORDER BY n."headLevel" DESC
     LIMIT 1 |]

  for nodeIds $ \(nodeId, paramId) -> do
    Just node <- get (fromId nodeId)
    Just params <- get (fromId paramId)
    return (nodeId, node, _parameters_protoInfo params)




insertClientDelegates :: (Monad m, PersistBackend m, PostgresRaw m) => Set PublicKeyHash -> m ()
insertClientDelegates pkhs = do
  let inPkhs = Pg.In $ Set.toList pkhs
  (existingIds :: [Id Delegate], existingPkhs :: [PublicKeyHash]) <-
    first (map toId) . unzip <$> project (AutoKeyField, Delegate_publicKeyHashField) CondEmpty

  let newPkhs = pkhs `Set.difference` Set.fromList existingPkhs
  for_ newPkhs $ \pkh -> insertAndNotify $ Delegate pkh False


