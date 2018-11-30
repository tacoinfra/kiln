{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Workers where
import Backend.Schema
import Common.Schema
import Data.Foldable (for_)
import Data.Set (Set)
import qualified Data.Set as Set
import Database.Groundhog.Postgresql

import Tezos.Types


insertClientBakers :: (Monad m, PersistBackend m) => Set PublicKeyHash -> m ()
insertClientBakers pkhs = do
  existingPkhs :: [PublicKeyHash] <- project Baker_publicKeyHashField CondEmpty
  let newPkhs = pkhs `Set.difference` Set.fromList existingPkhs
  for_ newPkhs $ \pkh -> insertNotify $ Baker pkh Nothing False


