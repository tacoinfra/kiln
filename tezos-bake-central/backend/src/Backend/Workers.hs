{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Workers where
import Backend.Schema
import Common.Schema
import Data.Foldable (for_)
import Data.Set (Set)
import qualified Data.Set as Set
import Database.Groundhog.Postgresql

import Tezos.Types


insertClientDelegates :: (Monad m, PersistBackend m) => Set PublicKeyHash -> m ()
insertClientDelegates pkhs = do
  existingPkhs :: [PublicKeyHash] <- project Delegate_publicKeyHashField CondEmpty
  let newPkhs = pkhs `Set.difference` Set.fromList existingPkhs
  for_ newPkhs $ \pkh -> insertNotify $ Delegate pkh Nothing False


