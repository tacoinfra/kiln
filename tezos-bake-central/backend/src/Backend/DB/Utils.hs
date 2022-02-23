{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Backend.DB.Utils
  ( getSchemaName
  ) where

import Data.List.Split (wordsBy)
import Data.Proxy (Proxy (..))
import Database.Groundhog.Core (PersistBackend, PhantomDb, PurePersistField(..), queryRaw)
import Database.Groundhog.Instances ()
import Database.Groundhog.Generic (mapAllRows)

-- | Reimplementaion of @getSchemaName@ from 'Rhyolite.Backend.Listen' that is more
-- safe when lazy evalutaions are unexceptedly evaluated.
getSchemaName :: forall m. PersistBackend m => m String
getSchemaName =  do
  searchPath <- getSearchPath
  let searchPathComponents = wordsBy (== ',') searchPath
      schemaName = case searchPathComponents of
       (x:_:_:_) -> x
       _ -> "public"
  return schemaName
  where
    getSearchPath :: PersistBackend m => m String
    getSearchPath = do
      queryRaw False "SHOW search_path" [] (mapAllRows (pure . fst . fromPurePersistValues (Proxy @(PhantomDb m)))) >>= \case
        [searchPath] -> return searchPath
        _ -> error "getSearchPath: Unexpected result from queryRaw"
