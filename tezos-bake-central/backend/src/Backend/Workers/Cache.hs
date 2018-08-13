{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Backend.Workers.Cache where

import Control.Monad.Logger(runNoLoggingT)
import Data.Functor.Identity
import Data.Maybe (listToMaybe)
import Data.Foldable(for_)
import Data.Either(partitionEithers)
import Data.Pool (Pool)
import Database.Groundhog.Postgresql
import Control.Concurrent.MVar
import Data.Constraint(Dict(..))
import Data.Dependent.Map (DMap, DSum(..))
import Data.Time(UTCTime, getCurrentTime, addUTCTime)
import Data.Ratio ((%))
import Data.Maybe(catMaybes)
-- import Say (say, sayErr, sayShow)
import Rhyolite.Backend.DB.PsqlSimple (In (..), Only (..), PostgresRaw, Values (..), executeQ, queryQ)
import qualified Data.Aeson as Aeson
import qualified Data.Dependent.Map as DMap

import Rhyolite.Backend.DB (RunDb, getTime, openDb, runDb, selectMap)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Schema (HasId, Id(..), Json(..))
import Rhyolite.Request.Class
import Rhyolite.Concurrent (worker)
import Backend.CachedNodeRPC
import Common.Schema(GenericCacheEntry(..))
import Backend.Schema () -- need instances
import Backend.Schema (stripOnly, Field(..))

import Tezos.Types (ChainId)


classifyCacheEntry :: ChainId -> UTCTime -> DSum NodeQuery CachedResult -> IO (Maybe (Either GenericCacheEntry (DSum NodeQuery CachedResult)))
classifyCacheEntry chainId maxTTL (q :=> (CachedResult cx)) = do
  tryReadMVar cx >>= return . \case
    Nothing -> -- the mvar is not resolved yet, we don't know its ttl, retain it for now.
      Just $ Right $ q :=> CachedResult cx
    Just x -> case x of
      Left _ ->
        -- throw away all errors, we'll try again,
        -- TODO: some errors may be perminant, lets just keep those.
        Nothing
      Right (CacheLine value used) ->
        if used < maxTTL
          then
            let
              kJson = requestToJSON q
              vJson = case requestResponseToJSON q of
                Dict -> Aeson.toJSON value
            in Just $ Left $ GenericCacheEntry
                { _genericCacheEntry_chainId = chainId
                , _genericCacheEntry_key = Json kJson
                , _genericCacheEntry_value = Json vJson
                }
          else Just $ Right $ q :=> CachedResult cx

-- TODO: another way we could do this is to add an extra thread awaiting each
-- unresolved MVar, and push the value into the database immediately when it is
-- resolved (without blocking the main thread)
cacheWorker :: Int -> NodeDataSource -> IO (IO ())
cacheWorker delay dsrc = worker delay $ do
  let db = _nodeDataSource_pool dsrc
  let chainId = _nodeDataSource_chain dsrc
  let maxTTL = fromRational $ (toInteger (delay * 2) % 1000000)
  -- say "evicting cache"
  modifyMVar (_nodeDataSource_cache dsrc) $ \cache -> do
    now <- addUTCTime maxTTL <$> getCurrentTime
    (writeBackThese, retainThese) <- fmap (partitionEithers . catMaybes) $ traverse (classifyCacheEntry chainId now) $ DMap.toAscList cache
    runNoLoggingT $ runDb (Identity db) $ for_ writeBackThese $ \cacheEntry -> do
      let kJson = _genericCacheEntry_key cacheEntry
      have :: Maybe (Id GenericCacheEntry) <- listToMaybe . stripOnly <$> [queryQ|
        SELECT c."id"
        FROM "GenericCacheEntry" c
        WHERE c."chainId" = ?chainId
          AND c."key" = ?kJson |]
      case have of
        Just entryId ->
          update
            [GenericCacheEntry_valueField =. (_genericCacheEntry_value cacheEntry)]
            (AutoKeyField ==. fromId entryId)
        Nothing -> insert_ cacheEntry
    return (DMap.fromAscList retainThese, ())
  -- say "okay, done?"
