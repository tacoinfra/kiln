{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Workers.Cache where

import Control.Concurrent.STM (TVar, atomically, orElse)
import Control.Monad.Logger (logDebug)
import qualified Data.Aeson as Aeson
import Data.Dependent.Map (DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.DB.PsqlSimple (executeMany)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Schema (Json (..))

import Tezos.Types (ChainId)

import Backend.CachedNodeRPC (CacheLine (..), NodeDataSource (..), NodeQuery (..))
import Backend.Common (workerWithDelay)
import Backend.STM (MonadSTM (liftSTM), readTVar', writeTVar')
import Backend.Schema (RawCacheEntry (..))
import ExtraPrelude

classifyCacheEntry
  :: (MonadSTM m)
  => ChainId
  -> UTCTime
  -> DSum NodeQuery (Compose TVar CacheLine)
  -> m (Maybe (Either RawCacheEntry (DSum NodeQuery (Compose TVar CacheLine))))
classifyCacheEntry chainId expireTime (q :=> Compose cx) =
  readTVar' cx <&> \(CacheLine value raw used dirty) -> if used < expireTime
    then
      case dirty of
        Nothing ->
          Just $ Left RawCacheEntry
            { _rawCacheEntry_chainId = chainId
            , _rawCacheEntry_key = Json (Aeson.toJSON q)
            , _rawCacheEntry_value = raw
            }
        Just _ -> Nothing
    else
      Just $ Right $ q :=> Compose cx

cacheWorker :: NominalDiffTime -> NodeDataSource -> IO (IO ())
cacheWorker delay dsrc = workerWithDelay (pure delay) $ \_ -> do
  let maxTTL = delay * 2
  expireTime <- addUTCTime maxTTL <$> getCurrentTime
  compactCache expireTime dsrc

compactCache :: UTCTime -> NodeDataSource -> IO ()
compactCache expireTime dsrc = do
  let
    chainId = _nodeDataSource_chain dsrc
    cacheVar = _nodeDataSource_cache dsrc

  (writeBackThese, numRetained) <- atomically $ do
    cache <- readTVar' cacheVar
    (writeBackThese, retainThese) <- fmap (partitionEithers . catMaybes) $ for (DMap.toAscList cache) $ \entry ->
      -- If the classification would be retried we just assume this key is still
      -- in active use and should be kept in-memory.
      liftSTM $ classifyCacheEntry chainId expireTime entry
        `orElse` pure (Just $ Right entry)
    writeTVar' cacheVar $ DMap.fromAscList retainThese
    pure (writeBackThese, length retainThese)

  let db = _nodeDataSource_pool dsrc
  runLoggingEnv (_nodeDataSource_logger dsrc) $ do
    $(logDebug) $ "Flushing cache: " <> tshow (length writeBackThese) <> " aged into database, " <> tshow numRetained <> " kept in-memory"
    runDb (Identity db) $
      void $ executeMany [sql|
        INSERT INTO "RawCacheEntry" ("chainId", key, value)
        VALUES (?, ?, ?) ON CONFLICT ("chainId", key) DO NOTHING
        |]
        [ (chainId, k, v)
        | entry <- writeBackThese
        , let k = _rawCacheEntry_key entry
        , let v = _rawCacheEntry_value entry
        ]
