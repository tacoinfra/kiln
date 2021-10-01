{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Workers.Cache where

import Control.Concurrent.STM (TVar, atomically, orElse)
import Control.Monad.Catch (catch, SomeException(..))
import Control.Monad.Logger (logDebug, logDebugNS)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Dependent.Map (DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Database.Groundhog.Postgresql (Postgresql(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Rhyolite.Backend.DB (getTime, runDb)
import Rhyolite.Backend.DB.PsqlSimple (executeMany, executeQ)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Schema (Json (..))

import Backend.CachedNodeRPC (CacheLine (..), NodeDataSource (..), NodeQuery (..), RpcResult(..))
import Backend.Common (workerWithDelay)
import Backend.STM (MonadSTM (liftSTM), readTVar', writeTVar')
import ExtraPrelude

classifyCacheEntry
  :: (MonadSTM m)
  => UTCTime
  -> DSum NodeQuery (Compose TVar CacheLine)
  -> m (Maybe (Either (Json Aeson.Value, LBS.ByteString) (DSum NodeQuery (Compose TVar CacheLine))))
classifyCacheEntry expireTime (q :=> Compose cx) =
  readTVar' cx <&> \(CacheLine result used dirty) -> if used < expireTime
    then
      case dirty of
        Nothing ->
          Just $ Left (Json (Aeson.toJSON q), _rpcResult_raw result)
        Just _ -> Nothing
    else
      Just $ Right $ q :=> Compose cx

cacheWorker :: NominalDiffTime -> NominalDiffTime -> NodeDataSource -> IO (IO ())
cacheWorker delay dbCacheTTL dsrc = workerWithDelay "cacheWorker" (pure delay) $ \_ -> do
  let maxTTL = delay * 2
  expireTime <- addUTCTime maxTTL <$> getCurrentTime
  compactCache expireTime dsrc
  trimCache dbCacheTTL dsrc

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
      liftSTM $ classifyCacheEntry expireTime entry
        `orElse` pure (Just $ Right entry)
    writeTVar' cacheVar $ DMap.fromAscList retainThese
    pure (writeBackThese, length retainThese)

  let db = _nodeDataSource_pool dsrc
  runLoggingEnv (_nodeDataSource_logger dsrc) $ do
    now <- runDb (Identity db) getTime
    $(logDebug) $ "Flushing cache: " <> tshow (length writeBackThese) <> " aged into database, " <> tshow numRetained <> " kept in-memory"
    runDb (Identity db) $
      void $ executeMany [sql|
        INSERT INTO "RawCacheEntry" ("chainId", key, value, "addedAt")
        VALUES (?, ?, ?, ?) ON CONFLICT ("chainId", key) DO NOTHING
        |]
        [ (chainId, k, v, now)
        | (k, v) <- writeBackThese
        ]
        `catch`
        (\(SomeException e) -> do
            logDebugNS "kiln-debugging" (T.pack $ show e)
            error "There was a key confilct during cache compaction. Please view the logs\
            \ at namespace \"kiln-debugging\" for more information."
        )

trimCache :: NominalDiffTime -> NodeDataSource -> IO ()
trimCache dbCacheTTL dsrc = runLoggingEnv (_nodeDataSource_logger dsrc) $ do
  void $ runDb (Identity (_nodeDataSource_pool dsrc)) [executeQ|
    DELETE FROM "RawCacheEntry" where EXTRACT (EPOCH FROM now()) - EXTRACT (EPOCH FROM "addedAt") > ?dbCacheTTL
  |]
