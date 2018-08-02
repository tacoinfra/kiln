{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Workers.Client where

import Backend.Config (AppConfig (..), HasAppConfig, getAppConfig)
import Common (tshow)
import Common.Schema
import Common.Verification (ForkInfo (..), ForkStatus (..), validateForkyBlocks)
import Control.Concurrent.MVar
import Control.Exception.Safe (Handler (..), catch, catches, finally, throwIO)
import Control.Lens.TH (makeLenses)
import Control.Monad (join, unless, void, when, (<=<))
import Control.Monad.IO.Class
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Control.Monad.Reader (MonadReader, runReaderT)
import Data.Foldable (fold, foldl', for_, toList, traverse_)
import Data.Function (on, (&))
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (nonEmpty)
import Data.Pool (Pool)
import Data.Semigroup (Semigroup, Sum (..), getSum, (<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Traversable (for)
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Simple as Http
import Rhyolite.Backend.DB (RunDb, getTime, openDb, runDb, selectMap)
import Rhyolite.Backend.DB.PsqlSimple (In (..), Only (..), PostgresRaw, Values (..), executeQ, queryQ)
import Rhyolite.Backend.Listen (NotificationType (..), insertAndNotify, insertAndNotify_, notifyEntityId,
                                updateAndNotify)
import Rhyolite.Concurrent (worker)
import Rhyolite.Schema (Id (..), Json (..))
import Safe (maximumByMay, maximumMay)
import Say (say, sayErr, sayShow)

import Tezos.Types

import Backend.CachedNodeRPC
import Backend.ChainHealth (scanForkInfo)
import Backend.Common (worker')
import Backend.Errors
import Backend.Schema
import Backend.Workers

data ClientWorkerContext = ClientWorkerContext
  { _clientWorkerContext_appConfig :: !AppConfig
  , _clientWorkerContext_NodeDataSource :: !NodeDataSource
  }
makeLenses 'ClientWorkerContext
instance HasAppConfig ClientWorkerContext where
  getAppConfig = clientWorkerContext_appConfig
instance HasNodeDataSource ClientWorkerContext where
  nodeDataSource = clientWorkerContext_NodeDataSource

clientWorker
  :: AppConfig
  -> NodeDataSource
  -> IO (IO ())
clientWorker appCfg nds =
  worker' (readTimeBetweenBlocks nds) $ \delay ->
    readMVar (_nodeDataSource_parameters nds) >>= \protoInfo ->
      runNoLoggingT $ runDb (Identity (_nodeDataSource_pool nds)) $
        runReaderT (doUpdate delay protoInfo) (ClientWorkerContext appCfg nds)

  where
    doUpdate delay protoInfo = do
      say "Update client cycle."
      now <- getTime
      let maxTime = Just (addUTCTime (- delay) now)

      let blockHeightTimeout :: NominalDiffTime = fromIntegral $ max 15 $ (5*) $ sum $ take 3 $ toList $ _protoInfo_timeBetweenBlocks protoInfo

      toUpdate :: [(Id Client, ClientAddress)] <- [queryQ|
        SELECT id, address
        FROM "Client" c
        WHERE (c.updated < ?maxTime OR c.updated IS NULL) AND NOT c.deleted
        ORDER BY updated NULLS FIRST
      |]

      clientDelegates <- for toUpdate $ \(cid, address) -> do
        let handlingHttpExc f = (Just <$> f) `catches`
              [ Handler $ \(e :: Http.JSONException) -> sayErr (tshow e) $> Nothing
              , Handler $ \(e :: Http.HttpException) -> sayErr (tshow e) $> Nothing
              ]

        result <- handlingHttpExc $ do
          say $ "Updating client at " <> address

          -- TODO: abstract this into a ClientRPC like the way there's a NodeRPC
          clientConfig :: ClientConfig <- fmap Http.getResponseBody $ Http.httpJSON =<< Http.parseRequest (T.unpack address <> "/config")
          let clientConfigJson = Json clientConfig

          report :: Report <- fmap Http.getResponseBody $ Http.httpJSON =<< Http.parseRequest (T.unpack address <> "/events")
          let reportJson = Json report

          for_ (maximumByMay (compare `on` _event_time) $ _report_seen report) $ \seenEvent ->
            if addUTCTime blockHeightTimeout (_event_time seenEvent) < now then
              reportNoBakerHeartbeatError cid (_event_detail seenEvent)
            else
              clearNoBakerHeartbeatError cid

          -- TODO: this is quite "wrong" in the sense that we haven't confirmed the
          -- acceptance of this block, we should really only use this event to
          -- know if the baker itself is active.  The reqards should be computed
          -- based on nodes reporting new blocks.  Even if we baked, if that was
          -- a different branch, there's no reward.
          let bakingReward delegate blk = _protoInfo_blockReward protoInfo + getSum ((foldMap . foldMap) (Sum . sumFees delegate . _bakedEventOperation_data) (_bakedEvent_operations $ _event_detail blk))
              rewardDelay l =
                let c = fromIntegral l `div` _protoInfo_blocksPerCycle protoInfo + 1
                    rc = c + (let Cycle x = _protoInfo_preservedCycles protoInfo in fromIntegral x)
                in rc * _protoInfo_blocksPerCycle protoInfo
              insertValues = Values ["text", "varchar", "int8", "int8"]
                [ (delegatePkh, toBase58Text (_bakedEvent_hash $ _event_detail b), rewardDelay (blockLevel b) , bakingReward delegatePkh b)
                | b <- _report_baked report
                , delegatePkh <- _clientConfig_delegates clientConfig
                ]
          unless (null $ _report_baked report) $ void $ [executeQ|
            INSERT INTO "PendingReward" (delegate, hash, level, amount)
            SELECT d.id, x.hash, x.level, x.amount
            FROM ?insertValues x (delegate_pkh, hash, level, amount)
            JOIN "Delegate" d ON d."publicKeyHash" = x.delegate_pkh
            ON CONFLICT DO NOTHING |]

          _ <- [executeQ| INSERT INTO "ClientInfo" (client, report, config)
                          VALUES (?cid, ?reportJson, ?clientConfigJson)
                          ON CONFLICT (client) DO UPDATE SET
                            report = ?reportJson
                          , config = ?clientConfigJson
                          |]
          forkInfo <- scanForkInfo now report
          validateForkyBlocks sayShow forkInfo

          updateAndNotify cid [Client_updatedField =. Just now]

          -- TODO: Add back errors reported by client RPC
          -- case sortBy (compare `on` _event_time) (_report_errors report) of
          --   [] -> return ()
          --   es -> do
          --     lastError <- liftIO $ readIORef lastErrorRef
          --     let (new,_) = span ((>= lastError) . Just . _error_time) (mkErr <$> es)
          --     case new of
          --       [] -> return ()
          --       (x:_) -> do
          --         liftIO $ writeIORef lastErrorRef (Just $ _error_time x)
          --         queueAllEmails new
          -- TODO.  debounce below as above
          flip validateForkyBlocks forkInfo $ \errors -> case nonEmpty errors of
            -- Nothing -> clearClientOnForkError cid
            Just es -> for_ es $ \e -> do
              let tooOld = case _forkInfo_forkStatus e of
                    Left ForkStatus_TooOld -> True
                    _ -> False
              -- what this SHOULD be
              -- reportClientOnForkError cid tooOld (_forkInfo_hash e) (_forkInfo_time e)
              return ()
          return $ _clientConfig_delegates clientConfig

        case result of
          Nothing -> [] <$ reportInaccessibleEndpointError EndpointType_Client address
          Just xs -> xs <$ clearInaccessibleEndpointError EndpointType_Client address

      insertClientDelegates (Set.fromList $ concat clientDelegates)
