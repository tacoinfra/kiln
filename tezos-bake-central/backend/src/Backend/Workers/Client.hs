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
import Common.Schema
import Common.Verification (validateForkyBlocks)
import Control.Concurrent.MVar
import Control.Exception.Safe (Handler (..), catches)
import Control.Lens.TH (makeLenses)
import Control.Monad (unless, void)
import Control.Monad.Logger (logInfo, logErrorSH, logDebugSH)
import Control.Monad.Reader (runReaderT)
import Data.Foldable (for_, toList)
import Data.Function (on)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (nonEmpty)
import Data.Semigroup (Sum (..), getSum, (<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Data.Traversable (for)
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Simple as Http
import Rhyolite.Backend.DB (getTime, runDb)
import Rhyolite.Backend.DB.PsqlSimple (Values (..), executeQ, queryQ)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Schema (Id (..), Json (..))
import Safe (maximumByMay)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.Types

import Backend.Alerts (clearNoBakerHeartbeatError, reportNoBakerHeartbeatError)
import Backend.CachedNodeRPC
import Backend.ChainHealth (scanForkInfo)
import Backend.Common (worker')
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
  worker' $ (*> waitForNewHeadWithTimeout nds) $
    readMVar (_nodeDataSource_parameters nds) >>= \protoInfo ->
      runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity (_nodeDataSource_pool nds)) $
        runReaderT (doUpdate protoInfo) (ClientWorkerContext appCfg nds)

  where
    doUpdate protoInfo = do
      $(logInfo) "Update client cycle."
      now <- getTime
      let delay = calcTimeBetweenBlocks protoInfo
      let maxTime = Just (addUTCTime (- delay) now)

      let blockHeightTimeout :: NominalDiffTime = fromIntegral $ max 15 $ (5*) $ sum $ take 3 $ toList $ _protoInfo_timeBetweenBlocks protoInfo

      toUpdate :: [(Id Client, URI, Maybe T.Text)] <- [queryQ|
        SELECT id, address, alias
        FROM "Client" c
        WHERE (c.updated < ?maxTime OR c.updated IS NULL) AND NOT c.deleted
        ORDER BY updated NULLS FIRST
      |]

      clientDelegates <- for toUpdate $ \(cid, address, _alias) -> do
        let handlingHttpExc f = (Just <$> f) `catches`
              [ Handler $ \(e :: Http.JSONException) -> $(logErrorSH) e $> Nothing
              , Handler $ \(e :: Http.HttpException) -> $(logErrorSH) e $> Nothing
              ]

        _result <- handlingHttpExc $ do
          $(logInfo) $ "Updating client at " <> Uri.render address

          -- TODO: abstract this into a ClientRPC like the way there's a NodeRPC
          clientConfig :: ClientConfig <- fmap Http.getResponseBody $ Http.httpJSON =<< Http.parseRequest (T.unpack (Uri.render address) <> "/config")
          let clientConfigJson = Json clientConfig

          report :: Report <- fmap Http.getResponseBody $ Http.httpJSON =<< Http.parseRequest (T.unpack (Uri.render address) <> "/events")
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
          validateForkyBlocks ($(logDebugSH) . (,) ("validateForkyBlocks" :: String)) forkInfo

          updateIdNotify cid [Client_updatedField =. Just now]

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
            Nothing -> pure () --clearClientOnForkError cid
            Just es -> for_ es $ \_e -> do
              -- let tooOld = case _forkInfo_forkStatus e of
              --       Left ForkStatus_TooOld -> True
              --       _ -> False
              -- what this SHOULD be
              -- reportClientOnForkError cid tooOld (_forkInfo_hash e) (_forkInfo_time e)
              return ()
          return $ _clientConfig_delegates clientConfig

        --case result of
        --  Nothing -> [] <$ reportInaccessibleEndpointError EndpointType_Client address alias
        --  Just xs -> xs <$ clearInaccessibleEndpointError EndpointType_Client address
        pure []

      insertClientDelegates (Set.fromList $ concat clientDelegates)
