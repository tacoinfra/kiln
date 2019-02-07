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
import Control.Exception.Safe (Handler (..), catches)
import Control.Lens.TH (makeLenses)
import Control.Monad.Logger (logDebugSH, logErrorSH, logInfo)
import Control.Monad.Reader (runReaderT)
import Data.Foldable (for_, traverse_, toList)
import Data.Function (on)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Data.Traversable (for)
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Simple as Http
import Rhyolite.Backend.DB (getTime, runDb)
import Rhyolite.Backend.DB.PsqlSimple (queryQ)
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
    withParams nds $ \protoInfo ->
      runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity (_nodeDataSource_pool nds)) $
        runReaderT (doUpdate protoInfo) (ClientWorkerContext appCfg nds)

  where
    doUpdate protoInfo = do
      $(logInfo) "Update client cycle."
      now <- getTime
      let delay = calcTimeBetweenBlocks protoInfo
      let maxTime = Just (addUTCTime (- delay) now)

      let blockHeightTimeout :: NominalDiffTime = fromIntegral $ max 15 $ (5*) $ sum $ take 3 $ toList $ _protoInfo_timeBetweenBlocks protoInfo

      toUpdate :: [(Id BakerDaemonExternal, URI, Maybe T.Text)] <- [queryQ|
        SELECT c.id, c."data#data#address", c."data#data#alias"
        FROM "BakerDaemonExternal" c
        WHERE NOT c."data#deleted" AND
          (c."data#data#updated" < ?maxTime OR c."data#data#updated" IS NULL)
        ORDER BY c."data#data#updated" NULLS FIRST
      |]

      _clientBakers <- for toUpdate $ \(Id cid, address, _alias) -> do
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

          let bakerDaemonInfo = (BakerDaemonInfo cid (BakerDaemonInfoData reportJson clientConfigJson))
          insertByAll bakerDaemonInfo
            >>= either (const $ replaceBy BakerDaemonInfoId bakerDaemonInfo) (const $ return ())
          forkInfo <- scanForkInfo now report
          validateForkyBlocks ($(logDebugSH) . (,) ("validateForkyBlocks" :: String)) forkInfo

          update
            [BakerDaemonExternal_dataField ~> DeletableRow_dataSelector ~> BakerDaemonExternalData_updatedSelector =. Just now]
            (BakerDaemonExternal_idField ==. cid)
          project (BakerDaemonExternal_dataField ~> DeletableRow_dataSelector)
                  (BakerDaemonExternal_idField ==. cid)
            >>= traverse_ (notify . Notify_BakerDaemonExternal cid . Just)


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
          return $ _clientConfig_bakers clientConfig

        --case result of
        --  Nothing -> [] <$ reportInaccessibleEndpointError EndpointType_Client address alias
        --  Just xs -> xs <$ clearInaccessibleEndpointError EndpointType_Client address
        pure []

      --insertClientBakers (Set.fromList $ concat clientBakers)
      pure ()

--insertClientBakers :: (Monad m, PersistBackend m) => Set PublicKeyHash -> m ()
--insertClientBakers pkhs = do
--  existingPkhs :: [PublicKeyHash] <- project Baker_publicKeyHashField CondEmpty
--  let newPkhs = pkhs `Set.difference` Set.fromList existingPkhs
--  for_ newPkhs $ \pkh -> insertNotify $ Baker pkh Nothing False
