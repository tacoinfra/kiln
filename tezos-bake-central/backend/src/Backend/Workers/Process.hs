{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wall -Werror #-}

-- Kiln managed process/daemon
module Backend.Workers.Process where

import Control.Monad (when, unless)
import Control.Monad.Catch (bracket)
import Control.Monad.Logger (MonadLogger, logWarnSH, logDebugSH, logWarn, logInfoSH)
import Control.Monad.Trans.Control
import Data.Pool (Pool)
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.DB.PsqlSimple (queryQ, fromOnly)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Rhyolite.Backend.Schema (fromId)
import System.Process (CreateProcess, withCreateProcess, getProcessExitCode, terminateProcess)
import System.IO (hFlush)
import System.IO.Temp (withTempFile)

import Data.Time (getCurrentTime, addUTCTime)
import Data.Word
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.ByteString.Lazy as LBS

import Backend.Common
import Backend.Schema
import Common.Schema
import ExtraPrelude

import Tezos.Json

-- Daemon Process Management Worker
-- The flow is roughly like this
-- - Obtain lock with finalizer, delay 1s
--     If the ProcessData.backend is not null/Nothing then it could mean that
--     the previous worker did not exit cleanly, or the kiln process died without doing
--     a clean termination of the node, or there is another kiln process running this daemon.
--     So we wait for 5 min from the updated time before starting the daemon again
--     We keep this value "updated" when we are running daemon.
--
-- - Wait/Start loop
--   After we have the lock, we either wait for start signal (_processData_running == True)
--   or start the daemon if its already true.
--   On getting a start signal following are done
--   - Initialization
--       User specified code which could access NodeConfig and do DB transactions
--   - Start the daemon process
--     After starting the daemon we have a "Monitor/Stop loop", delay 1s
--     it waits for process stop signal (_processData_running == False) and terminates it
--     Also monitors if process terminates unexpectedly.

processWorker
  :: ( MonadIO m
     )
  => LoggingEnv
  -> Pool Postgresql
  -> NodeConfigFile
  -> (forall m'. (Monad m', MonadIO m', PersistBackend m', MonadLogger m') => FilePath -> m' a)
  -> (a -> FilePath -> CreateProcess)
  -> Id ProcessData
  -> Maybe (Maybe ProcessData -> Notify)
  -> m (IO ())
processWorker logger db config initialize process pid makeNotify = worker' $ do
  waitUntilShouldRun
  bracket obtainLock freeLock $ \_ -> do
    updateState ProcessState_Initializing
    withNodeConfig config $ \configFile -> do
      v <- runLoggingEnv logger $ runDb (Identity db) $ initialize configFile
      updateState ProcessState_Starting
      withCreateProcess (process v configFile) procMonitor
    threadDelay' 10
  where
    state_ = ProcessData_stateField
    updated_ = ProcessData_updatedField
    backend_ = ProcessData_backendField
    running_ = ProcessData_runningField
    waitUntilShouldRun = do
      shouldRun <- runLoggingEnv logger $ runDb (Identity db) $
        or <$> project running_ (AutoKeyField ==. (fromId pid))
      unless shouldRun $ threadDelay' 1 >> waitUntilShouldRun

    obtainLock = runLoggingEnv logger $ do
      lockId :: Int <- runDb (Identity db) $
        [queryQ| SELECT nextval('"ProcessLockUniqueId"') |] <&> fromOnly . head
      $(logDebugSH) ("Obtaining lock for process:" :: Text, pid, ", LockId:" :: Text, lockId)
      let
        state = ProcessState_Stopped
        {-# INLINE claim #-}
        claim = do
          now <- liftIO $ getCurrentTime
          let nowMinus5min = addUTCTime (-600) now
          pd <- runDb (Identity db) $ do
            update [state_ =. state, updated_ =. Just now, backend_ =. Just lockId]
              ((AutoKeyField ==. (fromId pid))
               &&. (backend_ ==. (Nothing :: Maybe Int) ||. updated_ <. Just nowMinus5min))
            project backend_ (AutoKeyField ==. (fromId pid))
          case pd of
            [] -> error "ProcessData not found in DB"
            (lockId':_) -> do
              unless (Just lockId == lockId') $ do
                $(logWarn) "internalnode LOCK HELD"
                threadDelay' 3
                *> claim
      claim
      return ()

    freeLock _ = runLoggingEnv logger $ do
      $(logDebugSH) ("Freeing lock for process:" :: Text, pid)
      now <- liftIO $ getCurrentTime
      void $ runDb (Identity db) $
        update [updated_ =. Just now, backend_ =. (Nothing :: Maybe Int)]
          (AutoKeyField ==. (fromId pid))

    procMonitor _ _ _ ph = do
      runLoggingEnv logger $ go
      where
        {-# INLINE go #-}
        go :: forall m1. (MonadLogger m1, MonadIO m1, MonadBaseControl IO m1) => m1 ()
        go = do
          shouldRun <- runDb (Identity db)
            (or <$> project running_ (AutoKeyField ==. (fromId pid)))
          (liftIO $ getProcessExitCode ph) >>= \case
            Nothing -> do
              updateState ProcessState_Running
              unless shouldRun $ liftIO $ terminateProcess ph
              (threadDelay' 1) *> go
            Just _ -> if shouldRun
              then do
                updateState ProcessState_Failed
                $(logWarnSH) ("Process exited unexpectedly:" :: Text, pid)
              else do
                updateState ProcessState_Stopped
                $(logInfoSH) ("Process exited successfully:" :: Text, pid)

    updateState :: (MonadIO m, MonadBaseControl IO m) => ProcessState -> m ()
    updateState state = runLoggingEnv logger $ runDb (Identity db) $ do
      $(logDebugSH) ("putState:" :: Text, pid, state)
      get (fromId pid) >>= \case
        Nothing -> return ()
        Just p ->
          when (_processData_state p /= state) $ do
            now <- liftIO getCurrentTime
            update [state_ =. state, updated_ =. Just now]
              (AutoKeyField ==. (fromId pid))
            mapM_ (\f -> notify $ f $ Just $ p
              { _processData_state = state
              , _processData_updated = Just now
              }) makeNotify

withNodeConfig :: NodeConfigFile -> (FilePath -> IO a) -> IO a
withNodeConfig nodeConfig f = withTempFile "." ".tezos-node-config.json" $ \nodeConfigPath nodeConfigHandle -> do
  (LBS.hPut nodeConfigHandle $ Aeson.encode nodeConfig)
  (hFlush nodeConfigHandle)
  f nodeConfigPath

-- TODO this code should be moved somewhere else
defaultConfig :: NodeConfigFile
defaultConfig = NodeConfigFile
  { _nodeConfigFile_p2p = NodeConfigP2P
    { _nodeConfigP2P_expectedProofOfWork = Nothing
    , _nodeConfigP2P_bootstrapPeers = Nothing
    , _nodeConfigP2P_listenAddr = Nothing
    , _nodeConfigP2P_privateMode = Nothing
    , _nodeConfigP2P_disableMempool = Nothing
  }
  , _nodeConfigFile_dataDir = Just "./.tezos-node"
  , _nodeConfigFile_rpc = Just NodeConfigRPC
    { _nodeConfigRPC_listenAddr = Just "127.0.0.1"
    , _nodeConfigRPC_corsOrigin = Nothing
    , _nodeConfigRPC_corsHeaders = Nothing
    , _nodeConfigRPC_crt = Nothing
    , _nodeConfigRPC_key = Nothing
    }
  , _nodeConfigFile_log = Nothing
  , _nodeConfigFile_shell = Nothing
  }

data NodeConfigRPC = NodeConfigRPC
  { _nodeConfigRPC_listenAddr :: !(Maybe Text)
  , _nodeConfigRPC_corsOrigin :: !(Maybe [Text])
  , _nodeConfigRPC_corsHeaders :: !(Maybe [Text])
  , _nodeConfigRPC_crt :: !(Maybe Text)
  , _nodeConfigRPC_key :: !(Maybe Text)
  }

data NodeConfigP2PLimits = NodeConfigP2PLimits
  { _nodeConfigP2PLimits_connectionTimeout :: !(Maybe Double)
  , _nodeConfigP2PLimits_authenticationTimeout :: !(Maybe Double)
  , _nodeConfigP2PLimits_minConnections :: !(Maybe Word16)
  , _nodeConfigP2PLimits_expectedConnections :: !(Maybe Word16)
  , _nodeConfigP2PLimits_maxConnections :: !(Maybe Word16)
  , _nodeConfigP2PLimits_backlog :: !(Maybe Word8)
  , _nodeConfigP2PLimits_maxIncomingConnections :: !(Maybe Word8)
  , _nodeConfigP2PLimits_maxDownloadSpeed :: !(Maybe Int)
  , _nodeConfigP2PLimits_maxUploadSpeed :: !(Maybe Int)
  , _nodeConfigP2PLimits_swapLinger :: !(Maybe Double)
  , _nodeConfigP2PLimits_binaryChunksSize :: !(Maybe Word8)
  , _nodeConfigP2PLimits_readBufferSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_readQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_writeQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_incomingAppMessageQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_incomingMessageQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_outgoingMessageQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_knownPointsHistorySize :: !(Maybe Word16)
  , _nodeConfigP2PLimits_knownPeerIdsHistorySize :: !(Maybe Word16)
  , _nodeConfigP2PLimits_maxKnownPoints :: !(Maybe (Int, Int))
  , _nodeConfigP2PLimits_maxKnownPeerIds :: !(Maybe (Int, Int))
  , _nodeConfigP2PLimits_greylistTimeout :: !(Maybe Int)
  }

data NodeConfigP2P = NodeConfigP2P
  { _nodeConfigP2P_expectedProofOfWork :: !(Maybe Double)
  , _nodeConfigP2P_bootstrapPeers :: !(Maybe [Text])
  , _nodeConfigP2P_listenAddr :: !(Maybe Text)
  , _nodeConfigP2P_privateMode :: !(Maybe Bool)
  , _nodeConfigP2P_disableMempool :: !(Maybe Bool)
  }
data NodeConfigLog = NodeConfigLog
  { _nodeConfigLog_output :: !(Maybe Text)
  , _nodeConfigLog_level :: !(Maybe Text)
  , _nodeConfigLog_rules :: !(Maybe Text)
  , _nodeConfigLog_template :: !(Maybe Text)
  }

data NodeConfigShell = NodeConfigShell
  { _nodeConfigShell_peerValidator :: !(Maybe NodeConfigShellPeerValidator)
  , _nodeConfigShell_blockValidator :: !(Maybe NodeConfigShellBlockValidator)
  , _nodeConfigShell_prevalidator :: !(Maybe NodeConfigShellPrevalidator)
  , _nodeConfigShell_chainValidator :: !(Maybe NodeConfigShellChainValidator)
  }

data NodeConfigShellPeerValidator = NodeConfigShellPeerValidator
  { _nodeConfigShellPeerValidator_blockHeaderRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_blockOperationsRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_protocolRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_newHeadRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_workerBacklogSize :: !(Maybe Word16)
  , _nodeConfigShellPeerValidator_workerBacklogLevel :: !(Maybe Text)
  , _nodeConfigShellPeerValidator_workerZombieLifetime :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_workerZombieMemory :: !(Maybe Double)
  }

data NodeConfigShellBlockValidator = NodeConfigShellBlockValidator
  { _nodeConfigShellBlockValidator_protocolRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellBlockValidator_workerBacklogSize :: !(Maybe Word16)
  , _nodeConfigShellBlockValidator_workerBacklogLevel :: !(Maybe Text)
  , _nodeConfigShellBlockValidator_workerZombieLifetime :: !(Maybe Double)
  , _nodeConfigShellBlockValidator_workerZombieMemory :: !(Maybe Double)
  }
data NodeConfigShellPrevalidator = NodeConfigShellPrevalidator
  { _nodeConfigShellPrevalidator_operationsRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPrevalidator_maxRefusedOperations :: !(Maybe Word16)
  , _nodeConfigShellPrevalidator_workerBacklogSize :: !(Maybe Word16)
  , _nodeConfigShellPrevalidator_workerBacklogLevel :: !(Maybe Text)
  , _nodeConfigShellPrevalidator_workerZombieLifetime :: !(Maybe Double)
  , _nodeConfigShellPrevalidator_workerZombieMemory :: !(Maybe Double)
  }
data NodeConfigShellChainValidator = NodeConfigShellChainValidator
  { _nodeConfigShellChainValidator_bootstrapThreshold :: !(Maybe Word8)
  , _nodeConfigShellChainValidator_workerBacklogSize :: !(Maybe Word16)
  , _nodeConfigShellChainValidator_workerBacklogLevel :: !(Maybe Text)
  , _nodeConfigShellChainValidator_workerZombieLifetime :: !(Maybe Double)
  , _nodeConfigShellChainValidator_workerZombieMemory :: !(Maybe Double)
  }

data NodeConfigFile = NodeConfigFile
  { _nodeConfigFile_p2p :: !NodeConfigP2P
  , _nodeConfigFile_dataDir :: !(Maybe FilePath)
  , _nodeConfigFile_rpc :: !(Maybe NodeConfigRPC)
  , _nodeConfigFile_log :: !(Maybe NodeConfigLog)
  , _nodeConfigFile_shell :: !(Maybe NodeConfigShell)
  }

concat <$> traverse (Aeson.deriveJSON tezosJsonOptions
  { Aeson.fieldLabelModifier
    = map (\case {'_' -> '-'; x -> x})
    . Aeson.fieldLabelModifier tezosJsonOptions
  , Aeson.omitNothingFields = True
    })
  [ ''NodeConfigFile
  , ''NodeConfigLog
  , ''NodeConfigP2P
  , ''NodeConfigRPC
  , ''NodeConfigShell
  , ''NodeConfigShellBlockValidator
  , ''NodeConfigShellChainValidator
  , ''NodeConfigShellPeerValidator
  , ''NodeConfigShellPrevalidator
  ]
