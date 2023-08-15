{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DoAndIfThenElse #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Process.Common where

import UnliftIO.Concurrent (forkIO)
import UnliftIO.Async (Concurrently(..), runConcurrently)
import UnliftIO.STM (TBQueue, atomically, isFullTBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Concurrent.STM (flushTBQueue)
import UnliftIO.Exception (bracket, finally, onException)
import Control.Monad.Logger (MonadLoggerIO, MonadLogger, logInfoNS, logInfoSH, logWarn, logWarnSH, logDebug)
import Control.Monad.IO.Unlift (MonadUnliftIO, withUnliftIO, unliftIO)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit (ConduitT, runConduit, (.|))
import qualified Data.Conduit.List as CL
import Data.Conduit.Process (CreateProcess, getStreamingProcessExitCode, streamingProcessHandleRaw, terminateProcess)
import Data.Streaming.Process (StreamingProcessHandle, streamingProcess)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (getCurrentTime, addUTCTime, NominalDiffTime)
import Data.Void (Void)
import Database.Id.Groundhog
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB.PsqlSimple (queryQ, fromOnly)
import Rhyolite.Backend.DB.Serializable
import System.Posix.Signals (signalProcess, sigKILL)
import qualified UnliftIO.Process as Proc
import System.Process (getPid)
import UnliftIO.Environment (getEnvironment)
import System.FilePath ((</>))
import UnliftIO.IO (IOMode(..), hFlush, withFile)

import Backend.Common
import Backend.Config
import Backend.Env
import Backend.NodeRPC (HasNodeDataSource)
import Backend.Process.Alerts
import Backend.Schema
import Common.App (DaemonType(..), daemonName)
import Common.Schema
import ExtraPrelude

import Orphans.Instances ()

-- | Updates the state of the process and performs the notify
-- if the notify function is provided.
updateProcessState
  :: ( MonadLogger m
     , PersistBackend m
     , MonadIO m
     )
  => Id ProcessData
  -> Maybe (Maybe ProcessData -> (NotifyTag n, n))
  -> ProcessState -> m ()
updateProcessState pid makeNotify state = do
  let state_   = ProcessData_stateField
      updated_ = ProcessData_updatedField
  $(logDebug) $
    "Update process state for process " <> tshow pid <> " to " <> tshow state
  get (fromId pid) >>= \case
    Nothing -> return ()
    Just p ->
      when (_processData_state p /= state) $ do
        now <- liftIO getCurrentTime
        update [state_ =. state, updated_ =. Just now]
          (AutoKeyField ==. fromId pid)
        for_ makeNotify $ \f -> do
          uncurry notify $ f $ Just $ p
            { _processData_state = state
            , _processData_updated = Just now
            }

withNodeConfig :: MonadUnliftIO m => AppConfig -> (FilePath -> m a) -> m a
withNodeConfig appConfig f = withFile (nodeDataDir appConfig </> "config.json") ReadWriteMode $ \nodeConfigHandle -> do
  liftIO $ LBS.hPut nodeConfigHandle $ either Aeson.encode Aeson.encode $ _appConfig_kilnNodeConfig appConfig
  hFlush nodeConfigHandle
  f $ nodeDataDir appConfig </> "config.json"

createProcessWithStreams
  :: MonadUnliftIO m
  => CreateProcess -> ConduitT () ByteString m () -> ConduitT ByteString Void m () -> ConduitT ByteString Void m ()
  -> m StreamingProcessHandle
createProcessWithStreams cp producerStdin consumerStdout consumerStderr = withUnliftIO $ \u -> do
  ((sinkStdin, closeStdin) , (sourceStdout, closeStdout), (sourceStderr, closeStderr), sph) <- streamingProcess cp
  void $ forkIO $ void $ runConcurrently (
      (,,)
      <$> Concurrently (unliftIO u $ runConduit $ producerStdin .| sinkStdin)
      <*> Concurrently (unliftIO u $ runConduit $ sourceStdout .| consumerStdout)
      <*> Concurrently (unliftIO u $ runConduit $ sourceStderr .| consumerStderr))
    `finally` (closeStdin >> closeStdout >> closeStderr)
    `onException` (liftIO . terminateProcess . streamingProcessHandleRaw) sph
  return sph

waitUntilShouldRun
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => Id ProcessData
  -> m Bool
  -> m ()
waitUntilShouldRun pid runPrestartCheck = do
  canRun <- runTransaction $ do
    isStopped <- all (== ProcessControl_Stop) <$> project ProcessData_controlField (AutoKeyField ==. fromId pid)
    -- We don't restart process with non-empty error log. It means that this process just failed with error.
    -- We guarantee this condition by the fact that in all other cases we clean the log.
    hasEmptyErrorLog <- fmap (isNothing . head) $ project ProcessData_errorLogField $ AutoKeyField ==. fromId pid
    pure $ not isStopped && hasEmptyErrorLog
  prestartCheck <- runPrestartCheck
  unless (canRun && prestartCheck) $ threadDelay' 1 *> waitUntilShouldRun pid runPrestartCheck

withProcessLock
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => Id ProcessData
  -> m ()
  -> m ()
withProcessLock pid act = bracket (obtainLock pid) (freeLock pid) $ \_ -> act

-- | Obtain lock with finalizer, delay 1s
-- If the ProcessData.backend is not null/Nothing then it could mean that
-- the previous worker did not exit cleanly, or the kiln process died without doing
-- a clean termination of the node, or there is another kiln process running this daemon.
-- So we wait for 30s from the updated time before starting the daemon again
-- We keep this value "updated" when we are running daemon.
obtainLock
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => Id ProcessData
  -> m ()
obtainLock pid = do
  lockId <- runTransaction $
    [queryQ| SELECT nextval('"ProcessLockUniqueId"') |] <&> fromOnly . head
  $(logDebug) $ "Obtaining lock for process: " <> tshow pid <> ", lockId: " <> tshow lockId
  claimLock pid lockId

claimLock
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => Id ProcessData
  -> Int
  -> m ()
claimLock pid lockId = do
  now <- liftIO getCurrentTime
  let nowMinus30Sec = addUTCTime (-30) now
  pd <- runTransaction $ do
    update
      [ ProcessData_stateField   =. ProcessState_Stopped
      , ProcessData_updatedField =. Just now
      , ProcessData_backendField =. Just lockId
      ] $
      (AutoKeyField ==. fromId pid) &&. (
        ProcessData_backendField ==. (Nothing :: Maybe Int) ||.
        ProcessData_updatedField <. Just nowMinus30Sec
      )
    project ProcessData_backendField (AutoKeyField ==. fromId pid)
  case pd of
    [] -> error $ "ProcessData with id " <> show pid <> "not found in DB"
    (lockId':_) -> do
      unless (Just lockId == lockId') $ do
        $(logWarn) $ "Could not acquire lock for process with id " <> tshow pid
        threadDelay' 3 *> claimLock pid lockId

freeLock
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => Id ProcessData
  -> ()
  -> m ()
freeLock pid _ = do
  $(logDebug) $ "Freeing lock for process: " <> tshow pid
  now <- liftIO getCurrentTime
  runTransaction $
    update
      [ ProcessData_updatedField =. Just now
      , ProcessData_backendField =. (Nothing :: Maybe Int)
      ] (AutoKeyField ==. fromId pid)

startProcMonitor
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadBaseNoPureAborts IO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => CreateProcess
  -> [(String, String)]
  -> DaemonType
  -> Id ProcessData
  -> (ProcessState -> Serializable ())
  -> m ()
startProcMonitor procHandler env daemonType pid updateState = do
  currentEnv <- getEnvironment
  let proc' = procHandler
          { Proc.std_out = Proc.CreatePipe
          , Proc.std_err = Proc.CreatePipe
          , Proc.env = Just $ currentEnv <> env
          }
  $(logInfoSH) ("processWorker: running process" :: Text, proc')
  procMonitor proc' daemonType pid updateState
  threadDelay' 10


-- | "Monitor/Stop loop", delay 1s
-- it waits for the process' stop signal (_processData_running == False) and terminates it
-- Also monitors if the process terminates unexpectedly.
procMonitor
  :: ( MonadUnliftIO m
     , MonadBaseNoPureAborts IO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasNodeDataSource e
     , HasAppConfig e
     )
  => CreateProcess
  -> DaemonType
  -> Id ProcessData
  -> (ProcessState -> Serializable ())
  -> m ()
procMonitor cp daemonType pid updateState = do
  let namespace = daemonLogNamespace daemonType
      errorLogBufferSize = 30
  -- Buffer containing the last few lines of stderr.
  -- Needed to correctly display the error message in case of a process fail.
  errorLogBuffer <- newTBQueueIO @_ @Text errorLogBufferSize

  ph <- createProcessWithStreams cp (return ())
    (CL.mapM_ $ logInfoNS namespace . decodeUtf8)
    (CL.mapM_ $ \line ->
      let decodedLine = decodeUtf8 line in
          logInfoNS namespace decodedLine *> writeBuffer decodedLine errorLogBuffer)
  go Nothing errorLogBuffer ph
  where
    writeBuffer :: MonadUnliftIO m => Text -> TBQueue Text -> m ()
    writeBuffer line buffer = liftIO $ atomically $ do
      isFull <- isFullTBQueue buffer
      when isFull $
        void $ readTBQueue buffer
      writeTBQueue buffer line

    {-# INLINE go #-}
    go (mCount :: Maybe Int) buffer ph = do
      let getPC = \case
            [] -> ProcessControl_Stop
            (c:_) -> c
      procControl <- runTransaction
        (getPC <$> project ProcessData_controlField (AutoKeyField ==. fromId pid))
      liftIO (getStreamingProcessExitCode ph) >>= \case
        Nothing -> do
          runTransaction $ updateState ProcessState_Running
          let
            stop = procControl /= ProcessControl_Run
            timeoutInSec = 60 :: Int
            delayInSec = 1 :: NominalDiffTime
            rawPh = streamingProcessHandleRaw ph
          liftIO $ when stop $ do
            if mCount < Just (ceiling $ fromIntegral timeoutInSec / delayInSec)
              then terminateProcess rawPh
              else getPid rawPh >>= traverse_ (signalProcess sigKILL)
          threadDelay' delayInSec *> go (if stop then Just (maybe 1 (+ 1) mCount) else Nothing) buffer ph
        Just _ -> case procControl of
          ProcessControl_Stop -> do
            runTransaction $ updateState ProcessState_Stopped
            $(logInfoSH) ("Process exited successfully:" :: Text, pid)
          ProcessControl_Restart -> do
            runTransaction $ do
              updateState ProcessState_Stopped
              update [ProcessData_controlField =. ProcessControl_Run] (AutoKeyField ==. fromId pid)
            $(logInfoSH) ("Process exited successfully, restarting:" :: Text, pid)
          ProcessControl_Run -> do
            appConfig <- askAppConfig
            runTransaction $ do
              updateState ProcessState_Failed
              errorLog <- fmap T.unlines $ liftIO $ atomically $ flushTBQueue buffer
              update [ProcessData_errorLogField =. Just errorLog] $
                AutoKeyField ==. fromId pid
              flip runReaderT appConfig $ queueFailedProcessAlert (daemonName daemonType) errorLog
            $(logWarnSH) ("Process exited unexpectedly:" :: Text, pid)

daemonLogNamespace :: DaemonType -> Text
daemonLogNamespace = \case
  DaemonType_Node -> "kiln-node"
  DaemonType_Baker -> "kiln-baker"
