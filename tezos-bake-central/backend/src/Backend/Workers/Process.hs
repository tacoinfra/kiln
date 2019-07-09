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

{-# OPTIONS_GHC -Wall -Werror #-}

-- Kiln managed process/daemon
module Backend.Workers.Process where

import Control.Concurrent.Async (withAsync)
import Control.Exception.Safe (tryJust, throwIO)
import Control.Monad.Catch (bracket, catch)
import Control.Monad.Logger (MonadLogger, LoggingT, logDebugSH, logInfoNS, logInfoSH, logWarn, logWarnSH)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool)
import qualified Data.Text as T
import Data.Time (getCurrentTime, addUTCTime, NominalDiffTime)
import Database.Groundhog.Postgresql
import Named
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.DB.PsqlSimple (queryQ, fromOnly)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Rhyolite.Backend.Schema (fromId)
import System.Posix.Signals (signalProcess, sigKILL)
import System.Process (CreateProcess, withCreateProcess, getProcessExitCode, terminateProcess)
import qualified System.Process as Proc
import System.Exit (ExitCode(..))
import System.IO (hFlush, hGetLine)
import System.IO.Error (isEOFError)
import System.IO.Temp (withTempFile)

import Backend.Common
import Backend.Config
import Backend.Schema
import Common.Schema
import ExtraPrelude

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
  :: (MonadIO m)
  => (    "db" :! Pool Postgresql
       -> "updateState" :! (ProcessState -> IO ())
       -> "configFile" :! FilePath
       -> IO a
     )
  -> "logger" :! LoggingEnv
  -> "db" :! Pool Postgresql
  -> "config" :! AppConfig
  -> "logNamespace" :! Text
  -> "mkProcess" :! (a -> FilePath -> CreateProcess)
  -> "pid" :! Id ProcessData
  -> "pidToRunAfter" :! Maybe (Id ProcessData)
  -> "mkNotify" :! Maybe (Maybe ProcessData -> (NotifyTag n, n))
  -> m (IO ())
processWorker initialize (Arg logger) (Arg db) (Arg appConfig) (Arg namespace) (Arg mkProcess) (Arg pid) (Arg pidToRunAfter) (Arg makeNotify) = worker' $ do
  waitUntilShouldRun
  bracket obtainLock freeLock $ \_ -> do
    inDb $ updateState ProcessState_Initializing
    withNodeConfig appConfig $ \configFile -> do
      let
        initF = initialize ! #db db ! #updateState (\ps -> inDb $ updateState ps) ! #configFile configFile
      v <- catch initF $ \e -> do
        inDb $ do
          update [control_ =. ProcessControl_Stop] (AutoKeyField ==. fromId pid)
          updateState ProcessState_Failed
        throwIO (e :: ExitCode)
      inDb $ updateState ProcessState_Starting
      let procSpec = (mkProcess v configFile)
            { Proc.std_out = Proc.CreatePipe
            , Proc.std_err = Proc.CreatePipe
            }
      withCreateProcess procSpec procMonitor
    threadDelay' 10
  where
    inDb :: (MonadIO m, MonadBaseNoPureAborts IO m) => DbPersist Postgresql (LoggingT m) a -> m a
    inDb = runLoggingEnv logger . runDb (Identity db)
    updateState :: (MonadLogger m, PersistBackend m, MonadIO m) => ProcessState -> m ()
    updateState = updateProcessState pid makeNotify
    state_ = ProcessData_stateField
    updated_ = ProcessData_updatedField
    backend_ = ProcessData_backendField
    control_ = ProcessData_controlField
    waitUntilShouldRun = do
      canRun <- runLoggingEnv logger $ runDb (Identity db) $ do
        isStopped <- all (== ProcessControl_Stop) <$> project control_ (AutoKeyField ==. fromId pid)
        otherProcessRunning <- case pidToRunAfter of
          Nothing -> pure True
          Just pid1 -> all (== ProcessState_Running) <$> project state_ (AutoKeyField ==. fromId pid1)
        pure $ (not isStopped) && otherProcessRunning
      unless canRun $ threadDelay' 1 *> waitUntilShouldRun

    obtainLock = runLoggingEnv logger $ do
      lockId :: Int <- runDb (Identity db) $
        [queryQ| SELECT nextval('"ProcessLockUniqueId"') |] <&> fromOnly . head
      $(logDebugSH) ("Obtaining lock for process:" :: Text, pid, ", LockId:" :: Text, lockId)
      let
        state = ProcessState_Stopped
        {-# INLINE claim #-}
        claim = do
          now <- liftIO getCurrentTime
          let nowMinus5min = addUTCTime (-60 * 5) now
          pd <- runDb (Identity db) $ do
            update [state_ =. state, updated_ =. Just now, backend_ =. Just lockId]
              ((AutoKeyField ==. fromId pid)
               &&. (backend_ ==. (Nothing :: Maybe Int) ||. updated_ <. Just nowMinus5min))
            project backend_ (AutoKeyField ==. fromId pid)
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
      now <- liftIO getCurrentTime
      void $ runDb (Identity db) $
        update [updated_ =. Just now, backend_ =. (Nothing :: Maybe Int)]
          (AutoKeyField ==. fromId pid)

    procMonitor _stdin hStdout hStderr ph = do
      withHandleCopyWith (logInfoNS namespace) hStdout $ do
        -- logInfoNS for stderr is intentional, the node prints the usual messages also on stderr
        withHandleCopyWith (logInfoNS namespace) hStderr $ do
          runLoggingEnv logger $ go Nothing
      where
        withHandleCopyWith perLine h' f = case h' of
          Nothing -> f
          Just h -> withAsync forEachHandleLine $ const f
            where
              forEachHandleLine = runLoggingEnv logger $
                fix $ \loop -> do
                  liftIO (tryJust (guard . isEOFError) (hGetLine h)) >>= \case
                    Left _ -> pure ()
                    Right ln -> perLine (T.pack ln) *> loop

        {-# INLINE go #-}
        go :: forall m1. (MonadLogger m1, MonadIO m1, MonadBaseNoPureAborts IO m1) => Maybe Int -> m1 ()
        go mCount = do
          let getPC = \case
                [] -> ProcessControl_Stop
                (c:_) -> c
          procControl <- runDb (Identity db)
            (getPC <$> project control_ (AutoKeyField ==. fromId pid))
          liftIO (getProcessExitCode ph) >>= \case
            Nothing -> do
              inDb $ updateState ProcessState_Running
              let
                stop = procControl /= ProcessControl_Run
                timeoutInSec = 60 :: Int
                delayInSec = 1 :: NominalDiffTime
              liftIO $ when stop $ if mCount < Just (ceiling $ (fromIntegral timeoutInSec) / delayInSec)
                then terminateProcess ph
                else Proc.getPid ph >>= traverse_ (signalProcess sigKILL)
              threadDelay' delayInSec *> go (if stop then Just (maybe 1 (+ 1) mCount) else Nothing)
            Just _ -> case procControl of
              ProcessControl_Stop -> do
                inDb $ updateState ProcessState_Stopped
                $(logInfoSH) ("Process exited successfully:" :: Text, pid)
              ProcessControl_Restart -> do
                inDb $ do
                  updateState ProcessState_Stopped
                  update [control_ =. ProcessControl_Run] (AutoKeyField ==. fromId pid)
                $(logInfoSH) ("Process exited successfully, restarting:" :: Text, pid)
              ProcessControl_Run -> do
                inDb $ updateState ProcessState_Failed
                $(logWarnSH) ("Process exited unexpectedly:" :: Text, pid)

updateProcessState
  :: ( MonadLogger m
     , PersistBackend m
     , MonadIO m
     )
  => Id ProcessData
  -> Maybe (Maybe ProcessData -> (NotifyTag n, n))
  -> ProcessState -> m ()
updateProcessState pid makeNotify state = do
  let
    state_ = ProcessData_stateField
    updated_ = ProcessData_updatedField
  $(logDebugSH) ("putState:" :: Text, pid, state)
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

withNodeConfig :: AppConfig -> (FilePath -> IO a) -> IO a
withNodeConfig appConfig f = withTempFile (_appConfig_kilnDataDir appConfig) ".tezos-node-config.json" $ \nodeConfigPath nodeConfigHandle -> do
  LBS.hPut nodeConfigHandle $ Aeson.encode $ _appConfig_kilnNodeConfig appConfig
  hFlush nodeConfigHandle
  f nodeConfigPath
