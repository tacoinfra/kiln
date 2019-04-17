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
import Data.Pool (Pool)
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.DB.PsqlSimple (queryQ, fromOnly)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Rhyolite.Backend.Schema (fromId)
import System.Process (CreateProcess, withCreateProcess, getProcessExitCode, terminateProcess)
import System.IO (hFlush)
import System.IO.Temp (withTempFile)

import Data.Time (getCurrentTime, addUTCTime)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS

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
  :: ( MonadIO m
     )
  => LoggingEnv
  -> Pool Postgresql
  -> AppConfig
  -> (forall m'. (Monad m', MonadIO m', MonadLogger m', MonadBaseNoPureAborts IO m') => Pool Postgresql -> (ProcessState -> m' ()) -> FilePath -> m' a)
  -> (a -> FilePath -> CreateProcess)
  -> Id ProcessData
  -> Maybe (Maybe ProcessData -> (NotifyTag n, n))
  -> m (IO ())
processWorker logger db appConfig initialize process pid makeNotify = worker' $ do
  waitUntilShouldRun
  bracket obtainLock freeLock $ \_ -> do
    updateState ProcessState_Initializing
    withNodeConfig appConfig $ \configFile -> do
      v <- runLoggingEnv logger $ initialize db updateState configFile
      updateState ProcessState_Starting
      withCreateProcess (process v configFile) procMonitor
    threadDelay' 10
  where
    state_ = ProcessData_stateField
    updated_ = ProcessData_updatedField
    backend_ = ProcessData_backendField
    control_ = ProcessData_controlField
    waitUntilShouldRun = do
      isStopped <- runLoggingEnv logger $ runDb (Identity db) $
        all (== ProcessControl_Stop) <$> project control_ (AutoKeyField ==. fromId pid)
      when isStopped $ threadDelay' 1 >> waitUntilShouldRun

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
        go :: forall m1. (MonadLogger m1, MonadIO m1, MonadBaseNoPureAborts IO m1) => m1 ()
        go = do
          let getPC = \case
                [] -> ProcessControl_Stop
                (c:_) -> c
          procControl <- runDb (Identity db)
            (getPC <$> project control_ (AutoKeyField ==. (fromId pid)))
          (liftIO $ getProcessExitCode ph) >>= \case
            Nothing -> do
              updateState ProcessState_Running
              case procControl of
                ProcessControl_Run -> return ()
                _ -> liftIO $ terminateProcess ph
              (threadDelay' 1) *> go
            Just _ -> case procControl of
              ProcessControl_Stop ->
                $(logInfoSH) ("Process exited successfully:" :: Text, pid)
              ProcessControl_Restart -> do
                $(logInfoSH) ("Process exited successfully, restarting:" :: Text, pid)
                runDb (Identity db) $ update [control_ =. ProcessControl_Run] (AutoKeyField ==. (fromId pid))
              ProcessControl_Run ->
                $(logWarnSH) ("Process exited unexpectedly:" :: Text, pid)

    updateState :: (MonadIO m, MonadBaseNoPureAborts IO m) => ProcessState -> m ()
    updateState state = runLoggingEnv logger $ runDb (Identity db) $ do
      $(logDebugSH) ("putState:" :: Text, pid, state)
      get (fromId pid) >>= \case
        Nothing -> return ()
        Just p ->
          when (_processData_state p /= state) $ do
            now <- liftIO getCurrentTime
            update [state_ =. state, updated_ =. Just now]
              (AutoKeyField ==. (fromId pid))
            for_ makeNotify $ \f -> do
              uncurry notify $ f $ Just $ p
                    { _processData_state = state
                    , _processData_updated = Just now
                    }

withNodeConfig :: AppConfig -> (FilePath -> IO a) -> IO a
withNodeConfig appConfig f = withTempFile (_appConfig_kilnDataDir appConfig) ".tezos-node-config.json" $ \nodeConfigPath nodeConfigHandle -> do
  (LBS.hPut nodeConfigHandle $ Aeson.encode $ _appConfig_kilnNodeConfig appConfig)
  (hFlush nodeConfigHandle)
  f nodeConfigPath
