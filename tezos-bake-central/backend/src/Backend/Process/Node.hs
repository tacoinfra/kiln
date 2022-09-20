{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoDoAndIfThenElse #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Backend.Process.Node where

import Control.Exception.Safe (throwIO, tryJust)
import Control.Monad.Logger (MonadLogger, logInfoNS, logDebug, logWarn, logError, logErrorNS)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder as Builder
import Data.Dependent.Sum (DSum (..))
import qualified Data.HashMap.Lazy as HashMap
import Data.Pool (Pool)
import Data.List (isInfixOf)
import Data.Version
import Database.Groundhog.Postgresql
import Named
import Rhyolite.Backend.DB (MonadBaseNoPureAborts, runDb, project1)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Snap.Core (addToOutput, MonadSnap)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removePathForcibly)
import System.Exit (ExitCode(..))
import qualified System.FilePath as FilePath
import System.Process as Proc
import System.IO (hGetContents)
import System.IO.Error (isEOFError)
import qualified System.IO.Streams as Streams
import System.Which (staticWhich)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Backend.Config (AppConfig (..), nodeDataDir, BinaryPaths(..))
import Backend.NodeRPC
import Backend.Schema
import Backend.Workers.Process
import Common.Route (ExportLog(..))
import Common.Schema
import ExtraPrelude

needsCarthageStorageUpgrade :: Version -> Bool
needsCarthageStorageUpgrade = (< Version [0,0,4] [])

nixNodePath :: FilePath
nixNodePath = $(staticWhich "tezos-node")

internalNodeWorker :: (MonadIO m, MonadBaseNoPureAborts IO m)
  => AppConfig -> LoggingEnv -> Pool Postgresql -> Maybe BinaryPaths -> m (IO ())
internalNodeWorker appConfig logger db maybePaths = do
  -- Always create a NodeInternal and corresponsing ProcessData
  (nid, pid) <- runLoggingEnv logger $ runDb (Identity db) $ do
    project1 (NodeInternal_idField, NodeInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
      (Just v) -> return v
      Nothing -> do
        let processData = ProcessData
              { _processData_control = ProcessControl_Stop
              , _processData_state = ProcessState_Stopped
              , _processData_updated = Nothing
              , _processData_backend = Nothing
              , _processData_errorLog = Nothing
              }

        pid <- insert' processData
        nid <- insert' Node
        insert $ NodeInternal
          { _nodeInternal_id = nid
          , _nodeInternal_data = DeletableRow
            { _deletableRow_data = pid
            , _deletableRow_deleted = True
            }
          }
        return (nid, pid)

  let
    nodePath = maybe nixNodePath _binaryPaths_nodePath maybePaths
    nodeRpcPort = show $ _appConfig_kilnNodeRpcPort appConfig
    nodeNetPort = show $ _appConfig_kilnNodeNetPort appConfig
    nodeExtraArgs = maybe [] (words . T.unpack) $ _appConfig_kilnNodeCustomArgs appConfig
    -- use the user supplied config file if specified
    -- we can only specify this option once
    hasUserConfigFile = "--config-file" `elem` nodeExtraArgs
    nodeArgs configPath dataDir = [ "run" ]
      ++ (if hasUserConfigFile then [] else [ "--config-file", configPath]) ++
      [
        "--data-dir", dataDir,
        "--rpc-addr", "127.0.0.1:" <> nodeRpcPort,
        "--net-addr", "0.0.0.0:" <> nodeNetPort
      ]
      ++ nodeExtraArgs
  liftIO $ createDirectoryIfMissing True (nodeDataDir appConfig)
  processWorker
    (\updateState -> withNodeConfig appConfig $ \nodeConfigPath ->
      initNode ! #logger logger ! #config appConfig ! #nodePath nodePath ! #configFile nodeConfigPath ! #db db ! #updateState updateState
    )
    ! #logger logger
    ! #db db
    ! #config appConfig
    ! #logNamespace "kiln-node"
    ! #mkProcess (\(dataDir, extraArgs) -> withNodeConfig appConfig $ \nodeConfigPath ->
                    return $ Right $ proc nodePath (nodeArgs nodeConfigPath dataDir ++ extraArgs))
    ! #pid pid
    ! #prestartCheck (pure True)
    ! #mkNotify (Just (\pd -> (NotifyTag_NodeInternal, (nid, pd))))
    ! #jsonErrorLogsHandler Nothing

getKilnNodeVersion :: MonadIO m => FilePath -> m (Maybe Version)
getKilnNodeVersion versionFile = liftIO $ do
  vf <- LBS.readFile versionFile
  let parse :: Text -> Maybe Version
      parse = Aeson.decode . LBS.fromStrict . T.encodeUtf8 . tshow
  pure $ parse =<< HashMap.lookup ("version" :: Text) =<< Aeson.decode vf

initNode
  :: "logger" :! LoggingEnv
  -> "config" :! AppConfig
  -> "nodePath" :! FilePath
  -> "configFile" :! FilePath
  -> "db" :! Pool Postgresql
  -> "updateState" :! (ProcessState -> IO ())
  -> IO (FilePath, [String])
initNode (Arg logger) (Arg appConfig) (Arg nodePath) (Arg nodeConfigPath) _ (Arg updateState) = runLoggingEnv logger $ do
  let dataDir = nodeDataDir appConfig
  let identityFile = dataDir `FilePath.combine` "identity.json"
      versionFile  = dataDir `FilePath.combine` "version.json"
      storeFolder  = dataDir `FilePath.combine` "store"

  versionFileExists <- liftIO $ doesFileExist versionFile
  mVersion <- if not versionFileExists then pure Nothing else getKilnNodeVersion versionFile
  when (versionFileExists && maybe True needsCarthageStorageUpgrade mVersion) $ liftIO $ do
    throwIO InternalNodeFailureReason_CarthageUpgrade
  identityFileExists <- liftIO $ doesFileExist identityFile
  unless identityFileExists $ do
    -- Generate Identity
    lift $ updateState (ProcessState_Node NodeProcessState_GeneratingIdentity)
    runCommandWithLogging nodePath ["identity", "generate", "--config-file", T.pack nodeConfigPath, "--data-dir", T.pack dataDir]
  storeExists <- liftIO $ doesDirectoryExist storeFolder
  -- If there is some data in the storage, we try to upgrade it in case upgrade
  -- is required
  when storeExists $ do
    -- In case node storage is up to date this is essentially a no-op
    (exitCode, out', err') <- liftIO $ readProcessWithExitCode nodePath
      ["upgrade", "--data-dir", dataDir, "--config-file", nodeConfigPath, "storage"] ""
    -- Currently there is no nice way to check whether upgrade was successful, see
    -- https://gitlab.com/tezos/tezos/-/issues/1687.
    -- However, we still do this check and hope that the aformentioned issue
    -- will be resolved in the future release.
    case exitCode of
      ExitSuccess ->
        unless ("node dir is up-to-date" `isInfixOf` out') $ do
          liftIO $ removePathForcibly $ dataDir `FilePath.combine` "lmdb_store_to_remove"
          logInfoNS "kiln-node" "Kiln node storage was successfully upgraded"
      _ -> do
        logErrorNS "kiln-node" $ "Kiln node storage upgrade failed with: " <> T.pack err'
        liftIO $ throwIO exitCode
  let useArchiveMode = False
      extraArgs = if useArchiveMode
        then ["--history-mode", "archive"]
        else []
  return (dataDir, extraArgs)
  where
    runCommandWithLogging :: (MonadLogger m, MonadIO m) => FilePath -> [Text] -> m ()
    runCommandWithLogging cmd args = do
      (exitCode, out', err') <- liftIO (readProcessWithExitCode cmd (T.unpack <$> args) "")
      let
        out = T.pack out'
        err = T.pack err'
      if exitCode == ExitSuccess
        then do
          logInfoNS "kiln-node" $ "Got output from : " <> T.pack cmd <> " " <> tshow args <> " --> " <> out
        else do
          logErrorNS "kiln-node" $ "Command Failed : (stdout): " <> T.pack cmd <> " " <> tshow args <> "\n<STDOUT>\n" <> out <> "\n<STDERR>\n" <> err
          liftIO $ throwIO exitCode

handleExportLogs :: MonadSnap m => NodeDataSource -> DSum ExportLog Identity -> m ()
handleExportLogs nds lType = do
  let
    logger = _nodeDataSource_logger nds
    logIdentifier :: String
    logIdentifier = "kiln-" <> case lType of
      ExportLog_Baker :=> _ -> "baker"
      ExportLog_Node :=> _ -> "node"
    command = (shell $ unwords
      [ "journalctl"
      , "--no-hostname"
      , "--no-pager"
      , "-t", logIdentifier
      ])
      { Proc.std_out = Proc.CreatePipe
      , Proc.std_err = Proc.CreatePipe
      }

  runLoggingEnv logger $ do
    $(logDebug) $ "Exporting logs for: " <> T.pack logIdentifier
      <> "\nRunning command :" <> tshow command
  addToOutput $ \str -> do
    withCreateProcess command $ \_ mStdout mStderr ph -> for_ mStdout $ \stdout -> do
      iStr <- Streams.handleToInputStream stdout
      iStr1 <- Streams.map Builder.byteString iStr
      Streams.connect iStr1 str
      waitForProcess ph >>= runLoggingEnv logger . \case
        ExitSuccess -> $(logDebug) "Exported logs successfully"
        ExitFailure code -> do
          $(logWarn) $ "Error in exporting logs: journalctl returned: " <> tshow code
          for_ mStderr $ \stderr -> liftIO (tryJust (guard . isEOFError) (hGetContents stderr)) >>= \case
            Left _ -> pure ()
            Right c -> $(logError) $ "Stderr: " <> T.pack c
    pure str
