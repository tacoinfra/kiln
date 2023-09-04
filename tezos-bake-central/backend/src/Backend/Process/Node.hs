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

import UnliftIO.Exception (Handler (..), catches, throwIO, tryJust)
import Control.Applicative (optional)
import Control.Exception.Safe (MonadThrow, throwString)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, logInfoNS, logDebug, logWarn, logError, logErrorNS)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as P
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder as Builder
import Data.Dependent.Sum (DSum (..))
import qualified Data.HashMap.Lazy as HashMap
import Data.List (isInfixOf)
import Data.Version
import Database.Id.Groundhog (fromId)
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB (MonadBaseNoPureAborts, project1)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Snap.Core (addToOutput, MonadSnap)
import UnliftIO.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removePathForcibly)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import UnliftIO.Process as Proc
import System.IO (hGetContents)
import System.IO.Error (isEOFError)
import qualified System.IO.Streams as Streams
import System.Which (staticWhich)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Backend.Alerts (reportInternalNodeFailed)
import Backend.Common.Worker (worker')
import Backend.Config
import Backend.Env
import Backend.NodeRPC
import Backend.Process.Common
import Backend.Schema
import Common.App (DaemonType(..))
import Common.Route (ExportLog(..))
import Common.Schema
import ExtraPrelude

needsCarthageStorageUpgrade :: Version -> Bool
needsCarthageStorageUpgrade = (< Version [0,0,4] [])

nixNodePath :: FilePath
nixNodePath = $(staticWhich "tezos-node")

internalNodeWorker
  :: ( MonadUnliftIO w
     , MonadUnliftIO m
     , MonadBaseNoPureAborts IO m
     )
  => AppConfig
  -> NodeDataSource
  -> Maybe BinaryPaths
  -> m (w ())
internalNodeWorker appConfig nds maybePaths = runLoggerWithEnv $ do
  -- Always create a NodeInternal and corresponsing ProcessData
  (nid, pid) <- runTransaction $ do
    project1 (NodeInternal_idField, NodeInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
      (Just v) -> return v
      Nothing -> do
        let processData = ProcessData
              { _processData_control = ProcessControl_Stop
              , _processData_state = ProcessState_Stopped
              , _processData_updated = Nothing
              , _processData_backend = Nothing
              , _processData_errorLog = Nothing
              , _processData_restartCount = 0
              , _processData_restartAt = Nothing
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
  nodeDirPath <- getKilnNodeDataDir
  liftIO $ createDirectoryIfMissing True nodeDirPath

  mkWorker $ do
    let runPrestartCheck = pure True -- We don't have a prestart check for the node process
        updateState = updateNodeProcessState pid nid
        initFailed = do
          update
            [ ProcessData_controlField =. ProcessControl_Stop
            ] (AutoKeyField ==. fromId pid)
          updateState ProcessState_Failed
    waitUntilShouldRun pid DaemonType_Node runPrestartCheck
    withProcessLock pid $ do
      runTransaction $ updateState ProcessState_Initializing
      let
        initNodeProcess = withNodeConfig appConfig $ \nodeConfigPath ->
          initNode nodePath nodeConfigPath nid pid
      dataDir <- catches initNodeProcess
        [ Handler $ \(e :: InternalNodeFailureReason) ->
            runTransaction (initFailed *> runReaderT (reportInternalNodeFailed pid e) appConfig) *> throwIO e
        , Handler $ \(e :: ExitCode) -> runTransaction initFailed *> throwIO e
        ]
      runTransaction $ updateState ProcessState_Starting
      procHandler <- withNodeConfig appConfig $ \nodeConfigPath ->
        pure $ proc nodePath (nodeArgs nodeConfigPath dataDir)
      startProcMonitor procHandler [] DaemonType_Node pid updateState
  where
    mkWorker act = worker' "nodeProcessWorker" $
      flip runReaderT (KilnEnv appConfig nds) $ runLogger act
    runLoggerWithEnv act = flip runReaderT (KilnEnv appConfig nds) $ runLogger act

getKilnNodeVersionFromVersionFile :: MonadIO m => FilePath -> m (Maybe Version)
getKilnNodeVersionFromVersionFile versionFile = liftIO $ do
  vf <- LBS.readFile versionFile
  let parse :: Text -> Maybe Version
      parse = Aeson.decode . LBS.fromStrict . T.encodeUtf8 . tshow
  pure $ parse =<< HashMap.lookup ("version" :: Text) =<< Aeson.decode vf

initNode
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasNodeDataSource e
     , HasAppConfig e
     )
  => FilePath
  -> FilePath
  -> Id Node
  -> Id ProcessData
  -> m FilePath
initNode nodePath nodeConfigPath nodeId pid = do
  dataDir <- getKilnNodeDataDir
  let identityFile = dataDir </> "identity.json"
      versionFile  = dataDir </> "version.json"
      storeFolder  = dataDir </> "store"

  versionFileExists <- liftIO $ doesFileExist versionFile
  mVersion <- if not versionFileExists then pure Nothing else getKilnNodeVersionFromVersionFile versionFile
  when (versionFileExists && maybe True needsCarthageStorageUpgrade mVersion) $ liftIO $ do
    throwIO InternalNodeFailureReason_CarthageUpgrade
  identityFileExists <- liftIO $ doesFileExist identityFile
  unless identityFileExists $ do
    -- Generate Identity
    runTransaction $ updateNodeProcessState pid nodeId (ProcessState_Node NodeProcessState_GeneratingIdentity)
    runCommandWithLogging nodePath ["identity", "generate", "--config-file", T.pack nodeConfigPath, "--data-dir", T.pack dataDir]
  storeExists <- liftIO $ doesDirectoryExist storeFolder
  -- If there is some data in the storage, we try to upgrade it in case upgrade
  -- is required
  when storeExists $ do
    -- In case node storage is up to date this is essentially a no-op
    (exitCode, out', err') <- liftIO $ readProcessWithExitCode nodePath
      ["upgrade", "--data-dir", dataDir, "--config-file", nodeConfigPath, "storage"] ""
    case exitCode of
      ExitSuccess ->
        unless ("node dir is up-to-date" `isInfixOf` out') $ do
          liftIO $ removePathForcibly $ dataDir </> "lmdb_store_to_remove"
          logInfoNS "kiln-node" "Kiln node storage was successfully upgraded"
      _ -> do
        logErrorNS "kiln-node" $ "Kiln node storage upgrade failed with: " <> T.pack err'
        liftIO $ throwIO exitCode
  return dataDir
  where
    runCommandWithLogging :: (MonadLogger m, MonadIO m) => FilePath -> [Text] -> m ()
    runCommandWithLogging cmd args = do
      (exitCode, out', err') <- liftIO (readProcessWithExitCode cmd (T.unpack <$> args) "")
      let
        out = T.pack out'
        err = T.pack err'
      case exitCode of
        ExitSuccess ->
          logInfoNS "kiln-node" $ T.concat
            [ "Got output from "
            , fullCmdText
            , ":\n"
            , out
            ]
        ExitFailure ec -> do
          logErrorNS "kiln-node" $ T.concat
            [ fullCmdText
            , " failed with exit code "
            , tshow ec
            , "\nstdout:\n"
            , out
            , "\nstderr:\n"
            , err
            ]
          liftIO $ throwIO $ InternalNodeInitFailed err

updateNodeProcessState
  :: ( MonadLogger m
     , PersistBackend m
     , MonadIO m
     )
  => Id ProcessData
  -> Id Node
  -> ProcessState
  -> m ()
updateNodeProcessState pid nodeId ps =
  let mkNotification = Just $ \pd -> (NotifyTag_NodeInternal, (nodeId, pd))
  in updateProcessState pid mkNotification ps

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

-- | Returns the parsed output of 'octez-node --version' command.
-- Throws an exception in case of command failure or result parsing failure.
getKilnNodeVersion :: (MonadLoggerIO m, MonadThrow m) => AppConfig -> m MajorMinorVersion
getKilnNodeVersion appConfig = do
  let mbCustomBinaryPaths = _appConfig_binaryPaths appConfig
      nodePath = maybe nixNodePath _binaryPaths_nodePath mbCustomBinaryPaths
      args = ["--version"]
      cmdText = T.pack $ unwords (nodePath : args)
  (exitCode, stdout, stderr) <- readProcessWithExitCode nodePath args ""
  case exitCode of
    ExitSuccess -> do
      let stdoutText = T.pack stdout
      $(logDebug) $ cmdText <> " command finished successfully. stdout: " <> stdoutText
      parseNodeVersion stdoutText
    ExitFailure ec -> do
      let stderrText = T.pack stderr
      throwString $ T.unpack $ cmdText <> " failed with exit code " <> tshow ec <> ". stderr: " <> stderrText
  where
    -- Parses the output of 'octez-node --version' command. Example output:
    -- 344d8da5 (2023-06-14 14:32:54 +0200) (17.1)
    nodeVersionParser :: P.Parser MajorMinorVersion
    nodeVersionParser = do
      let open  = P.char '('
          close = P.char ')'
      P.skipSpace >> P.many1 (P.letter <|> P.digit) >> P.skipSpace
      open >> P.takeTill (== ')') >> close >> P.skipSpace
      open
      major <- P.decimal
      P.char '.'
      minor <- P.decimal
      mbRc <- optional $ P.string "~rc" >> P.decimal
      close
      let additionalInfo = maybe Release ReleaseCandidate mbRc
      pure $ MajorMinorVersion major minor Nothing additionalInfo

    parseNodeVersion :: MonadThrow m => Text -> m MajorMinorVersion
    parseNodeVersion stdout = case P.parseOnly nodeVersionParser stdout of
      Left err -> throwString err
      Right v -> pure v
