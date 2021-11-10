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

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.NodeCmd where

import Control.Exception.Safe (throwIO, tryJust)
import Control.Monad.Logger (MonadLogger, logInfoNS, logDebug, logWarn, logError, logErrorNS)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder as Builder
import Data.Dependent.Map (DSum (..))
import qualified Data.HashMap.Lazy as HashMap
import Data.Pool (Pool)
import Data.List (find, isInfixOf)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Version
import Database.Groundhog.Postgresql
import Named
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (runDb, project1)
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
import Text.URI (render)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Tezos.Types (ProtocolHash)

import Backend.CachedNodeRPC
import Backend.Config (AppConfig (..), kilnNodeRpcURI, nodeDataDir, tezosClientDataDir, BinaryPaths(..))
import Backend.Schema
import Backend.Workers.Process
import Common.Route (ExportLog(..))
import Common.Schema
import ExtraPrelude

needsCarthageStorageUpgrade :: Version -> Bool
needsCarthageStorageUpgrade = (< Version [0,0,4] [])

nixNodePath :: FilePath
nixNodePath = $(staticWhich "tezos-node")

bakerPath :: NonEmpty (ProtocolHash, FilePath, FilePath) -> Maybe ProtocolHash -> FilePath
bakerPath = getPath (view _2)

endorserPath :: NonEmpty (ProtocolHash, FilePath, FilePath) -> Maybe ProtocolHash -> FilePath
endorserPath = getPath (view _3)

getPath :: ((ProtocolHash, FilePath, FilePath) -> FilePath)
  -> NonEmpty (ProtocolHash, FilePath, FilePath) -> Maybe ProtocolHash -> FilePath
getPath f paths = \case
  Nothing -> f $ NonEmpty.head paths
  Just p -> maybe e f $ find (\(p', _, _) -> p' == p) paths
    where
      e = error ("tezos-baker/endorser not available for the given protocol: " <> show p)

-- You cannot use a mainnet binary against a babylonnet node because the mainnet
-- binary expects a .tezos-node/<chain_id>/protocol dir
-- https://gitlab.com/tezos/tezos/compare/mainnet...babylonnet#a59616ef23c1f6b8d578e385e82f6c4d4dadedde_49_46
tezosBinaryPaths :: NonEmpty (ProtocolHash, FilePath, FilePath)
tezosBinaryPaths = NonEmpty.fromList
  [ ("PtGRANADsDU8R9daYKAgWnQYAJ64omN1o3KMGVCykShA97vQbvV"
    , $(staticWhich "tezos-baker-010-PtGRANAD")
    , $(staticWhich "tezos-endorser-010-PtGRANAD")
    ),
    ("PtHangz2aRngywmSRGGvrcTyMbbdpWdpFKuS4uMWxg2RaH9i1qx"
    , $(staticWhich "tezos-baker-011-PtHangz2")
    , $(staticWhich "tezos-endorser-011-PtHangz2")
    )
  ]

-- TODO: use postgres for "process-id's"

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
                    return $ proc nodePath (nodeArgs nodeConfigPath dataDir ++ extraArgs))
    ! #pid pid
    ! #pidToRunAfter Nothing
    ! #mkNotify (Just (\pd -> (NotifyTag_NodeInternal, (nid, pd))))

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


-- Start Baker and Endorser
bakerDaemonProcess :: (MonadIO m, MonadBaseNoPureAborts IO m)
  => AppConfig -> LoggingEnv -> Pool Postgresql -> Maybe BinaryPaths -> m (IO ())
bakerDaemonProcess appConfig logger db maybePaths = do
  (nodePPid, bdid) <- runLoggingEnv logger $ runDb (Identity db) $ do
    -- nodePPid should always be a Just value
    nodePPid <- project1 (NodeInternal_dataField ~> DeletableRow_dataSelector) CondEmpty
    project1 (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
      (Just bdid) -> return (nodePPid, bdid)
      Nothing -> do
        let processData = ProcessData
              { _processData_control = ProcessControl_Stop
              , _processData_state = ProcessState_Stopped
              , _processData_updated = Nothing
              , _processData_backend = Nothing
              }

        bpid <- insert' processData
        epid <- insert' processData
        tbpid <- insert' processData
        tepid <- insert' processData
        nid <- insert' BakerDaemon
        let bdid = BakerDaemonInternalData
              { _bakerDaemonInternalData_alias = "ledger_kiln"
              , _bakerDaemonInternalData_publicKeyHash = Nothing
              , _bakerDaemonInternalData_insufficientFunds = False
              , _bakerDaemonInternalData_protocol = psdd
              , _bakerDaemonInternalData_bakerProcessData = bpid
              , _bakerDaemonInternalData_endorserProcessData = epid
              , _bakerDaemonInternalData_altProtocol = Nothing
              , _bakerDaemonInternalData_altBakerProcessData = tbpid
              , _bakerDaemonInternalData_altEndorserProcessData = tepid
              }
            -- Add this as default protocol, we will anyways fix this in protocolMonitorWorker once the synced node is available
            psdd :: ProtocolHash
            psdd = "PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP"
        insert $ BakerDaemonInternal
          { _bakerDaemonInternal_id = nid
          , _bakerDaemonInternal_data = DeletableRow
            { _deletableRow_data = bdid
            , _deletableRow_deleted = True
            }
          }
        return (nodePPid, bdid)
  let
    aliasT = _bakerDaemonInternalData_alias bdid
    bpid1 = _bakerDaemonInternalData_bakerProcessData bdid
    epid1 = _bakerDaemonInternalData_endorserProcessData bdid
    bpid2 = _bakerDaemonInternalData_altBakerProcessData bdid
    epid2 = _bakerDaemonInternalData_altEndorserProcessData bdid
    alias = T.unpack aliasT
    bakerArgs = [ "--endpoint", T.unpack $ render $ kilnNodeRpcURI appConfig
                , "--base-dir", tezosClientDataDir appConfig
                , "run", "with", "local", "node", nodeDataDir appConfig
                , alias]
    endorserArgs = [ "--endpoint", T.unpack $ render $  kilnNodeRpcURI appConfig
                   , "--base-dir", tezosClientDataDir appConfig
                   , "run"
                   , alias]
    pw (pathF, args) pid = processWorker
      (\_ -> runLoggingEnv logger $ runDb (Identity db) $ fetchProtocol pid)
      ! #logger logger
      ! #db db
      ! #config appConfig
      ! #mkProcess (\proto -> return $ proc (pathF proto) args)
      ! #pid pid
      ! #pidToRunAfter nodePPid
      ! #mkNotify Nothing
    bakerPw = pw (bakerPath paths, bakerArgs) ! #logNamespace "kiln-baker"
    endorserPw = pw (endorserPath paths, endorserArgs) ! #logNamespace "kiln-endorser"
    paths = maybe tezosBinaryPaths _binaryPaths_bakerEndorserPaths maybePaths

  -- We run two sets of ProcessWorkers, which one actually runs the main baker/alt baker
  -- depends upon the protocol set for that PID.
  -- This allows us to switch a 'alt baker' to 'main baker' without actually restarting the baker
  -- ie bp1 starts as main baker, bp2 as alt baker
  -- after voting period ends, we simply stop the bp1 and set bpid2 as 'bakerProcessData'
  -- So bp2 process keeps on running but is now identified as 'main baker'
  bp1 <- bakerPw bpid1
  bp2 <- bakerPw bpid2
  ep1 <- endorserPw epid1
  ep2 <- endorserPw epid2
  return (bp1 *> bp2 *> ep1 *> ep2)

-- protocol is a variable field, and therefore it is fetched everytime we restart process
fetchProtocol
  :: (PersistBackend m)
  => Id ProcessData -> m (Maybe ProtocolHash)
fetchProtocol pid =
  project1 (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
    Nothing -> error "BakerDaemonInternal table empty"
    Just bdid ->
      let
        tbpid = _bakerDaemonInternalData_altBakerProcessData bdid
        tepid = _bakerDaemonInternalData_altEndorserProcessData bdid
      in if pid == tbpid || pid == tepid
        then return $ _bakerDaemonInternalData_altProtocol bdid
        else return $ Just $ _bakerDaemonInternalData_protocol bdid

handleExportLogs :: MonadSnap m => NodeDataSource -> DSum ExportLog Identity -> m ()
handleExportLogs nds lType = do
  let
    logger = _nodeDataSource_logger nds
    logIdentifier :: String
    logIdentifier = "kiln-" <> case lType of
      ExportLog_Baker :=> _ -> "baker"
      ExportLog_Endorser :=> _ -> "endorser"
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
