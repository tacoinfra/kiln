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

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.NodeCmd where

import Control.Monad.Logger (MonadLogger, logInfo)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HashMap
import Data.Pool (Pool)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Version
import Database.Groundhog.Postgresql
import Named
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (runDb, project1)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import System.Directory (doesFileExist)
import System.FilePath (combine)
import System.Process (readProcess, proc)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Tezos.Base58Check (ProtocolHash)
import Backend.Workers.Process
import ExtraPrelude
import System.Which
import Tezos.Chain (NamedChain(..))
import Backend.Config (AppConfig (..), nodeDataDir, tezosClientDataDir, BinaryPaths(..))
import Backend.Schema
import Common.Schema

nodePaths :: NamedChain -> FilePath
nodePaths NamedChain_Mainnet = $(staticWhich "mainnet-tezos-node")
nodePaths NamedChain_Alphanet = $(staticWhich "alphanet-tezos-node")
nodePaths NamedChain_Zeronet = $(staticWhich "zeronet-tezos-node")

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

tezosBinaryPaths :: NonEmpty (ProtocolHash, FilePath, FilePath)
tezosBinaryPaths =
  ( "PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP"
  , $(staticWhich "mainnet-tezos-baker-003-PsddFKi3")
  , $(staticWhich "mainnet-tezos-endorser-003-PsddFKi3")
  ) :|
    [ ( "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"
      , $(staticWhich "mainnet-tezos-baker-004-Pt24m4xi")
      , $(staticWhich "mainnet-tezos-endorser-004-Pt24m4xi")
      )
    , ( "PtG6cmhhWF8AY5gVQhCaUASbgu8CGebkGPdNSX26m3CSnxvih9v"
      , $(staticWhich "zeronet-tezos-baker-alpha")
      , $(staticWhich "zeronet-tezos-endorser-alpha")
      )
    ]

-- TODO: use postgres for "process-id's"

internalNodeWorker :: (MonadIO m, MonadBaseNoPureAborts IO m)
  => AppConfig -> LoggingEnv -> Pool Postgresql -> Either NamedChain BinaryPaths -> m (IO ())
internalNodeWorker appConfig logger db namedChainOrPaths = do
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
    nodePath = either nodePaths _binaryPaths_nodePath namedChainOrPaths
    nodeRpcPort = show $ _appConfig_kilnNodeRpcPort appConfig
    nodeNetPort = show $ _appConfig_kilnNodeNetPort appConfig
    nodeExtraArgs = maybe [] (words . T.unpack) $ _appConfig_kilnNodeCustomArgs appConfig
    useArchiveMode = True
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
      ++ (if useArchiveMode then ["--history-mode", "archive"] else [])
      ++ nodeExtraArgs

  processWorker
    (initNode ! #logger logger ! #config appConfig ! #nodePath nodePath)
    ! #logger logger
    ! #db db
    ! #config appConfig
    ! #logNamespace "kiln-node"
    ! #mkProcess (\dataDir nodeConfigPath -> proc nodePath (nodeArgs nodeConfigPath dataDir))
    ! #pid pid
    ! #pidToRunAfter Nothing
    ! #mkNotify (Just (\pd -> (NotifyTag_NodeInternal, (nid, pd))))

runCommandWithInfoLogging :: (MonadLogger m, MonadIO m) => FilePath -> [Text] -> m Text
runCommandWithInfoLogging cmd args = do
  out <- T.pack <$> liftIO (readProcess cmd (T.unpack <$> args) "")
  $(logInfo) $ "Running command " <> T.pack cmd <> " " <> tshow args <> " --> " <> out
  pure out

initNode
  :: (MonadIO m)
  => "logger" :! LoggingEnv
  -> "config" :! AppConfig
  -> "nodePath" :! FilePath
  -> "db" :! Pool Postgresql
  -> "updateState" :! (ProcessState -> m ())
  -> "configFile" :! FilePath
  -> m FilePath
initNode (Arg logger) (Arg appConfig) (Arg nodePath) _ (Arg updateState) (Arg nodeConfigPath) = runLoggingEnv logger $ do
  let dataDir = nodeDataDir appConfig
  let versionFile = dataDir `combine` "version.json"
  let identityFile = dataDir `combine` "identity.json"
  hasVersionFile <- liftIO $ doesFileExist versionFile
  if hasVersionFile
    then do
      vf <- liftIO $ LBS.readFile versionFile
      let
        v = getVersion =<< HashMap.lookup ("version" :: Text) =<< Aeson.decode vf
        getVersion :: Text -> Maybe Version
        getVersion = Aeson.decode . LBS.fromStrict . T.encodeUtf8 . tshow
      case v of
        Nothing -> pure ()
        Just ver -> when (ver < (Version [0,0,3] [])) $ do
          void $ runCommandWithInfoLogging nodePath
            ["upgrade", "storage", "--data-dir", T.pack dataDir]
    else do
      void $ runCommandWithInfoLogging nodePath
        ["config", "show", "--config-file", T.pack nodeConfigPath, "--data-dir", T.pack dataDir]

  haveIdentityFile <- liftIO $ doesFileExist identityFile
  when (not haveIdentityFile) $ do
    lift $ updateState ProcessState_GeneratingIdentity
    void $ runCommandWithInfoLogging nodePath ["identity", "generate", "--config-file", T.pack nodeConfigPath, "--data-dir", T.pack dataDir]

  return dataDir

-- Start Baker and Endorser
bakerDaemonProcess :: (MonadIO m, MonadBaseNoPureAborts IO m)
  => AppConfig -> LoggingEnv -> Pool Postgresql -> Either NamedChain BinaryPaths -> m (IO ())
bakerDaemonProcess appConfig logger db namedChainOrPaths = do
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
    nodeRpcPort = show $ _appConfig_kilnNodeRpcPort appConfig
    alias = T.unpack aliasT
    bakerArgs = [ "--port", nodeRpcPort
                , "--base-dir", tezosClientDataDir appConfig
                , "run", "with", "local", "node", nodeDataDir appConfig
                , alias]
    endorserArgs = [ "--port", nodeRpcPort
                   , "--base-dir", tezosClientDataDir appConfig
                   , "run"
                   , alias]
    pw (pathF, args) pid = processWorker
      (\(Arg db_) _ _ -> fetchProtocol pid db_)
      ! #logger logger
      ! #db db
      ! #config appConfig
      ! #mkProcess (\proto _nodeConfigPath -> proc (pathF proto) args)
      ! #pid pid
      ! #pidToRunAfter nodePPid
      ! #mkNotify Nothing
    bakerPw = pw (bakerPath paths, bakerArgs) ! #logNamespace "kiln-baker"
    endorserPw = pw (endorserPath paths, endorserArgs) ! #logNamespace "kiln-endorser"
    paths = either (const tezosBinaryPaths) _binaryPaths_bakerEndorserPaths namedChainOrPaths

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
  :: (MonadIO m, MonadLogger m, MonadBaseNoPureAborts IO m)
  => Id ProcessData -> Pool Postgresql -> m (Maybe ProtocolHash)
fetchProtocol pid db = runDb (Identity db) $ do
  project1 (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
    Nothing -> error "BakerDaemonInternal table empty"
    Just bdid ->
      let
        tbpid = _bakerDaemonInternalData_altBakerProcessData bdid
        tepid = _bakerDaemonInternalData_altEndorserProcessData bdid
      in if pid == tbpid || pid == tepid
        then return $ _bakerDaemonInternalData_altProtocol bdid
        else return $ Just $ _bakerDaemonInternalData_protocol bdid
