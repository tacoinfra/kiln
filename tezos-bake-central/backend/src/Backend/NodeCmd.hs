{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoDoAndIfThenElse #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.NodeCmd where

import Control.Monad.Trans.Control
import Data.Pool (Pool)
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB (runDb, project1)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import System.Directory (doesFileExist)
import System.FilePath (combine)
import System.Process (readProcess, proc)
import qualified Data.Text as T

import Backend.Workers.Process
import ExtraPrelude
import System.Which
import Tezos.Chain (NamedChain(..))
import Backend.Config (AppConfig (..))
import Backend.Schema
import Common.Schema

-- TODO XXX OBVIOUSLY BAD
nodePaths :: NamedChain -> FilePath
nodePaths NamedChain_Mainnet = $(staticWhich "mainnet-tezos-node")
nodePaths NamedChain_Alphanet = $(staticWhich "alphanet-tezos-node")
nodePaths NamedChain_Zeronet = $(staticWhich "zeronet-tezos-node")

bakerPaths :: NamedChain -> FilePath
bakerPaths NamedChain_Mainnet = $(staticWhich "mainnet-tezos-baker-003-PsddFKi3")
bakerPaths NamedChain_Alphanet = $(staticWhich "alphanet-tezos-baker-003-PsddFKi3")
bakerPaths NamedChain_Zeronet = $(staticWhich "zeronet-tezos-baker-alpha")

endorserPaths :: NamedChain -> FilePath
endorserPaths NamedChain_Mainnet = $(staticWhich "mainnet-tezos-endorser-003-PsddFKi3")
endorserPaths NamedChain_Alphanet = $(staticWhich "alphanet-tezos-endorser-003-PsddFKi3")
endorserPaths NamedChain_Zeronet = $(staticWhich "zeronet-tezos-endorser-alpha")

-- TODO: configurable data-dir with CLI
-- TODO: use postgres for "process-id's"

internalNodeWorker :: (MonadIO m, MonadBaseControl IO m)
  => AppConfig -> LoggingEnv -> Pool Postgresql -> NamedChain -> m (IO ())
internalNodeWorker appConfig logger db namedChain = do
  -- Always create a NodeInternal and corresponsing ProcessData
  (nid, pid) <- runLoggingEnv logger $ runDb (Identity db) $ do
    project1 (NodeInternal_idField, NodeInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
      (Just v) -> return v
      Nothing -> do
        let processData = ProcessData
              { _processData_running = False
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
    nodePath = nodePaths namedChain
    nodePort = show $ _appConfig_kilnNodePort appConfig
  processWorker logger db
    defaultConfig
    (initNode nodePath)
    (\_ nodeConfigPath -> proc nodePath ["run", "--config-file", nodeConfigPath, "--rpc-addr", ":" <> nodePort])
    pid
    (Just (Notify_NodeInternal nid))

initNode :: (MonadIO m) => FilePath -> FilePath -> m ()
initNode nodePath nodeConfigPath = do
  let nodeConfig = defaultConfig
  let dataDir = fromMaybe (error "specify data-dir") $ _nodeConfigFile_dataDir nodeConfig
  let versionFile = dataDir `combine` "version.json"
  let identityFile = dataDir `combine` "identity.json"
  -- liftIO . putStrLn =<< liftIO (readProcess "cat" [nodeConfigPath] "")
  haveVersionFile <- liftIO $ doesFileExist versionFile
  when (not haveVersionFile) $
    liftIO . putStrLn =<< liftIO (readProcess nodePath ["config", "show", "--config-file", nodeConfigPath] "")

  haveIdentityFile <- liftIO $ doesFileExist identityFile
  when (not haveIdentityFile) $
    liftIO . putStrLn =<< liftIO (readProcess nodePath ["identity", "generate", "--config-file", nodeConfigPath] "")
  return ()

-- Start Baker and Endorser
bakerDaemonProcess :: (MonadIO m, MonadBaseControl IO m)
  => AppConfig -> LoggingEnv -> Pool Postgresql -> NamedChain -> m (IO (), IO ())
bakerDaemonProcess appConfig logger db namedChain = do
  (_nid, BakerDaemonInternalData _ _ bpid epid) <- runLoggingEnv logger $ runDb (Identity db) $ do
    project1 ( BakerDaemonInternal_idField
             , BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
      (Just v) -> return v
      Nothing -> do
        let processData = ProcessData
              { _processData_running = False
              , _processData_state = ProcessState_Stopped
              , _processData_updated = Nothing
              , _processData_backend = Nothing
              }

        bpid <- insert' processData
        epid <- insert' processData
        nid <- insert' BakerDaemon
        let v = BakerDaemonInternalData "ledger_kiln" Nothing bpid epid
        insert $ BakerDaemonInternal
          { _bakerDaemonInternal_id = nid
          , _bakerDaemonInternal_data = DeletableRow
            { _deletableRow_data = v
            , _deletableRow_deleted = True
            }
          }
        return (nid, v)
  let nodePort = show $ _appConfig_kilnNodePort appConfig
  bp <- processWorker logger db defaultConfig
    fetchAlias
    (\alias _nodeConfigPath -> proc (bakerPaths namedChain) ["--port", nodePort, "run", "with", "local", "node", "./.tezos-node", alias])
    bpid
    Nothing
  ep <- processWorker logger db defaultConfig
    fetchAlias
    (\alias _nodeConfigPath -> proc (endorserPaths namedChain) ["--port", nodePort, "run", alias])
    epid
    Nothing
  return (bp, ep)

fetchAlias :: (forall m'. (MonadIO m', PersistBackend m') => FilePath -> m' String)
fetchAlias _ = do
  project1 (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
    Nothing -> error "BakerDaemonInternal table empty"
    (Just (BakerDaemonInternalData alias _ _ _)) -> return $ T.unpack alias
