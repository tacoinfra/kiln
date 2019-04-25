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

import Control.Monad.Logger (MonadLogger)
import Data.Pool (Pool)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (runDb, project1)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import System.Directory (doesFileExist)
import System.FilePath (combine)
import System.Process (readProcess, proc)
import qualified Data.Text as T

import Tezos.Base58Check (ProtocolHash)
import Backend.Workers.Process
import ExtraPrelude
import System.Which
import Tezos.Chain (NamedChain(..))
import Backend.Config (AppConfig (..), nodeDataDir, tezosClientDataDir, BinaryPaths(..))
import Backend.Schema
import Common.Schema

-- TODO XXX OBVIOUSLY BAD
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

zeronetPaths :: NonEmpty (ProtocolHash, FilePath, FilePath)
zeronetPaths = ( psdd
    , $(staticWhich "zeronet-tezos-baker-003-PsddFKi3")
    , $(staticWhich "zeronet-tezos-endorser-003-PsddFKi3")
    ) :|
    [ ( pt24
      , $(staticWhich "zeronet-tezos-baker-004-Pt24m4xi")
      , $(staticWhich "zeronet-tezos-endorser-004-Pt24m4xi")
      )
    ]
  where
    psdd :: ProtocolHash
    psdd = "PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP"
    -- alpha = "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
    pt24 = "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"

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
    nodePort = show $ _appConfig_kilnNodePort appConfig
    nodeExtraArgs = maybe [] (words . T.unpack) $ _appConfig_kilnNodeCustomArgs appConfig
    -- (19/04/03) after zeronet reset, now it no longer supports archive mode
    useArchiveMode = False
  processWorker logger db appConfig
    (initNode appConfig nodePath)
    (\dataDir nodeConfigPath -> proc nodePath $ ["run", "--config-file", nodeConfigPath, "--data-dir", dataDir, "--rpc-addr", ":" <> nodePort] ++ nodeExtraArgs ++ if useArchiveMode then ["--history-mode", "archive"] else [])
    pid
    (Just (\pd -> (NotifyTag_NodeInternal, (nid, pd))))

initNode :: (MonadIO m)
  => AppConfig
  -> FilePath
  -> Pool Postgresql
  -> (ProcessState -> m ())
  -> FilePath
  -> m FilePath
initNode appConfig nodePath _ updateState nodeConfigPath = do
  let dataDir = nodeDataDir appConfig
  let versionFile = dataDir `combine` "version.json"
  let identityFile = dataDir `combine` "identity.json"
  -- liftIO . putStrLn =<< liftIO (readProcess "cat" [nodeConfigPath] "")
  haveVersionFile <- liftIO $ doesFileExist versionFile
  when (not haveVersionFile) $
    liftIO . putStrLn =<< liftIO (readProcess nodePath ["config", "show", "--config-file", nodeConfigPath, "--data-dir", dataDir] "")

  haveIdentityFile <- liftIO $ doesFileExist identityFile
  when (not haveIdentityFile) $ do
    updateState ProcessState_GeneratingIdentity
    liftIO . putStrLn =<< liftIO (readProcess nodePath ["identity", "generate", "--config-file", nodeConfigPath, "--data-dir", dataDir] "")
  return dataDir

-- Start Baker and Endorser
bakerDaemonProcess :: (MonadIO m, MonadBaseNoPureAborts IO m)
  => AppConfig -> LoggingEnv -> Pool Postgresql -> Either NamedChain BinaryPaths -> m (IO ())
bakerDaemonProcess appConfig logger db namedChainOrPaths = do
  (_nid, bdid) <- runLoggingEnv logger $ runDb (Identity db) $ do
    project1 ( BakerDaemonInternal_idField
             , BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
      (Just v) -> return v
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
        let v = BakerDaemonInternalData
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
            { _deletableRow_data = v
            , _deletableRow_deleted = True
            }
          }
        return (nid, v)
  let
    aliasT = _bakerDaemonInternalData_alias bdid
    bpid1 = _bakerDaemonInternalData_bakerProcessData bdid
    epid1 = _bakerDaemonInternalData_endorserProcessData bdid
    bpid2 = _bakerDaemonInternalData_altBakerProcessData bdid
    epid2 = _bakerDaemonInternalData_altEndorserProcessData bdid
    nodePort = show $ _appConfig_kilnNodePort appConfig
    alias = T.unpack aliasT
    bakerArgs = [ "--port", nodePort
                , "--base-dir", tezosClientDataDir appConfig
                , "run", "with", "local", "node", nodeDataDir appConfig
                , alias]
    endorserArgs = ["--port", nodePort
                   , "--base-dir", tezosClientDataDir appConfig
                   , "run"
                   , alias]
    pw (pathF, args) pid = processWorker logger db appConfig
      (fetchProtocol pid)
      (\proto _nodeConfigPath -> proc (pathF proto) args)
      pid
      Nothing
    bakerPw = pw (bakerPath paths, bakerArgs)
    endorserPw = pw (endorserPath paths, endorserArgs)
    paths = either (const zeronetPaths) _binaryPaths_bakerEndorserPaths namedChainOrPaths

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
fetchProtocol :: (MonadIO m, MonadLogger m, MonadBaseNoPureAborts IO m)
  => Id ProcessData -> Pool Postgresql -> a -> b -> m (Maybe ProtocolHash)
fetchProtocol pid db _ _ = runDb (Identity db) $ do
  project1 (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
    Nothing -> error "BakerDaemonInternal table empty"
    Just bdid ->
      let
        tbpid = _bakerDaemonInternalData_altBakerProcessData bdid
        tepid = _bakerDaemonInternalData_altEndorserProcessData bdid
      in if pid == tbpid || pid == tepid
        then return $ _bakerDaemonInternalData_altProtocol bdid
        else return $ Just $ _bakerDaemonInternalData_protocol bdid
