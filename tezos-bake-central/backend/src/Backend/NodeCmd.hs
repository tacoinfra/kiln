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
import Backend.Config (AppConfig (..), nodeDataDir, tezosClientDataDir)
import Backend.Schema
import Common.Schema

-- TODO XXX OBVIOUSLY BAD
nodePaths :: NamedChain -> FilePath
nodePaths NamedChain_Mainnet = $(staticWhich "mainnet-tezos-node")
nodePaths NamedChain_Alphanet = $(staticWhich "alphanet-tezos-node")
nodePaths NamedChain_Zeronet = $(staticWhich "zeronet-tezos-node")

bakerPaths :: NamedChain -> Maybe ProtocolHash -> FilePath
bakerPaths n = \case
  Nothing -> snd $ NonEmpty.head paths
  Just p -> maybe e snd $ find ((== p8) . fst) paths
    where
      -- drop '(fromString "'
      p8 = take 8 $ drop 13 $ show p
      e = error ("tezos-baker not available for the given chain:" <> (show n) <> " and protocol: " <> p8)
  where
    paths = case n of
      NamedChain_Mainnet -> ("PsddFKi3", $(staticWhich "mainnet-tezos-baker-003-PsddFKi3")) :| []
      NamedChain_Alphanet -> ("PsddFKi3", $(staticWhich "alphanet-tezos-baker-003-PsddFKi3")) :| []
      NamedChain_Zeronet -> ("PsGn8G5U", $(staticWhich "zeronet-tezos-baker-004-PsGn8G5U")) :|
        [ ("PsuzFErA", $(staticWhich "zeronet-tezos-baker-004-PsuzFErA"))
        , ("ProtoALp", $(staticWhich "zeronet-tezos-baker-alpha"))
        ]

endorserPaths :: NamedChain -> Maybe ProtocolHash -> FilePath
endorserPaths n = \case
  Nothing -> snd $ NonEmpty.head paths
  Just p -> maybe e snd $ find ((== p8) . fst) paths
    where
      -- drop '(fromString "'
      p8 = take 8 $ drop 13 $ show p
      e = error ("tezos-endorser not available for the given chain:" <> (show n) <> " and protocol: " <> p8)
  where
    paths = case n of
      NamedChain_Mainnet -> ("PsddFKi3", $(staticWhich "mainnet-tezos-endorser-003-PsddFKi3")) :| []
      NamedChain_Alphanet -> ("PsddFKi3", $(staticWhich "alphanet-tezos-endorser-003-PsddFKi3")) :| []
      NamedChain_Zeronet -> ("PsGn8G5U", $(staticWhich "zeronet-tezos-endorser-004-PsGn8G5U")) :|
        [ ("PsuzFErA", $(staticWhich "zeronet-tezos-endorser-004-PsuzFErA"))
        , ("ProtoALp", $(staticWhich "zeronet-tezos-endorser-alpha"))
        ]

-- TODO: use postgres for "process-id's"

internalNodeWorker :: (MonadIO m, MonadBaseNoPureAborts IO m)
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
    useArchiveMode = namedChain == NamedChain_Zeronet
  processWorker logger db appConfig
    (initNode appConfig nodePath)
    (\dataDir nodeConfigPath -> proc nodePath $ ["run", "--config-file", nodeConfigPath, "--data-dir", dataDir, "--rpc-addr", ":" <> nodePort] ++ if useArchiveMode then ["--history-mode", "archive"] else [])
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
  => AppConfig -> LoggingEnv -> Pool Postgresql -> NamedChain -> m (IO (), IO (), IO (), IO ())
bakerDaemonProcess appConfig logger db namedChain = do
  (_nid, BakerDaemonInternalData aliasT _ _ _ bpid1 epid1 _ bpid2 epid2) <- runLoggingEnv logger $ runDb (Identity db) $ do
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
        tbpid <- insert' processData
        tepid <- insert' processData
        nid <- insert' BakerDaemon
        let v = BakerDaemonInternalData "ledger_kiln" Nothing False Nothing bpid epid Nothing tbpid tepid
        insert $ BakerDaemonInternal
          { _bakerDaemonInternal_id = nid
          , _bakerDaemonInternal_data = DeletableRow
            { _deletableRow_data = v
            , _deletableRow_deleted = True
            }
          }
        return (nid, v)
  let nodePort = show $ _appConfig_kilnNodePort appConfig
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
        (\proto _nodeConfigPath -> proc (pathF namedChain proto) args)
        pid
        Nothing
      bakerPw = pw (bakerPaths, bakerArgs)
      endorserPw = pw (endorserPaths, endorserArgs)

  -- We run two sets of ProcessWorkers, which one actually runs the main baker/test baker
  -- depends upon the protocol set for that PID.
  -- This allows us to switch a 'test baker' to 'main baker' without actually restarting the baker
  -- ie bp1 starts as main baker, bp2 as test baker
  -- after voting period ends, we simply stop the bp1 and set bpid2 as 'bakerProcessData'
  -- So bp2 process keeps on running but is now identified as 'main baker'
  bp1 <- bakerPw bpid1
  bp2 <- bakerPw bpid2
  ep1 <- endorserPw epid1
  ep2 <- endorserPw epid2
  return (bp1, bp2, ep1, ep2)

-- protocol is a variable field, and therefore it is fetched everytime we restart process
fetchProtocol :: (MonadIO m, MonadLogger m, MonadBaseNoPureAborts IO m)
  => Id ProcessData -> Pool Postgresql -> a -> b -> m (Maybe ProtocolHash)
fetchProtocol pid db _ _ = runDb (Identity db) $ do
  project1 (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
    Nothing -> error "BakerDaemonInternal table empty"
    (Just (BakerDaemonInternalData _ _ _ proto _ _ testProto tbpid tepid)) ->
      if pid == tbpid || pid == tepid
        then return testProto
        else return proto
