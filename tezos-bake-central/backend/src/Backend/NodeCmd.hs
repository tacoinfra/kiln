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

-- import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask, finally)
import Control.Monad.IO.Class ()
import Control.Monad.Logger (MonadLogger, logInfoSH, logDebugSH, logWarn)
import Control.Monad.Trans.Control
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Word
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.DB.PsqlSimple (executeQ, queryQ, fromOnly)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import System.Directory (doesFileExist, removeDirectoryRecursive)
import System.FilePath (combine)
import System.IO (hFlush)
import System.IO.Temp (withTempFile)
import System.Process (readProcess, withCreateProcess, proc, getProcessExitCode, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import ExtraPrelude
import System.Which
import Tezos.Chain (NamedChain(..))
import Tezos.Json
import Backend.Common (threadDelay')
import Backend.Common (worker')
import Backend.Config (AppConfig (..))
import Backend.Schema
import Common.Schema

defaultConfig :: NodeConfigFile
defaultConfig = NodeConfigFile
  { _nodeConfigFile_p2p = NodeConfigP2P
    { _nodeConfigP2P_expectedProofOfWork = Nothing
    , _nodeConfigP2P_bootstrapPeers = Nothing
    , _nodeConfigP2P_listenAddr = Nothing
    , _nodeConfigP2P_privateMode = Nothing
    , _nodeConfigP2P_disableMempool = Nothing
  }
  , _nodeConfigFile_dataDir = Just "./.tezos-node"
  , _nodeConfigFile_rpc = Just NodeConfigRPC
    { _nodeConfigRPC_listenAddr = Just "127.0.0.1"
    , _nodeConfigRPC_corsOrigin = Nothing
    , _nodeConfigRPC_corsHeaders = Nothing
    , _nodeConfigRPC_crt = Nothing
    , _nodeConfigRPC_key = Nothing
    }
  , _nodeConfigFile_log = Nothing
  , _nodeConfigFile_shell = Nothing
  }

-- TODO XXX OBVIOUSLY BAD
nodePaths :: NamedChain -> FilePath
nodePaths NamedChain_Mainnet = $(staticWhich "mainnet-tezos-node")
nodePaths NamedChain_Alphanet = $(staticWhich "alphanet-tezos-node")
nodePaths NamedChain_Zeronet = $(staticWhich "zeronet-tezos-node")

-- TODO: configurable data-dir with CLI
-- TODO: use postgres for "process-id's"

withNodeLock :: (MonadMask m, MonadBaseControl IO m, MonadIO m) => LoggingEnv -> Pool Postgresql -> (Int -> m ()) -> m ()
withNodeLock logger db f = do
  pid <- runLoggingEnv logger $ do
    pid :: Int <- runDb (Identity db) $ [queryQ|SELECT nextval('"NodeInternal_pid"') |] <&> fromOnly . head
    $(logInfoSH) ("internal node pid:" :: Text, pid)
    let
      {-# INLINE claim #-}
      claim :: forall m1. (MonadBaseControl IO m1, MonadIO m1, MonadLogger m1) => m1 ()
      claim = do
        let state = NodeInternalState_Stopped
        result <- runDb (Identity db) $ do
          _ <- [executeQ|
            UPDATE "NodeInternal"
            SET "data#data#stateUpdated" = NOW()
              , "data#data#backend" = ?pid
              , "data#data#state" = ?state
            WHERE "data#data#backend" IS NULL
               OR "data#data#stateUpdated" < NOW() - interval '5 minutes'|]
          [queryQ|
            SELECT "data#data#backend"
              FROM "NodeInternal"
          |]
        case result of
          [] -> threadDelay' 1 *> claim
          (pid':_) -> do
            $(logDebugSH) ("internal node pid:" :: Text, pid, result)
            if pid == fromOnly pid'
              then return ()
              else do
                $(logWarn) "internalnode LOCK HELD"
                threadDelay' 1
                *> claim
    claim
    return pid
  finally (f pid) $ runLoggingEnv logger $ runDb (Identity db) [executeQ|
    UPDATE "NodeInternal"
    SET "data#data#stateUpdated" = NOW()
      , "data#data#backend" = NULL
    WHERE "data#data#backend" = ?pid |]
    >>= liftIO . print

internalNodeWorker :: MonadIO m => AppConfig -> LoggingEnv -> Pool Postgresql -> NamedChain -> m (IO ())
internalNodeWorker appConfig logger db namedChain = worker' $ withNodeLock logger db $ \pid -> runLoggingEnv logger $ do
  let
    waitUntilShouldRun = do
      shouldRun <- any fromOnly <$> runDb (Identity db) [queryQ|
        SELECT COALESCE(BOOL_OR("data#data#running"), false)
        FROM "NodeInternal"
        |]
      if shouldRun
        then return ()
        else threadDelay' 5 *> waitUntilShouldRun
  waitUntilShouldRun
  callNode appConfig logger db (nodePaths namedChain) pid
  threadDelay' 10

putState :: (MonadBaseControl IO m, MonadIO m) => LoggingEnv -> Pool Postgresql -> Int -> NodeInternalState -> m ()
putState logger db pid state = void $ runLoggingEnv logger $ runDb (Identity db) $ do
  $(logDebugSH) ("putState" :: Text, pid, state)
  let
    id_ = NodeInternal_idField
    data_ = NodeInternal_dataField ~> DeletableRow_dataSelector
    backend_ = data_ ~> NodeInternalData_backendSelector
    state_ = data_ ~> NodeInternalData_stateSelector
    backend = Just pid

  result <- project NodeInternalConstructor $ state_ /=. state &&. (backend_ ==. backend ||. backend_ ==. (Nothing :: Maybe Int))
  update [backend_ =. backend , state_ =. state] $ id_ `in_` fmap _nodeInternal_id result
  for_ result $ \(NodeInternal nid ndata) ->
    notify (Notify_NodeInternal nid $ Just $ (_deletableRow_data $ ndata)
      { _nodeInternalData_state = state
      , _nodeInternalData_backend = backend
      })

callNode :: (MonadBaseControl IO m, MonadIO m, MonadMask m) => AppConfig -> LoggingEnv -> Pool Postgresql -> FilePath -> Int -> m ()
callNode appConfig logger db nodePath pid = (putState logger db pid NodeInternalState_Initializing *>) $ withTempFile "." ".tezos-node-config.json" $ \nodeConfigPath nodeConfigHandle -> do
  let nodeConfig = defaultConfig
  let dataDir = fromMaybe (error "specify data-dir") $ _nodeConfigFile_dataDir nodeConfig
  let versionFile = dataDir `combine` "version.json"
  let identityFile = dataDir `combine` "identity.json"
  let nodePort = show $ _appConfig_kilnNodePort appConfig
  liftIO (print nodeConfigPath)
  liftIO (LBS.hPut nodeConfigHandle $ Aeson.encode nodeConfig)
  liftIO (hFlush nodeConfigHandle)
  -- liftIO . putStrLn =<< liftIO (readProcess "cat" [nodeConfigPath] "")
  haveVersionFile <- liftIO $ doesFileExist versionFile
  when (not haveVersionFile) $
    liftIO . putStrLn =<< liftIO (readProcess nodePath ["config", "show", "--config-file", nodeConfigPath] "")

  haveIdentityFile <- liftIO $ doesFileExist identityFile
  when (not haveIdentityFile) $
    liftIO . putStrLn =<< liftIO (readProcess nodePath ["identity", "generate", "--config-file", nodeConfigPath] "")

  putState logger db pid NodeInternalState_Starting

  liftIO $ withCreateProcess (proc nodePath ["run", "--config-file", nodeConfigPath, "--rpc-addr", ":" <> nodePort]) go0
    where
      go0 _ _ _ ph = runLoggingEnv logger go
        where
          {-# INLINE go #-}
          go :: forall m1. (MonadLogger m1, MonadIO m1, MonadBaseControl IO m1) => m1 ()
          go = do
            nis <- runDb (Identity db) selectAll
            let shouldDelete = any (_deletableRow_deleted . _nodeInternal_data . snd) nis
                shouldRun = any (\(_, ni) -> let dr = _nodeInternal_data ni in not (_deletableRow_deleted dr) && _nodeInternalData_running (_deletableRow_data dr)) nis
            when shouldDelete $ do
              liftIO $ terminateProcess ph
              -- Wait 5s for clean termination | TODO need to recover from processes which haven't actually terminated?
              _ <- liftIO $ timeout 5e6 $ waitForProcess ph
              for_ (_nodeConfigFile_dataDir defaultConfig) $ \dir -> do
                $(logWarn) $ "Removing tezos-node data from " <> T.pack dir
                liftIO $ removeDirectoryRecursive dir
            liftIO (getProcessExitCode ph) >>= \case
              Nothing -> do
                putState logger db pid NodeInternalState_Running
                when (not shouldRun) $ liftIO $ terminateProcess ph
                threadDelay' 1 *> go
              Just e -> liftIO $
                if shouldRun
                  -- TODO: logging!
                  then do
                    putState logger db pid NodeInternalState_Failed
                    print ("node exited unexpectedly" :: Text, e)
                  else do
                    putState logger db pid NodeInternalState_Stopped
                    print ("node exited sucessfully" :: Text, e)



data NodeConfigRPC = NodeConfigRPC
  { _nodeConfigRPC_listenAddr :: !(Maybe Text)
  , _nodeConfigRPC_corsOrigin :: !(Maybe [Text])
  , _nodeConfigRPC_corsHeaders :: !(Maybe [Text])
  , _nodeConfigRPC_crt :: !(Maybe Text)
  , _nodeConfigRPC_key :: !(Maybe Text)
  }

data NodeConfigP2PLimits = NodeConfigP2PLimits
  { _nodeConfigP2PLimits_connectionTimeout :: !(Maybe Double)
  , _nodeConfigP2PLimits_authenticationTimeout :: !(Maybe Double)
  , _nodeConfigP2PLimits_minConnections :: !(Maybe Word16)
  , _nodeConfigP2PLimits_expectedConnections :: !(Maybe Word16)
  , _nodeConfigP2PLimits_maxConnections :: !(Maybe Word16)
  , _nodeConfigP2PLimits_backlog :: !(Maybe Word8)
  , _nodeConfigP2PLimits_maxIncomingConnections :: !(Maybe Word8)
  , _nodeConfigP2PLimits_maxDownloadSpeed :: !(Maybe Int)
  , _nodeConfigP2PLimits_maxUploadSpeed :: !(Maybe Int)
  , _nodeConfigP2PLimits_swapLinger :: !(Maybe Double)
  , _nodeConfigP2PLimits_binaryChunksSize :: !(Maybe Word8)
  , _nodeConfigP2PLimits_readBufferSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_readQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_writeQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_incomingAppMessageQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_incomingMessageQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_outgoingMessageQueueSize :: !(Maybe Int)
  , _nodeConfigP2PLimits_knownPointsHistorySize :: !(Maybe Word16)
  , _nodeConfigP2PLimits_knownPeerIdsHistorySize :: !(Maybe Word16)
  , _nodeConfigP2PLimits_maxKnownPoints :: !(Maybe (Int, Int))
  , _nodeConfigP2PLimits_maxKnownPeerIds :: !(Maybe (Int, Int))
  , _nodeConfigP2PLimits_greylistTimeout :: !(Maybe Int)
  }

data NodeConfigP2P = NodeConfigP2P
  { _nodeConfigP2P_expectedProofOfWork :: !(Maybe Double)
  , _nodeConfigP2P_bootstrapPeers :: !(Maybe [Text])
  , _nodeConfigP2P_listenAddr :: !(Maybe Text)
  , _nodeConfigP2P_privateMode :: !(Maybe Bool)
  , _nodeConfigP2P_disableMempool :: !(Maybe Bool)
  }
data NodeConfigLog = NodeConfigLog
  { _nodeConfigLog_output :: !(Maybe Text)
  , _nodeConfigLog_level :: !(Maybe Text)
  , _nodeConfigLog_rules :: !(Maybe Text)
  , _nodeConfigLog_template :: !(Maybe Text)
  }

data NodeConfigShell = NodeConfigShell
  { _nodeConfigShell_peerValidator :: !(Maybe NodeConfigShellPeerValidator)
  , _nodeConfigShell_blockValidator :: !(Maybe NodeConfigShellBlockValidator)
  , _nodeConfigShell_prevalidator :: !(Maybe NodeConfigShellPrevalidator)
  , _nodeConfigShell_chainValidator :: !(Maybe NodeConfigShellChainValidator)
  }

data NodeConfigShellPeerValidator = NodeConfigShellPeerValidator
  { _nodeConfigShellPeerValidator_blockHeaderRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_blockOperationsRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_protocolRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_newHeadRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_workerBacklogSize :: !(Maybe Word16)
  , _nodeConfigShellPeerValidator_workerBacklogLevel :: !(Maybe Text)
  , _nodeConfigShellPeerValidator_workerZombieLifetime :: !(Maybe Double)
  , _nodeConfigShellPeerValidator_workerZombieMemory :: !(Maybe Double)
  }

data NodeConfigShellBlockValidator = NodeConfigShellBlockValidator
  { _nodeConfigShellBlockValidator_protocolRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellBlockValidator_workerBacklogSize :: !(Maybe Word16)
  , _nodeConfigShellBlockValidator_workerBacklogLevel :: !(Maybe Text)
  , _nodeConfigShellBlockValidator_workerZombieLifetime :: !(Maybe Double)
  , _nodeConfigShellBlockValidator_workerZombieMemory :: !(Maybe Double)
  }
data NodeConfigShellPrevalidator = NodeConfigShellPrevalidator
  { _nodeConfigShellPrevalidator_operationsRequestTimeout :: !(Maybe Double)
  , _nodeConfigShellPrevalidator_maxRefusedOperations :: !(Maybe Word16)
  , _nodeConfigShellPrevalidator_workerBacklogSize :: !(Maybe Word16)
  , _nodeConfigShellPrevalidator_workerBacklogLevel :: !(Maybe Text)
  , _nodeConfigShellPrevalidator_workerZombieLifetime :: !(Maybe Double)
  , _nodeConfigShellPrevalidator_workerZombieMemory :: !(Maybe Double)
  }
data NodeConfigShellChainValidator = NodeConfigShellChainValidator
  { _nodeConfigShellChainValidator_bootstrapThreshold :: !(Maybe Word8)
  , _nodeConfigShellChainValidator_workerBacklogSize :: !(Maybe Word16)
  , _nodeConfigShellChainValidator_workerBacklogLevel :: !(Maybe Text)
  , _nodeConfigShellChainValidator_workerZombieLifetime :: !(Maybe Double)
  , _nodeConfigShellChainValidator_workerZombieMemory :: !(Maybe Double)
  }

data NodeConfigFile = NodeConfigFile
  { _nodeConfigFile_p2p :: !NodeConfigP2P
  , _nodeConfigFile_dataDir :: !(Maybe FilePath)
  , _nodeConfigFile_rpc :: !(Maybe NodeConfigRPC)
  , _nodeConfigFile_log :: !(Maybe NodeConfigLog)
  , _nodeConfigFile_shell :: !(Maybe NodeConfigShell)
  }

concat <$> traverse (Aeson.deriveJSON tezosJsonOptions
  { Aeson.fieldLabelModifier
    = map (\case {'_' -> '-'; x -> x})
    . Aeson.fieldLabelModifier tezosJsonOptions
  , Aeson.omitNothingFields = True
    })
  [ ''NodeConfigFile
  , ''NodeConfigLog
  , ''NodeConfigP2P
  , ''NodeConfigRPC
  , ''NodeConfigShell
  , ''NodeConfigShellBlockValidator
  , ''NodeConfigShellChainValidator
  , ''NodeConfigShellPeerValidator
  , ''NodeConfigShellPrevalidator
  ]
