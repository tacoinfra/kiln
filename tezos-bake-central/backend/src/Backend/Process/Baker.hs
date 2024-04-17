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
{-# LANGUAGE RecordWildCards #-}

module Backend.Process.Baker where

import Conduit (runConduit, sourceHandle, (.|))
import UnliftIO.STM (atomically, writeTQueue)
import Control.Monad (liftM2)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, logDebug, logError)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import qualified Data.Conduit.List as CL
import Data.Either.Combinators (maybeToRight)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Database.Id.Groundhog (fromId)
import Database.Groundhog.Postgresql
import Fmt (pretty)
import GHC.IO.Handle.FD (handleToFd)
import Rhyolite.Backend.DB (MonadBaseNoPureAborts, project1)
import UnliftIO.Concurrent (forkIO, killThread)
import UnliftIO.Exception (bracket, finally)
import UnliftIO.Process as Proc
import UnliftIO.IO (Handle, hClose)
import System.Which (staticWhich)
import Text.URI (render)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Tezos.Types

import Backend.Alerts (reportLedgerDisconnection)
import Backend.Common.Baker
import Backend.Common.Worker (worker')
import Backend.Config (AppConfig (..),  BinaryPaths(..), BakerPath(..), HasAppConfig (..), kilnNodeRpcURI, nodeDataDir, tezosClientDataDir)
import Backend.Env
import Backend.NodeRPC
import Backend.Process.Common
import Backend.Process.Errors
import Backend.Schema
import Backend.Workers.TezosClient (checkLedgerHighWatermark)
import Common.App
import Common.Schema
import ExtraPrelude

getBakerPath
  :: NonEmpty BakerPath
  -> Maybe ProtocolHash
  -> Either BakerBootstrapError FilePath
getBakerPath paths mbProto =
  maybeToRight (BakerBootstrapError mbProto) $ case mbProto of
    Nothing -> _bakerPath_path $ NonEmpty.head paths
    Just protoHash -> _bakerPath_path =<< find (\bp -> _bakerPath_proto bp == protoHash) paths

defaultBakerPaths :: NonEmpty BakerPath
defaultBakerPaths = NonEmpty.fromList [parisPath, oxfordPath]
  where
    parisPath = BakerPath
      { _bakerPath_proto = ParisBProtocolHash
      , _bakerPath_path = Just $(staticWhich "tezos-baker-PtParisB")
      }
    oxfordPath = BakerPath
      { _bakerPath_proto = OxfordProtocolHash
      , _bakerPath_path = Just $(staticWhich "tezos-baker-Proxford")
      }

bakerDaemonProcess
  :: ( MonadUnliftIO m
     , MonadUnliftIO w
     , MonadBaseNoPureAborts IO m
     , MonadBaseNoPureAborts IO w
     )
  => AppConfig
  -> NodeDataSource
  -> Maybe BinaryPaths
  -> m (w ())
bakerDaemonProcess appConfig nds mbCustomPaths = runLoggerWithEnv $ do
  bdid <- runTransaction $ do
    project1 (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
      Just bdid -> return bdid
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

        bpid <- insert' processData
        tbpid <- insert' processData
        nid <- insert' BakerDaemon
        let bdid = BakerDaemonInternalData
              { _bakerDaemonInternalData_alias = "ledger_kiln"
              , _bakerDaemonInternalData_publicKeyHash = Nothing
              , _bakerDaemonInternalData_protocol = psdd
              , _bakerDaemonInternalData_bakerProcessData = bpid
              , _bakerDaemonInternalData_altProtocol = Nothing
              , _bakerDaemonInternalData_altBakerProcessData = tbpid
              }
            -- Add this as default protocol, we will anyways fix this in protocolMonitorWorker once the synced node is available
            psdd :: ProtocolHash
            psdd = ProtocolHash "PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP"
        insert $ BakerDaemonInternal
          { _bakerDaemonInternal_id = nid
          , _bakerDaemonInternal_data = DeletableRow
            { _deletableRow_data = bdid
            , _deletableRow_deleted = True
            }
          }
        return bdid
  let
    bpid1 = _bakerDaemonInternalData_bakerProcessData bdid
    bpid2 = _bakerDaemonInternalData_altBakerProcessData bdid
    paths = maybe defaultBakerPaths _binaryPaths_bakerPaths mbCustomPaths
    mkBakerWorker pid = bakerProcessWorker appConfig nds pid paths

  -- We run two sets of bakerProcessWorkers, which one actually runs the main baker/alt baker
  -- depends upon the protocol set for that PID.
  -- This allows us to switch a 'alt baker' to 'main baker' without actually restarting the baker
  -- ie bp1 starts as main baker, bp2 as alt baker
  -- after voting period ends, we simply stop the bp1 and set bpid2 as 'bakerProcessData'
  -- So bp2 process keeps on running but is now identified as 'main baker'
  bp1 <- mkBakerWorker bpid1
  bp2 <- mkBakerWorker bpid2
  return (bp1 *> bp2)
  where
    runLoggerWithEnv act = flip runReaderT (KilnEnv appConfig nds) $ runLogger act

-- protocol is a variable field, and therefore it is fetched everytime we restart process
fetchProtocol
  :: (PersistBackend m)
  => Id ProcessData
  -> m (Maybe ProtocolHash)
fetchProtocol pid =
  project1 (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
    Nothing -> error "BakerDaemonInternal table empty"
    Just bdid ->
      let
        tbpid = _bakerDaemonInternalData_altBakerProcessData bdid
      in if pid == tbpid
        then return $ _bakerDaemonInternalData_altProtocol bdid
        else return $ Just $ _bakerDaemonInternalData_protocol bdid

createBakerProcess
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => NonEmpty BakerPath
  -> Maybe ProtocolHash
  -> m (Either BakerBootstrapError CreateProcess)
createBakerProcess paths mbProto = runExceptT $ do
  bakerPath <- liftEither $ getBakerPath paths mbProto
  bakerArgs <- lift getBakerArgs
  pure $ proc bakerPath bakerArgs

getBakerArgs
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => m [String]
getBakerArgs = do
  mbBakerData <- runTransaction $ project1
    (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty
  chainId <- askChainId
  appConfig <- askAppConfig
  let
    bakerData = case mbBakerData of
      Nothing -> error "'getBakerArgs': 'BakerDaemonInternalData' is 'Nothing'."
      Just bd -> bd
    pkh = flip fromMaybe (_bakerDaemonInternalData_publicKeyHash bakerData) $
        error "'getBakerArgs': baker public key hash is 'Nothing'."
    alias = T.unpack $ _bakerDaemonInternalData_alias bakerData
    protocolAgnosticArgs =
      [ "--endpoint", T.unpack $ render $ kilnNodeRpcURI appConfig
      , "--base-dir", tezosClientDataDir appConfig
      , "run", "with", "local", "node", nodeDataDir appConfig
      , alias
      ]
  extraArgs <- runTransaction $ select $
    BakerExtraArgs_publicKeyHashField ==. pkh &&.
    BakerExtraArgs_chainIdField ==. chainId
  $(logDebug) $ "Baker extra args: " <> tshow extraArgs
  let extraArgsCmd = fmap T.unpack $ concatMap toCmdArg extraArgs
  bakerCustomArgs <- getKilnBakerCustomArgs
  pure $ protocolAgnosticArgs <> bakerCustomArgs <> extraArgsCmd

-- | Octez-node needs some time before it becomes able to respond to RPC queries.
-- Due to this, daemons may fail with connection timeout. So we check that node
-- is actually able to respond to requests before starting the baker.
checkKilnNodeAvailability
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasNodeDataSource e
     , HasAppConfig e
     )
  => m Bool
checkKilnNodeAvailability = do
  appConfig <- askAppConfig
  nds <- asks (view nodeDataSource)
  isRight <$> sendKilnNodeHealthcheckRequest appConfig nds

-- | To initialize baker process we need to get its extra arguments from the database
-- for this we need to make sure that its public key hash presents in 'BakerDaemonInternal' table
checkBakerPkhPresence
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasNodeDataSource e
     , HasAppConfig e
     )
  => m Bool
checkBakerPkhPresence = isJust . join <$> do
  runTransaction $ project1
    (  BakerDaemonInternal_dataField
    ~> DeletableRow_dataSelector
    ~> BakerDaemonInternalData_publicKeyHashSelector
    ) CondEmpty

bakerPrestartCheck
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasNodeDataSource e
     , HasAppConfig e
     )
  => m Bool
bakerPrestartCheck = liftM2 (&&) checkKilnNodeAvailability checkBakerPkhPresence

updateBakerProcessState
  :: ( MonadLogger m
     , PersistBackend m
     , MonadIO m
     )
  => Id ProcessData
  -> ProcessState
  -> m ()
updateBakerProcessState pid ps = updateProcessState pid Nothing ps

jsonLogsConsumer
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => Handle
  -> m ()
jsonLogsConsumer h = runConduit $ sourceHandle h .| CL.mapM_ (\errlogLine -> runLogger $ do
    case Aeson.eitherDecodeStrict errlogLine of
      Left decodingErr ->
        $(logError) $ T.unlines
          [ "Failed to decode error reported by baker daemons: " <> T.pack decodingErr
          , "Error: " <> T.decodeUtf8 errlogLine
          ]
      Right ev -> do
        $(logError) $ "Baker daemon reported an error: " <> pretty ev
        handleDaemonErrorEvent ev
  )

handleDaemonErrorEvent
  :: ( MonadUnliftIO m
     , MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => ErrorEvent
  -> m ()
handleDaemonErrorEvent e = do
  db <- askPool
  appConfig <- askAppConfig
  nds <- asks (view nodeDataSource)
  let trace = _errorEvent_trace e
      isLedgerNotFound = \case
        ErrorTrace_LedgerNotFound -> True
        _ -> False
      isWrongApp = \case
        ErrorTrace_LedgerError msg | "Application level error (sign-with-hash): Parse error" `T.isPrefixOf` msg -> True
        _ -> False
      isWrongHWM = \case
        ErrorTrace_LedgerError msg | "Application level error (sign-with-hash): Incorrect data" `T.isPrefixOf` msg -> True
        _ -> False
      hasLedgerDisconnection = any isLedgerNotFound trace
      hasWrongApp = any isWrongApp trace
      needToResetHWM = any isWrongHWM trace
  when (hasLedgerDisconnection || hasWrongApp) $ reportLedgerDisconnection db appConfig hasWrongApp
  when hasLedgerDisconnection $ runTransaction $ do
    mbConnectedLedger :: Maybe ConnectedLedger <- fmap listToMaybe $ select CondEmpty
    for_ mbConnectedLedger $ \connectedLedger -> do
      update
        [ ConnectedLedger_ledgerIdentifierField =. (Nothing :: Maybe LedgerIdentifier)
        ] CondEmpty
      notify NotifyTag_ConnectedLedger $ Just $ connectedLedger { _connectedLedger_ledgerIdentifier = Nothing }
  when needToResetHWM $ do
    let ledgerIOQueue = _nodeDataSource_ledgerIOQueue nds
    liftIO $ atomically $ writeTQueue ledgerIOQueue $ checkLedgerHighWatermark appConfig nds

bakerProcessWorker
  :: ( MonadUnliftIO m
     , MonadUnliftIO w
     , MonadBaseNoPureAborts IO m
     )
  => AppConfig
  -> NodeDataSource
  -> Id ProcessData
  -> NonEmpty BakerPath
  -> m (w ())
bakerProcessWorker appConfig nds pid paths = mkWorker $ do
  let updateState = updateBakerProcessState pid
  waitUntilShouldRun pid DaemonType_Baker bakerPrestartCheck
  withProcessLock pid $ do
    runTransaction $ updateState ProcessState_Initializing
    mbProtoHash <- runTransaction $ fetchProtocol pid
    runTransaction $ updateState ProcessState_Starting
    eiProcHandler <- createBakerProcess paths mbProtoHash
    case eiProcHandler of
      Left err -> runTransaction $ update
        [ ProcessData_errorLogField =. Just (T.pack $ pretty err)
        , ProcessData_stateField =. ProcessState_Stopped
        , ProcessData_controlField =. ProcessControl_Stop
        ] $ AutoKeyField ==. fromId pid
      Right procHandler ->
        let closeHandles (h1, h2) = hClose h1 >> hClose h2
            withReadWriteHandles = bracket Proc.createPipe closeHandles
        in
        withReadWriteHandles $ \(readHandle, writeHandle) -> do
          writeFD <- liftIO $ handleToFd writeHandle
          handlerThreadId <- forkIO $ jsonLogsConsumer readHandle
          let
            -- Logging env variables below are set based on the logging documentation from
            -- https://tezos.gitlab.io/user/logging.html#file-descriptor-sinks
            envVarName = "TEZOS_EVENTS_CONFIG"
            envVarValue = mconcat
              [ "file-descriptor-path:///dev/fd/"
              , show writeFD
              , "?format=one-per-line&level-at-least=error"
              ]
            tezosLogEnv = [(envVarName, envVarValue)]
          startProcMonitor procHandler tezosLogEnv DaemonType_Baker pid updateState
            `finally` killThread handlerThreadId
  where
    mkWorker act = worker' "bakerProcessWorker" $
      flip runReaderT (KilnEnv appConfig nds) $ runLogger act
