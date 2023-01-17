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
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Monad (liftM2)
import Control.Monad.Logger (LoggingT, logDebug, logError)
import qualified Data.Aeson as Aeson
import qualified Data.Conduit.List as CL
import Data.Either.Combinators (maybeToRight)
import Data.Pool (Pool)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Database.Groundhog.Postgresql
import Fmt (pretty)
import Named
import Rhyolite.Backend.DB (MonadBaseNoPureAborts, getTime, runDb, project1)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import System.Process as Proc
import System.IO (Handle)
import System.Which (staticWhich)
import Text.URI (render)
import qualified Data.Text as T

import Tezos.NodeRPC (NodeRPCContext(..), QueryNode(rIsBootstrapped), RpcError, nodeRPC)
import Tezos.Types

import Backend.Alerts (reportLedgerDisconnection)
import Backend.Common.Baker
import Backend.Config (AppConfig (..),  BinaryPaths(..), BakerPath(..), kilnNodeRpcURI, nodeDataDir, tezosClientDataDir)
import Backend.NodeRPC
import Backend.Process.Errors
import Backend.Schema
import Backend.Workers.Process
import Backend.Workers.TezosClient (checkLedgerHighWatermark)
import Common.App
import Common.Schema
import ExtraPrelude

getBakerPath :: NonEmpty BakerPath -> Maybe ProtocolHash -> Maybe FilePath
getBakerPath paths = \case
  Nothing -> _bakerPath_path $ NonEmpty.head paths
  Just protoHash ->
    let err = error ("tezos-baker is not available for the given protocol: " <> T.unpack (toBase58Text protoHash))
    in maybe err _bakerPath_path $ find (\bp -> _bakerPath_proto bp == protoHash) paths

defaultBakerPaths :: NonEmpty BakerPath
defaultBakerPaths = NonEmpty.fromList [limaPath, kathmanduPath]
  where
    limaPath = BakerPath
      { _bakerPath_proto = LimaProtocolHash
      , _bakerPath_path = Just $(staticWhich "tezos-baker-PtLimaPt")
      }
    kathmanduPath = BakerPath
      { _bakerPath_proto = KathmanduProtocolHash
      , _bakerPath_path = Just $(staticWhich "tezos-baker-PtKathma")
      }

-- Start Baker and Endorser
bakerDaemonProcess :: (MonadIO m, MonadBaseNoPureAborts IO m)
  => AppConfig -> NodeDataSource -> LoggingEnv -> Pool Postgresql -> Maybe BinaryPaths -> m (IO ())
bakerDaemonProcess appConfig nds logger db mbCustomPaths = do
  bdid <- runLoggingEnv logger $ runDb (Identity db) $ do
    project1 (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty >>= \case
      (Just bdid) -> return bdid
      Nothing -> do
        let processData = ProcessData
              { _processData_control = ProcessControl_Stop
              , _processData_state = ProcessState_Stopped
              , _processData_updated = Nothing
              , _processData_backend = Nothing
              , _processData_errorLog = Nothing
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
            psdd = "PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP"
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

    -- tezos-node needs some time before it becomes able to respond to RPC queries.
    -- Due to this, daemons may fail with connection timeout. So we check that node
    -- is actually able to respond to requests before starting the baker.
    checkKilnNodeAvailability :: IO Bool
    checkKilnNodeAvailability = isRight <$> do
      runExceptT @RpcError . flip runReaderT (NodeRPCContext (_nodeDataSource_httpMgr nds) (render $ kilnNodeRpcURI appConfig)) $
        runLoggingEnv logger $ nodeRPC (rIsBootstrapped $ _nodeDataSource_chain nds)

    -- to initialize baker process we need to get its extra arguments from the database
    -- for this we need to make sure that its public key hash presents in 'BakerDaemonInternal' table
    checkBakerPkhPresence :: IO Bool
    checkBakerPkhPresence = isJust . join <$> do
      runLoggingEnv logger $ runDb (Identity db) $ project1
        (  BakerDaemonInternal_dataField
        ~> DeletableRow_dataSelector
        ~> BakerDaemonInternalData_publicKeyHashSelector
        ) CondEmpty

    bakerPrestartCheck :: IO Bool
    bakerPrestartCheck = liftM2 (&&) checkKilnNodeAvailability checkBakerPkhPresence

    pw mkProcess pid  = processWorker
      (\_ -> runLoggingEnv logger $ runDb (Identity db) $ fetchProtocol pid)
      ! #logger logger
      ! #db db
      ! #config appConfig
      ! #mkProcess mkProcess
      ! #pid pid
      ! #prestartCheck bakerPrestartCheck
      ! #mkNotify Nothing

    jsonLogsConsumer :: Handle -> IO ()
    jsonLogsConsumer h = runConduit $ sourceHandle h .| CL.mapM_ (\errlogLine -> runLoggingEnv logger $ do
        case Aeson.eitherDecodeStrict errlogLine of
          Left decodingErr ->
            $(logError) $ "Failed to decode error reported by baker daemons: " <> T.pack decodingErr
          Right ev -> do
            $(logError) $ "Baker daemon reported an error: " <> pretty ev
            handleDaemonErrorEvent ev
      )

    handleDaemonErrorEvent :: ErrorEvent -> LoggingT IO ()
    handleDaemonErrorEvent e = do
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
      when hasLedgerDisconnection $ runDb (Identity db) $ do
        mbConnectedLedger :: Maybe ConnectedLedger <- fmap listToMaybe $ select CondEmpty
        for_ mbConnectedLedger $ \connectedLedger -> do
          now <- getTime
          update
            [ ConnectedLedger_ledgerIdentifierField =. (Nothing :: Maybe LedgerIdentifier)
            , ConnectedLedger_updatedField =. Just now
            ] CondEmpty
          notify NotifyTag_ConnectedLedger $ Just $ connectedLedger { _connectedLedger_ledgerIdentifier = Nothing }
      when needToResetHWM $ do
        let ledgerIOQueue = _nodeDataSource_ledgerIOQueue nds
        liftIO $ atomically $ writeTQueue ledgerIOQueue $ checkLedgerHighWatermark appConfig nds

    paths = maybe defaultBakerPaths _binaryPaths_bakerPaths mbCustomPaths

    mkBakerProcess = createBakerProcess appConfig logger db paths
    bakerPw = pw mkBakerProcess ! #logNamespace "kiln-baker" ! #jsonErrorLogsHandler (Just jsonLogsConsumer)

  -- We run two sets of ProcessWorkers, which one actually runs the main baker/alt baker
  -- depends upon the protocol set for that PID.
  -- This allows us to switch a 'alt baker' to 'main baker' without actually restarting the baker
  -- ie bp1 starts as main baker, bp2 as alt baker
  -- after voting period ends, we simply stop the bp1 and set bpid2 as 'bakerProcessData'
  -- So bp2 process keeps on running but is now identified as 'main baker'
  bp1 <- bakerPw bpid1
  bp2 <- bakerPw bpid2
  return (bp1 *> bp2)

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
      in if pid == tbpid
        then return $ _bakerDaemonInternalData_altProtocol bdid
        else return $ Just $ _bakerDaemonInternalData_protocol bdid

createBakerProcess
  :: AppConfig
  -> LoggingEnv
  -> Pool Postgresql
  -> NonEmpty BakerPath
  -> Maybe ProtocolHash
  -> IO (Either DaemonBootstrapError CreateProcess)
createBakerProcess appConfig logger db paths mbProto = do
  let bakerPath = getBakerPath paths mbProto
  bakerArgs <- getBakerArgs appConfig logger db
  pure $ createDaemonProcess bakerPath bakerArgs "tezos-baker" mbProto

-- | Creates daemon process from the binary path and arguments.
-- Returns either 'CreateProcess' or error message if path or arguments
-- are not specified.
createDaemonProcess
 :: Maybe FilePath
 -> Either DaemonBootstrapError [String]
 -> Text
 -> Maybe ProtocolHash
 -> Either DaemonBootstrapError CreateProcess
createDaemonProcess path args daemonName mbProto = do
  let
    eiBinaryPath = maybeToRight (DaemonBootstrapError_NoBinary daemonName mbProto) path
  binaryPath <- eiBinaryPath
  binaryArgs <- args
  pure $ proc binaryPath binaryArgs

getBakerArgs
  :: AppConfig
  -> LoggingEnv
  -> Pool Postgresql
  -> IO (Either DaemonBootstrapError [String])
getBakerArgs appConfig logger db = do
  mbBakerData <- runLoggingEnv logger $ runDb (Identity db) $ project1
    (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector) CondEmpty
  let
    bakerData = case mbBakerData of
      Nothing -> error "'getBakerArgs': 'BakerDaemonInternalData' is 'Nothing'."
      Just bd -> bd
    pkh = flip fromMaybe (_bakerDaemonInternalData_publicKeyHash bakerData) $
        error "'getBakerArgs': baker public key hash is 'Nothing'."
    chainId = _appConfig_chainId appConfig
    alias = T.unpack $ _bakerDaemonInternalData_alias bakerData
  extraArgs <- runLoggingEnv logger $ runDb (Identity db) $ select $
    BakerExtraArgs_publicKeyHashField ==. pkh &&.
    BakerExtraArgs_chainIdField ==. chainId
  runLoggingEnv logger $
    $(logDebug) $ "Baker extra args: " <> tshow extraArgs
  let extraArgsCmd = fmap T.unpack $ concatMap toCmdArg extraArgs
  bakerCustomArgs <- runLoggingEnv logger $ getKilnBakerCustomArgs appConfig
  pure $ Right $ protocolAgnosticArgs alias <> bakerCustomArgs <> extraArgsCmd
  where
    protocolAgnosticArgs alias =
      [ "--endpoint", T.unpack $ render $ kilnNodeRpcURI appConfig
      , "--base-dir", tezosClientDataDir appConfig
      , "run", "with", "local", "node", nodeDataDir appConfig
      , alias
      ]
