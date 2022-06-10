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

module Backend.Process.Baker where

import Conduit (runConduit, sourceHandle, (.|))
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
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (getTime, runDb, project1)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import System.Process as Proc
import System.IO (Handle)
import System.Which (staticWhich)
import Text.URI (render)
import qualified Data.Text as T

import Tezos.NodeRPC (NodeRPCContext(..), QueryNode(rIsBootstrapped), RpcError, nodeRPC)
import Tezos.Types (LedgerIdentifier, ProtocolHash, toBase58Text)

import Backend.Common.Baker
import Backend.Config (AppConfig (..), kilnNodeRpcURI, nodeDataDir, tezosClientDataDir, BinaryPaths(..), BakerEndorserPaths(..))
import Backend.NodeRPC
import Backend.Process.Errors (ErrorEvent(..), ErrorTrace(..))
import Backend.Schema
import Backend.Workers.Process
import Backend.Workers.TezosClient (reportLedgerDisconnection)
import Common.App
import Common.Schema
import ExtraPrelude

getBakerPath :: NonEmpty BakerEndorserPaths -> Maybe ProtocolHash -> Maybe FilePath
getBakerPath = getPath _bakerEndorserPaths_bakerPath

getEndorserPath :: NonEmpty BakerEndorserPaths -> Maybe ProtocolHash -> Maybe FilePath
getEndorserPath = getPath _bakerEndorserPaths_endorserPath

getPath
  :: (BakerEndorserPaths -> Maybe FilePath)
  -> NonEmpty BakerEndorserPaths
  -> Maybe ProtocolHash
  -> Maybe FilePath
getPath getter paths = \case
  Nothing -> getter $ NonEmpty.head paths
  Just p -> maybe e getter $ find (\bep -> _bakerEndorserPaths_proto bep == p) paths
    where
      e = error ("tezos-baker/endorser not available for the given protocol: " <> show p)

-- You cannot use a mainnet binary against a babylonnet node because the mainnet
-- binary expects a .tezos-node/<chain_id>/protocol dir
-- https://gitlab.com/tezos/tezos/compare/mainnet...babylonnet#a59616ef23c1f6b8d578e385e82f6c4d4dadedde_49_46
tezosBinaryPaths :: NonEmpty BakerEndorserPaths
tezosBinaryPaths = NonEmpty.fromList [ithacaPaths, jakartaPaths]
  where
    ithacaPaths = BakerEndorserPaths
      { _bakerEndorserPaths_proto = IthacaProtocolHash
      , _bakerEndorserPaths_bakerPath = Just $(staticWhich "tezos-baker-012-Psithaca")
      , _bakerEndorserPaths_endorserPath = Nothing
      }
    jakartaPaths = BakerEndorserPaths
      { _bakerEndorserPaths_proto = JakartaProtocolHash
      , _bakerEndorserPaths_bakerPath = Just $(staticWhich "tezos-baker-013-PtJakart")
      , _bakerEndorserPaths_endorserPath = Nothing
      }

-- Start Baker and Endorser
bakerDaemonProcess :: (MonadIO m, MonadBaseNoPureAborts IO m)
  => AppConfig -> NodeDataSource -> LoggingEnv -> Pool Postgresql -> Maybe BinaryPaths -> m (IO ())
bakerDaemonProcess appConfig nds logger db maybePaths = do
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
        epid <- insert' processData
        tbpid <- insert' processData
        tepid <- insert' processData
        nid <- insert' BakerDaemon
        let bdid = BakerDaemonInternalData
              { _bakerDaemonInternalData_alias = "ledger_kiln"
              , _bakerDaemonInternalData_publicKeyHash = Nothing
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
        return bdid
  let
    bpid1 = _bakerDaemonInternalData_bakerProcessData bdid
    epid1 = _bakerDaemonInternalData_endorserProcessData bdid
    bpid2 = _bakerDaemonInternalData_altBakerProcessData bdid
    epid2 = _bakerDaemonInternalData_altEndorserProcessData bdid

    -- tezos-node needs some time before it becomes able to respond to RPC queries.
    -- Due to this, daemons may fail with connection timeout. So we check that node
    -- is actually able to respond to requests before starting baker/endorser
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
          hasLedgerDisconnection = any isLedgerNotFound trace
          hasWrongApp = any isWrongApp trace
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

    paths = maybe tezosBinaryPaths _binaryPaths_bakerEndorserPaths maybePaths

    mkBakerProcess = createBakerProcess appConfig logger db paths
    mkEndorserProcess = createEndorserProcess appConfig paths bdid

    bakerPw = pw mkBakerProcess ! #logNamespace "kiln-baker" ! #jsonErrorLogsHandler (Just jsonLogsConsumer)
    endorserPw = pw mkEndorserProcess ! #logNamespace "kiln-endorser" ! #jsonErrorLogsHandler Nothing

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

createBakerProcess
  :: AppConfig
  -> LoggingEnv
  -> Pool Postgresql
  -> NonEmpty BakerEndorserPaths
  -> Maybe ProtocolHash
  -> IO (Either Text CreateProcess)
createBakerProcess appConfig logger db paths mbProto = do
  let bakerPath = getBakerPath paths mbProto
  bakerArgs <- getBakerArgs appConfig logger db mbProto
  pure $ createDaemonProcess bakerPath bakerArgs "tezos-baker" mbProto

createEndorserProcess
  :: AppConfig
  -> NonEmpty BakerEndorserPaths
  -> BakerDaemonInternalData
  -> Maybe ProtocolHash
  -> IO (Either Text CreateProcess)
createEndorserProcess appConfig paths bakerData mbProto = do
  let endorserPath = getEndorserPath paths mbProto
  pure $ createDaemonProcess endorserPath (Right endorserArgs) "tezos-endorser" mbProto
  where
    alias = T.unpack $ _bakerDaemonInternalData_alias bakerData
    endorserArgs =
      [ "--endpoint", T.unpack $ render $  kilnNodeRpcURI appConfig
      , "--base-dir", tezosClientDataDir appConfig
      , "run"
      , alias
      ]

-- | Creates daemon process from the binary path and arguments.
-- Returns either 'CreateProcess' or error message if path or arguments
-- are not specified.
createDaemonProcess
 :: Maybe FilePath
 -> Either Text [String]
 -> Text
 -> Maybe ProtocolHash
 -> Either Text CreateProcess
createDaemonProcess path args daemonName mbProto = do
  let
    prettyProtoHash = maybe "<unknown protocol>" toBase58Text mbProto
    eiBinaryPath = flip maybeToRight path $
      daemonName <> " is not available for the given protocol: " <> prettyProtoHash
  binaryPath <- eiBinaryPath
  binaryArgs <- args
  pure $ proc binaryPath binaryArgs

getBakerArgs
  :: AppConfig
  -> LoggingEnv
  -> Pool Postgresql
  -> Maybe ProtocolHash
  -> IO (Either Text [String])
getBakerArgs appConfig logger db mbProtoHash = do
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
  case mbProtoHash of
    Just IthacaProtocolHash ->
      pure $ Right $ protocolAgnosticArgs alias
    Just JakartaProtocolHash -> do
      extraArgs <- runLoggingEnv logger $ runDb (Identity db) $ select $
        BakerExtraArgs_publicKeyHashField ==. pkh &&.
        BakerExtraArgs_chainIdField ==. chainId
      runLoggingEnv logger $
        $(logDebug) $ "Baker extra args: " <> tshow extraArgs
      let extraArgsCmd = fmap T.unpack $ concatMap toCmdArg extraArgs
      pure $ Right $ protocolAgnosticArgs alias <> extraArgsCmd
    _ ->
      pure $ Left $ "'getBakerArgs': unknown protocol "
      <> maybe "<unknown protocol>" toBase58Text mbProtoHash
  where
    protocolAgnosticArgs alias =
      [ "--endpoint", T.unpack $ render $ kilnNodeRpcURI appConfig
      , "--base-dir", tezosClientDataDir appConfig
      , "run", "with", "local", "node", nodeDataDir appConfig
      , alias
      ] <> maybe [] (words . T.unpack) (_appConfig_kilnBakerCustomArgs appConfig)
