{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

-- {-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Snapshot where

import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Logger (LoggingT (..), MonadLogger, logInfo, logWarn, runStderrLoggingT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Pool (Pool)
import Data.Sequence (Seq)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql, in_, isFieldNothing, (&&.), (=.), (==.))
import Rhyolite.Backend.DB (getTime, runDb, selectMap, project1)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Snap.Core (MonadSnap, route)
import qualified Snap.Core as Snap
import Snap.Util.FileUploads
import System.Directory

import Tezos.Base58Check (fromBase58, toBase58)
import Tezos.Block (VeryBlockLike (..))
import Tezos.Operation (Ballot)
import Tezos.PublicKey
import Tezos.Types

import Backend.CachedNodeRPC
import Backend.Config (AppConfig (..), defaultNodeConfigFile, nodeDataDir, BinaryPaths(..))
import Backend.STM (atomicallyWith)
import Common.Schema (SnapshotMeta(..))
import ExtraPrelude

handleSnapshotUpload :: AppConfig -> NodeDataSource -> Pool Postgresql -> Snap.Snap ()
handleSnapshotUpload appConfig nds db = do
  let
    uploadPolicy = defaultUploadPolicy
    uploadTmpLocation = _appConfig_kilnDataDir appConfig <> "/snapshots_tmp/"
    uploadLocation = _appConfig_kilnDataDir appConfig <> "/snapshots/"
    partUploadPolicy _ = allowWithMaximumSize (10*1000*1000*1000)
    uploadHandler p = \case
      Left e -> putStrLn $ show e
      Right fp -> runLoggingEnv (_nodeDataSource_logger nds) $ do
        $(logWarn) "Upload successful."
        now <- liftIO $ getCurrentTime
        let fileName = maybe "file" (T.unpack . T.decodeUtf8) $ partFileName p
            randomStr = show now
            filePath = randomStr <> fileName
            storePath = uploadLocation <> filePath
        smId <- runDb (Identity db) $ do
          insert $ SnapshotMeta (T.pack fileName) (T.pack filePath) now Nothing Nothing Nothing Nothing
        liftIO $ renameFile fp storePath
        -- mHeadInfo <- getHeadInfo storePath
        -- update db
        -- notify

  liftIO $ createDirectoryIfMissing True uploadTmpLocation
  liftIO $ createDirectoryIfMissing True uploadLocation
  void $ handleFileUploads uploadTmpLocation uploadPolicy partUploadPolicy uploadHandler

-- import by using invalid hash
-- get correct hash from stderr?
importMetaInfo :: SnapshotMeta -> FilePath -> IO (Either SnapshotError SnapshotMeta)
importMetaInfo sm parentDir = withTempDirectory parentDir $ \tmpDir -> do
  let dummyBlockHash = "BLKSXJhUn8L51dWR8L5MPNhTXgeF3MGLPyroBb9oHBtcFUAFze"
  $(logWarn) $ "importing snapshot meta info: "
  (exitCode, stdout, stderr) <- liftIO $ Process.readProcessWithExitCode
  (nodePaths chain) (["snapshot", "import", fileName, "--data-dir", tmpDir, "--block", dummyBlockHash) ""

  case exitCode of
    ExitSuccess -> pure $ T.strip $ T.pack stdout
    ExitFailure _ -> do
      $(logWarn) $ "runClientCommand failed: " <> T.pack stderr

      let strippedLines = fmap T.strip $ T.lines $ T.pack stderr
          warnings = takeWhile (/= "Error:") $ drop 1 $ dropWhile (/= "Warning:") strippedLines
          errors = filter (/= "Error:") $ dropWhile (/= "Error:") strippedLines
          fatal = drop 1 $ dropWhile (/= "Fatal error:") $ fmap T.strip $ T.lines $ T.pack stdout -- yes, fatal errors go to stdout
      case handleError warnings (fatal ++ errors) of
        Right t -> pure t
        Left e -> do
          $(logWarn) $ T.pack $ show e
          throwError e

    getActualHeadHash = \case
      _importingData : _retrievingData: _context: _store: _computingPreds: _cleaningDir: _error : errMsg : actualBlk : dummyBlkHash: 
        | T.isPrefixOf "The block contained in the file is" errMsg
        | T.isPrefixOf (toBase58Text dummyBlk) dummyBlkHash
        , Just blkHash <- headMay $ T.words actualBlk
        -> Just blkHash
      _ -> Nothing

-- Jul  3 03:50:26 - shell.snapshots: Importing data from snapshot file ../alphanet-snapshot-02072019.full
-- Jul  3 03:50:26 - shell.snapshots: Retrieving and validating data. This can take a while, please bear with us
-- Context: 333K elements, 26MiB read
-- Store: 484K elements, 654MiB read
-- Computing predecessors table 484K elements
-- Jul  3 03:52:36 - node.main: Cleaning directory ./dir because of failure
-- tezos-node: Error:
--               The block contained in the file is
--             BLmyk5EDbe5DMXzmFhoBKt6DQSLi9AxStGPuPx2KV14cexstcGD instead of
--             BLKSXJhUn8L51dWR8L5MPNhTXgeF3MGLPyroBb9oHBtcFUAFzeK.


-- make sure the current dir is empty/clean dir
-- change processstate
-- import data to the dir and start node, 
importSnapshotData appConfig logger db sm = do
  let
    nodePath = either nodePaths _binaryPaths_nodePath namedChainOrPaths
    dataDir = nodeDataDir appConfig
  liftIO $ removeDirectoryRecursive dataDir
  liftIO $ createDirectoryIfMissing True dataDir
  nodePPid <- project1 (NodeInternal_dataField ~> DeletableRow_dataSelector) CondEmpty
  let
    inDb :: (MonadIO m, MonadBaseNoPureAborts IO m) => DbPersist Postgresql (LoggingT m) a -> m a
    inDb = runLoggingEnv logger . runDb (Identity db)
  inDb $ updateProcessState nodePPid ProcessState_Initializing
  
  (exitCode, stdout, stderr) <- liftIO $ Process.readProcessWithExitCode
    nodePath (["snapshot", "import", fileName, "--data-dir", dataDir, "--block", headBlock) ""

  case exitCode of
    ExitSuccess -> do
          updateNode = do
            (getInternalNode >>=) $ traverse_ $ \(nid, nodeData) -> do
              let pid = _deletableRow_data nodeData
              update [ProcessData_controlField =. c] (AutoKeyField ==. fromId pid)
              processData <- getId $ _deletableRow_data nodeData
              notify NotifyTag_NodeInternal (nid, processData)

    ExitFailure _ -> do
      $(logWarn) $ "runClientCommand failed: " <> T.pack stderr
