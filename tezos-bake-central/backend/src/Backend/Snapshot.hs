{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Snapshot where

import Control.Concurrent
import Control.Exception.Safe (IOException, try)
import Control.Monad.Catch (MonadMask, catch, finally, onException)
import Control.Monad.Trans.Resource (MonadUnliftIO, runResourceT)
import Control.Monad.Logger
import Data.Conduit.Binary (sinkFileCautious)
import Data.Int (Int32)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Database.Id.Groundhog (fromId)
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql(..), (=.), (==.))
import Rhyolite.Backend.DB (getTime, runDb, project1, MonadBaseNoPureAborts)
import Rhyolite.Backend.DB.Serializable
import Rhyolite.Backend.Logging
import Network.HTTP.Client (HttpException, parseRequest)
import Network.HTTP.Simple (httpSink)
import Safe
import qualified Snap.Core as Snap
import Snap.Util.FileUploads
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath.Posix (takeFileName)
import qualified System.Process as Process
import System.Posix.Signals (signalProcess, sigKILL)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import Text.URI (URI, renderStr)

import Tezos.NodeRPC (_cachedHistory_blocks)
import Tezos.Types
import qualified Tezos.LRUHashMap as LRUHashMap

import Backend.Common
import Backend.Config
import Backend.NodeCmd
import Backend.NodeRPC
import Backend.Schema
import Backend.Workers.Process
import Common.Schema
import ExtraPrelude

handleSnapshotUpload
  :: AppConfig
  -> NodeDataSource
  -> MVar ()
  -> Snap.Snap ()
handleSnapshotUpload appConfig nds lockMVar = do
  liftIO $ createDirectoryIfMissing True uploadTmpLocation
    `catch` (\(e :: IOException) -> runLoggingEnv logger $ $(logWarn) ("Make dir failed: " <> tshow uploadTmpLocation <> "\nError: " <> tshow e))
  void $ handleFileUploads uploadTmpLocation defaultUploadPolicy partUploadPolicy uploadHandler
  where
    logger = _nodeDataSource_logger nds
    uploadTmpLocation = _appConfig_kilnDataDir appConfig <> "/snapshots_tmp/"
    storeLocation = _appConfig_kilnDataDir appConfig <> "/snapshots/"
    partUploadPolicy _ = allowWithMaximumSize (10 * 1024 * 1024 * 1024) -- 10gb
    withLockRelease m = liftIO $ finally m (tryTakeMVar lockMVar)

    takeLock = liftIO $ tryPutMVar lockMVar ()

    releaseLock :: (MonadIO m, MonadLogger m) => m ()
    releaseLock = liftIO (tryTakeMVar lockMVar) >>= \case
      Nothing -> $(logError) "Attempted to release a lock that was not held!"
      Just () -> pure ()

    uploadHandler :: PartInfo -> Either PolicyViolationException FilePath -> IO ()
    uploadHandler p v = runLoggingEnv logger $ flip onException releaseLock $ takeLock >>= \case
      False -> do
        $(logWarn) "Upload already in progress, ignoring this request."
      True -> maybe releaseLock (const $ pure ()) =<< case v of
        Left e -> Nothing <$ $(logError) ("Could not upload file: " <> tshow e)
        Right fp -> fmap Just $ do
          $(logDebug) "Upload successful."
          cleanupDir storeLocation
          let
            snapshotFileName = maybe "file" (T.unpack . T.decodeUtf8) $ partFileName p
            storePath = storeLocation <> snapshotFileName
          liftIO $ do
            renameFile fp storePath
            forkIO $ withLockRelease $ runLoggingEnv logger $ do
              (smId, sm) <- initSnapshotMeta snapshotFileName storePath nds Nothing
              importSnapshotData appConfig nds sm smId True

validateSnapshotFilePath
  :: (MonadIO m, MonadLogger m)
  => AppConfig
  -> FilePath
  -> m (Either SnapshotImportError ())
validateSnapshotFilePath appConfig fp = do
  $(logInfo) $ "validateSnapshotFilePath: path = " <> T.pack fp
  doesExist <- liftIO $ doesFileExist fp
  if doesExist then do
    doesHavePermissions <- fmap readable $ liftIO $ getPermissions fp
    if doesHavePermissions then do
      let
        nodePath = maybe nixNodePath _binaryPaths_nodePath $ _appConfig_binaryPaths appConfig
        args = ["snapshot", "info", fp]
      (exitCode, _, stderr) <- liftIO $ Process.readProcessWithExitCode nodePath args ""
      case exitCode of
        ExitSuccess   -> do
          $(logInfo) "validateSnapshotFilePath: snapshot file is valid"
          pure $ Right ()
        ExitFailure _ -> do
          $(logInfo) $ "validateSnapshotFilePath: invalid snapshot file, stderr = " <> T.pack stderr
          pure $ Left SnapshotImportError_InvalidSnapshot
    else
      pure $ Left SnapshotImportError_PermissionDenied
  else do
    $(logInfo) "validateSnapshotFilePath: file not found"
    pure $ Left SnapshotImportError_FileNotFound

handleSnapshotFilePathImport
  :: (MonadLogger m, MonadLoggerIO m, MonadIO m, MonadMask m, MonadBaseNoPureAborts IO m)
  => AppConfig
  -> NodeDataSource
  -> FilePath
  -> m ()
handleSnapshotFilePathImport appConfig nds fp = do
  (smId, sm) <- initSnapshotMeta (takeFileName fp) fp nds Nothing
  void $ liftIO $ forkIO $ runLoggingEnv (_nodeDataSource_logger nds) $
    importSnapshotData appConfig nds sm smId False

cleanupDir :: (MonadLogger m, MonadIO m, MonadMask m) => FilePath -> m ()
cleanupDir dir = do
  liftIO (removeDirectoryRecursive dir)
    `catch` \(e :: IOException) -> $(logWarn) ("Remove dir failed: " <> tshow dir <> ": " <> tshow e)
  liftIO (createDirectoryIfMissing True dir)
    `catch` \(e :: IOException) -> $(logWarn) ("Make dir failed: " <> tshow dir <> ": " <> tshow e)

-- Example output on success
-- stderr:
-- Jul  6 19:39:08 - shell.snapshots: Importing data from snapshot file ./.kiln/snapshots/main.snapshot
-- Jul  6 19:39:08 - shell.snapshots: You may consider using the --block <block_hash> argument to verify that the block imported is the one you expect
-- Jul  6 19:39:08 - shell.snapshots: Retrieving and validating data. This can take a while, please bear with us
-- Jul  6 19:45:44 - shell.snapshots: Setting current head to block BLWxHkBhZfaj
-- Jul  6 19:45:45 - shell.snapshots: Setting history-mode to full
-- Jul  6 19:45:46 - shell.snapshots: Successful import from file ./.kiln/snapshots/main.snapshot

handleSnapshotDownload
  :: forall m. (MonadLogger m, MonadLoggerIO m, MonadIO m, MonadMask m, MonadBaseNoPureAborts IO m, MonadUnliftIO m)
  => AppConfig
  -> NodeDataSource
  -> URI
  -> m ()
handleSnapshotDownload appConfig nds snapshotURI = void $ liftIO $ forkIO $ runLoggingEnv (_nodeDataSource_logger nds) $ do
  let db = _nodeDataSource_pool nds
      logger = _nodeDataSource_logger nds
      dataDir = nodeDataDir appConfig
  cleanupDir storeLocation
  nodePPid <- runDb (Identity db) $ project1
    ( NodeInternal_idField
    , NodeInternal_dataField ~> DeletableRow_dataSelector
    ) CondEmpty
  for_ nodePPid $ \(nid, pid) -> do
    (smId, sm) <- initSnapshotMeta snapshotFileName storePath nds (Just snapshotURI)
    downloaderThread <- liftIO $ forkIO $ do
      res <- try $ do
        request <- parseRequest $ renderStr snapshotURI
        runResourceT $ httpSink request $ \_ -> sinkFileCautious storePath
      case res of
        Left (e :: HttpException) -> runLoggingEnv logger $ do
          let errText = T.pack (show e)
          $(logError) $ "Snapshot download failed with:" <> errText
          runDb (Identity db) $ do
            update [ SnapshotMeta_downloadErrorField =. Just errText ] (AutoKeyField ==. smId)
            traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId
            updateProcessState pid
              (Just (\pd -> (NotifyTag_NodeInternal, (nid, pd))))
              (ProcessState_Node NodeProcessState_DownloadFailed)
        Right _ -> runLoggingEnv logger $ runDb (Identity db) $
          updateProcessState pid
            (Just (\pd -> (NotifyTag_NodeInternal, (nid, pd))))
            (ProcessState_Node NodeProcessState_DownloadComplete)

    let
      cleanUpNode = do
        runDb (Identity db) $ removeNodeDbImpl (Right ())
        liftIO $ removeDirectoryRecursive dataDir
      go = do
        ps <- fmap headMay $ runDb (Identity db) $ project ProcessData_stateField (AutoKeyField ==. fromId pid)
        case ps of
          (Just (ProcessState_Node NodeProcessState_DownloadComplete)) -> do
            importSnapshotData appConfig nds sm smId True
          (Just (ProcessState_Node NodeProcessState_DownloadCanceled)) -> do
            liftIO $ killThread downloaderThread
            cleanUpNode
          (Just (ProcessState_Node NodeProcessState_DownloadFailed)) -> do
            cleanUpNode
          _ -> threadDelay' 1 >> go
    go
  where
    snapshotFileName = "snapshot"
    storeLocation = _appConfig_kilnDataDir appConfig <> "/snapshots/"
    storePath = storeLocation <> snapshotFileName

data BlockLikeData where
  BlockPrefixHash :: Text -> BlockLikeData
  BlockHash :: BlockHash -> BlockLikeData
  BlockLike :: BlockLike blk => blk -> BlockLikeData

importSnapshotData
  :: (MonadLogger m, MonadLoggerIO m, MonadIO m, MonadMask m, MonadBaseNoPureAborts IO m)
  => AppConfig
  -> NodeDataSource
  -> SnapshotMeta
  -> Key SnapshotMeta BackendSpecific
  -> Bool
  -> m ()
importSnapshotData appConfig nds sm smId shouldRemoveSnapshotFile = do
  let
    logger = _nodeDataSource_logger nds
    nodePath = maybe nixNodePath _binaryPaths_nodePath $ _appConfig_binaryPaths appConfig
    dataDir = nodeDataDir appConfig
    storePath = T.unpack $ _snapshotMeta_storePath sm
    inDb :: (MonadIO m, MonadBaseNoPureAborts IO m, MonadLoggerIO m, MonadLogger m) => Serializable a -> m a
    inDb = runDb (Identity $ _nodeDataSource_pool nds)

  $(logDebug) "importSnapshotData: cleaning old data dir"
  cleanupDir dataDir

  let
    updateState' nodePPid s = for_ nodePPid $ \(nid, pid) ->
      updateProcessState pid (Just (\pd -> (NotifyTag_NodeInternal, (nid, pd)))) (ProcessState_Node s)

  nodePPid <- inDb $ do
    nodePPid <- project1
      ( NodeInternal_idField
      , NodeInternal_dataField ~> DeletableRow_dataSelector
      ) CondEmpty
    updateState' nodePPid NodeProcessState_ImportingSnapshot
    pure nodePPid

  let
    updateState :: (MonadLogger m1, PersistBackend m1, MonadIO m1) => NodeProcessState -> m1 ()
    updateState = updateState' nodePPid

    importFailed msg stderr = do
      $(logError) msg
      update [ SnapshotMeta_importErrorField =. Just stderr ] (AutoKeyField ==. smId)
      traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId
      updateState NodeProcessState_ImportFailed

    procSpec configFile = (Process.proc nodePath ["snapshot", "import", storePath, "--data-dir", dataDir,"--config-file", configFile])
      { Process.std_out = Process.CreatePipe
      , Process.std_err = Process.CreatePipe
      }
    procMonitor _hStdin _hStdout hStderr ph = runLoggingEnv logger go
      where
        {-# INLINE go #-}
        go = do
          let getPC = \case
                [] -> ProcessControl_Stop
                (c:_) -> c
          procControl <- inDb (getPC <$> project SnapshotMeta_controlField (AutoKeyField ==. smId))
          liftIO (Process.getProcessExitCode ph) >>= \case
            Nothing -> do
              inDb $ updateState NodeProcessState_ImportingSnapshot
              let
                stop = procControl /= ProcessControl_Run
                delayInSec = 1 :: NominalDiffTime
              when stop $ do
                inDb $ updateState NodeProcessState_ImportCanceled
                liftIO $ Process.getPid ph >>= traverse_ (signalProcess sigKILL)
              threadDelay' delayInSec *> go
            Just exitCode -> do
              stderr <- case hStderr of
                Nothing -> $(logError) "hStderr is Nothing" >> pure ""
                Just h -> liftIO $ T.hGetContents h `catch` \(_ :: IOError) -> runLoggingEnv logger ($(logError) "Failed to get stderr" >> pure "")
              case exitCode of
                ExitSuccess -> void $ do
                  $(logDebug) $ "importSnapshotData success: stderr: " <> stderr
                  (infoExitCode, infoStdout, _) <- liftIO $ Process.readProcessWithExitCode nodePath ["snapshot", "info", storePath] ""
                  when (infoExitCode == ExitSuccess) $ do
                    let
                      blockHashRegex, levelRegex :: String
                      blockHashRegex = "block hash ([A-Za-z0-9]*)"
                      levelRegex = "at level ([0-9]*)"

                      extractFromSnapshotInfo :: String -> String -> (String -> Maybe a) -> Maybe a
                      extractFromSnapshotInfo source regex parse =
                        let (_, _, _, matches) = source =~ regex :: (String, String, String, [String]) in
                          parse =<< listToMaybe matches

                      mBlkHash  = extractFromSnapshotInfo infoStdout blockHashRegex
                        ((either (const Nothing) Just) . fromBase58 . fromString)
                      mLevel = extractFromSnapshotInfo infoStdout levelRegex (fmap fromIntegral . readMaybe @Int32)
                    whenJust mBlkHash $ \blkHash -> void $ do
                      mBlk <- flip runReaderT nds $ runExceptT @CacheError $ runNodeQueryT $ do
                        nodeQueryDataSourceSafe $ NodeQuery_BlockHeader blkHash
                      inDb $ do
                        updateSnapshotMeta mBlkHash mLevel (mBlk ^? _Right . timestamp) smId
                  inDb $ updateState NodeProcessState_ImportComplete
                ExitFailure _ -> inDb $ importFailed "importSnapshotData failed: " stderr

  liftIO $ withNodeConfig appConfig $ \configFile -> do
    runLoggingEnv logger $ $(logInfoSH) ("importSnapshotData: running process" :: Text, procSpec configFile)
    Process.withCreateProcess (procSpec configFile) procMonitor

  when shouldRemoveSnapshotFile $
    removeFileLogging storePath

  -- Do cleanup after cancel import
  procControl <- inDb $ project SnapshotMeta_controlField (AutoKeyField ==. smId)
  case headMay procControl of
    Just ProcessControl_Stop -> do
      inDb $ removeNodeDbImpl (Right ())
      liftIO $ removeDirectoryRecursive dataDir
    _ -> pure ()

initSnapshotMeta
  :: MonadLoggerIO m
  => FilePath
  -> FilePath
  -> NodeDataSource
  -> Maybe URI
  -> m (Key SnapshotMeta BackendSpecific, SnapshotMeta)
initSnapshotMeta fileName storePath nds mbUri = runDb (Identity $ _nodeDataSource_pool nds) $ do
  now <- getTime
  let
    sm = SnapshotMeta
      { _snapshotMeta_filename = T.pack fileName
      , _snapshotMeta_storePath = T.pack storePath
      , _snapshotMeta_uploadTime = now
      , _snapshotMeta_importError = Nothing
      , _snapshotMeta_importCompleteTime = Nothing
      , _snapshotMeta_headBlock = Nothing
      , _snapshotMeta_headBlockPrefix = Nothing
      , _snapshotMeta_headBlockLevel = Nothing
      , _snapshotMeta_headBlockBakeTime = Nothing
      , _snapshotMeta_control = ProcessControl_Run
      , _snapshotMeta_mbUri = mbUri
      , _snapshotMeta_downloadError = Nothing
      }
  deleteAll sm
  k <- insert sm
  notify NotifyTag_SnapshotMeta sm
  pure (k, sm)

updateSnapshotMeta
  :: (PersistBackend m)
  => Maybe BlockHash
  -> Maybe RawLevel
  -> Maybe UTCTime
  -> Key SnapshotMeta BackendSpecific
  -> m ()
updateSnapshotMeta mbBlockHash mbLevel mbTimestamp smId = do
  now <- getTime
  update
    [ SnapshotMeta_headBlockField =. mbBlockHash
    , SnapshotMeta_headBlockLevelField =. mbLevel
    , SnapshotMeta_headBlockBakeTimeField =. mbTimestamp
    , SnapshotMeta_importCompleteTimeField =. Just now
    ]
    (AutoKeyField ==. smId)
  traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId

-- | Find the block with the given hash prefix
completeBlockHash :: Text -> CachedHistory' -> Maybe BlockHash
completeBlockHash prefix' history =
  find (T.isPrefixOf prefix' . toBase58Text) $
    LRUHashMap.keys $ _cachedHistory_blocks history

removeFileLogging :: (MonadLogger m, MonadIO m, MonadMask m) => FilePath -> m ()
removeFileLogging f = liftIO (removeFile f) `catch` \(e :: IOException) -> $(logError) $ "Failed to remove file: " <> T.pack f <> ": " <> tshow e
