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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Snapshot where

import Control.Concurrent.Async (async)
import Control.Applicative (optional)
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO, writeTVar)
import Control.Exception.Safe (IOException, MonadCatch, MonadThrow, SomeException (..), handle, throw, throwString, try)
import Control.Monad.Catch (MonadMask, catch, finally, onException)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Logger
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import qualified Data.Attoparsec.Text as P
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.Conduit (ConduitT, await, yield, (.|))
import Data.Conduit.Binary (sinkFileCautious)
import qualified Data.Conduit.List as CL
import Data.Conduit.Process (StreamingProcessHandle, getStreamingProcessExitCode, streamingProcessHandleRaw, terminateProcess)
import Data.Either.Combinators (whenLeft)
import Data.Foldable (maximumBy)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Database.Id.Groundhog (fromId)
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql(..), (=.), (==.))
import Rhyolite.Backend.DB (getTime, runDb, project1, MonadBaseNoPureAborts)
import Rhyolite.Backend.DB.Serializable
import Rhyolite.Backend.Logging
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http
import Safe
import qualified Snap.Core as Snap
import Snap.Util.FileUploads
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath.Posix (takeFileName)
import System.Process (CreateProcess)
import qualified System.Process as Process
import Text.Read (readMaybe)
import Text.URI (URI, mkURI, relativeTo, render, renderStr)
import qualified Text.URI.QQ as Uri

import Tezos.Types

import Backend.Common
import Backend.Config
import Backend.Http (doRequestLBSThrows)
import Backend.NodeRPC
import Backend.Process.Common
import Backend.Process.Node (getKilnNodeVersion, nixNodePath)
import Backend.Schema
import Common.Schema
import Common.Snapshot
import ExtraPrelude

-- | Type alias for the default key of 'SnapshotMeta' table defined for convenience.
type SnapshotMetaId = Key SnapshotMeta BackendSpecific

-- | Auxiliary data type which represents the snapshot import options
-- that depend on the snapshot import source:
--
-- e.g we don't need to remove the snapshot file if this is the
-- file uploaded by user and we don't ask user to verify the
-- snapshot downloaded from the snapshot provider metadata because it could
-- be done automatically since snapshot's head block is known
data SnapshotImportOptions = SnapshotImportOptions
  { sioRemoveSnapshotFile :: Bool
  , sioVerifySnapshot :: Bool
  }

-- | Auxiliary data type which represents
-- 'octez-node snapshot info <path> --json' result.
data SnapshotInfoResult = SnapshotInfoResult
  { sirVersion :: Int
  , sirBlockHash :: BlockHash
  , sirLevel :: RawLevel
  , sirTimestamp :: UTCTime
  } deriving (Show, Eq, Generic)

instance FromJSON SnapshotInfoResult where
  parseJSON = withObject "SnapshotInfoResult" $ \o -> do
    h <- o .: "snapshot_header"
    sirVersion   <- h .: "version"
    sirBlockHash <- h .: "block_hash"
    sirLevel     <- h .: "level"
    sirTimestamp <- h .: "timestamp"
    pure $ SnapshotInfoResult {..}

-- | This data type represents the policy of reporting the errors
-- when a snapshot download action runs more than once
-- (e.g. in case of retries and using the fallback options)
data ErrorReportPolicy
  = DontReportError -- ^ Don't report error to user since it will be retried
  | ReportError -- ^ Report the error to user
  | OverrideUrl URI -- ^ Report the error, but override the url in the error message (used when fallback options are used)

-- | Auxiliary data type which represents snapshot download state.
data SnapshotDownloadState
  = Downloading
  | Failed Text
  | Completed

-- | Execute the 'octez-node snapshot info <path> --json' command and
-- return either @SnapshotInfoResult@ or error message in case of
-- command execution/result decoding error.
execSnapshotInfo :: MonadLoggerIO m => String -> String -> m (Either Text SnapshotInfoResult)
execSnapshotInfo storePath nodePath = do
  (infoExitCode, infoStdout, infoStderr) <- liftIO $ Process.readProcessWithExitCode nodePath
    [ "snapshot"
    , "info"
    , storePath
    , "--json"
    ] ""
  let parse = eitherDecode . LBS.fromStrict . T.encodeUtf8 . T.pack
      mkErrMsg s = "Unexpected result of 'snapshot info' command: " <> T.pack s
  pure $ case infoExitCode of
    ExitFailure c -> Left $ T.concat
      [ "'octez-node snapshot info' failed with exit code "
      , tshow c
      , ":\n"
      , T.pack infoStderr
      ]
    ExitSuccess -> first mkErrMsg $ parse infoStdout

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
    storeLocation = snapshotStorePath appConfig
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
              (smId, sm) <- initSnapshotMeta appConfig (Just storePath) nds Nothing
              let
                importOptions = SnapshotImportOptions
                  { sioRemoveSnapshotFile = True
                  , sioVerifySnapshot = True
                  }
              importSnapshotData appConfig nds sm smId importOptions

validateSnapshotFilePath
  :: (MonadIO m, MonadLoggerIO m)
  => FilePath
  -> m (Either SnapshotImportError ())
validateSnapshotFilePath fp = do
  let filePathText = T.pack fp
  $(logDebug) $ "Validating snapshot file: " <> filePathText
  doesExist <- liftIO $ doesFileExist fp
  if doesExist then do
    doesHavePermissions <- fmap readable $ liftIO $ getPermissions fp
    if doesHavePermissions then do
      (exitCode, stdout, stderr) <- liftIO $ Process.readProcessWithExitCode "file" [fp] ""
      let cmdText = T.pack $ "file " <> fp
          stdoutText = T.pack stdout
      case exitCode of
        ExitFailure ec -> do
          $(logError) $ T.concat
            [ cmdText
            , " failed with exit code "
            , tshow ec
            , ". stderr:\n"
            , T.pack stderr
            ]
          pure $ Left SnapshotImportError_UnknownError
        ExitSuccess -> do
          $(logDebug) $ cmdText <> " finished successfully. stdout:\n" <> stdoutText
          pure $ if "tar archive" `T.isSuffixOf` T.strip stdoutText
          then Right ()
          else Left SnapshotImportError_InvalidSnapshot
    else
      pure $ Left SnapshotImportError_PermissionDenied
  else do
    $(logError) $ "File not found: " <> filePathText
    pure $ Left SnapshotImportError_FileNotFound

handleSnapshotFilePathImport
  :: (MonadLogger m, MonadLoggerIO m, MonadIO m, MonadMask m, MonadBaseNoPureAborts IO m)
  => AppConfig
  -> NodeDataSource
  -> FilePath
  -> m ()
handleSnapshotFilePathImport appConfig nds fp = do
  (smId, sm) <- initSnapshotMeta appConfig (Just fp) nds Nothing
  let
    importOptions = SnapshotImportOptions
      { sioRemoveSnapshotFile = False
      , sioVerifySnapshot = True
      }
  void $ liftIO $ forkIO $ runLoggingEnv (_nodeDataSource_logger nds) $
    importSnapshotData appConfig nds sm smId importOptions

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

-- | Synchronously download snapshot from the given url.
handleSnapshotDownloadSync
  :: forall m.
  ( MonadLogger m
  , MonadLoggerIO m
  , MonadIO m
  , MonadMask m
  , MonadBaseNoPureAborts IO m
  , MonadUnliftIO m
  )
  => AppConfig
  -> NodeDataSource
  -> URI
  -> (SnapshotMetaId, SnapshotMeta)
  -> m ()
handleSnapshotDownloadSync appConfig nds snapshotURI snapshotMeta =
  runLoggingEnv (_nodeDataSource_logger nds) $ do
    let
      importOptions = SnapshotImportOptions
        { sioRemoveSnapshotFile = True
        , sioVerifySnapshot = True
        }
    downloadSnapshot appConfig nds snapshotURI snapshotMeta importOptions

downloadSnapshot
  :: forall m.
  ( MonadLogger m
  , MonadLoggerIO m
  , MonadIO m
  , MonadMask m
  , MonadBaseNoPureAborts IO m
  , MonadUnliftIO m
  )
  => AppConfig
  -> NodeDataSource
  -> URI
  -> (SnapshotMetaId, SnapshotMeta)
  -> SnapshotImportOptions
  -> m ()
downloadSnapshot appConfig nds snapshotURI (smId, sm) importOptions = do
  let snapshotUriStr = renderStr snapshotURI
  $(logDebug) $ "Downloading node snapshot from " <> T.pack snapshotUriStr
  let dataDir = nodeDataDir appConfig
  cleanupDir storeLocation
  nodePPid <- runDb (Identity db) $ project1
    ( NodeInternal_idField
    , NodeInternal_dataField ~> DeletableRow_dataSelector
    ) CondEmpty
  for_ nodePPid $ \(nid, pid) -> do
    -- We don't want to set either 'download complete' or 'download
    -- failed' process state because this way node state on UI will
    -- be also changed.
    --
    -- When download completed successfully, we need to check that
    -- downloaded file is a valid node snapshot with 'snapshot info'
    -- command.
    --
    -- When download failed, we try to download the snapshot from
    -- the fallback provider before displaying the download failure
    --
    -- To determine the download state without affecting UI, we use
    -- 'TVar' instead of changing the 'ProcessState' column in the
    -- 'ProcessData' table.
    downloadStateTVar <- liftIO $ newTVarIO Downloading
    downloaderThread <- liftIO $ forkIO $ do
      res <- try $ do
        request <- Http.parseRequest snapshotUriStr
        runResourceT $ Http.httpSink request $ \resp -> do
          let status = Http.getResponseStatusCode resp
              statusLogText = T.pack snapshotUriStr <> " responded with status " <> tshow status
          runLoggingEnv logger $ case status of
            200 -> $(logDebug) statusLogText
            _   -> $(logError) statusLogText
          updateDownloadProgress (getContentLength resp) .| sinkFileCautious storePath
      case res of
        Left (e :: Http.HttpException) ->
          let errText = "Snapshot download failed with: " <> T.pack (show e)
          in do
            runLoggingEnv logger $
              $(logError) errText
            atomically $ writeTVar downloadStateTVar $ Failed errText
        Right () -> atomically $ writeTVar downloadStateTVar Completed

    let
      cleanUpNode = do
        runDb (Identity db) $ removeNodeDbImpl (Right ())
        liftIO $ removeDirectoryRecursive dataDir
      handleFailure = liftIO $ do
        runLoggingEnv logger $ runDb (Identity db) clearDownloadProgress
        snapshotExists <- doesFileExist storePath
        when snapshotExists $ removeFile storePath

      go = do
        downloadState <- liftIO $ readTVarIO downloadStateTVar
        ps <- fmap headMay $ runDb (Identity db) $ project ProcessData_stateField (AutoKeyField ==. fromId pid)
        case (downloadState, ps) of
          (_, Just (ProcessState_Node NodeProcessState_DownloadComplete)) ->
            importSnapshotData appConfig nds sm smId importOptions
          (Completed, _) -> do
            let nodePath = maybe nixNodePath _binaryPaths_nodePath $ _appConfig_binaryPaths appConfig
            snapshotInfoRes <- execSnapshotInfo (T.unpack $ _snapshotMeta_storePath sm) nodePath
            case snapshotInfoRes of
              Left err -> throwString $ T.unpack err
              Right _ -> do
                runLoggingEnv logger $ runDb (Identity db) $ do
                  updateProcessState pid
                    (Just (\pd -> (NotifyTag_NodeInternal, (nid, pd))))
                    (ProcessState_Node NodeProcessState_DownloadComplete)
                  runLoggingEnv logger $ runDb (Identity db) clearDownloadProgress
                threadDelay' 1 >> go
          (Failed failMsg, _) -> do
            handleFailure
            throwString $ T.unpack failMsg
          (_, Just (ProcessState_Node NodeProcessState_DownloadCanceled)) -> do
            runLoggingEnv logger $ runDb (Identity db) clearDownloadProgress
            liftIO $ killThread downloaderThread
            cleanUpNode
          (_, Just (ProcessState_Node NodeProcessState_DownloadFailed)) -> handleFailure
          _ -> threadDelay' 1 >> go
    go
  where
    db = _nodeDataSource_pool nds
    logger = _nodeDataSource_logger nds
    storeLocation = snapshotStorePath appConfig
    storePath = storeLocation <> defaultSnapshotFileName

    -- Returns the value of 'Content-Length' response header if present.
    getContentLength :: Http.Response a -> Maybe Int
    getContentLength resp = case Http.getResponseHeader "Content-Length" resp of
      [v] -> readMaybe $ T.unpack $ T.decodeUtf8 v
      _ -> Nothing

    updateDownloadProgress
      :: Maybe Int
      -> ConduitT ByteString ByteString (ResourceT IO) ()
    updateDownloadProgress mbTotalSizeInt =
      updateDownloadProgress' 0 Nothing
      where
        mbTotalSize :: Maybe Double
        mbTotalSize = fromIntegral @Int @Double <$> mbTotalSizeInt

        updateDownloadProgress'
          :: Double
          -> Maybe Int
          -> ConduitT ByteString ByteString (ResourceT IO) ()
        updateDownloadProgress' curProgress curPercentage = do
          mbChunk <- await
          whenJust mbChunk $ \chunk -> do
            let newProgress = curProgress + fromIntegral @Int @Double (BS.length chunk)
                newPercentage = calcProgressPercentage newProgress <$> mbTotalSize
            -- If 'Content-Length' header isn't set, we don't update
            -- the progress.
            whenJust mbTotalSize $ \_ -> do
              when (curPercentage /= newPercentage) $
                runLoggingEnv logger $ runDb (Identity db) $
                  updateSnapshotMetaDownloadProgress newPercentage
            yield chunk
            updateDownloadProgress' newProgress newPercentage

    clearDownloadProgress :: Serializable ()
    clearDownloadProgress = updateSnapshotMetaDownloadProgress Nothing

    updateSnapshotMetaDownloadProgress
      :: Maybe Int
      -> Serializable ()
    updateSnapshotMetaDownloadProgress newProgress = do
      update
        [SnapshotMeta_downloadProgressField =. newProgress]
        (AutoKeyField ==. smId)
      traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId

    calcProgressPercentage :: Double -> Double -> Int
    calcProgressPercentage cur total = floor $ 100 * cur / total

importSnapshotData
  :: (MonadLogger m, MonadLoggerIO m, MonadIO m, MonadMask m, MonadBaseNoPureAborts IO m)
  => AppConfig
  -> NodeDataSource
  -> SnapshotMeta
  -> SnapshotMetaId
  -> SnapshotImportOptions
  -> m ()
importSnapshotData appConfig nds sm smId SnapshotImportOptions{..} = do
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

    importFailed stderr = do
      let
        isNotProgressLine l = not
          $  "nodes"    `T.isInfixOf` l
          && "contents" `T.isInfixOf` l
          && "commits"  `T.isInfixOf` l
        -- Since import progress output is printed to 'stderr' (see tezos/#5213),
        -- it contains large escaped import progress, which we don't want display
        -- to user, so we remove import progress lines from 'stderr' before
        -- saving it to db.
        filteredStderr = T.unlines $ filter isNotProgressLine $ T.lines stderr
      $(logError) $ "Failed to import node snapshot: " <> filteredStderr
      update [ SnapshotMeta_importErrorField =. Just filteredStderr ] (AutoKeyField ==. smId)
      traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId
      updateState NodeProcessState_ImportFailed

    procSpec configFile = let
      blockArg = flip (maybe []) (sm ^. snapshotMeta_headBlock) $ \bh ->
        [ "--block"
        , T.unpack $ blockHashToBase58Text bh
        ]
      args =
        [ "snapshot", "import", storePath
        , "--data-dir", dataDir
        , "--config-file", configFile
        , "--progress-display-mode", "always"
        ] <> blockArg
      cp = Process.proc nodePath args
      in cp { Process.std_out = Process.CreatePipe, Process.std_err = Process.CreatePipe }

    procMonitorStream cp SnapshotInfoResult{..} = runLoggingEnv logger $ do
      stderrLogVar :: TVar ByteString <- liftIO $ newTVarIO ""
      ph <- liftIO $ mkSnapshotImportStreamingProcess cp stderrLogVar
      go ph stderrLogVar
      where
        {-# INLINE go #-}
        go ph stderrLogVar = do
          let getPC = \case
                [] -> ProcessControl_Stop
                (c:_) -> c
          procControl <- inDb (getPC <$> project SnapshotMeta_controlField (AutoKeyField ==. smId))
          getStreamingProcessExitCode ph >>= \case
            Nothing -> do
              inDb $ updateState NodeProcessState_ImportingSnapshot
              let
                stop = procControl /= ProcessControl_Run
                delayInSec = 1 :: NominalDiffTime
              when stop $ do
                inDb $ updateState NodeProcessState_ImportCanceled
                liftIO $ terminateProcess $ streamingProcessHandleRaw ph
              threadDelay' delayInSec *> go ph stderrLogVar
            Just exitCode -> do
              stderr <- fmap T.decodeUtf8 $ liftIO $ readTVarIO stderrLogVar
              case exitCode of
                ExitSuccess -> void $ do
                  $(logDebug) $ "importSnapshotData success: stderr: " <> stderr
                  inDb $ updateSnapshotMeta sirBlockHash sirLevel sirTimestamp smId
                  inDb $
                    if sioVerifySnapshot
                    then updateState NodeProcessState_ImportComplete
                    else startNodeDaemon
                ExitFailure _ -> case procControl of
                  ProcessControl_Stop -> do
                    -- Do cleanup if the snapshot import was canceled
                    inDb $ removeNodeDbImpl (Right ())
                    liftIO $ removeDirectoryRecursive dataDir
                  _ -> inDb $ importFailed stderr

  liftIO $ withNodeConfig appConfig $ \configFile -> do
    runLoggingEnv logger $ $(logInfoSH) ("importSnapshotData: running process" :: Text, procSpec configFile)
    mbSnapshotInfoResult <- runLoggingEnv logger $ execSnapshotInfo storePath nodePath
    case mbSnapshotInfoResult of
      Left err -> runLoggingEnv logger $ inDb $ importFailed err
      Right snapshotInfoResult -> procMonitorStream (procSpec configFile) snapshotInfoResult

  when sioRemoveSnapshotFile $
    removeFileLogging storePath

  -- Clear import log when import finished to avoid its flickering
  -- appearance on UI when user starts new node.
  runLoggingEnv logger $ inDb $ do
    update
      [SnapshotMeta_importLogField =. (Nothing :: Maybe Text)]
      (AutoKeyField ==. smId)
    traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId
  where
    mkSnapshotImportStreamingProcess
      :: (MonadUnliftIO m)
      => CreateProcess      -- ^ Process to run in the streaming mode
      -> TVar ByteString    -- ^ @TVar@ for collecting process error output
      -> m StreamingProcessHandle
    mkSnapshotImportStreamingProcess cp stderrLogVar = do
      let logger = _nodeDataSource_logger nds
          db     = _nodeDataSource_pool nds
          logStderrLine stderrLine = liftIO $ atomically $ modifyTVar stderrLogVar (<> stderrLine)
      let
        -- Starting from the version 5 of Tezos node snapshots, the progress log
        -- is printed to 'stderr'. For more context, see tezos/#5213.
        --
        -- Since 'stderr' may contain not only import progress, but also
        -- import errors and warnings, we have the parser to distinguish
        -- the import progress lines:
        -- Example progress line: "645k contents / 717k nodes / 1 commits"
        progressLogParser = do
          let keywords = P.string <$> ["contents", "nodes", "commits"]
              space    = P.skipWhile isSpace
              sep      = space >> P.char '/' >> space
              countK   = P.decimal @Int >> optional (P.char 'k')
              entry    = countK >> space >> P.choice keywords
          void $ entry >> P.count 2 (sep >> entry)
        updateImportProgress stderrLine = do
          let line = T.strip $ T.decodeUtf8 stderrLine
              isProgressLine = isRight . P.parseOnly progressLogParser
          when (isProgressLine line) $
            runLoggingEnv logger $ runDb (Identity db) $ updateSnapshotMetaImportLog line smId
      createProcessWithStreams cp (return ()) (return ()) (CL.mapM_ $ \line ->
        updateImportProgress line *> logStderrLine line)

initSnapshotMeta
  :: MonadLoggerIO m
  => AppConfig
  -> Maybe FilePath
  -> NodeDataSource
  -> Maybe URI
  -> m (SnapshotMetaId, SnapshotMeta)
initSnapshotMeta appConfig mbStorePath nds mbUri = runDb (Identity $ _nodeDataSource_pool nds) $ do
  let storePath = mbStorePath ?: (snapshotStorePath appConfig <> defaultSnapshotFileName)
      fileName  = maybe defaultSnapshotFileName takeFileName mbStorePath
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
      , _snapshotMeta_importLog = Nothing
      , _snapshotMeta_downloadProgress = Nothing
      }
  deleteAll sm
  k <- insert sm
  notify NotifyTag_SnapshotMeta sm
  pure (k, sm)

updateSnapshotMeta
  :: (PersistBackend m)
  => BlockHash
  -> RawLevel
  -> UTCTime
  -> SnapshotMetaId
  -> m ()
updateSnapshotMeta mbBlockHash mbLevel mbTimestamp smId = do
  now <- getTime
  update
    [ SnapshotMeta_headBlockField =. Just mbBlockHash
    , SnapshotMeta_headBlockLevelField =. Just mbLevel
    , SnapshotMeta_headBlockBakeTimeField =. Just mbTimestamp
    , SnapshotMeta_importCompleteTimeField =. Just now
    ]
    (AutoKeyField ==. smId)
  traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId

updateSnapshotMetaImportLog
  :: (PersistBackend m)
  => Text
  -> SnapshotMetaId
  -> m ()
updateSnapshotMetaImportLog importLog smId = do
  update [SnapshotMeta_importLogField =. Just importLog] (AutoKeyField ==. smId)
  traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId

removeFileLogging :: (MonadLogger m, MonadIO m, MonadMask m) => FilePath -> m ()
removeFileLogging f = liftIO (removeFile f) `catch` \(e :: IOException) -> $(logError) $ "Failed to remove file: " <> T.pack f <> ": " <> tshow e

snapshotProviderUri :: NamedChain -> KnownSnapshotProvider -> URI
snapshotProviderUri namedChain = \case
  KnownSnapshotProvider_TzInit region -> tzInitUri namedChain region

-- | The path where the node snapshot is stored.
snapshotStorePath :: AppConfig -> FilePath
snapshotStorePath appConfig = _appConfig_kilnDataDir appConfig <> "/snapshots/"

-- | Default name of node snapshot file which is used when the file isn't
-- uploaded by user.
defaultSnapshotFileName :: FilePath
defaultSnapshotFileName = "snapshot"

-- | Default name of snapshot metadata file.
snapshotMetadataFileName :: URI
snapshotMetadataFileName = [Uri.uri|tezos-snapshots.json|]

-- | Given the url, tries to recognize whether this url points to
-- snapshot provider metadata or specific node snapshot, and then
-- downloads the snapshot.
--
-- First, it tries to download and validate node snapshot from given url.
-- If it fails, tries to download the latest rolling snapshot from
-- the metadata from given url. If it fails too, tries to do download
-- the latest rolling snapshot from the metadata from 'url/tezos-snapshots.json'.
handleDownloadSnapshotByUrlOverloaded
  :: ( MonadIO m
     , MonadCatch m
     )
  => AppConfig
  -> NodeDataSource
  -> URI
  -> m ()
handleDownloadSnapshotByUrlOverloaded appConfig nds uri =
  void $ liftIO $ forkIO $ runLoggingEnv logger $ do
    (smId, sm) <- initSnapshotMeta appConfig Nothing nds (Just uri)
    $(logDebug) $ "Trying to download snapshot by url: " <> uriText
    resByUrl <- try @_ @SomeException $ handleSnapshotDownloadSync appConfig nds uri (smId, sm)
    whenLeft resByUrl $ \e -> do
      $(logError) $ T.unlines
        [ "Failed to download snapshot by url:"
        , tshow e
        ]
      $(logDebug) $ "Trying to download snapshot from provider: " <> uriText
      resFromProvider <- try @_ @SomeException $
        handleDownloadSnapshotFromProviderSync appConfig nds uri smId DontReportError
      whenLeft resFromProvider $ \e' -> do
        $(logError) $ T.unlines
          [ "Failed to download snapshot from provider:"
          , tshow e'
          ]
        uri' <- case snapshotMetadataFileName `relativeTo` uri of
          Just u -> pure u
          Nothing -> throwString $ concat
            [ "Failed to make "
            , renderStr snapshotMetadataFileName
            , " relative to "
            , renderStr uri
            ]
        $(logDebug) $ "Trying to download snapshot from provider: " <> render uri'
        handleDownloadSnapshotFromProviderSync appConfig nds uri' smId (OverrideUrl uri)
  where
    logger = _nodeDataSource_logger nds
    uriText = render uri

-- | Synchronously handle internal node bootstrap using the @SnapshotImportSource_KnownSnapshotProviderSource@
-- option. This function downloads latest rolling snapshot from the given snapshot provider by parsing
-- the metadata from the provider URL.
handleDownloadSnapshotFromProviderSync
  :: ( MonadIO m
     , MonadMask m
     , MonadBaseNoPureAborts IO m
     , MonadUnliftIO m
     )
  => AppConfig
  -> NodeDataSource
  -> URI
  -> SnapshotMetaId
  -> ErrorReportPolicy
  -> m ()
handleDownloadSnapshotFromProviderSync appConfig nds providerUrl smId errPolicy = runLoggingEnv logger $ do
  let
    handleFetchMetadataError (e :: SomeException) = do
      $(logError) $ "Snapshot download failed with: " <> tshow e
      -- We don't want to immediately inform the user that
      -- an error has occurred until we have tried all the download options.
      --
      -- See 'handleDownloadSnapshotByUrlOverloaded' for more context.
      case errPolicy of
        DontReportError -> pure ()
        ReportError -> handleSnapshotDownloadFailure nds smId $ mkErrorText providerUrl
        OverrideUrl u -> handleSnapshotDownloadFailure nds smId $ mkErrorText u
      throw e
  $(logDebug) $ "Downloading snapshot metadata from " <> T.pack (renderStr providerUrl)
  latestSnapshotMetadata <- handle handleFetchMetadataError $ do
    metadata <- downloadSnapshotMetadata httpMgr providerUrl
    kilnNodeVersion <- liftIO $ runLoggingEnv logger $ getKilnNodeVersion appConfig
    findLatestCompatibleSnapshot appConfig kilnNodeVersion compatibleSnapshotVersion metadata
  latestSnapshotUri <- mkURI $ latestSnapshotMetadata ^. snapshotMetadata_url
  $(logDebug) $ "Found latest snapshot url " <> T.pack (renderStr latestSnapshotUri)
  updatedSnapshotMeta <- updateSnapshotMeta' latestSnapshotMetadata latestSnapshotUri
  let
    importOptions = SnapshotImportOptions
      { sioRemoveSnapshotFile = True
      , sioVerifySnapshot = False
      }
  downloadSnapshot appConfig nds latestSnapshotUri (smId, updatedSnapshotMeta) importOptions
  where
    logger = _nodeDataSource_logger nds
    db = _nodeDataSource_pool nds
    httpMgr = _nodeDataSource_httpMgr nds
    mkErrorText url = T.concat
      [ "Unable to download latest snapshot from "
      , render url
      , ". Please choose another option."
      ]

    updateSnapshotMeta'
      :: (MonadLoggerIO m)
      => SnapshotMetadata
      -> URI
      -> m SnapshotMeta
    updateSnapshotMeta' m url = runDb (Identity db) $ do
      update
        [ SnapshotMeta_mbUriField =. Just url
        , SnapshotMeta_headBlockField =. (Just $ m ^. snapshotMetadata_blockHash :: Maybe BlockHash)
        , SnapshotMeta_headBlockLevelField =. (Just $ m ^. snapshotMetadata_blockHeight :: Maybe RawLevel)
        , SnapshotMeta_headBlockBakeTimeField =. (Just $ m ^. snapshotMetadata_blockTimestamp :: Maybe UTCTime)
        ] (AutoKeyField ==. smId)
      mbUpdatedSnapshotMeta <- get smId
      let errMsg = "Inconsistent db state: SnapshotMeta not found"
          updatedSnapshotMeta = mbUpdatedSnapshotMeta ?: error errMsg
      notify NotifyTag_SnapshotMeta updatedSnapshotMeta
      pure updatedSnapshotMeta

getChainName :: MonadThrow m => AppConfig -> m NamedChain
getChainName appConfig = case identifyChain $ _appConfig_chainId appConfig of
  Just n -> pure n
  Nothing -> throwString "Cannot resolve chain name"

-- | Accepts a list of providers and tries to download the snapshot from each,
-- in order, until one succeeds.
handleDownloadSnapshotFromProviderAsync
  :: ( MonadIO m
     , MonadMask m
     , MonadBaseNoPureAborts IO m
     , MonadUnliftIO m
     )
  => AppConfig
  -> NodeDataSource
  -> [KnownSnapshotProvider]
  -> m ()
handleDownloadSnapshotFromProviderAsync appConfig nds providers =
  void $ liftIO $ async $
    runLoggingEnv (_nodeDataSource_logger nds) $ downloadFromProvider providers
  where
    downloadFromProvider [] = pure ()
    downloadFromProvider (p:ps) = downloadFromProvider_ p >>= \case
      Right _ -> pure ()
      Left (e, smId) -> do
        case ps of
          [] -> do
            handleSnapshotDownloadFailure nds smId "Snapshot download failed from all of the known providers. Please choose another option."
          (_:_) -> do
            $(logDebug) $ "Download failed with error: " <> tshow e <> "\n trying another alternative.."
            downloadFromProvider ps

    downloadFromProvider_ (KnownSnapshotProvider_TzInit region) = do
      chainName <- getChainName appConfig
      let uri = tzInitUri chainName region
      (smId, sm) <- initSnapshotMeta appConfig Nothing nds (Just uri)
      try (handleSnapshotDownloadSync appConfig nds uri (smId, sm)) >>= \case
        Left (e :: SomeException) -> pure $ Left (e, smId)
        Right _ -> pure $ Right ()

-- | Download the list of snapshot metadata from the given provider url.
downloadSnapshotMetadata
  :: ( MonadLoggerIO m
     , MonadThrow m
     )
  => Http.Manager
  -> URI
  -> m [SnapshotMetadata]
downloadSnapshotMetadata mgr providerUri = do
  let uriStr = renderStr providerUri
  resp <- doRequestLBSThrows mgr uriStr
  let body = Http.getResponseBody resp
      statusCode = Http.getResponseStatusCode resp
      statusLogText = T.pack uriStr <> " responded with status " <> tshow statusCode
  case statusCode of
    200 -> $(logDebug) statusLogText
    _ -> do
      $(logError) statusLogText
      $(logError) $ "Response body: " <> T.decodeUtf8 (LBS.toStrict body)
      throwString $ "Expected metadata response status to be 200, but got " <> show statusCode
  either throwString (pure . unSnapshotMetadataList) $ eitherDecode body

-- | Given the list of snapshot metadata, find the latest compatible snapshot url
-- using the following strategy:
--
-- 1. Try to find the snapshot made by exactly the same version of 'octez-node'
-- that is used by Kiln.
-- 2. If there is none, try to find the snapshot made by the same major version
-- of 'octez-node' and less - minor version with 'snapshot_version' equal to the
-- snapshot version which is supported by current version of Kiln node (currently set to 6).
-- 3. If there is none, try to find the snapshot with the 'snapshot_version' which
-- is supported by Kiln node.
--
-- TODO [#212] update this algorithm based on the issue description.
findLatestCompatibleSnapshot
  :: (MonadThrow m)
  => AppConfig
  -> MajorMinorVersion
  -> Int
  -> [SnapshotMetadata]
  -> m SnapshotMetadata
findLatestCompatibleSnapshot _ _ _ [] = throwString "Got empty metadata list from the snapshot provider"
findLatestCompatibleSnapshot appConfig kilnNodeVersion neededSnapshotVersion metadata = do
  let mbChainName = showNamedChain <$> identifyChain chainId
      releaseSnapshots = flip filter metadata $ \m ->
        m ^. snapshotMetadata_tezosVersion . majorMinorVersion_additional_info == Release
  chainName <- maybe (throwString "Snapshot metadata doesn't support custom chains") pure mbChainName
  case filter (\m -> defaultFilter chainName m && isExactSameVersion m) releaseSnapshots of
    [] -> case filter (\m -> defaultFilter chainName m && isSameMajorVersion m) releaseSnapshots of
      [] -> case filter (defaultFilter chainName) releaseSnapshots of
        [] -> throwString "Couldn't find compatible snapshot in the snapshot provider metadata"
        m -> pure $ maximumByBlockHeight m
      m -> pure $ maximumByBlockHeight m
    m -> pure $ maximumByBlockHeight m
  where
    chainId = _appConfig_chainId appConfig
    defaultFilter chainName m
      =  isNeededChain chainName m
      && isRolling m
      && isTezosSnapshot m
      && isCompatibleSnapshotVersion m
    isCompatibleSnapshotVersion m =
      let versionDiff = subtract <$> (m ^. snapshotMetadata_snapshotVersion) <*> Just neededSnapshotVersion
      in versionDiff `elem` [Just 0, Just 1, Just 2]
    isNeededChain chainName m = m ^. snapshotMetadata_chainName == chainName
    isRolling m = m ^. snapshotMetadata_historyMode
      == SnapshotHistoryMode_Rolling
    isTezosSnapshot m = m ^. snapshotMetadata_artifactType
      == SnapshotArtifactType_TezosSnapshot
    isExactSameVersion m = m ^. snapshotMetadata_tezosVersion
      == kilnNodeVersion
    isSameMajorVersion m = let tezosVersion = m ^. snapshotMetadata_tezosVersion
      in tezosVersion ^. majorMinorVersion_major == kilnNodeVersion ^. majorMinorVersion_major
      && tezosVersion ^. majorMinorVersion_minor < kilnNodeVersion ^. majorMinorVersion_minor
    byBlockHeight m1 m2 = compare
      (m1 ^. snapshotMetadata_blockHeight)
      (m2 ^. snapshotMetadata_blockHeight)
    maximumByBlockHeight m = maximumBy byBlockHeight m

-- The version of node snapshot which is compatible with the
-- version of 'octez-node' binary that is used in Kiln
--
-- TODO [#212] [tezos/#6319] get this info from the node.
compatibleSnapshotVersion :: Int
compatibleSnapshotVersion = 7

-- | Update the 'SnapshotMeta' table and set the correct internal node's
-- process state in case of snapshot download error.
handleSnapshotDownloadFailure
  :: (MonadIO m)
  => NodeDataSource
  -> SnapshotMetaId
  -> Text
  -> m ()
handleSnapshotDownloadFailure nds smId errText = runLoggingEnv logger $ do
  runDb (Identity db) $ do
    mbInternalNodeData <- getInternalNode
    for_ mbInternalNodeData $ \(nodeId, pid) -> do
      update [ SnapshotMeta_downloadErrorField =. Just errText ] (AutoKeyField ==. smId)
      traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId
      updateProcessState (_deletableRow_data pid)
        (Just (\pd -> (NotifyTag_NodeInternal, (nodeId, pd))))
        (ProcessState_Node NodeProcessState_DownloadFailed)
  where
    logger = _nodeDataSource_logger nds
    db = _nodeDataSource_pool nds
