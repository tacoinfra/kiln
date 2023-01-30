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

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Snapshot where

import Control.Concurrent
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Exception.Safe (IOException, MonadThrow, throwString, try)
import Control.Monad.Catch (MonadMask, catch, finally, onException)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger
import Data.Aeson (eitherDecode)
import Data.ByteString (ByteString)
import Data.Conduit.Binary (sinkFileCautious)
import qualified Data.Conduit.List as CL
import Data.Conduit.Process (getStreamingProcessExitCode, streamingProcessHandleRaw, terminateProcess)
import Data.Foldable (maximumBy)
import Data.Int (Int32)
import Data.String (IsString(..))
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
import qualified System.Process as Process
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import Text.URI (URI, mkURI, renderStr)
import Text.URI.QQ (uri)

import Tezos.Types

import Backend.Common
import Backend.Config
import Backend.Http (doRequestLBSThrows)
import Backend.NodeRPC
import Backend.Process.Node (nixNodePath)
import Backend.Schema
import Backend.Workers.Process
import Common.Schema
import ExtraPrelude

-- | Type alias for the default key of 'SnapshotMeta' table defined for convenience.
type SnapshotMetaId = Key SnapshotMeta BackendSpecific

-- | Auxiliary data type which represents the snapshot import options
-- that depend on the snapshot import source:
--
-- e.g we don't need to remove the snapshot file if this is the
-- file uploaded by user and we don't ask user to verify the
-- snapshot downloaded from xtz-shots metadata because it could
-- be done automatically since snapshot's head block is known
data SnapshotImportOptions = SnapshotImportOptions
  { sioRemoveSnapshotFile :: Bool
  , sioVerifySnapshot :: Bool
  }

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

handleSnapshotDownload
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
  -> m ()
handleSnapshotDownload appConfig nds snapshotURI =
  void $ liftIO $ forkIO $ runLoggingEnv (_nodeDataSource_logger nds) $ do
    snapshotMeta <- initSnapshotMeta appConfig Nothing nds (Just snapshotURI)
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
  let db = _nodeDataSource_pool nds
      logger = _nodeDataSource_logger nds
      dataDir = nodeDataDir appConfig
  cleanupDir storeLocation
  nodePPid <- runDb (Identity db) $ project1
    ( NodeInternal_idField
    , NodeInternal_dataField ~> DeletableRow_dataSelector
    ) CondEmpty
  for_ nodePPid $ \(nid, pid) -> do
    downloaderThread <- liftIO $ forkIO $ do
      res <- try $ do
        request <- Http.parseRequest $ renderStr snapshotURI
        runResourceT $ Http.httpSink request $ \_ -> sinkFileCautious storePath
      case res of
        Left (e :: Http.HttpException) ->
          let errText = T.pack (show e)
          in handleSnapshotDownloadFailure nds smId errText
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
            importSnapshotData appConfig nds sm smId importOptions
          (Just (ProcessState_Node NodeProcessState_DownloadCanceled)) -> do
            liftIO $ killThread downloaderThread
            cleanUpNode
          (Just (ProcessState_Node NodeProcessState_DownloadFailed)) -> liftIO $ do
            snapshotExists <- doesFileExist storePath
            when snapshotExists $ removeFile storePath
          _ -> threadDelay' 1 >> go
    go
  where
    storeLocation = snapshotStorePath appConfig
    storePath = storeLocation <> defaultSnapshotFileName

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

    importFailed msg stderr = do
      $(logError) msg
      update [ SnapshotMeta_importErrorField =. Just stderr ] (AutoKeyField ==. smId)
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

    procMonitorStream cp = runLoggingEnv logger $ do
      stderrLogVar :: TVar ByteString <- liftIO $ newTVarIO ""
      let logStderrLine stderrLine = liftIO $ atomically $ modifyTVar stderrLogVar (<> stderrLine)
          updateImportProgress stdoutLine = do
            -- Progress output has some additional characters that are used to
            -- animate the progress. These characters shoudn't be displayed in Kiln UI,
            -- so we drop the prefix of the @stdoutLine@.
            let line = T.drop 5 $ T.decodeUtf8 stdoutLine
            runLoggingEnv logger $ inDb $ updateSnapshotMetaImportLog line smId
      ph <- liftIO $
        createProcessWithStreams cp (return ()) (CL.mapM_ updateImportProgress) (CL.mapM_ logStderrLine)
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
                  (infoExitCode, infoStdout, _) <- liftIO $ Process.readProcessWithExitCode nodePath ["snapshot", "info", storePath] ""
                  when (infoExitCode == ExitSuccess) $ do
                    (mBlkHash, mLevel, mTimestamp) <- getSnapshotInfo infoStdout sm
                    inDb $ updateSnapshotMeta mBlkHash mLevel mTimestamp smId
                  inDb $
                    if sioVerifySnapshot
                    then updateState NodeProcessState_ImportComplete
                    else startNodeDaemon
                ExitFailure _ -> inDb $ importFailed "importSnapshotData failed: " stderr

  liftIO $ withNodeConfig appConfig $ \configFile -> do
    runLoggingEnv logger $ $(logInfoSH) ("importSnapshotData: running process" :: Text, procSpec configFile)
    procMonitorStream (procSpec configFile)

  when sioRemoveSnapshotFile $
    removeFileLogging storePath

  -- Do cleanup after cancel import
  procControl <- inDb $ project SnapshotMeta_controlField (AutoKeyField ==. smId)
  case headMay procControl of
    Just ProcessControl_Stop -> do
      inDb $ removeNodeDbImpl (Right ())
      liftIO $ removeDirectoryRecursive dataDir
    _ -> pure ()
  where
    -- | Get snapshot's block hash, level and timestamp either from
    -- @SnapshotMeta@ if they're already known or from 'octez-node snapshot info'
    -- command's stdout.
    --
    -- This data is already known in case when we downloaded the snapshot from
    -- xtz-shots metadata which provides the hash, level and timestamp as well.
    getSnapshotInfo
      :: ( MonadLoggerIO m
         , MonadBaseNoPureAborts IO m
         )
      => String
      -> SnapshotMeta
      -> m ( Maybe BlockHash
           , Maybe RawLevel
           , Maybe UTCTime
           )
    getSnapshotInfo stdout snapshotMeta = do
      let
        mBlkHashFromStdout = extractBlockHash stdout
        mLevelFromStdout   = extractLevel stdout

        mBlkHash = snapshotMeta ^. snapshotMeta_headBlock <|> mBlkHashFromStdout
        mLevel   = snapshotMeta ^. snapshotMeta_headBlockLevel <|> mLevelFromStdout
      mTimestamp <- case snapshotMeta ^. snapshotMeta_headBlockBakeTime of
        Nothing -> fmap join $ for mBlkHash $ \blkHash -> do
          mBlk <- flip runReaderT nds $ runExceptT @KilnRpcError $ runNodeQueryT $
            nodeQueryDataSourceSafe $ nodeQuery_BlockHeader blkHash
          pure $ mBlk ^? _Right . timestamp
        t -> pure t
      pure (mBlkHash, mLevel, mTimestamp)
      where
        blockHashRegex, levelRegex :: String
        blockHashRegex = "block hash ([A-Za-z0-9]*)"
        levelRegex = "at level ([0-9]*)"

        extractFromStdout :: String -> String -> (String -> Maybe a) -> Maybe a
        extractFromStdout source regex parse =
          let (_, _, _, matches) = source =~ regex :: (String, String, String, [String]) in
            parse =<< listToMaybe matches

        extractBlockHash :: String -> Maybe BlockHash
        extractBlockHash src = BlockHash <$> extractFromStdout src blockHashRegex
          (either (const Nothing) Just . fromBase58 . fromString)

        extractLevel :: String -> Maybe RawLevel
        extractLevel src = extractFromStdout src levelRegex (fmap fromIntegral . readMaybe @Int32)

initSnapshotMeta
  :: MonadLoggerIO m
  => AppConfig
  -> Maybe FilePath
  -> NodeDataSource
  -> Maybe URI
  -> m (SnapshotMetaId, SnapshotMeta)
initSnapshotMeta appConfig mbStorePath nds mbUri = runDb (Identity $ _nodeDataSource_pool nds) $ do
  let storePath = mbStorePath ?: snapshotStorePath appConfig <> defaultSnapshotFileName
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
  -> SnapshotMetaId
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

-- | URI of xtz-shots snapshot metadata.
xtzShotsMetadataUri :: URI
xtzShotsMetadataUri = [uri|https://xtz-shots.io/tezos-snapshots.json|]

-- | The path where the node snapshot is stored.
snapshotStorePath :: AppConfig -> FilePath
snapshotStorePath appConfig = _appConfig_kilnDataDir appConfig <> "/snapshots/"

-- | Default name of node snapshot file which is used when the file isn't
-- uploaded by user.
defaultSnapshotFileName :: FilePath
defaultSnapshotFileName = "snapshot"

-- | Handle internal node bootstrap using the @SnapshotImportSource_XtzShotsMetadataSource@
-- option. This function downloads latest rolling snapshot from xtz-shots.io by parsing
-- the metadata from @xtzShotsMetadataUri@.
handleDownloadXtzShotsMetadata
  :: (MonadIO m)
  => AppConfig
  -> NodeDataSource
  -> m ()
handleDownloadXtzShotsMetadata appConfig nds = void $ liftIO $ forkIO $ runLoggingEnv logger $ do
  (smId, _) <- initSnapshotMeta appConfig Nothing nds Nothing
  let handleFetchMetadataError = handleSnapshotDownloadFailure nds smId errText
  latestSnapshotMetadata <- flip onException handleFetchMetadataError $ do
    metadata <- downloadXtzShotsMetadata httpMgr
    findLatestSnapshot appConfig metadata
  latestSnapshotUri <- mkURI $ latestSnapshotMetadata ^. xtzShotsMetadata_url
  $(logDebug) $ "Found latest snapshot url " <> T.pack (renderStr latestSnapshotUri)
  updatedSnapshotMeta <- updateSnapshotMeta' latestSnapshotMetadata smId latestSnapshotUri
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
    errText = "Unable to download latest snapshot from xtz-shots. Please choose another option."

    updateSnapshotMeta'
      :: (MonadLoggerIO m)
      => XtzShotsMetadata
      -> SnapshotMetaId
      -> URI
      -> m SnapshotMeta
    updateSnapshotMeta' m smId url = runDb (Identity db) $ do
      update
        [ SnapshotMeta_mbUriField =. Just url
        , SnapshotMeta_headBlockField =. (Just $ m ^. xtzShotsMetadata_blockHash :: Maybe BlockHash)
        , SnapshotMeta_headBlockLevelField =. (Just $ m ^. xtzShotsMetadata_blockHeight :: Maybe RawLevel)
        , SnapshotMeta_headBlockBakeTimeField =. (Just $ m ^. xtzShotsMetadata_blockTimestamp :: Maybe UTCTime)
        ] (AutoKeyField ==. smId)
      mbUpdatedSnapshotMeta <- get smId
      let errMsg = "Inconsistent db state: SnapshotMeta not found"
          updatedSnapshotMeta = mbUpdatedSnapshotMeta ?: error errMsg
      notify NotifyTag_SnapshotMeta updatedSnapshotMeta
      pure updatedSnapshotMeta

-- | Download the list of snapshot metadata from @xtzShotsMetadataUri@.
downloadXtzShotsMetadata :: (MonadIO m, MonadThrow m) => Http.Manager -> m [XtzShotsMetadata]
downloadXtzShotsMetadata mgr = do
  resp <- doRequestLBSThrows mgr (renderStr xtzShotsMetadataUri)
  let body = Http.getResponseBody resp
  either throwString pure $ eitherDecode body

-- | Given the list of snapshot metadata fetched from @xtzShotsMetadataUri@
-- find the latest rolling snapshot url.
findLatestSnapshot :: (MonadThrow m) => AppConfig -> [XtzShotsMetadata] -> m XtzShotsMetadata
findLatestSnapshot _ [] = throwString "Got empty metadata list from xtz-shots"
findLatestSnapshot appConfig metadata = do
  let mbChainName = showNamedChain <$> identifyChain chainId
  chainName <- maybe (throwString "xtz-shots doesn't support custom chains") pure mbChainName
  let
    isNeededChain m = m ^. xtzShotsMetadata_chainName == chainName
    filteredMetadata = flip filter metadata $ \m ->
      isNeededChain m && isRolling m && isTezosSnapshot m
  when (null filteredMetadata) $
    throwString "There is no rolling tezos snapshot in xtz-shots metadata"
  pure $ maximumBy byBlockHeight filteredMetadata
  where
    chainId = _appConfig_chainId appConfig
    isRolling m = m ^. xtzShotsMetadata_historyMode
      == XtzShotsSnapshotHistoryMode_Rolling
    isTezosSnapshot m = m ^. xtzShotsMetadata_artifactType
      == XtzShotsArtifactType_TezosSnapshot
    byBlockHeight m1 m2 = compare
      (m1 ^. xtzShotsMetadata_blockHeight)
      (m2 ^. xtzShotsMetadata_blockHeight)

-- | Update the 'SnapshotMeta' table and set the correct internal node's
-- process state in case of snapshot download error.
handleSnapshotDownloadFailure
  :: (MonadIO m)
  => NodeDataSource
  -> SnapshotMetaId
  -> Text
  -> m ()
handleSnapshotDownloadFailure nds smId errText = runLoggingEnv logger $ do
  $(logError) $ "Snapshot download failed with:" <> errText
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
