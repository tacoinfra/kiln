{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedSums #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Snapshot where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe (IOException)
import Control.Monad.Catch (MonadMask, catch, finally, onException)
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger
import Data.ByteString.Base58
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql, (=.), (==.))
import Rhyolite.Backend.DB (getTime, runDb, project1, MonadBaseNoPureAborts)
import Rhyolite.Backend.Logging
import Safe
import qualified Snap.Core as Snap
import Snap.Util.FileUploads
import System.Directory
import System.Exit (ExitCode(..))
import qualified System.Process as Process

import Tezos.NodeRPC (_cachedHistory_blocks)
import Tezos.Types
import Tezos.Common.Base58Check

import Backend.CachedNodeRPC
import Backend.Common
import Backend.Config
import Backend.NodeCmd
import Backend.Schema
import Backend.Workers.Process
import Common.Schema
import ExtraPrelude

handleSnapshotUpload
  :: AppConfig
  -> NodeDataSource
  -> Either NamedChain a
  -> MVar ()
  -> Snap.Snap ()
handleSnapshotUpload appConfig nds chain lockMVar = do
  liftIO $ createDirectoryIfMissing True uploadTmpLocation
    `catch` (\(e :: IOException) -> runLoggingEnv logger $ $(logWarn) ("Make dir failed: " <> tshow uploadTmpLocation <> "\nError: " <> tshow e))
  void $ handleFileUploads uploadTmpLocation uploadPolicy partUploadPolicy uploadHandler
  where
    inDb :: (MonadIO m, MonadBaseNoPureAborts IO m, MonadLogger m) => DbPersist Postgresql m a -> m a
    inDb = runDb (Identity $ _nodeDataSource_pool nds)

    logger = _nodeDataSource_logger nds
    uploadPolicy = defaultUploadPolicy
    uploadTmpLocation = _appConfig_kilnDataDir appConfig <> "/snapshots_tmp/"
    storeLocation = _appConfig_kilnDataDir appConfig <> "/snapshots/"
    partUploadPolicy _ = allowWithMaximumSize (10*1024*1024*1024) -- 10gb
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
            fileName = maybe "file" (T.unpack . T.decodeUtf8) $ partFileName p
            storePath = storeLocation <> fileName
          (smId, sm) <- inDb $ do
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
                }
            deleteAll sm
            k <- insert sm
            notify NotifyTag_SnapshotMeta sm
            pure (k, sm)
          liftIO $ do
            renameFile fp storePath
            forkIO $ withLockRelease $ do
              let timeoutSeconds = 60*60*10
              timeout' timeoutSeconds (runLoggingEnv logger $ importSnapshotData appConfig nds chain sm smId) >>= \case
                Just _ -> pure ()
                Nothing -> runLoggingEnv logger $ flip finally (removeFileLogging storePath) $ do
                  $(logError) "Could not import snapshot: Timeout"
                  inDb $ do
                    nodePPid <- project1
                      ( NodeInternal_idField
                      , NodeInternal_dataField ~> DeletableRow_dataSelector
                      ) CondEmpty
                    for_ nodePPid $ \(nid, pid) -> updateProcessState pid
                      (Just (\pd -> (NotifyTag_NodeInternal, (nid, pd))))
                      (ProcessState_Node NodeProcessState_ImportTimeout)

cleanupDir :: (MonadLogger m, MonadIO m, MonadMask m) => FilePath -> m ()
cleanupDir dir = do
  liftIO (removeDirectoryRecursive dir)
    `catch` \(e :: IOException) -> $(logWarn) ("Remove dir failed: " <> tshow dir <> ": " <> tshow e)
  liftIO (createDirectoryIfMissing True dir)
    `catch` \(e :: IOException) -> $(logWarn) ("Make dir failed: " <> tshow dir <> ": " <> tshow e)

importSnapshotData
  :: (MonadLogger m, MonadIO m, MonadMask m, MonadBaseNoPureAborts IO m)
  => AppConfig
  -> NodeDataSource
  -> Either NamedChain a
  -> SnapshotMeta
  -> Key SnapshotMeta BackendSpecific
  -> m ()
importSnapshotData appConfig nds chain sm smId = do
  let
    nodePath = either nodePaths (const $ nodePaths NamedChain_Mainnet) chain
    dataDir = nodeDataDir appConfig
    storePath = T.unpack $ _snapshotMeta_storePath sm
    inDb :: (MonadIO m, MonadBaseNoPureAborts IO m, MonadLogger m) => DbPersist Postgresql m a -> m a
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
    updateState = updateState' nodePPid
    procSpec = Process.proc nodePath ["snapshot", "import", storePath, "--data-dir", dataDir]
  $(logDebug) "importSnapshotData: starting import"
  (exitCode, _stdout, stderr) <- readCreateProcessWithExitCodeWithLogging procSpec ""

-- Example output on success
-- stderr:
-- Jul  6 19:39:08 - shell.snapshots: Importing data from snapshot file ./.kiln/snapshots/main.snapshot
-- Jul  6 19:39:08 - shell.snapshots: You may consider using the --block <block_hash> argument to verify that the block imported is the one you expect
-- Jul  6 19:39:08 - shell.snapshots: Retrieving and validating data. This can take a while, please bear with us
-- Jul  6 19:45:44 - shell.snapshots: Setting current head to block BLWxHkBhZfaj
-- Jul  6 19:45:45 - shell.snapshots: Setting history-mode to full
-- Jul  6 19:45:46 - shell.snapshots: Successful import from file ./.kiln/snapshots/main.snapshot

  let
    importFailed msg = do
      $(logError) msg
      update [ SnapshotMeta_importErrorField =. Just stderr ] (AutoKeyField ==. smId)
      traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId
      updateState NodeProcessState_ImportFailed

  removeFileLogging storePath
  case exitCode of
    ExitSuccess -> void $ do
      $(logDebug) $ "importSnapshotData success: stderr: " <> stderr
      let
        prefixStr = "Setting current head to block "
        mBlkHashPrefix = headMay =<< T.words <$> T.stripPrefix prefixStr (snd $ T.breakOn prefixStr stderr)
      case mBlkHashPrefix of
        Nothing -> inDb $ importFailed $ "importSnapshotData failed: could not parse blk blkHash" <> stderr
        Just blkHashPrefix -> void $ do
          hist <- liftIO $ readTVarIO $ _nodeDataSource_history nds
          let
            mBlkHash = completeBlockHash blkHashPrefix hist

          mBlk <- for mBlkHash $ \blkHash -> flip runReaderT nds $ runExceptT @CacheError $ runNodeQueryT $ do
            nodeQueryDataSourceSafe $ NodeQuery_BlockHeader blkHash
          let
            blkDetails :: (# Text | BlockHash | BlockHeader #)
            blkDetails = case either (const Nothing) Just =<< mBlk of
              Just blk -> (# | | blk #)
              Nothing -> case mBlkHash of
                Just blkHash -> (# | blkHash | #)
                Nothing -> (# blkHashPrefix | | #)
          inDb $ do
            updateSnapshotMeta blkDetails smId
            updateState NodeProcessState_ImportComplete
    ExitFailure _ -> inDb $ importFailed $ "importSnapshotData failed: " <> stderr

updateSnapshotMeta
  :: (PersistBackend m, BlockLike blk)
  => (# Text | BlockHash | blk #)
  -> Key SnapshotMeta BackendSpecific
  -> m ()
updateSnapshotMeta blkDetails smId = do
  now <- getTime
  case blkDetails of
    (# hashPrefix | | #) -> update
      [ SnapshotMeta_headBlockPrefixField =. Just hashPrefix
      , SnapshotMeta_importCompleteTimeField =. Just now
      ]
      (AutoKeyField ==. smId)
    (# | blkHash | #) -> update
      [ SnapshotMeta_headBlockField =. Just blkHash
      , SnapshotMeta_importCompleteTimeField =. Just now
      ]
      (AutoKeyField ==. smId)
    (# | | blk #) -> update
      [ SnapshotMeta_headBlockField =. (Just $ blk ^. hash)
      , SnapshotMeta_headBlockLevelField =. (Just $ blk ^. level)
      , SnapshotMeta_headBlockBakeTimeField =. (Just $ blk ^. timestamp)
      , SnapshotMeta_importCompleteTimeField =. Just now
      ]
      (AutoKeyField ==. smId)
  traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId

-- Simple test
-- for_ (Map.keys $ _cachedHistory_blocks hist) $ \blk ->
--   when (Just blk /= (completeBlockHash (T.take 12 $ toBase58Text blk) hist)) $ print $ ("Did not work", blk)
completeBlockHash :: Text -> CachedHistory' -> Maybe BlockHash
completeBlockHash prefix' history = (checkBlockHash =<< fst =<< mHashes)
  <|> (checkBlockHash =<< snd =<< mHashes)
  where
    checkBlockHash blk = if T.isPrefixOf prefix' (toBase58Text blk)
      then Just blk
      else Nothing
    mHashes :: Maybe (Maybe BlockHash, Maybe BlockHash)
    mHashes = (\p -> (fst <$> Map.lookupLE p blks, fst <$> Map.lookupGE p blks)) <$> mPrefix
    blks = _cachedHistory_blocks history
    mPrefix :: Maybe BlockHash
    mPrefix = HashedValue . toShort . BS.drop prefixDropLen <$> decodeBase58 bitcoinAlphabet (T.encodeUtf8 appendedPrefix)
    prefixDropLen = BS.length $ Tezos.Common.Base58Check.prefix (Proxy @'HashType_BlockHash)
    blkHashLength = 51 :: Int
    appendedPrefix = prefix' <> T.replicate (blkHashLength - T.length prefix') "1"

removeFileLogging :: (MonadLogger m, MonadIO m, MonadMask m) => FilePath -> m ()
removeFileLogging f = liftIO (removeFile f) `catch` \(e :: IOException) -> $(logError) $ "Failed to remove file: " <> T.pack f <> ": " <> tshow e
