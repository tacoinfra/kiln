{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}

-- {-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Snapshot where

import qualified Data.LCA.Online.Polymorphic as LCA
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger
import Data.ByteString.Base58
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Pool (Pool)
import Data.Sequence (Seq)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql, in_, isFieldNothing, (&&.), (=.), (==.))
import Rhyolite.Backend.DB (getTime, runDb, selectMap, project1, MonadBaseNoPureAborts)
import Rhyolite.Backend.Logging
import Snap.Core (MonadSnap, route)
import qualified Snap.Core as Snap
import Snap.Util.FileUploads
import System.Directory
import System.Exit (ExitCode(..))
import qualified System.Process as Process
import Unsafe.Coerce

import Tezos.Base58Check
-- (fromBase58, toBase58)
import Tezos.Block (VeryBlockLike (..))
import Tezos.History
import Tezos.Operation (Ballot)
import Tezos.PublicKey
import Tezos.ShortByteString (ShortByteString, fromShort, toShort)
import Tezos.Types

import Backend.CachedNodeRPC
import Backend.Common
import Backend.Config (AppConfig (..), defaultNodeConfigFile, nodeDataDir, BinaryPaths(..))
import Backend.NodeCmd
import Backend.STM (atomicallyWith)
import Backend.Schema
import Backend.Workers.Process
import Common.Schema
import ExtraPrelude

handleSnapshotUpload
  :: AppConfig
  -> NodeDataSource
  -> Pool Postgresql
  -> Either NamedChain a
  -> MVar ()
  -> Snap.Snap ()
handleSnapshotUpload appConfig nds db chain threadMVar = do
  (liftIO $ tryPutMVar threadMVar ()) >>= \case
    False -> runLoggingEnv logger $ do
      $(logWarn) "Upload already in progress, ignoring this request."
    True -> do
      -- We might have snapshot from a killed kiln process, so cleanup
      cleanupDir uploadTmpLocation
      cleanupDir storeLocation
      void $ handleFileUploads uploadTmpLocation uploadPolicy partUploadPolicy uploadHandler
  where
    inDb :: (MonadIO m, MonadBaseNoPureAborts IO m, MonadLogger m) => DbPersist Postgresql m a -> m a
    inDb = runDb (Identity db)
    logger = _nodeDataSource_logger nds
    uploadPolicy = defaultUploadPolicy
    uploadTmpLocation = _appConfig_kilnDataDir appConfig <> "/snapshots_tmp/"
    storeLocation = _appConfig_kilnDataDir appConfig <> "/snapshots/"
    partUploadPolicy _ = allowWithMaximumSize (10*1000*1000*1000) -- 10gb
    withLockRelease m = liftIO $ finally m (tryTakeMVar threadMVar)
    uploadHandler :: PartInfo -> Either PolicyViolationException FilePath -> IO ()
    uploadHandler p = \case
      Left e -> putStrLn $ show e
      Right fp -> runLoggingEnv logger $ do
        $(logWarn) "Upload successful."
        hist <- liftIO $ readTVarIO $ _nodeDataSource_history nds
        now <- liftIO $ getCurrentTime
        let
          fileName = maybe "file" (T.unpack . T.decodeUtf8) $ partFileName p
          storePath = storeLocation <> fileName
          sm = SnapshotMeta (T.pack fileName) (T.pack storePath) now Nothing Nothing Nothing Nothing Nothing
        smId <- inDb $ do
          deleteAll sm
          k <- insert sm
          notify NotifyTag_SnapshotMeta sm
          pure k
        liftIO $ renameFile fp storePath
        liftIO $ forkIO $ withLockRelease $ race_ (importSnapshotData appConfig nds logger db chain sm smId)
          $ runLoggingEnv logger $ do
            -- Wait for 10 hr, then give up
            threadDelay' (60*60*10)
            inDb $ do
              nodePPid <- project1 ( NodeInternal_idField
                                 , NodeInternal_dataField ~> DeletableRow_dataSelector) CondEmpty
              for nodePPid $ \(nid, pid) -> updateProcessState pid (Just (\pd -> (NotifyTag_NodeInternal, (nid, pd))))
                (ProcessState_Node NodeProcessState_ImportTimeout)
            $(logError) "Could not import snapshot: Timeout"
            liftIO $ removeFile storePath
        pure ()

cleanupDir :: (MonadIO m) => FilePath -> m ()
cleanupDir dir = liftIO $ do
  removeDirectoryRecursive dir `catch` (\(e :: IOException) -> pure ())
  createDirectoryIfMissing True dir

importSnapshotData
  :: AppConfig
  -> NodeDataSource
  -> LoggingEnv
  -> Pool Postgresql
  -> Either NamedChain a
  -> SnapshotMeta
  -> Key SnapshotMeta BackendSpecific
  -> IO ()
importSnapshotData appConfig nds logger db chain sm smId = runLoggingEnv logger $ do
  let
    nodePath = either nodePaths (const $ nodePaths NamedChain_Mainnet) chain
    dataDir = nodeDataDir appConfig
    storePath = T.unpack $ _snapshotMeta_storePath sm
    inDb :: (MonadIO m, MonadBaseNoPureAborts IO m, MonadLogger m) => DbPersist Postgresql m a -> m a
    inDb = runDb (Identity db)
  $(logWarn) $ "importSnapshotData : clean old dir "
  cleanupDir dataDir
  nodePPid <- inDb $ project1 ( NodeInternal_idField
                       , NodeInternal_dataField ~> DeletableRow_dataSelector) CondEmpty
  let
    updateState s = for nodePPid $ \(nid, pid) -> updateProcessState pid (Just (\pd -> (NotifyTag_NodeInternal, (nid, pd)))) (ProcessState_Node s)

  inDb $ updateState NodeProcessState_ImportingSnapshot
  $(logWarn) $ "importSnapshotData: starting import "
  (exitCode, stdout, stderr) <- liftIO $ Process.readProcessWithExitCode
    nodePath (["snapshot", "import", storePath, "--data-dir", dataDir]) ""

-- Example output on success
-- stderr:
-- Jul  6 19:39:08 - shell.snapshots: Importing data from snapshot file ./.kiln/snapshots/main.snapshot
-- Jul  6 19:39:08 - shell.snapshots: You may consider using the --block <block_hash> argument to verify that the block imported is the one you expect
-- Jul  6 19:39:08 - shell.snapshots: Retrieving and validating data. This can take a while, please bear with us
-- Jul  6 19:45:44 - shell.snapshots: Setting current head to block BLWxHkBhZfaj
-- Jul  6 19:45:45 - shell.snapshots: Setting history-mode to full
-- Jul  6 19:45:46 - shell.snapshots: Successful import from file ./.kiln/snapshots/main.snapshot

  liftIO $ removeFile storePath
  case exitCode of
    ExitSuccess -> void $ do
      $(logWarn) $ "importSnapshotData success: stderr: \n" <> T.pack stderr
      let mBlkHashPrefix = case lines stderr of
            (_1:_2:_3: settingCurrentHead:_5:_6:_)
              | blkH <- reverse $ take 12 $ reverse settingCurrentHead
              , length blkH == 12
              -> Just $ T.pack blkH
            _ -> Nothing
      $(logWarn) $ ("importSnapshotData: Parsed hash: " <> fromMaybe "nothing" mBlkHashPrefix)
      case mBlkHashPrefix of
        Nothing -> void $ do
          $(logWarn) $ "importSnapshotData failed: could not parse blk hash" <> T.pack stderr
          inDb $ do
            update [ SnapshotMeta_importErrorField =. Just (T.pack stderr) ] (AutoKeyField ==. smId)
            traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId
            updateState NodeProcessState_ImportFailed
        Just blkHashPrefix -> void $ do
          hist <- liftIO $ readTVarIO $ _nodeDataSource_history nds
          let
            blkHash = completeBlockHash blkHashPrefix hist

          mBlk <- for blkHash $ \hash -> flip runReaderT nds $ runExceptT @CacheError $ runNodeQueryT $ do
            header <- nodeQueryDataSourceSafe $ NodeQuery_BlockHeader hash
            pure $ mkVeryBlockLike $ (hash, header)
          let
            blkDetails :: Either Text (Either BlockHash VeryBlockLike)
            blkDetails = maybe (maybe (Left blkHashPrefix) (Right . Left) blkHash) (Right . Right)
                  (join $ either (const Nothing) Just <$> mBlk)
          inDb $ do
            updateSnapshotMeta blkDetails smId
            for nodePPid $ \(nid,_) -> updateNodeDetails blkDetails nid
            updateState NodeProcessState_ImportComplete

    ExitFailure _ -> void $ do
      $(logWarn) $ "importSnapshotData failed: " <> T.pack stderr
      inDb $ do
        update [ SnapshotMeta_importErrorField =. Just (T.pack stderr) ] (AutoKeyField ==. smId)
        traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId
        updateState NodeProcessState_ImportFailed

updateSnapshotMeta
  :: (Either Text (Either BlockHash VeryBlockLike))
  -> Key SnapshotMeta BackendSpecific
  -> DbPersist Postgresql (LoggingT IO) ()
updateSnapshotMeta blkDetails smId = do
  now <- getTime
  case blkDetails of
    Left hashPrefix -> update
      [ SnapshotMeta_headBlockPrefixField =. Just hashPrefix ]
      (AutoKeyField ==. smId)
    Right (Left hash) -> update
      [ SnapshotMeta_headBlockField =. Just hash ]
      (AutoKeyField ==. smId)
    Right (Right blk) -> update
      [ SnapshotMeta_headBlockField =. (Just $ blk ^. hash)
      , SnapshotMeta_headBlockLevelField =. (Just $ blk ^. level)
      , SnapshotMeta_headBlockBakeTimeField =. (Just $ blk ^. timestamp)
      ]
      (AutoKeyField ==. smId)
  traverse_ (notify NotifyTag_SnapshotMeta) =<< get smId

updateNodeDetails
  :: (Either Text (Either BlockHash VeryBlockLike))
  -> Id Node
  -> DbPersist Postgresql (LoggingT IO) ()
updateNodeDetails blkDetails nodeId = do
  let p = (NodeDetails_dataField ~>)
  now <- getTime
  case blkDetails of
    Left _hashPrefix -> pure ()
    Right (Left hash) ->
      project NodeDetails_idField (NodeDetails_idField `in_` [nodeId]) >>= \case
        [] -> insert $ NodeDetails
          { _nodeDetails_id = nodeId
          , _nodeDetails_data = mkNodeDetails
            { _nodeDetailsData_headBlockHash = Just hash
            , _nodeDetailsData_updated = Just now
            }
          }
        (_:_) -> update
          [ p NodeDetailsData_headBlockHashSelector =. Just hash
          , p NodeDetailsData_updatedSelector =. Just now
          ]
          (NodeDetails_idField `in_` [nodeId])
    Right (Right headBlockInfo) -> do
      project NodeDetails_idField (NodeDetails_idField `in_` [nodeId]) >>= \case
        [] -> insert $ NodeDetails
          { _nodeDetails_id = nodeId
          , _nodeDetails_data = mkNodeDetails
            { _nodeDetailsData_headLevel = Just (headBlockInfo ^. level)
            , _nodeDetailsData_headBlockHash = Just (headBlockInfo ^. hash)
            , _nodeDetailsData_headBlockBakedAt = Just (headBlockInfo ^. timestamp)
            , _nodeDetailsData_fitness = Just (headBlockInfo ^. fitness)
            , _nodeDetailsData_updated = Just now
            , _nodeDetailsData_headBlockPred = Just (headBlockInfo ^. predecessor)
            }
          }
        (_:_) -> update
          [ p NodeDetailsData_headLevelSelector =. Just (headBlockInfo ^. level)
          , p NodeDetailsData_headBlockHashSelector =. Just (headBlockInfo ^. hash)
          , p NodeDetailsData_headBlockBakedAtSelector =. Just (headBlockInfo ^. timestamp)
          , p NodeDetailsData_fitnessSelector =. Just (headBlockInfo ^. fitness)
          , p NodeDetailsData_updatedSelector =. Just now
          , p NodeDetailsData_headBlockPredSelector =. Just (headBlockInfo ^. predecessor)
          ]
          (NodeDetails_idField `in_` [nodeId])
  newNodeDetails <- project NodeDetails_dataField $ (NodeDetails_idField ==. nodeId) `limitTo` 1
  traverse_ (notify NotifyTag_NodeDetails . (nodeId,) . Just) newNodeDetails

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
    mHashes = (\p -> (getHash =<< Map.lookupLE p blks, getHash =<< Map.lookupGE p blks)) <$> prefix
    getHash = preview (_Just . _1) . LCA.uncons . snd
    blks :: Map ShortByteString (LCA.Path BlockHash ())
    blks = unsafeCoerce $ _cachedHistory_blocks history
    prefix :: Maybe ShortByteString
    prefix = toShort . BS.drop 2 <$> (decodeBase58 bitcoinAlphabet $ T.encodeUtf8 appendedPrefix)
    appendedPrefix = prefix' <> (T.replicate (51 - (T.length prefix')) "1")
