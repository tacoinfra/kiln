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
