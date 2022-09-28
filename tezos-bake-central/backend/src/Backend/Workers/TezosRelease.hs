{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Workers.TezosRelease where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Logger (logDebug)
import Data.Foldable
import Data.Pool (Pool)
import Data.Time (NominalDiffTime)
import Data.Text (Text)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Backend.DB (runDb)
import qualified Data.Text as T
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http

import Backend.Common
import Backend.Http (doRequestLBS)
import Backend.NodeRPC

import Backend.Schema
import Common.Schema

latestTezosReleaseWorker
  :: NominalDiffTime
  -> Text
  -> Maybe Text
  -> NodeDataSource
  -> Pool Postgresql
  -> IO (IO ())
latestTezosReleaseWorker delay projId mRelease nds db =
    workerWithDelay "latestTezosReleaseWorker" (pure delay) $ const $ runLoggingEnv (_nodeDataSource_logger nds) $  runDb (Identity db) $ do
      $(logDebug) $ fold
          [ "Discovering version of the latest tezos release"
          , " for projectId: "
          , projId]
      let httpMgr  = _nodeDataSource_httpMgr nds
      latestRelease <- getLatestTezosRelease httpMgr projId mRelease
      notify NotifyTag_LatestTezosRelease latestRelease

getLatestTezosRelease
  :: forall m. (MonadIO m)
  => Http.Manager
  -> Text
  -> Maybe Text
  -> m (Maybe MajorMinorVersion)
getLatestTezosRelease httpMgr projId mrelease = do
    releaseResp <- doRequestLBS httpMgr (T.unpack releaseLink)
    return $ case releaseResp of
         Left _ -> Nothing
         Right body -> getRelease mrelease getReleaseTag (Http.getResponseBody body)
  where
    releaseLink = "https://gitlab.com/api/v4/projects/" <> projId <> "/releases"
