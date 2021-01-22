{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Workers.TezosRelease where

import Control.Lens
import Control.Error
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Logger (logDebug)
import Data.Aeson (Value(..))
import Data.Aeson.Lens
import Data.Attoparsec.Text hiding (try)
import qualified Data.ByteString.Lazy as LB
import Data.Foldable
import Data.Ord
import Data.Pool (Pool)
import Data.Time (NominalDiffTime)
import Control.Exception.Safe (try)
import Data.Text (Text)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Backend.DB (runDb)
import qualified Data.Text as T
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http

import Backend.Common.Worker
import Backend.CachedNodeRPC

import Backend.Schema
import Common.Schema

{- TODO: A little bit of randomness could be added to the delay? -}
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
    releaseResp :: Either Http.HttpException (Http.Response LB.ByteString) <-
        liftIO $ try $ Http.httpLBS =<< (Http.setRequestManager httpMgr <$> Http.parseRequest (T.unpack releaseLink))
    return $ case releaseResp of
         Left _ -> Nothing
         Right body -> getRelease mrelease getReleaseTag (Http.getResponseBody body)
  where
    releaseLink = "https://gitlab.com/api/v4/projects/" <> projId <> "/releases"

getRelease :: AsValue s => Maybe Text -> (Value -> Maybe c) -> s -> Maybe c
getRelease mr f = case mr of
   Nothing ->
       maximumByOf values (comparing $ (^? key "tag_name" . _String) >=> hush . parseMajorMinorVersion) >=> f
   Just release ->
       findOf values ((== Just release) . (^? key "tag_name" . _String)) >=> f

getReleaseTag :: Value -> Maybe MajorMinorVersion
getReleaseTag = (^? key "tag_name" . _String) >=> hush . parseMajorMinorVersion

parseMajorMinorVersion :: Text -> Either String MajorMinorVersion
parseMajorMinorVersion = parseOnly $ do
    option () (skip (== 'v'))
    major <- decimal
    skip (== '.')
    minor <- decimal
    extra <- option Nothing $ do
      skip (== '.')
      Just <$> decimal
    endOfInput
    return $ MajorMinorVersion major minor extra Release
