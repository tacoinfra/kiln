
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Upgrade where

import Control.Error hiding (err, isRight)
import Control.Exception.Safe (try)
import Control.Monad
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Logger (MonadLogger, logError, logInfo)
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as Bz
import Data.Pool (Pool)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime)
import qualified Data.Version as V
import Database.Groundhog.Postgresql
import Database.Id.Class
import Database.Id.Groundhog
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (getTime, runDb)
import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.Backend.Logging (LoggingEnv, runLoggingEnv)

import Backend.Alerts
import Backend.Alerts.Common
import Backend.Config (AppConfig(..))
import Backend.Common
import Backend.Schema
import Backend.Version (parseVersion)
import Common.Schema
import Common.Alerts
import ExtraPrelude
import Tezos.Types

upgradeCheckWorker
  :: MonadIO m
  => NamedChain
  -> Maybe Text
  -> Text
  -> NominalDiffTime
  -> LoggingEnv
  -> Http.Manager
  -> Pool Postgresql
  -> AppConfig
  -> m (IO ())
upgradeCheckWorker chain mrelease gitLabProjectId delay logger httpMgr db appConfig = do
  liftIO $ runLoggingEnv logger $ runDb (Identity db) $ clearUnrelatedNetworkUpdateError chain
  workerWithDelay "upgradeCheckWorker" (pure delay) $ const $ runLoggingEnv logger $ do
    $(logInfo) "Checking for newer version"
    notifyChainUpgrade chain mrelease gitLabProjectId httpMgr db appConfig
    void $ updateUpstreamVersion httpMgr (runDb (Identity db))

notifyChainUpgrade
  :: ( MonadIO m, MonadLogger m, MonadBaseNoPureAborts IO m)
  => NamedChain
  -> Maybe Text
  -> Text
  -> Http.Manager
  -> Pool Postgresql
  -> AppConfig
  -> m ()
notifyChainUpgrade namedChain mrelease gitLabProjectId httpMgr db appConfig =
  getTezosReleaseCommit httpMgr gitLabProjectId mrelease >>= \case
    Left err -> $(logError) err -- TODO use proper log message
    Right version -> runDb (Identity db) $ do
      mLastVersion <- getLatestNamedChainUpgradeLog namedChain
      -- Although we are reporting ErrorLogNetworkUpdate, it is currently not used in frontend
      -- the only effect it has is to send an email
      when (preview (_Just . _3) mLastVersion /= Just version) $ reportNew mLastVersion version
  where
    reportNew mLastVersion version = do
      now <- getTime
      forM_ mLastVersion $ \case
        (logId, Nothing, _) -> update [ErrorLog_stoppedField =. Just now] (AutoKeyField `in_` [fromId (logId :: Id ErrorLog)])
        _ -> return ()
      let errorLog = ErrorLog
            { _errorLog_started = now
            , _errorLog_stopped = if isNothing mLastVersion then Just now else Nothing
            , _errorLog_lastSeen = now
            , _errorLog_noticeSentAt = Just now
            , _errorLog_chainId = _appConfig_chainId appConfig
            }
      eid <- toId <$> insert errorLog
      _ <- insert ErrorLogNetworkUpdate
        { _errorLogNetworkUpdate_log = eid
        , _errorLogNetworkUpdate_namedChain = namedChain
        , _errorLogNetworkUpdate_version = version
        , _errorLogNetworkUpdate_gitLabProjectId = gitLabProjectId
        }
      notifyDefault (Id eid :: Id ErrorLogNetworkUpdate)
      -- Only send an email when we get a new value, not when we initially
      -- populate the cache.
      when (isJust mLastVersion) $ do
        let (header, bodyFirstPara) = networkUpdateDescription
        flip runReaderT appConfig $ queueAlert (Just eid) $ Alert Unresolved header $ T.unlines
          -- Need to change this, since the updates don't come on each branch.
          [ bodyFirstPara
          , "Get the new software here  🡒  " <> "https://gitlab.com/tezos/tezos/-/releases"
          ]

getLatestNamedChainUpgradeLog :: (PersistBackend m, PostgresRaw m) => NamedChain -> m (Maybe (Id ErrorLog, Maybe UTCTime, TezosVersion))
getLatestNamedChainUpgradeLog namedChain =
  listToMaybe <$> [queryQ|
    SELECT el.id, el.stopped AT TIME ZONE 'UTC', elua.version
    FROM "ErrorLogNetworkUpdate" elua
    JOIN "ErrorLog" el
    ON elua.log = el.id
    WHERE elua."namedChain" = ?namedChain
    ORDER BY el.started DESC
    LIMIT 1
    |]

updateUpstreamVersion
  :: (MonadIO m, PersistBackend db)
  => Http.Manager
  -> (forall a. db a -> m a)
  -> m ()
updateUpstreamVersion httpMgr inDb =
  inDb . setUpstreamVersion =<< runExceptT (getUpstreamVersion httpMgr)

setUpstreamVersion :: (PersistBackend m) => Either UpgradeCheckError V.Version -> m ()
setUpstreamVersion v = do
  now <- getTime
  existingId' :: Maybe (Id UpstreamVersion) <-
    fmap toId . listToMaybe <$> project AutoKeyField (
      (UpstreamVersion_updatedField ==. UpstreamVersion_updatedField) -- help type inference
      `limitTo` 1)

  case existingId' of
    Nothing -> do
      let
        new = UpstreamVersion
          { _upstreamVersion_error = preview _Left v
          , _upstreamVersion_version = preview _Right v
          , _upstreamVersion_updated = now
          , _upstreamVersion_dismissed = False
          }
      notify NotifyTag_UpstreamVersion . (, new) =<< insert' new
    Just existingId -> do
      updateId existingId
        [ UpstreamVersion_errorField =. preview _Left v
        , UpstreamVersion_versionField =. preview _Right v
        , UpstreamVersion_updatedField =. now
        , UpstreamVersion_dismissedField =. False
        ]
      getId existingId >>= traverse_ (notify NotifyTag_UpstreamVersion . (existingId,))

{- If we don't specify, then just get the latest release, hence the  -}
{- maybe type. -}
getTezosReleaseCommit :: (MonadIO m) => Http.Manager -> Text -> Maybe Text -> m (Either Text TezosVersion)
getTezosReleaseCommit httpMgr projectId mrelease = do
  let url = gitlabApiBaseUrl <> "/projects/" <> projectId <> "/releases"
      getCommit = (^? key "commit" . key "id" . _String)
  resp' :: Either Http.HttpException (Http.Response Bz.ByteString) <- liftIO $ try $
    Http.httpLBS =<< (Http.setRequestManager httpMgr <$> Http.parseRequest (T.unpack url))
  return $ case resp' of
    Left ex -> Left $ T.pack $ show ex
    Right body -> case getRelease mrelease getCommit $ Http.getResponseBody body of
         Nothing -> let msg = "No commit found"
           in Left $ case mrelease of
                Nothing -> msg <> " at latest release."
                Just s -> msg <> " found for this release: " <> s <> "."
         Just commit -> Right $ TezosVersion (Left commit)

gitlabApiBaseUrl :: Text
gitlabApiBaseUrl = "https://gitlab.com/api/v4"

releaseGitLab :: Text
releaseGitLab = gitlabApiBaseUrl <> "/projects/19392551/releases"

getUpstreamVersion :: (MonadError UpgradeCheckError m, MonadIO m) => Http.Manager -> m V.Version
getUpstreamVersion httpMgr = do
  resp' :: Either Http.HttpException (Http.Response Bz.ByteString) <- liftIO $ try $
    Http.httpLBS =<< (Http.setRequestManager httpMgr <$> Http.parseRequest (T.unpack releaseGitLab))
  case resp' of
    Left _ -> throwError UpgradeCheckError_UpstreamUnreachable
    Right resp -> case Http.getResponseStatusCode resp of
      {- the releases are sorted in descending order based on
      "released_at", hence we get the first. -}
      200 -> case Http.getResponseBody resp ^? nth 0 . key "tag_name" . _String >>= parseVersion of
        Nothing -> throwError UpgradeCheckError_UpstreamUnparseable
        Just x -> pure x
      _ -> throwError UpgradeCheckError_UpstreamMissing
