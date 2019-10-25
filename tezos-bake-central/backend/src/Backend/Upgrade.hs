{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Backend.Upgrade where

import Control.Exception.Safe (try)
import Control.Monad
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Logger (MonadLogger, logError, logInfo)
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as Bz
import Data.Maybe
import Data.Pool (Pool)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
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
import Backend.Common (workerWithDelay)
import Backend.Schema
import Backend.Version (parseVersion)
import Common.Schema
import Common.Alerts
import ExtraPrelude
import Tezos.Types

upgradeCheckWorker
  :: MonadIO m
  => Maybe NamedChain
  -> Text
  -> Text
  -> NominalDiffTime
  -> LoggingEnv
  -> Http.Manager
  -> Pool Postgresql
  -> AppConfig
  -> m (IO ())
upgradeCheckWorker mchain gitLabProjectId upgradeBranch delay logger httpMgr db appConfig = do
  liftIO $ forM_ mchain $ runLoggingEnv logger . runDb (Identity db) . clearUnrelatedNetworkUpdateError
  workerWithDelay (pure delay) $ const $ runLoggingEnv logger $ do
    $(logInfo) "Checking for newer version"
    forM_ mchain $ \chain -> notifyChainUpgrade chain gitLabProjectId httpMgr db appConfig
    void $ updateUpstreamVersion upgradeBranch httpMgr (runDb (Identity db))

notifyChainUpgrade
  :: ( MonadIO m, MonadLogger m, MonadBaseNoPureAborts IO m)
  => NamedChain
  -> Text
  -> Http.Manager
  -> Pool Postgresql
  -> AppConfig
  -> m ()
notifyChainUpgrade namedChain gitLabProjectId httpMgr db appConfig =
  getTezosBranch httpMgr gitLabProjectId (showNamedChain namedChain) >>= \case
    Left err -> $(logError) err -- TODO use proper log message
    Right commitId -> runDb (Identity db) $ do
      mLastCommit <- getLatestNamedChainUpgradeLog namedChain
      when (preview (_Just . _3) mLastCommit /= Just commitId) $ do
        now <- getTime
        forM_ mLastCommit $ \case
          (logId, Nothing, _) -> update [ErrorLog_stoppedField =. Just now] (AutoKeyField `in_` [fromId (logId :: Id ErrorLog)])
          _ -> return ()
        let errorLog = ErrorLog
              { _errorLog_started = now
              , _errorLog_stopped = if isNothing mLastCommit then Just now else Nothing
              , _errorLog_lastSeen = now
              , _errorLog_noticeSentAt = Just now
              , _errorLog_chainId = _appConfig_chainId appConfig
              }
        eid <- toId <$> insert errorLog
        _ <- insert ErrorLogNetworkUpdate
          { _errorLogNetworkUpdate_log = eid
          , _errorLogNetworkUpdate_namedChain = namedChain
          , _errorLogNetworkUpdate_commit = commitId
          , _errorLogNetworkUpdate_gitLabProjectId = gitLabProjectId
          }
        notifyDefault (Id eid :: Id ErrorLogNetworkUpdate)
        -- Only send an email when we get a new value, not when we initially
        -- populate the cache.
        when (isJust mLastCommit) $ do
          let (header, bodyFirstPara) = networkUpdateDescription namedChain
          flip runReaderT appConfig $ queueAlert (Just eid) $ Alert Unresolved header $ T.unlines
            [ bodyFirstPara
            , "Get the new software here  🡒  " <> "https://gitlab.com/tezos/tezos/tree/" <> showNamedChain namedChain
            ]

getLatestNamedChainUpgradeLog :: (PersistBackend m, PostgresRaw m) => NamedChain -> m (Maybe (Id ErrorLog, Maybe UTCTime, Text))
getLatestNamedChainUpgradeLog namedChain =
  listToMaybe <$> [queryQ|
    SELECT el.id, el.stopped AT TIME ZONE 'UTC', elua.commit
    FROM "ErrorLogNetworkUpdate" elua
    JOIN "ErrorLog" el
    ON elua.log = el.id
    WHERE elua."namedChain" = ?namedChain
    ORDER BY el.started DESC
    LIMIT 1
    |]

updateUpstreamVersion
  :: (MonadIO m, PersistBackend db)
  => Text
  -> Http.Manager
  -> (forall a. db a -> m a)
  -> m ()
updateUpstreamVersion upgradeBranch httpMgr inDb =
  inDb . setUpstreamVersion =<< runExceptT (getUpstreamVersion upgradeBranch httpMgr)

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

getTezosBranch :: (MonadIO m) => Http.Manager -> Text -> Text -> m (Either Text Text)
getTezosBranch httpMgr projectId branch = do
  let url = gitlabApiBaseUrl <> "/projects/" <> projectId <> "/repository/branches/" <> branch
  resp' :: Either Http.HttpException (Http.Response Bz.ByteString) <- liftIO $ try $ do
    Http.httpLBS =<< (Http.setRequestManager httpMgr <$> Http.parseRequest (T.unpack url))
  return $ case resp' of
    Left ex -> Left $ T.pack $ show ex
    Right body -> case Http.getResponseBody body ^? key "commit" . key "id" . _String of
      Nothing -> Left "No commit found for this branch"
      Just commit -> Right commit

gitlabApiBaseUrl :: Text
gitlabApiBaseUrl = "https://gitlab.com/api/v4"

upstreamGitLab :: Text -> Text
upstreamGitLab branch = gitlabApiBaseUrl <> "/projects/6318296/repository/files/tezos-bake-central%2Fbackend%2Fbackend.cabal/raw?ref=" <> branch

getUpstreamVersion :: (MonadError UpgradeCheckError m, MonadIO m) => Text -> Http.Manager -> m V.Version
getUpstreamVersion upgradeBranch httpMgr = do
  resp' :: Either Http.HttpException (Http.Response Bz.ByteString) <- liftIO $ try $
    Http.httpLBS =<< (Http.setRequestManager httpMgr <$> Http.parseRequest (T.unpack $ upstreamGitLab upgradeBranch))
  case resp' of
    Left _ -> throwError UpgradeCheckError_UpstreamUnreachable
    Right resp -> case Http.getResponseStatusCode resp of
      200 -> case parseCabalFileVersion $ decodeUtf8 $ Bz.toStrict $ Http.getResponseBody resp of
        Nothing -> throwError UpgradeCheckError_UpstreamUnparseable
        Just x -> pure x
      _ -> throwError UpgradeCheckError_UpstreamMissing

parseCabalFileVersion :: Text -> Maybe V.Version
parseCabalFileVersion cabalFile =
  join $ lookup "version" $
    (T.toLower . T.strip *** parseVersion . T.strip) . breakOnNoDelim ':' <$> T.lines cabalFile

-- | Like 'breakOn' but does not keep delimiter.
breakOnNoDelim :: Char -> Text -> (Text, Text)
breakOnNoDelim delim = second (T.drop 1) . T.breakOn (T.singleton delim)
