{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Upgrade where

import Control.Arrow ((***))
import Control.Exception.Safe (try)
import Control.Monad (join)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first, second)
import qualified Data.ByteString.Lazy as Bz
import Data.Functor (void)
import Data.Functor.Identity (Identity (..))
import Data.Maybe (listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (NominalDiffTime)
import Data.Typeable (Typeable)
import qualified Data.Version as V
import Database.Groundhog.Postgresql (PersistBackend, Postgresql)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Https
import qualified Network.HTTP.Simple as Http
import Rhyolite.Backend.DB (RunDb, getTime, openDb, runDb, selectMap)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Say (say)

import Backend.Common (workerWithDelay)
import Backend.Config (AppConfig)
import Backend.Errors (clearUpgradeNotice, reportUpgradeNotice)
import Backend.Version (parseVersion, version)
import Common.Schema (UpgradeCheckError (..))

upgradeCheckWorker
  :: MonadIO m
  => Text
  -> NominalDiffTime
  -> AppConfig
  -> Http.Manager
  -> Pool Postgresql
  -> m (IO ())
upgradeCheckWorker upgradeBranch delay appConfig httpMgr db = workerWithDelay (pure delay) $ const $ do
    say "Checking for upgrades"
    void $ checkForUpgrade upgradeBranch httpMgr appConfig (runNoLoggingT . runDb (Identity db))

checkForUpgrade
  :: (MonadIO m, PersistBackend trx, PostgresLargeObject trx, MonadIO trx)
  => Text
  -> Http.Manager
  -> AppConfig
  -> (forall a. trx a -> m a)
  -> m (Either UpgradeCheckError V.Version)
checkForUpgrade upgradeBranch httpMgr appConfig withTransaction = do
  result <- runExceptT (getUpstreamVersion upgradeBranch httpMgr)
  withTransaction $ flip runReaderT appConfig $ case result of
    e@(Left _) -> reportUpgradeNotice e
    Right upstreamVersion -> if upstreamVersion > version then
        reportUpgradeNotice (Right upstreamVersion)
      else
        clearUpgradeNotice
  pure result


upstreamGitLab :: Text -> Text
upstreamGitLab branch = "https://gitlab.com/api/v4/projects/6318296/repository/files/tezos-bake-central%2Fbackend%2Fbackend.cabal/raw?ref=" <> branch

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
