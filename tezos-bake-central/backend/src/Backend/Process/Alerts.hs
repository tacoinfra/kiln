{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Process.Alerts
  ( sendFailedProcessAlert
  , sendProcessRestartedAlert
  ) where

import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Reader (MonadReader, runReaderT)
import Data.Text (Text)
import qualified Data.Text as T

import Backend.Alerts.Common
import Backend.Config (HasAppConfig, askAppConfig)
import Backend.Env (runTransaction)
import Backend.NodeRPC (HasNodeDataSource)

sendAlert
  :: ( MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => Alert
  -> m ()
sendAlert alert = do
  appConfig <- askAppConfig
  runTransaction $ flip runReaderT appConfig $ do
    queueEmailAlert alert
    queueTelegramAlert alert

sendFailedProcessAlert
  :: ( MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => Text
  -> Text
  -> m ()
sendFailedProcessAlert name errorLog = sendAlert $ mkFailedProcessAlert name errorLog

sendProcessRestartedAlert
  :: ( MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => Text
  -> m ()
sendProcessRestartedAlert name = sendAlert $ mkRestartedProcessAlert name

mkFailedProcessAlert :: Text -> Text -> Alert
mkFailedProcessAlert name errorLog =
  Alert Unresolved (name <> " failed") $ T.unwords
    [ name
    , " failed during work. Logs:\n"
    , errorLog
    ]

mkRestartedProcessAlert :: Text -> Alert
mkRestartedProcessAlert name =
  Alert Resolved ("Resolved: " <> name <> " has been automatically restarted by Kiln") ""
