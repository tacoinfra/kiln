{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Process.Alerts
  ( queueFailedProcessAlert
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Groundhog.Postgresql (PersistBackend)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.Serializable (Serializable)

import Backend.Alerts.Common
import Backend.Config (HasAppConfig)

queueFailedProcessAlert
  :: ( PersistBackend m, PostgresLargeObject m, MonadIO m
     , MonadReader a m, HasAppConfig a, MonadLogger m
     , MonadBase Serializable m
     )
  => Text
  -> Text
  -> m ()
queueFailedProcessAlert name errorLog = do
  let alert = mkFailedProcessAlert name errorLog
  queueEmailAlert alert
  queueTelegramAlert alert

mkFailedProcessAlert :: Text -> Text -> Alert
mkFailedProcessAlert name errorLog =
  Alert Unresolved (name <> " failed") $ T.unwords
    [ name
    , " failed during work. Logs:\n"
    , errorLog
    ]
