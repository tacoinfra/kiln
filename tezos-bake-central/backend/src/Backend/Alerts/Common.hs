{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Backend.Alerts.Common where

import Control.Monad.Logger (MonadLogger, logInfoS, logErrorS)
import qualified Data.Text.Lazy as TL
import Database.Groundhog.Core (Cond (CondEmpty), select)
import Database.Groundhog.Postgresql (PersistBackend)
import Network.Mail.Mime (Address (..), simpleMail')
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, executeQ)
import Rhyolite.Backend.EmailWorker (queueEmail)

import Backend.Config (AppConfig (..), HasAppConfig, askAppConfig)
import Backend.Schema ()
import Common.Schema
import ExtraPrelude

data Alert = Alert
  { _is_recovery :: !Bool
  , _alert_subject :: !Text
  , _alert_content :: !Text
  }

recoveryAlert = Alert True
newAlert = Alert False

queueAlert
  :: ( PersistBackend m, PostgresLargeObject m, MonadIO m
     , MonadReader a m, HasAppConfig a, MonadLogger m
     )
  => Alert -> m ()
queueAlert alert = do
  queueEmailAlert alert
  queueTelegramAlert alert
  let logger = if _is_recovery alert then $(logInfoS) else $(logErrorS)
  logger "Kiln" (_alert_subject alert <> ": " <> _alert_content alert)

queueTelegramAlert
  :: (PersistBackend m, PostgresRaw m)
  => Alert -> m ()
queueTelegramAlert alert = do
  let message = _alert_subject alert <> "\n\n" <> _alert_content alert
  void [executeQ|
    INSERT INTO "TelegramMessageQueue" (recipient, message, created)
    SELECT tr.id recipient, ?message message, NOW() created
    FROM "TelegramRecipient" tr
    JOIN "TelegramConfig" tc ON tc.id = tr.config
    WHERE tc.enabled AND NOT tr.deleted
  |]


queueEmailAlert
  :: ( PersistBackend m, PostgresLargeObject m, MonadIO m
     , MonadReader a m, HasAppConfig a
     )
  => Alert -> m ()
queueEmailAlert message = do
  recipients <- select CondEmpty
  fromAddr <- _appConfig_emailFromAddress <$> askAppConfig
  for_ recipients $ \n -> do
    let mail = simpleMail'
          (Address Nothing $ _notificatee_email n) -- to
          fromAddr                                 -- from
          (_alert_subject message)                 -- subject
          (TL.fromStrict $ _alert_content message)
    queueEmail mail Nothing
