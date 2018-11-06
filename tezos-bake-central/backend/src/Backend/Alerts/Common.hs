{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Backend.Alerts.Common where

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Database.Groundhog.Core (Cond (CondEmpty), select)
import Database.Groundhog.Postgresql (PersistBackend)
import Network.Mail.Mime (Address (..), simpleMail')
import Reflex.Dom.Core (DomBuilder, renderStatic)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, executeQ)
import Rhyolite.Backend.EmailWorker (queueEmail)

import Backend.Config (AppConfig (..), HasAppConfig, askAppConfig)
import Backend.Schema ()
import Common.Schema
import ExtraPrelude

data Alert = Alert
  { _alert_subject :: !Text
  , _alert_content :: !(forall t m. DomBuilder t m => m ())
  }

queueAlert
  :: ( PersistBackend m, PostgresLargeObject m, MonadIO m
     , MonadReader a m, HasAppConfig a
     )
  => Alert -> m ()
queueAlert alert = do
  queueEmailAlert alert
  queueTelegramAlert alert

queueTelegramAlert
  :: (PersistBackend m, PostgresRaw m, MonadIO m)
  => Alert -> m ()
queueTelegramAlert alert = do
  body <- fmap (T.decodeUtf8 . snd) $ liftIO $ renderStatic $ _alert_content alert
  let message = _alert_subject alert <> "\n\n" <> body
  _ <- [executeQ|
    INSERT INTO "TelegramMessageQueue" (recipient, message, created)
    SELECT tr.id recipient, ?message message, NOW() created
    FROM "TelegramRecipient" tr
    JOIN "TelegramConfig" tc ON tc.id = tr.config
    WHERE tc.enabled AND NOT tr.deleted
  |]
  pure ()


queueEmailAlert
  :: ( PersistBackend m, PostgresLargeObject m, MonadIO m
     , MonadReader a m, HasAppConfig a
     )
  => Alert -> m ()
queueEmailAlert message = do
  recipients <- select CondEmpty
  fromAddr <- _appConfig_emailFromAddress <$> askAppConfig
  content <- fmap (TL.fromStrict . T.decodeUtf8 . snd) $ liftIO $ renderStatic $ _alert_content message
  for_ recipients $ \n -> do
    let mail = simpleMail'
          (Address Nothing $ _notificatee_email n) -- to
          fromAddr                                 -- from
          (_alert_subject message)                 -- subject
          content
    queueEmail mail Nothing
