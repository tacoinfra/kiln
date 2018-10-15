{-# LANGUAGE RankNTypes #-}

module Backend.Alerts.Common where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Foldable (for_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Database.Groundhog.Core (select, Cond (CondEmpty))
import Database.Groundhog.Postgresql (PersistBackend)
import Network.Mail.Mime (Address (..), Mail, simpleMail')
import Reflex.Dom.Core (DomBuilder, renderStatic)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.EmailWorker (queueEmail)

import Backend.Config (AppConfig (..), HasAppConfig, askAppConfig)
import Backend.Schema
import Common.Schema

data Alert = Alert
  { _alert_subject :: !Text
  , _alert_content :: !(forall t m. DomBuilder t m => m ())
  }

queueAlert
  :: ( PersistBackend m, PostgresLargeObject m, MonadIO m
     , MonadReader a m, HasAppConfig a
     )
  => Alert -> m ()
queueAlert = queueEmailAlert

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
