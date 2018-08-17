{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Backend.Config where

import Control.Lens (Lens', view)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans (lift)
import Database.Groundhog (DbPersist (..), PersistBackend)
import Network.Mail.Mime (Address)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject (..))

newtype AppConfig = AppConfig
  { _appConfig_emailFromAddress :: Address
  }

class HasAppConfig a where
  getAppConfig :: Lens' a AppConfig

instance HasAppConfig AppConfig where
  getAppConfig = id

askAppConfig :: (HasAppConfig a, MonadReader a m) => m AppConfig
askAppConfig = asks $ view getAppConfig
