{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Backend.Config where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Network.Mail.Mime (Address)

import Control.Monad.Trans (lift)
import Database.Groundhog (DbPersist (..), PersistBackend)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject (..))

newtype AppConfig = AppConfig {
  _appConfig_emailFromAddress :: Address
  }

type HasAppConfig m = MonadReader AppConfig m

getAppConfig :: HasAppConfig m => m AppConfig
getAppConfig = ask
