{-# LANGUAGE FlexibleContexts #-}

module Backend.NotifyHandler where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Pool (Pool)
import Data.Semigroup
import Database.Groundhog.Postgresql
import Focus.Backend.Listen

import Common.App

notifyHandler
  :: (MonadBaseControl IO m, MonadIO m, Monoid a, Semigroup a)
  => Pool Postgresql
  -> NotifyMessage
  -> BakeViewSelector a
  -> m (BakeView a)
notifyHandler db nm vs = do
  return mempty