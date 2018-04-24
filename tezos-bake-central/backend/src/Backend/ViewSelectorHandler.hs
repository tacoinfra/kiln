{-# LANGUAGE FlexibleContexts #-}

module Backend.ViewSelectorHandler where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Pool (Pool)
import Data.Semigroup
import Database.Groundhog.Postgresql
import Focus.Backend.App
import Focus.Backend.DB
import qualified Web.ClientSession as CS
import Control.Monad.Logger (runNoLoggingT)

import Common.App

viewSelectorHandler
  :: (MonadBaseControl IO m, MonadIO m, Monoid a, Semigroup a)
  => CS.Key
  -> Pool Postgresql
  -> QueryHandler (BakeViewSelector a) m
viewSelectorHandler csk db = QueryHandler $ \vs -> runNoLoggingT . runDb (Identity db) $ do
  return mempty