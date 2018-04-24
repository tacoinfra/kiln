{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Backend.RequestHandler where

import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Web.ClientSession as CS
import Data.Pool (Pool)
import Database.Groundhog.Postgresql
import Focus.Backend.App
import Focus.Backend.DB (runDb)
import Control.Monad.Logger (runNoLoggingT)

import Common.App

requestHandler
  :: (MonadBaseControl IO m, MonadIO m)
  => CS.Key
  -> Pool Postgresql
  -> RequestHandler Bake m
requestHandler csk db = RequestHandler $ \req -> runNoLoggingT . runDb (Identity db) $ return undefined