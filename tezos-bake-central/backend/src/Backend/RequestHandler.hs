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

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Backend.RequestHandler where

import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Web.ClientSession as CS
import Data.Pool (Pool)
import Database.Groundhog.Postgresql
import Focus.Api
import Focus.Backend.App
import Focus.Backend.DB (runDb)
import Focus.Backend.DB.PsqlSimple
import Focus.Backend.Listen
import Focus.Schema
import Control.Monad.Logger (runNoLoggingT)

import Common.App
import Common.Api
import Common.Schema
import Backend.Schema ()

requestHandler
  :: (MonadBaseControl IO m, MonadIO m)
  => CS.Key
  -> Pool Postgresql
  -> RequestHandler Bake m
requestHandler csk db = RequestHandler $ \req -> runNoLoggingT . runDb (Identity db) $
  case req of
    ApiRequest_Public r ->
      case r of
        PublicRequest_AddClient addr -> do
          _ <- insertAndNotify $ Client { _client_address = addr, _client_updated = Nothing }
          return ()
        PublicRequest_RemoveClient addr -> do
          cids <- [queryQ| DELETE FROM "Client" WHERE "address" = ?addr RETURNING id |]
          forM_ cids $ \(Only cid) -> notifyEntityId NotificationType_Delete (cid :: Id Client)
          return ()

    ApiRequest_Private key r ->
      case r of
        PrivateRequest_NoOp -> return ()
