{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.NotifyHandler where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Control
import Data.Aeson
import qualified Data.AppendMap as Map
import Data.Functor.Identity
import Data.Maybe
import Data.Pool (Pool)
import Data.Semigroup
import Database.Groundhog.Postgresql
import Focus.Backend.DB (runDb)
import Focus.Backend.Listen
import Focus.Backend.Schema.TH
import Focus.Schema

import Backend.Schema
import Common.App
import Common.Schema

notifyHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a, Semigroup a)
  => Pool Postgresql
  -> NotifyMessage
  -> BakeViewSelector a
  -> m (BakeView a)
notifyHandler db notifyMessage aggVS = runNoLoggingT . runDb (Identity db) $ do
  let handleClient = case fromJSON (_notifyMessage_value notifyMessage) of
        Success cid -> do
          client <- get $ fromId (cid :: Id Client)
          infos <- select (ClientInfo_clientField ==. cid)
          return $ case liftA2 (,) client (_bakeViewSelector_clients aggVS) of
            Nothing -> (mempty :: BakeView a)
              { _bakeView_clients = Map.singleton cid Map.empty
              }
            Just (c, a) -> (mempty :: BakeView a)
              { _bakeView_clients = Map.singleton cid (Map.singleton (_client_address c, listToMaybe infos) a)
              }
        Error e -> parseErr notifyMessage e
  case _notifyMessage_entityName notifyMessage of
    "Client" -> handleClient
    _ -> do
      liftIO . putStrLn $ "Unhandled NotifyMessage: " <> show notifyMessage
      return mempty

parseErr :: (MonadIO m, Show nm, Show err, Monoid r) => nm -> err -> m r
parseErr nm err = do
  liftIO . putStrLn $ "Unable to parse NotifyMessage: " <> show nm <> ": " <> show err
  return mempty