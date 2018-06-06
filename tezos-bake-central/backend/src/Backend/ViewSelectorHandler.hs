{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Backend.ViewSelectorHandler where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Pool (Pool)
import Data.Semigroup
import Database.Groundhog.Postgresql
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import qualified Web.ClientSession as CS
import Control.Monad.Logger (runNoLoggingT)
import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.Backend.Schema
import qualified Data.AppendMap as Map

import Backend.BalanceTracking
import Common.App
import Common.Schema

whenJust :: (Monad m, Monoid a) => Maybe t -> (t -> m a) -> m a
whenJust Nothing f = return mempty
whenJust (Just x) f = f x

viewSelectorHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a, Semigroup a)
  => CS.Key
  -> Pool Postgresql
  -> QueryHandler (BakeViewSelector a) m
viewSelectorHandler csk db = QueryHandler $ \vs -> runNoLoggingT . runDb (Identity db) $ do
  liftIO $ print $ void vs
  clients <- case _bakeViewSelector_clients vs of
    Nothing -> return mempty
    Just a -> do
      rs <- [queryQ| SELECT c.id, c.address, i.report, i.config, i.balance FROM "Client" c LEFT JOIN "ClientInfo" i ON c.id = i.client |]
      return (mempty :: BakeView a)
        { _bakeView_clients = Map.fromList $ do
            (cid, address, report, config, balance) <- rs
            return (cid, (First (Just (address, ClientInfo cid <$> report <*> config <*> balance)), a))
        }
  parameters <- whenJust (_bakeViewSelector_parameters vs) $ \a -> do
    rs <- selectAll
    return (mempty :: BakeView a)
      { _bakeView_parameters = Map.fromList [(nid, (First (Just info), a)) | (_, Parameters nid info) <- rs]
      }
  level <- whenJust (_bakeViewSelector_level vs) $ \a -> do
    rs <- selectAll
    return (mempty :: BakeView a)
      { _bakeView_level = Map.fromList [(toId nid, (First (_node_headLevel n), a)) | (nid, n) <- rs]
      }
  rewards <- whenJust (_bakeViewSelector_clients vs) $ \a -> do
    rewardMap <- getAllRewards a
    return (mempty :: BakeView a)
      { _bakeView_rewards = rewardMap
      }
  notificatees <- whenJust (_bakeViewSelector_notificatees vs) $ \a -> do
    rs <- selectAll
    return (mempty :: BakeView a)
      { _bakeView_notificatees = Map.fromList [(toId nid, (First (Just (_notificatee_email n)), a)) | (nid, n) <- rs]
      }

  return $ clients <> parameters <> level <> rewards <> notificatees
