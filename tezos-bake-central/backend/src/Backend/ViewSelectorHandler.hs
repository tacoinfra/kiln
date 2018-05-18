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
import Focus.Backend.App
import Focus.Backend.DB
import qualified Web.ClientSession as CS
import Control.Monad.Logger (runNoLoggingT)
import Focus.Backend.DB.PsqlSimple
import Focus.Backend.Schema.TH
import qualified Data.AppendMap as Map

import Backend.BalanceTracking
import Backend.Schema ()
import Common.App
import Common.Schema

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
  parameters <- case _bakeViewSelector_parameters vs of
    Nothing -> return mempty
    Just a -> do
      rs <- selectAll
      return (mempty :: BakeView a)
        { _bakeView_parameters = Map.fromList [(nid, (First (Just info), a)) | (_, Parameters nid info) <- rs]
        }
  level <- case _bakeViewSelector_level vs of
    Nothing -> return mempty
    Just a -> do
      rs <- selectAll
      return (mempty :: BakeView a)
        { _bakeView_level = Map.fromList [(toId nid, (First (_node_headLevel n), a)) | (nid, n) <- rs]
        }
  rewards <- case _bakeViewSelector_clients vs of
    Nothing -> return mempty
    Just a -> do
      rewardMap <- getAllRewards a
      return (mempty :: BakeView a)
        { _bakeView_rewards = rewardMap
        }
  return $ clients <> parameters <> level <> rewards
