{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Backend.ViewSelectorHandler where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.AppendMap as Map
import Data.Functor.Identity (Identity (..))
import Data.Pool (Pool)
import Data.Semigroup (First (..), Semigroup)
import Database.Groundhog.Postgresql
import Rhyolite.Backend.App (QueryHandler (..))
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.DB.PsqlSimple (In (..), queryQ)
import Rhyolite.Backend.Schema (toId)
import qualified Web.ClientSession as CS

import Backend.BalanceTracking
import Backend.Graphs
import Backend.Schema ()
import Common.App
import Common.Schema

whenJust :: (Monad m, Monoid a) => Maybe t -> (t -> m a) -> m a
whenJust Nothing f = return mempty
whenJust (Just x) f = f x

viewSelectorHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a, Semigroup a, Show a)
  => CS.Key
  -> Pool Postgresql
  -> QueryHandler (BakeViewSelector a) m
viewSelectorHandler csk db = QueryHandler $ \vs -> runNoLoggingT . runDb (Identity db) $ do
  clientAddresses <- whenJust (_bakeViewSelector_clientAddresses vs) $ \a -> do
    rs <- [queryQ| SELECT c.id, c.address FROM "Client" c |]
    return $ Map.fromList [(cid, (First (Just addr), a)) | (cid, addr) <- rs]
  clients <- do
    let selClients = In (Map.keys (_bakeViewSelector_clients vs))
    rs <- [queryQ| SELECT c.id, i.report, i.config, i.balance, i.node
                   FROM "Client" c LEFT JOIN "ClientInfo" i ON c.id = i.client
                   WHERE c.id IN ?selClients |]
    let clientInfo = Map.fromList $ do
          (cid, report, config, balance, node) <- rs
          return (cid, First (ClientInfo cid <$> report <*> config <*> balance <*> node))
    return (Map.intersectionWith (,) clientInfo (_bakeViewSelector_clients vs))
  parameters <- whenJust (_bakeViewSelector_parameters vs) $ \a -> do
    rs <- selectAll
    return $ Map.fromList [(nid, (First (Just info), a)) | (_, Parameters nid info) <- rs]
  nodes <- whenJust (_bakeViewSelector_nodes vs) $ \a -> do
    rs <- selectAll
    return $ Map.fromList [(toId nid, (First (Just n), a)) | (nid, n) <- rs]
  notificatees <- whenJust (_bakeViewSelector_notificatees vs) $ \a -> do
    rs <- selectAll
    return $ Map.fromList [(toId nid, (First (Just (_notificatee_email n)), a)) | (nid, n) <- rs]
  mailServers <- whenJust (_bakeViewSelector_mailServers vs) $ \a -> do
    rs <- selectAll
    return $ Map.fromList [(toId nid, (First (Just $ mailServerConfigToView n), a)) | (nid, n) <- rs ]
  maxLevel <- getMaxLevel
  summaryGraph <- case (_bakeViewSelector_summary vs, maxLevel) of
    (Just a, Just l) -> do
      rewards <- getAllRewards a
      mGraph <- liftIO $ cumulativeRewardsGraph (fromIntegral l) (fmap (getFirst . fst) rewards)
      return $ single mGraph a
    _ -> return mempty
  summary <- case _bakeViewSelector_summary vs of
    Nothing -> return mempty
    Just a -> do
      report <- getSummaryReport
      return $ single report a
  return $ (mempty :: BakeView a)
      { _bakeView_clients = clients
      , _bakeView_clientAddresses = clientAddresses
      , _bakeView_parameters = parameters
      , _bakeView_nodes = nodes
      , _bakeView_notificatees = notificatees
      , _bakeView_mailServers = mailServers
      , _bakeView_summaryGraph = summaryGraph
      , _bakeView_summary = summary
      }
