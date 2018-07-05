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
import Data.Maybe (listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup (First (..), Semigroup)
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple as Pg
import Rhyolite.App (single)
import Rhyolite.Backend.App (QueryHandler (..))
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.DB.PsqlSimple (In (..), queryQ)
import Rhyolite.Backend.Schema (toId)
import Rhyolite.Schema (Id)
import Data.Word

import Backend.BalanceTracking
import Backend.Graphs
import Backend.Schema ()
import Common (whenJust)
import Common.App
import Common.PublicKeyHash
import Common.Schema
import Common.Json (TezosWord64 (..))
import Common.Tez

viewSelectorHandler
  :: forall m a. (MonadBaseControl IO m, MonadIO m, Monoid a, Semigroup a, Show a)
  => Pool Postgresql
  -> QueryHandler (BakeViewSelector a) m
viewSelectorHandler db = QueryHandler $ \vs -> runNoLoggingT . runDb (Identity db) $ do
  clientAddresses <- whenJust (_bakeViewSelector_clientAddresses vs) $ \a -> do
    rs <- [queryQ| SELECT c.id, c.address FROM "Client" c |]
    return $ Map.fromList [(cid, (First (Just addr), a)) | (cid, addr) <- rs]
  clients <- do
    let selClients = In (Map.keys (_bakeViewSelector_clients vs))
    rs <- [queryQ| SELECT c.id, i.report, i.config
                   FROM "Client" c LEFT JOIN "ClientInfo" i ON c.id = i.client
                   WHERE c.id IN ?selClients |]
    let clientInfo = Map.fromList $ do
          (cid, report, config) <- rs
          return (cid, First (ClientInfo cid <$> report <*> config))
    return $ Map.intersectionWith (,) clientInfo (_bakeViewSelector_clients vs)
  parameters <- whenJust (_bakeViewSelector_parameters vs) $ \a -> do
    param :: Maybe Parameters <- fmap listToMaybe $ select $ CondEmpty `limitTo` 1
    return $ single (_parameters_protoInfo <$> param) a
  nodeAddresses <- whenJust (_bakeViewSelector_nodeAddresses vs) $ \a -> do
    rs <- [queryQ| SELECT n.id, n.address from "Node" n |]
    return $ Map.fromList [(nid, (First (Just n), a)) | (nid, n) <- rs]
  nodes <- do
    let selNodes = In $ Map.keys (_bakeViewSelector_nodes vs)
    rs <- [queryQ|
      SELECT n.id
        , n.address, n.identity, n."headLevel", n."headBlockHash", n."peerCount"
        , n."networkStat#totalSent" , n."networkStat#totalRecv" , n."networkStat#currentInflow" , n."networkStat#currentOutflow"
        , n."fitness"
      FROM "Node" n
      WHERE n.id IN ?selNodes |]
    let nodeInfo = Map.fromList $ do
          (nid, addr, ident) Pg.:. (headLevel, headBlockHash) Pg.:. (peerCount, totalSent, totalRecv, currentInflow, currentOutflow, fitness) <- rs
          return (nid, First $ Just (Node addr ident headLevel headBlockHash peerCount (NetworkStat totalSent totalRecv currentInflow currentOutflow) fitness))
    return (Map.intersectionWith (,) nodeInfo (_bakeViewSelector_nodes vs))
  delegates <- whenJust (_bakeViewSelector_delegates vs) $ \a -> do
    pkhs <- [queryQ| SELECT d."publicKeyHash" FROM "Delegate" d |]
    return $ Map.fromList $ (\(Pg.Only x) -> (x, a)) <$> pkhs
  delegateStats <- do
    let inKeys = In $ Map.keys (_bakeViewSelector_delegateStats vs)
    rs :: [(PublicKeyHash, Maybe (Id Delegate), Maybe Word64, Maybe Word64, Maybe Tez, Maybe Bool, Maybe Bool, Maybe PublicKeyHash, Maybe TezosWord64)]
      <- [queryQ|
          SELECT d."publicKeyHash"
            ,ds."delegate"
            ,ds."efficiency#bakedBlocks"
            ,ds."efficiency#bakingRights"
            ,ds."accountBalance"
            ,ds."accountSpendable"
            ,ds."accountSetable"
            ,ds."accountValue"
            ,ds."accountCounter"
          FROM "Delegate" d
          LEFT OUTER JOIN "DelegateStats" ds
            ON d."id" = ds."delegate"
          WHERE d."publicKeyHash"
          IN ?inKeys|]

    let
      toRsMap
        :: (PublicKeyHash, Maybe (Id Delegate), Maybe Word64, Maybe Word64, Maybe Tez, Maybe Bool, Maybe Bool, Maybe PublicKeyHash, Maybe TezosWord64)
        -> (PublicKeyHash, Maybe (BakeEfficiency, Account))
      toRsMap (publicKeyHash, dId, bakedBlocks, bakingRights, accountBalance, accountSpendable, accountSetable, accountValue, accountCounter) = (publicKeyHash, unDelegateStats publicKeyHash =<< delegateStats)
        where
          efficiency = BakeEfficiency <$> bakedBlocks <*> bakingRights
          delegateStats :: Maybe DelegateStats
          delegateStats = DelegateStats
              <$> dId
              <*> efficiency
              <*> pure accountBalance
              <*> pure accountSpendable
              <*> pure accountSetable
              <*> pure accountValue
              <*> pure accountCounter
    let rsMap = Map.fromList $ toRsMap <$> rs
    return $ Map.intersectionWith (,) (First <$> rsMap) (_bakeViewSelector_delegateStats vs)

  notificatees <- whenJust (_bakeViewSelector_notificatees vs) $ \a -> do
    rs <- selectAll
    return $ Map.fromList [(toId nid, (First (Just (_notificatee_email n)), a)) | (nid, n) <- rs]
  mailServer <- whenJust (_bakeViewSelector_mailServer vs) $ \a -> do
    ms <- fmap listToMaybe $ select $ CondEmpty `limitTo` 1
    return $ single (mailServerConfigToView <$> ms) a
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
  return $ BakeView
      { _bakeView_clients = clients
      , _bakeView_clientAddresses = clientAddresses
      , _bakeView_parameters = parameters
      , _bakeView_nodes = nodes
      , _bakeView_nodeAddresses = nodeAddresses
      , _bakeView_delegateStats = delegateStats
      , _bakeView_notificatees = notificatees
      , _bakeView_mailServer = mailServer
      , _bakeView_summaryGraph = summaryGraph
      , _bakeView_summary = summary
      , _bakeView_graphs = mempty
      , _bakeView_delegates = delegates
      }
