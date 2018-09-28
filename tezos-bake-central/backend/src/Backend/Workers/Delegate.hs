{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Workers.Delegate where

import Control.Concurrent.MVar (readMVar)
import Control.Lens ((^.))
import Control.Monad.Except (catchError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import Data.Semigroup ((<>))
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB (runDb, selectMap)
import Rhyolite.Schema (Id (..))
import Say (say, sayShow)

import Tezos.NodeRPC
import Tezos.Types

import Backend.CachedNodeRPC (NodeDataSource (..), dataSourceHead, dataSourceNode, waitForNewHeadWithTimeout)
import Backend.Common (worker')
import Backend.Schema
import Common (tshow)
import Common.Schema

delegateWorker
  :: MonadIO m
  => NodeDataSource
  -> m (IO ())
delegateWorker nds = worker' $ (*> waitForNewHeadWithTimeout nds) $ do
  protoInfo <- readMVar $ _nodeDataSource_parameters nds
  let db = _nodeDataSource_pool nds
  ctxM <- runReaderT dataSourceNode nds
  headM <- runReaderT dataSourceHead nds
  for_  ((,) <$> ctxM <*> headM) $ \(ctx, headBlock) -> flip runReaderT ctx  $ do
    say "Update delegate cycle."
    let
      headLevel :: RawLevel = headBlock ^. level
      latestCycle = headLevel `div` fromIntegral (_protoInfo_blocksPerCycle protoInfo)
    say $ "Head level is " <> tshow (unRawLevel headLevel) <> " in cycle " <> tshow (unRawLevel latestCycle)
    delegates :: Map (Id Delegate) Delegate <- runNoLoggingT $ runDb (Identity db) $ selectMap DelegateConstructor (Delegate_deletedField ==. False)
    let oops :: forall m. MonadIO m => RpcError -> m ()
        oops = sayShow
    runExceptT $ flip catchError oops $ do
      for_ delegates $ \delegate -> do
        let pkh = _delegate_publicKeyHash delegate
        say $ "Updating delegate " <> toPublicKeyHashText pkh
        -- accountStatus <- nodeRPC $ rContract chainId headBlockHash (Implicit $ _delegate_publicKeyHash delegate)
        -- TODO:
        --   get upcoming rights (for next N cycles)
        --   compute expected deposits obligations
        --   get current account balance
        --   get frozen deposits
        --   CHECK: spendable balance > (deposits due - frozen rewards)
        --   get delegate grace perioud
        --   CHECK: upcoming rights are before grace period expires
        return ()
