{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Workers.Delegate where

import Control.Concurrent.MVar (readMVar)
import Control.Lens ((^.))
import Control.Monad.Except (ExceptT (..), catchError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, logDebug, logErrorSH)
import Control.Monad.Reader (ReaderT (..))
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import Data.Semigroup ((<>))
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB (runDb, selectMap)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Rhyolite.Schema (Id (..))

import Tezos.NodeRPC
import Tezos.Types

import Backend.CachedNodeRPC (NodeDataSource (..), dataSourceHead, dataSourceNode, waitForNewHeadWithTimeout)
import Backend.Common (worker')
import Backend.Schema
import Common.Schema
import ExtraPrelude

delegateWorker
  :: forall m. MonadIO m
  => NodeDataSource
  -> m (IO ())
delegateWorker nds = worker' $ (*> waitForNewHeadWithTimeout nds) $ do
  protoInfo <- readMVar $ _nodeDataSource_parameters nds
  let db = _nodeDataSource_pool nds
  ctxM <- runReaderT dataSourceNode nds
  headM <- runReaderT dataSourceHead nds
  for_  ((,) <$> ctxM <*> headM) $ \(ctx, headBlock) -> flip runReaderT ctx $ (runLoggingEnv $ _nodeDataSource_logger nds) $ do
    $(logDebug) "Update delegate cycle."
    let
      headLevel :: RawLevel = headBlock ^. level
      latestCycle = headLevel `div` fromIntegral (_protoInfo_blocksPerCycle protoInfo)
    $(logDebug) $ "Head level is " <> tshow (unRawLevel headLevel) <> " in cycle " <> tshow (unRawLevel latestCycle)
    delegates :: Map (Id Delegate) Delegate <- runDb (Identity db) $ selectMap DelegateConstructor (Delegate_deletedField ==. False)
    -- let oops :: RpcError -> ExceptT RpcError (LoggingT (ReaderT NodeRPCContext)) ()
    let oops :: RpcError -> ExceptT RpcError (LoggingT (ReaderT NodeRPCContext IO)) ()
        oops = $(logErrorSH) . (,) ("delegateWorker" :: String)
    runExceptT $ flip catchError oops $ do
      for_ delegates $ \delegate -> do
        let pkh = _delegate_publicKeyHash delegate
        $(logDebug) $ "Updating delegate " <> toPublicKeyHashText pkh
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
