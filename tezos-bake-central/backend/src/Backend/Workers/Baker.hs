{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Workers.Baker where

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

bakerWorker
  :: forall m. MonadIO m
  => NodeDataSource
  -> m (IO ())
bakerWorker nds = worker' $ (*> waitForNewHeadWithTimeout nds) $ do
  protoInfo <- readMVar $ _nodeDataSource_parameters nds
  let db = _nodeDataSource_pool nds
  ctxM <- runReaderT dataSourceNode nds
  headM <- runReaderT dataSourceHead nds
  for_  ((,) <$> ctxM <*> headM) $ \(ctx, headBlock) -> flip runReaderT ctx $ (runLoggingEnv $ _nodeDataSource_logger nds) $ do
    $(logDebug) "Update baker cycle."
    let
      headLevel :: RawLevel = headBlock ^. level
      latestCycle = headLevel `div` fromIntegral (_protoInfo_blocksPerCycle protoInfo)
    $(logDebug) $ "Head level is " <> tshow (unRawLevel headLevel) <> " in cycle " <> tshow (unRawLevel latestCycle)
    bakers :: Map (Id Baker) Baker <- runDb (Identity db) $ selectMap BakerConstructor (Baker_deletedField ==. False)
    -- let oops :: RpcError -> ExceptT RpcError (LoggingT (ReaderT NodeRPCContext)) ()
    let oops :: RpcError -> ExceptT RpcError (LoggingT (ReaderT NodeRPCContext IO)) ()
        oops = $(logErrorSH) . (,) ("bakerWorker" :: String)
    runExceptT $ flip catchError oops $ do
      for_ bakers $ \baker -> do
        let pkh = _baker_publicKeyHash baker
        $(logDebug) $ "Updating baker " <> toPublicKeyHashText pkh
        -- accountStatus <- nodeRPC $ rContract chainId headBlockHash (Implicit $ _baker_publicKeyHash baker)
        -- TODO:
        --   get upcoming rights (for next N cycles)
        --   compute expected deposits obligations
        --   get current account balance
        --   get frozen deposits
        --   CHECK: spendable balance > (deposits due - frozen rewards)
        --   get baker grace perioud
        --   CHECK: upcoming rights are before grace period expires
        return ()
