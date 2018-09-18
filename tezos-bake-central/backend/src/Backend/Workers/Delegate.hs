{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Workers.Delegate where

import Control.Concurrent.MVar (readMVar)
import Control.Lens (ifor, ifor_, ix, to, (.~), (<&>), (^.), (^?), _Just, _Right)
import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Control.Monad.Reader (MonadReader, runReaderT)
import qualified Data.AppendMap as AppendMap
import Data.Foldable (fold, foldl', for_, toList, traverse_)
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup (Semigroup, Sum (..), getSum, (<>))
import qualified Data.Set as Set
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Client as Http (Manager, newManager)
import Rhyolite.Backend.DB (runDb, selectMap)
import Rhyolite.Backend.Listen (NotificationType (..), insertAndNotify, insertAndNotify_, notifyEntityId,
                                updateAndNotify)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Concurrent (worker)
import Rhyolite.Schema (Id (..), Json (..))
import Say (say, sayErr, sayShow)

import Tezos.Contract (ContractId (..))
import Tezos.NodeRPC
import Tezos.Types

import Backend.CachedNodeRPC (NodeDataSource (..), dataSourceHead, dataSourceNode, waitForNewHeadWithTimeout)
import Backend.Common (worker')
import Backend.Schema
import Backend.Workers
import Common (tshow)
import Common.Schema

delegateWorker
  :: MonadIO m
  => NodeDataSource
  -> m (IO ())
delegateWorker nds = worker' $ (*> waitForNewHeadWithTimeout nds) $ do
  protoInfo <- readMVar $ _nodeDataSource_parameters nds
  let chainId = _nodeDataSource_chain nds
      httpMgr = _nodeDataSource_httpMgr nds
      db = _nodeDataSource_pool nds
  ctxM <- runReaderT dataSourceNode nds
  headM <- runReaderT dataSourceHead nds
  for_  ((,) <$> ctxM <*> headM) $ \(ctx, head) -> flip runReaderT ctx  $ do
    say "Update delegate cycle."
    let
      headLevel :: RawLevel = head ^. level
      latestCycle = headLevel `div` fromIntegral (_protoInfo_blocksPerCycle protoInfo)
      levelRange = [max 0 (headLevel - 10) .. headLevel]
      headBlockHash = head ^. hash
    say $ "Head level is " <> tshow (unRawLevel headLevel) <> " in cycle " <> tshow (unRawLevel latestCycle)
    delegates :: Map (Id Delegate) Delegate <- runNoLoggingT $ runDb (Identity db) $ selectMap DelegateConstructor (Delegate_deletedField ==. False)
    let oops :: forall a m. MonadIO m => RpcError -> m ()
        oops = sayShow
    runExceptT $ flip catchError oops $ do
      ifor_ delegates $ \dId delegate -> do
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
