{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Workers.Delegate where

import Control.Concurrent.MVar
import Control.Lens (ifor, ifor_, ix, to, (.~), (<&>), (^.), (^?), _Just, _Right)
import Control.Monad.Except (ExceptT (..), MonadError, catchError, runExceptT, throwError)
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
import Rhyolite.Backend.DB (RunDb, getTime, openDb, runDb, selectMap)
import Rhyolite.Backend.Listen (NotificationType (..), insertAndNotify, insertAndNotify_, notifyEntityId,
                                updateAndNotify)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Concurrent (worker)
import Rhyolite.Schema (Id (..), Json (..))
import Say (say, sayErr, sayShow)

import Tezos.Contract (ContractId (..))
import Tezos.NodeRPC
import Tezos.Types

import Backend.CachedNodeRPC (NodeDataSource (..), dataSourceHead, dataSourceNode, readTimeBetweenBlocks)
import Backend.Common (worker')
import Backend.Schema
import Backend.Workers
import Common (tshow)
import Common.Schema

delegateWorker
  :: MonadIO m
  => NodeDataSource
  -> m (IO ())
delegateWorker nds = worker' (readTimeBetweenBlocks nds) $ \_ -> do
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
    say $ "Head level is " <> tshow headLevel <> " in cycle " <> tshow latestCycle
    delegates :: Map (Id Delegate) Delegate <- runNoLoggingT $ runDb (Identity db) $ selectMap DelegateConstructor (Delegate_deletedField ==. False)
    let oops :: forall a m. MonadIO m => RpcError -> m ()
        oops = sayShow
    runExceptT $ flip catchError oops $ do
      ifor_ delegates $ \dId delegate -> do
        let pkh = _delegate_publicKeyHash delegate
        say $ "Updating delegate " <> toPublicKeyHashText pkh
        accountStatus <- nodeRPC $ rContract chainId headBlockHash (Implicit $ _delegate_publicKeyHash delegate)

        let
          calcBakingEfficiency (numBakedAcc, numOpportunitiesAcc) = \case
            Nothing -> (numBakedAcc, numOpportunitiesAcc)
            Just True -> (numBakedAcc + 1, numOpportunitiesAcc + 1)
            Just False -> (numBakedAcc, numOpportunitiesAcc + 1)

        let
          numBaked = -1
          numOpportunities = -1
        runNoLoggingT $ runDb (Identity db) $ do
          delegateStatsId :: Maybe (Id DelegateStats) <- listToMaybe . fmap toId <$> project AutoKeyField ((DelegateStats_delegateField ==. dId) `limitTo` 1)
          case delegateStatsId of
            Nothing -> insertAndNotify_ DelegateStats
              { _delegateStats_delegate = dId
              , _delegateStats_accountBalance = Just $ _account_balance accountStatus
              , _delegateStats_accountSpendable = Just $ _account_spendable accountStatus
              , _delegateStats_accountSetable = Just $ _accountDelegate_setable $ _account_delegate accountStatus
              , _delegateStats_accountValue = _accountDelegate_value $ _account_delegate accountStatus
              , _delegateStats_accountCounter = Just $ _account_counter accountStatus
              }
            Just dsId -> updateAndNotify dsId
              [ DelegateStats_accountBalanceField =. Just (_account_balance accountStatus)
              , DelegateStats_accountSpendableField =. Just (_account_spendable accountStatus)
              , DelegateStats_accountSetableField =. Just (_accountDelegate_setable $ _account_delegate accountStatus)
              , DelegateStats_accountValueField =. _accountDelegate_value (_account_delegate accountStatus)
              , DelegateStats_accountCounterField =. Just (_account_counter accountStatus)
              ]

    --       return ()
