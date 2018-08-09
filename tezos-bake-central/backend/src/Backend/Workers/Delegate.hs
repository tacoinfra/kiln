{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Workers.Delegate where

import Backend.Schema
import Backend.Workers
import Common (tshow)
import Common.Schema
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

import Backend.CachedNodeRPC (NodeDataSource (..))

delegateWorker
  :: MonadIO m
  => Int -- Microseconds
  -> NodeDataSource
  -> Pool Postgresql
  -> m (IO ())
delegateWorker delay nds db = worker delay $ do
  let chainId = _nodeDataSource_chain nds
      httpMgr = _nodeDataSource_httpMgr nds
  mBestNode <- runNoLoggingT $ runDb (Identity db) queryBestNode
  for_  mBestNode $ \(nid, bestNode, protoInfo) -> flip runReaderT (NodeRPCContext httpMgr $ _node_address bestNode) $ do
    say "Update delegate cycle."
    let
      headLevel :: Integer = fromIntegral $ fromMaybe (error "queryBestNode returned unfit node") $ _node_headLevel bestNode
      latestCycle = headLevel `div` fromIntegral (_protoInfo_blocksPerCycle protoInfo)
      levelRange = [max 0 (headLevel - 10) .. headLevel]
      headBlockHash = fromMaybe (error "have block level but not hash!") $ _node_headBlockHash bestNode
    say $ "Head level is " <> tshow headLevel <> " in cycle " <> tshow latestCycle
    delegates :: Map (Id Delegate) Delegate <- runNoLoggingT $ runDb (Identity db) $ selectMap DelegateConstructor (Delegate_deletedField ==. False)
    -- TODO: rights don't change very much, and the node is very slow at computing large ranges of rights.  build up a set of rights slowly and cache them.
    let oops :: forall a m. MonadIO m => RpcError -> m ()
        oops = sayShow
    runExceptT $ flip catchError oops $ do
      allBakingRights <- nodeRPC (rBakingRights chainId headBlockHash (Set.fromList $ Left . RawLevel . fromIntegral <$> levelRange))
      -- Filter out baking rights that apply to levels in the future.
      -- map is from delegate*level to priorotiy
      let bakingRights :: AppendMap.AppendMap PublicKeyHash (Map RawLevel Priority) =
            AppendMap.filter (not . null) $ Map.filterWithKey (\k _ -> k <= fromIntegral headLevel) <$> bakingRightsMap allBakingRights
      ifor_ delegates $ \dId delegate -> do
        let pkh = _delegate_publicKeyHash delegate
        say $ "Updating delegate " <> toPublicKeyHashText pkh
        accountStatus <- nodeRPC $ rContract chainId headBlockHash (Implicit $ _delegate_publicKeyHash delegate)
        bakingRightsUtilized <- ifor (fromMaybe mempty $ bakingRights ^? ix pkh) $ \levelWithRight delegatePriority -> do
          blockWithRights <- nodeRPC (rBlockPred chainId headBlockHash (fromIntegral headLevel - fromIntegral levelWithRight))
          let
            -- The ID of the baker who baked this block
            baker = blockWithRights ^. block_metadata . blockMetadata_baker
          return $ if baker == pkh then Just True -- Our delegate baked this block so point for us!
            else case bakingRights ^? ix baker . ix levelWithRight of
                Nothing -> Nothing -- Can't find this baker in the table of rights
                Just bakerPriority -> if bakerPriority < delegatePriority
                  then Nothing -- The baker baked with higher priority so this block doesn't count either way.
                  else Just False -- The baker baked with lower priority, so point against us.
          -- return ()

        let
          calcBakingEfficiency (numBakedAcc, numOpportunitiesAcc) = \case
            Nothing -> (numBakedAcc, numOpportunitiesAcc)
            Just True -> (numBakedAcc + 1, numOpportunitiesAcc + 1)
            Just False -> (numBakedAcc, numOpportunitiesAcc + 1)

          (numBaked, numOpportunities) = foldl' calcBakingEfficiency (0, 0) bakingRightsUtilized

        runNoLoggingT $ runDb (Identity db) $ do
          delegateStatsId :: Maybe (Id DelegateStats) <- listToMaybe . fmap toId <$> project AutoKeyField ((DelegateStats_delegateField ==. dId) `limitTo` 1)
          case delegateStatsId of
            Nothing -> insertAndNotify_ DelegateStats
              { _delegateStats_delegate = dId
              , _delegateStats_efficiency = BakeEfficiency numBaked numOpportunities
              , _delegateStats_accountBalance = Just $ _account_balance accountStatus
              , _delegateStats_accountSpendable = Just $ _account_spendable accountStatus
              , _delegateStats_accountSetable = Just $ _accountDelegate_setable $ _account_delegate accountStatus
              , _delegateStats_accountValue = _accountDelegate_value $ _account_delegate accountStatus
              , _delegateStats_accountCounter = Just $ _account_counter accountStatus
              }
            Just dsId -> updateAndNotify dsId
              [ DelegateStats_efficiencyField =. BakeEfficiency numBaked numOpportunities
              , DelegateStats_accountBalanceField =. Just (_account_balance accountStatus)
              , DelegateStats_accountSpendableField =. Just (_account_spendable accountStatus)
              , DelegateStats_accountSetableField =. Just (_accountDelegate_setable $ _account_delegate accountStatus)
              , DelegateStats_accountValueField =. _accountDelegate_value (_account_delegate accountStatus)
              , DelegateStats_accountCounterField =. Just (_account_counter accountStatus)
              ]

    --       return ()
