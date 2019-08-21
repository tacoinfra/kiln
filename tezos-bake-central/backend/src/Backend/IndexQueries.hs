{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Backend.IndexQueries where

import Control.Applicative (ZipList (..))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger (logDebug)
import qualified Data.LCA.Online.Polymorphic as LCA
import Data.Ord (Down (..))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.String.Here.Interpolated (i)
import Data.Witherable (mapMaybe)
import Database.Groundhog.Postgresql (PersistBackend, insert, select, (&&.), (==.))
import Named
import Tezos.History
import qualified Tezos.ProtocolConstants
import Tezos.Types

import Rhyolite.Schema (Id (..))

import Backend.CachedNodeRPC
  ( CachedHistory'
  , MonadNodeQuery (asksNodeDataSource, nqAtomically, nqThrowError)
  , NodeDataSource(..)
  , NodeQuery(..)
  , NodeQueryT
  , branchPointPure
  , fittestBranchInHistory
  , levelAncestor
  , nodeQueryDataSourceSafe
  , nqTry

  -- Protocol constant
  , getProtocolIndex
  )
import Backend.Schema
import Backend.STM (readTVar')
import Common.Schema
import ExtraPrelude
import Safe (headMay)


getLatestProtocolConstants
  :: (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => NodeQueryT m (WithProtocolHash VeryBlockLike, ProtoInfo)
getLatestProtocolConstants = do
  histVar <- asksNodeDataSource _nodeDataSource_history
  hist <- nqAtomically $ readTVar' histVar
  branchBlock <- maybe (nqThrowError CacheError_NotEnoughHistory) pure $ fittestBranchInHistory hist
  (branchBlock,) . _protocolIndex_constants <$> getProtocolIndex (branchBlock ^. hash) (branchBlock ^. protocolHash)


levelToCycle
  :: (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => RawLevel -> NodeQueryT m Cycle
levelToCycle lvl = do
  (_, protoIx) <- getLatestProtocolConstants
  -- XXX We cheat here, as we dont expect the blocks/cycle to change
  pure $ Tezos.ProtocolConstants.levelToCycle protoIx lvl

firstLevelInCycle
  :: ( MonadNodeQuery (NodeQueryT m)
     , MonadMask m
     , PersistBackend m
     )
  => BlockHash -> Cycle -> NodeQueryT m RawLevel
firstLevelInCycle _branch c = do
  (_, protoIx) <- getLatestProtocolConstants
  pure $ Tezos.ProtocolConstants.firstLevelInCycle protoIx c

lastLevelInCycle
  :: ( MonadNodeQuery (NodeQueryT m)
     , MonadMask m
     , PersistBackend m
     )
  => BlockHash -> Cycle -> NodeQueryT m RawLevel
lastLevelInCycle branch c = fmap pred $ firstLevelInCycle branch (c + 1)


data RightsCycleInfo = RightsCycleInfo
  { _rightsCycleInfo_branch :: !BlockHash  -- the hash of the first block in some cycle
  , _rightsCycleInfo_cycle :: !Cycle
  , _rightsCycleInfo_minLevel :: !RawLevel -- the first level of _rightsCycleInfo_cycle
  , _rightsCycleInfo_maxLevel :: !RawLevel -- the last level of _rightsCycleInfo_cycle
  } deriving (Eq, Ord, Show, Generic, Typeable)

-- produce the list of the first blocks in the cycle for the previous 7 cycles ending on $blkHash$
cycleStartHashes
  :: forall m blk
   . ( MonadNodeQuery (NodeQueryT m)
     , MonadMask m
     , PersistBackend m
     , BlockLike blk
     )
  => blk -> NodeQueryT m [RightsCycleInfo]
cycleStartHashes branchBlock = do
  history <- nqAtomically . readTVar' =<< asksNodeDataSource _nodeDataSource_history

  let branchBlockHash = branchBlock ^. hash
  branchProtocolConstants <- nodeQueryDataSourceSafe $ NodeQuery_ProtocolConstants branchBlockHash
  cycle <- levelToCycle $ branchBlock ^. level
  let
    minLvl = _cachedHistory_minLevel history
    preservedCycles = branchProtocolConstants ^. protoInfo_preservedCycles
    cycles = [max 0 (cycle - (1 + preservedCycles)) .. cycle - 1] -- ignore the unconfirmed "current" cycle.
  (minLevels, maxLevels) <- fmap unzip $ for cycles $ \c -> liftA2 (,)
    (firstLevelInCycle branchBlockHash c)
    (pred <$> firstLevelInCycle branchBlockHash (succ c))
  let
    branches = maybe [] (\branch -> fmap (^. _1) $ takeWhileJust $ LCA.uncons . flip LCA.keep branch . fromIntegral . unRawLevel . subtract minLvl <$> minLevels) mbranch
    mbranch = branchBlockHash `Map.lookup` _cachedHistory_blocks history
  return $ getZipList $ RightsCycleInfo
    <$> ZipList branches
    <*> ZipList cycles
    <*> ZipList minLevels
    <*> ZipList maxLevels

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust [] = []
takeWhileJust (Just x: xs) = x:takeWhileJust xs
takeWhileJust (Nothing: _) = []
