{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.IndexQueries where

import Control.Applicative (ZipList (..))
import Control.Monad.Catch (MonadMask)
import Database.Groundhog.Postgresql (PersistBackend)

import Tezos.Types
import qualified Tezos.Unsafe

import Backend.CachedNodeRPC
  ( MonadNodeQuery (asksNodeDataSource, nqAtomically, nqThrowError)
  , NodeDataSource(..)
  , NodeQueryT
  , fittestBranchInHistory

  -- Protocol constant
  , getProtocolIndex
  , getProtocolConstants
  )

import Backend.STM (readTVar')
import Common.Schema
import ExtraPrelude

getLatestProtocolConstants
  :: (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => NodeQueryT m (BranchInfo, ProtoInfo)
getLatestProtocolConstants = do
  histVar <- asksNodeDataSource _nodeDataSource_history
  hist <- nqAtomically $ readTVar' histVar
  branchInfo <- maybe (nqThrowError CacheError_NotEnoughHistory) pure $ fittestBranchInHistory hist
  (branchInfo,) . _protocolIndex_constants <$> getProtocolIndex (branchInfo ^. hash) (branchInfo ^. protocolHash)


levelToCycle
  :: (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => RawLevel -> NodeQueryT m Cycle
levelToCycle lvl = do
  (_, protoIx) <- getLatestProtocolConstants
  -- XXX We cheat here, as we dont expect the blocks/cycle to change
  pure $ Tezos.Unsafe.unsafeAssumptionLevelToCycle protoIx lvl

firstLevelInCycle
  :: ( MonadNodeQuery (NodeQueryT m)
     , MonadMask m
     , PersistBackend m
     )
  => BlockHash -> Cycle -> NodeQueryT m RawLevel
firstLevelInCycle _branch c = do
  (_, protoIx) <- getLatestProtocolConstants
  pure $ Tezos.Unsafe.unsafeAssumptionFirstLevelInCycle protoIx c

lastLevelInCycle
  :: ( MonadNodeQuery (NodeQueryT m)
     , MonadMask m
     , PersistBackend m
     )
  => BlockHash -> Cycle -> NodeQueryT m RawLevel
lastLevelInCycle branch c = fmap pred $ firstLevelInCycle branch (c + 1)

rightsContextLevel
  :: ( MonadNodeQuery (NodeQueryT m)
     , MonadMask m
     , PersistBackend m
     )
  => BlockHash -> RawLevel -> NodeQueryT m RawLevel
rightsContextLevel ctx lvl = do
  protoInfo <- getProtocolConstants $ Left ctx
  pure $ Tezos.Unsafe.unsafeAssumptionRightsContextLevel protoInfo lvl

data RightsCycleInfo = RightsCycleInfo
  { _rightsCycleInfo_cycle :: !Cycle
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

  let branchBlockHash = branchBlock ^. hash
  branchProtocolConstants <- getProtocolConstants $ Left branchBlockHash
  cycle' <- levelToCycle $ branchBlock ^. level
  let
    preservedCycles = branchProtocolConstants ^. protoInfo_preservedCycles
    cycles = [max 0 (cycle' - (1 + preservedCycles)) .. cycle' - 1] -- ignore the unconfirmed "current" cycle.
  (minLevels, maxLevels) <- fmap unzip $ for cycles $ \c -> liftA2 (,)
    (firstLevelInCycle branchBlockHash c)
    (pred <$> firstLevelInCycle branchBlockHash (succ c))
  return $ getZipList $ RightsCycleInfo
    <$> ZipList cycles
    <*> ZipList minLevels
    <*> ZipList maxLevels

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust [] = []
takeWhileJust (Just x: xs) = x:takeWhileJust xs
takeWhileJust (Nothing: _) = []
