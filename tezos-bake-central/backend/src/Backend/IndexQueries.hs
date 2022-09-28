{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.IndexQueries where

import Control.Monad.Catch (MonadMask)
import Database.Groundhog.Postgresql (PersistBackend)

import Tezos.Types

import Backend.NodeRPC
  ( MonadNodeQuery (asksNodeDataSource, nqAtomically, nqThrowError)
  , NodeDataSource(..)
  , NodeQueryT
  , nodeQuery_Block

  -- Protocol constant
  , getProtocolIndex
  , getProtocolConstants

  , nodeQueryDataSourceSafe
  )

import Backend.STM (readTVar')
import Common.Schema
import ExtraPrelude

import Tezos.NodeRPC.Class ((~~))

getLatestProtocolConstants
  :: (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => NodeQueryT m (BranchInfo, ProtoInfo)
getLatestProtocolConstants = do
  latestFinalHeadVar <- asksNodeDataSource _nodeDataSource_latestFinalHead
  mbBranchInfo <- nqAtomically $ readTVar' latestFinalHeadVar
  flip (maybe (nqThrowError KilnRpcError_NoKnownHeads)) mbBranchInfo $ \branchInfo ->
    (branchInfo,) . _protocolIndex_constants <$> getProtocolIndex (branchInfo ^. hash) (branchInfo ^. protocolHash)


levelToCycle
  :: (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => (BlockHash, RawLevel) -> RawLevel -> NodeQueryT m Cycle
levelToCycle (branch, branchLevel) lvl = do
  fmap (view $ blockMetadata . blockMetadata_levelInfo . levelInfo_cycle) $
    nodeQueryDataSourceSafe $ nodeQuery_Block $ branch ~~ (branchLevel - lvl)

lastLevelInCycle
  :: ( MonadNodeQuery (NodeQueryT m)
     , MonadMask m
     , PersistBackend m
     )
  => BlockHash -> Cycle -> NodeQueryT m RawLevel
lastLevelInCycle branch c = do
  block <- nodeQueryDataSourceSafe $ nodeQuery_Block branch
  blocksPerCycle <- fmap (view protoInfo_blocksPerCycle) $ getProtocolConstants $ Left branch
  let levelInfo = block ^. blockMetadata . blockMetadata_levelInfo
      cycleDiff = levelInfo ^. levelInfo_cycle - c
      lastLevelInCurrentCycle = levelInfo ^. levelInfo_level + (blocksPerCycle - levelInfo ^. levelInfo_cyclePosition) - 1
  return $ lastLevelInCurrentCycle + fromIntegral cycleDiff * blocksPerCycle

endOfPreservedCycles :: RawLevel -> RawLevel -> ProtoInfo -> RawLevel
endOfPreservedCycles lvl cyclePosition protoInfo = lvl - cyclePosition +
  (protoInfo ^. protoInfo_blocksPerCycle) * fromIntegral (protoInfo ^. protoInfo_preservedCycles + 1) - 1
