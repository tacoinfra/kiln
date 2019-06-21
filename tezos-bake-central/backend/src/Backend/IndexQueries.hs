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
  )
import Backend.Schema
import Backend.STM (readTVar')
import Common.Schema
import ExtraPrelude
import Safe (headMay)


getLatestBlockWithProtocol
  :: (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => NodeQueryT m (Block, ProtocolIndex)
getLatestBlockWithProtocol = do
  histVar <- asksNodeDataSource _nodeDataSource_history
  hist <- nqAtomically $ readTVar' histVar
  branchBlock <- maybe (nqThrowError CacheError_NotEnoughHistory) pure $ fittestBranchInHistory hist
  branchBlockFull <- nodeQueryDataSourceSafe $ NodeQuery_Block $ branchBlock ^. hash
  (branchBlockFull,) <$> getProtocolIndex (branchBlock ^. hash) (branchBlockFull ^. block_protocol)

-- TODO: Pass history in
getProtocolIndex
  :: forall m
   . (MonadNodeQuery (NodeQueryT m), MonadMask m, PersistBackend m)
  => BlockHash -> ProtocolHash -> NodeQueryT m ProtocolIndex
getProtocolIndex branch protoHash = do
  (chainId, historyVar) <- asksNodeDataSource (_nodeDataSource_chain &&& _nodeDataSource_history)

  existingEntries :: [ProtocolIndex] <- select $
    ProtocolIndex_chainIdField ==. chainId &&. ProtocolIndex_hashField ==. protoHash

  history <- nqAtomically $ readTVar' historyVar
  case headMay [x | x <- existingEntries, isJust $ branchPointPure (x ^. hash) branch history] of
    Just existing -> pure existing
    Nothing -> do
      -- Search until we have the history up to the desired protocol.
      protocolHistory <- buildProtocolHistoryUntil
        ! #predicate (\blk -> blk ^. protocolHash == protoHash)
        ! #branch branch
        ! #history history

      case NE.nonEmpty $ sortOn (Down . (^. level)) $ toList protocolHistory of
        Nothing -> nqThrowError CacheError_NotEnoughHistory
        Just orderedFirstBlocks -> do
          -- To increase likelihood that a node knows the answer, we will use the *last* block
          -- in a protocol to get it's constants (the most recent block possible). To do this we
          -- pair up the protocols with the block immediately *prior* to the first block in the
          -- next protocol. For the most recent protocol, we will use 'branch' as the query block.
          let
            initOrderedLastBlockHashes = flip map (NE.init orderedFirstBlocks) $ \blk ->
              levelAncestor history (blk ^. level - 1) branch
            protocolQueryBlockMap = NE.zip orderedFirstBlocks (Just branch NE.:| initOrderedLastBlockHashes)

          protoIndexes :: [ProtocolIndex] <- fmap (mapMaybe (^? _Right) . toList) $
            for protocolQueryBlockMap $ \(firstBlock, queryBlockHash') -> nqTry $ do
              -- Before using the query block instead of 'firstBlock', make sure it's protocol really is
              -- the same. If not, fall back to 'firstBlock'.
              -- While this situation shouldn't happen, it's possible for protocols to be introduced
              -- apart from the amendment process. In this case we may actually skip one
              -- in the scan which would cause this logic to pair the wrong constants with a
              -- protocol hash--and that's just too scary to think about.
              queryBlock' <- for queryBlockHash' $ nodeQueryDataSourceSafe . NodeQuery_Block
              let
                actualQueryBlockHash = case queryBlock' of
                  Just queryBlock | queryBlock ^. protocolHash == firstBlock ^. protocolHash -> queryBlock ^. hash
                  _ -> firstBlock ^. hash
              constants <- nodeQueryDataSourceSafe $ NodeQuery_ProtocolConstants actualQueryBlockHash
              pure ProtocolIndex
                { _protocolIndex_chainId = chainId
                , _protocolIndex_hash = firstBlock ^. protocolHash
                , _protocolIndex_constants = constants
                , _protocolIndex_firstBlockHash = firstBlock ^. hash
                , _protocolIndex_firstBlockPredecessor = firstBlock ^. predecessor
                , _protocolIndex_firstBlockLevel = firstBlock ^. level
                , _protocolIndex_firstBlockFitness = firstBlock ^. fitness
                , _protocolIndex_firstBlockTimestamp = firstBlock ^. timestamp
                , _protocolIndex_firstBlockCycle = firstBlock ^. block_metadata . blockMetadata_level . level_cycle
                }

          for_ protoIndexes $ \protoIndex -> do
            insert protoIndex
            notifyDefault $ Id @ProtocolIndex (protoIndex ^. protocolIndex_chainId, protoIndex ^. protocolHash, protoIndex ^. hash)

          maybe (nqThrowError CacheError_NotEnoughHistory) pure $
            find ((protoHash ==) . view protocolHash) protoIndexes

buildProtocolHistoryUntil
  :: forall m
   . (MonadNodeQuery (NodeQueryT m), MonadMask m)
  => "predicate" :! (Block -> Bool)
  -> "branch" :! BlockHash
  -> "history" :! CachedHistory'
  -> NodeQueryT m (Map ProtocolHash Block)
buildProtocolHistoryUntil (Arg predicate) (Arg branch) (Arg history) = do
  branchBlock <- nodeQueryDataSourceSafe $ NodeQuery_Block branch
  go ! #currentBlock branchBlock
     ! #currentProtocol (branchBlock ^. block_protocol)
     ! #protocolHistory mempty
  where
    levelsBefore blk lvls = maybe (nqThrowError CacheError_NotEnoughHistory) pure $
      if blk ^. level - lvls < 0
      then Nothing
      else
        levelAncestor history (max (blk ^. level - lvls) (history ^. cachedHistory_minLevel)) (blk ^. hash)

    votingPeriodPosition = block_metadata . blockMetadata_level . level_votingPeriodPosition

    go :: "currentBlock" :! Block
       -> "currentProtocol" :! ProtocolHash
       -> "protocolHistory" :! Map ProtocolHash Block
       -> NodeQueryT m (Map ProtocolHash Block)
    go (Arg currentBlock) (Arg currentProtocol) (Arg protocolHistory) =
      case currentBlock ^. level == history ^. cachedHistory_minLevel of
        True -> do
          $(logDebug) [i|Got to the root searching for protocol: ${currentProtocol}|]
          -- If 'currentBlock' is at the minimum level, we call it the beginning of 'currentProtocol'.
          pure $ Map.insert currentProtocol currentBlock protocolHistory
        False -> do
          lastBlockHashInPreviousVotingPeriod <- levelsBefore currentBlock (currentBlock ^. votingPeriodPosition + 1)
          lastBlockInPreviousVotingPeriod <- nodeQueryDataSourceSafe $ NodeQuery_Block lastBlockHashInPreviousVotingPeriod -- HEADER ONLY?
          case currentProtocol == lastBlockInPreviousVotingPeriod ^. protocolHash of
            True -> do
              $(logDebug) [i|Found another voting period with the same protocol ${currentProtocol} - ${lastBlockInPreviousVotingPeriod ^. hash}|]
              go ! #currentBlock lastBlockInPreviousVotingPeriod
                 ! #currentProtocol currentProtocol
                 ! #protocolHistory protocolHistory
            False -> do
              $(logDebug) [i|Found a transition for ${currentProtocol} at ${lastBlockInPreviousVotingPeriod ^. hash}|]
              firstBlockHashInVotingPeriod <- levelsBefore currentBlock (currentBlock ^. votingPeriodPosition)
              firstBlockInVotingPeriod <- nodeQueryDataSourceSafe $ NodeQuery_Block firstBlockHashInVotingPeriod

              (lastBlockInPreviousProtocol, firstBlockInProtocol) <- case currentProtocol == firstBlockInVotingPeriod ^. protocolHash of
                True -> pure (lastBlockInPreviousVotingPeriod, firstBlockInVotingPeriod)
                False -> do
                  $(logDebug) [i|Entering binary search for ${currentProtocol}|]
                  maybe (nqThrowError $ CacheError_UnknownProtocol currentProtocol) pure =<<
                      binarySearch firstBlockInVotingPeriod currentBlock

              let protocolHistory' = Map.insert currentProtocol firstBlockInProtocol protocolHistory
              case predicate firstBlockInProtocol of
                True -> pure protocolHistory' -- We finished searching.
                False -> go ! #currentBlock lastBlockInPreviousProtocol
                            ! #currentProtocol (lastBlockInPreviousProtocol ^. protocolHash)
                            ! #protocolHistory protocolHistory'

    binarySearch :: Block -> Block -> NodeQueryT m (Maybe (Block, Block))
    binarySearch low high = do
      $(logDebug) [i|Protocol binary search between levels ${low ^. level} and ${high ^. level}|]
      binarySearch' low high

    binarySearch' :: Block -> Block -> NodeQueryT m (Maybe (Block, Block))
    binarySearch' low high
      | low ^. level >= high ^. level = pure Nothing
      | low ^. level == high ^. level - 1 =
          pure $ if low ^. protocolHash == high ^. protocolHash then Nothing else Just (low, high)
      | otherwise = do
        let halfwayLevel = high ^. level - ((high ^. level - low ^. level) `div` 2)
        halfway <- nodeQueryDataSourceSafe . NodeQuery_Block <=<
          maybe (nqThrowError CacheError_NotEnoughHistory) pure $
            levelAncestor history halfwayLevel branch
        case halfway of
          x | x ^. protocolHash == low ^. protocolHash -> binarySearch halfway high
            | x ^. protocolHash == high ^. protocolHash -> binarySearch low halfway
            | otherwise -> pure Nothing

levelToCycle
  :: (MonadNodeQuery (NodeQueryT m), MonadMask m)
  => RawLevel -> NodeQueryT m Cycle
levelToCycle lvl = do
  histVar <- asksNodeDataSource _nodeDataSource_history
  hist <- nqAtomically $ readTVar' histVar
  lvlBlockHash <- maybe (nqThrowError CacheError_NotEnoughHistory) pure $
    levelAncestor hist lvl . view hash =<< fittestBranchInHistory hist
  fmap (view _2) $ getPositionOfBlockFaster lvlBlockHash

-- | Tries to be more efficient about finding the level and cycle of a block by using
-- protocol data from the latest head block before resorting to querying the
-- block itself.
-- TODO: Actually write the faster version of this.
getPositionOfBlockFaster
  :: (MonadNodeQuery (NodeQueryT m), MonadMask m)
  => BlockHash -> NodeQueryT m (RawLevel, Cycle)
getPositionOfBlockFaster blkHash =
  -- NOTE: Level can always be calculated from history, but we're querying the block anyway
  -- so might as well get it this way.
  (view level &&& view (block_metadata . blockMetadata_level . level_cycle)) <$>
    nodeQueryDataSourceSafe (NodeQuery_Block blkHash)

firstLevelInCycle
  :: ( MonadNodeQuery (NodeQueryT m)
     , MonadMask m
     , PersistBackend m
     )
  => BlockHash -> Cycle -> NodeQueryT m RawLevel
firstLevelInCycle branch c = do
  branchBlock <- nodeQueryDataSourceSafe $ NodeQuery_Block branch
  protocolIndex <- getProtocolIndex (branchBlock ^. hash) (branchBlock ^. block_protocol)
  let
    branchProtocolConstants = protocolIndex ^. protocolIndex_constants
    firstLevelInBranchCycle = branchBlock ^. level - branchBlock ^. block_metadata . blockMetadata_level . level_cyclePosition -- TODO: Check for off-by-one
    cycl = block_metadata . blockMetadata_level . level_cycle
  case branchBlock ^. cycl of
    branchCycle
      | branchCycle == c ->
          -- The branch is on the cycle we're looking for so we can calculate the offset to
          -- the first block of that cycle.
          pure firstLevelInBranchCycle
      | branchCycle > c ->
          -- TODO: Get rid of the WARNING. If the desired cycle is in the future, we can
          -- know for certain which level it will be as long as it's not beyond the point
          -- of a possible protocol transition. Past that point we're just guessing and
          -- assuming that the protocol doesn't change or that $BLOCKS_PER_CYCLE doesn't
          -- change with the next protocol. We could return a sum type to encode
          -- "guessing" to let caller decide how certain they need to be.
          -- WARNING: We assume $BLOCKS_PER_CYCLE doesn't change in the future!
          pure $
            firstLevelInBranchCycle + branchProtocolConstants ^. protoInfo_blocksPerCycle * RawLevel (unCycle $ branchCycle - c)
      | otherwise -> do
          -- TODO: Using the same logic as the TODO above we can optimize this to avoid
          -- some RPC calls if we can determine that the desired cycle is within the
          -- range of blocks that must certainly be on the same protocol as the one
          -- we have in hand already.

          -- See if going all the way back to the beginning of this protocol is enough.
          -- If not, we'll have to recurse starting with the block that preceeds the
          -- first block of this protocol.
          case protocolIndex ^. protocolIndex_firstBlockCycle < c of
            True ->
              -- We can use the protocol constants of this block to calculate.
              pure $ protocolIndex ^. level + branchProtocolConstants ^. protoInfo_blocksPerCycle * RawLevel (unCycle $ c - protocolIndex ^. protocolIndex_firstBlockCycle)
            False -> do
              -- We can't use the protocol constants of this block so recurse backward starting
              -- with the last block in the previous protocol.
              history <- nqAtomically . readTVar' =<< asksNodeDataSource _nodeDataSource_history
              maybe (nqThrowError CacheError_NotEnoughHistory) (`firstLevelInCycle` c) $
                levelAncestor history (protocolIndex ^. level - 1) (protocolIndex ^. hash)

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
  :: forall m
   . ( MonadNodeQuery (NodeQueryT m)
     , MonadMask m
     , PersistBackend m
     )
  => BlockHash -> NodeQueryT m [RightsCycleInfo]
cycleStartHashes blkHash = do
  history <- nqAtomically . readTVar' =<< asksNodeDataSource _nodeDataSource_history
  -- TODO: Partial match
  let Just (branch, branchBlockHash) = do
        b <- blkHash `Map.lookup` _cachedHistory_blocks history
        branchHash <- case LCA.view b of
          LCA.Root -> Nothing
          LCA.Node bBlockHash _ _ -> Just bBlockHash
        pure (b, branchHash)

  branchBlock <- nodeQueryDataSourceSafe $ NodeQuery_Block branchBlockHash
  branchProtocolConstants <- nodeQueryDataSourceSafe $ NodeQuery_ProtocolConstants branchBlockHash
  let
    minLvl = _cachedHistory_minLevel history
    cycle = branchBlock ^. block_metadata . blockMetadata_level . level_cycle
    preservedCycles = branchProtocolConstants ^. protoInfo_preservedCycles
    cycles = [max 0 (cycle - (1 + preservedCycles)) .. cycle - 1] -- ignore the unconfirmed "current" cycle.
  (minLevels, maxLevels) <- fmap unzip $ for cycles $ \c -> liftA2 (,)
    (firstLevelInCycle branchBlockHash c)
    (pred <$> firstLevelInCycle branchBlockHash (succ c))
  let branches = fmap (^. _1) $ takeWhileJust $ LCA.uncons . flip LCA.keep branch . fromIntegral . unRawLevel . subtract minLvl <$> minLevels
  return $ getZipList $ RightsCycleInfo
    <$> ZipList branches
    <*> ZipList cycles
    <*> ZipList minLevels
    <*> ZipList maxLevels

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust [] = []
takeWhileJust (Just x: xs) = x:takeWhileJust xs
takeWhileJust (Nothing: _) = []
