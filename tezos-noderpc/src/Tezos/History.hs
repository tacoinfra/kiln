{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.History where

import Control.Concurrent.STM (TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.DeepSeq (NFData)
import Control.Lens (Lens, view, (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State.Strict (MonadState, get, modify, runState)
import Data.Foldable (for_, foldl')
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (), (<|))
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import qualified Data.LCA.Online.Polymorphic as LCA

import Tezos.NodeRPC
import Tezos.NodeRPC.Network
import Tezos.Types

data CachedHistory a = CachedHistory
  -- what i really need here is a cover tree (or some other metric index)
  -- a plausible alternative is to only keep the fittest n branches
  -- investigate: https://github.com/mikeizbicki/HLearn/blob/master/src/HLearn/Data/SpaceTree/CoverTree.hs
  { _cachedHistory_branches :: !(Map BlockHash (WithProtocolHash VeryBlockLike))
  , _cachedHistory_blocks :: !(Map BlockHash (LCA.Path BlockHash a))
  , _cachedHistory_minLevel :: !RawLevel
  , _cachedHistory_levelZero :: !RawLevel
  } deriving (Show, Typeable, Generic)
instance NFData a => NFData (CachedHistory a)
makeLenses 'CachedHistory

emptyCache :: RawLevel -> CachedHistory a
emptyCache lvl = CachedHistory Map.empty Map.empty lvl (RawLevel maxBound)

class HasCachedHistory f s t a b | s -> a, t -> b where
  cachedHistory :: Lens s t (f (CachedHistory a)) (f (CachedHistory b))

type LCABlockPath = LCA.Path BlockHash ()
type BlockMap  = Map BlockHash LCABlockPath

data BlockPath = BlockPath
  { _blockPath_currentPath :: !LCABlockPath
  , _blockPath_blockMap    :: !BlockMap
  }

-- This is a strict pair used when filling in an empty LCA cache from the postgresql
-- database, so that GHC can do some of it's optimization magic on a foldl' in the
-- tezos-bake-central backend.  However, managing branches on the initial fill
-- seems best left to something that is not a @WithProtocolHash VeryBlockLike@
data Prehistory a = Prehistory
  { _prehistory_branches :: !(Map BlockHash a)
  , _prehistory_blockMap :: !BlockMap
  }

initializeBlocks :: [BlockHash] -> BlockMap
initializeBlocks blks = _blockPath_blockMap (extendBlockPath blks emptyBlockPath)

-- | @'addHeadBlock' spine block history@ adds a head @block@ to the history,
-- while enforcing invariants. The @spine@ consists any hashes that need to be
-- added to the history in order to connect the head block back to already known
-- hashes in the history.   The first element of @spine@ must be in
-- '_cachedHistory_blocks', and the last element of @spine@ must be the
-- 'predecessor' hash of the @block@.   If @spine@ is @[]@,  then it's assumed
-- to be equivalent to @[block ^. predecessor]@.
--
-- Duplicate blocks are harmless beyond minor resource consumption.
-- The only times @addHeadBlock@ when will return 'Nothing' are when the
-- preconditions described above are violated.
addHeadBlock :: (BlockLike b, HasProtocolHash b)  => [BlockHash] -> b -> CachedHistory () -> Maybe (CachedHistory ())
addHeadBlock spine blk history = do
  case Map.lookup blkHash knownBlocks of
    Just _ -> Just history
    Nothing -> do
      let (firstBlk, rest) =
            case spine of
              [] -> (predHash, [])
              (x:xs) -> (x, xs)
      case skipKnownBlocks firstBlk rest of
        Nothing -> Nothing
        -- path is the LCA blockpath of lastKnownBlock
        Just (path, lastKnownBlock, newBlocks) -> do
          -- We used skipKnownBlocks to do some or all of the work that `last` would be doing here,
          -- so remember `(lastKnownBlock:newBlocks)` is the spine that connects to what we know,
          -- and we are checking if `last (lastKnownBlock:newBlocks)` is equal to `predHash`
          if (if null newBlocks then lastKnownBlock == predHash else last newBlocks == predHash)
          then Nothing
          else do
            let
              BlockPath path' blocks' = extendBlockPath newBlocks (BlockPath path knownBlocks)
              blk' = WithProtocolHash (mkVeryBlockLike blk) (blk ^. protocolHash)
              blocks'' = Map.insert blkHash (LCA.cons blkHash () path') blocks'
              branches' = Map.delete lastKnownBlock (_cachedHistory_branches history)
              branches'' = Map.insert blkHash blk' branches'
            Just $ CachedHistory {
              _cachedHistory_blocks = blocks''
            , _cachedHistory_branches = branches''
            , _cachedHistory_minLevel = _cachedHistory_minLevel history
            , _cachedHistory_levelZero = _cachedHistory_levelZero history
            }
  where
    blkHash = blk ^. hash
    predHash = blk ^. predecessor
    knownBlocks = _cachedHistory_blocks history

    -- skipKnownBlocks is conceptually operating on the guaranteed-nonempty list (a:bs), which is
    -- the spine (explicit or implicit) that was passed to the function
    -- the return value is the guaranteed-nonempty list (lastKnownBlock:unknownBlocks)
    skipKnownBlocks a bs =
      case Map.lookup a knownBlocks of
        Nothing -> Nothing
        Just path -> Just $ go path a bs
      where
        go path b [] =
          (path, b, [])
        go path b cs@(c:ds) =
          case Map.lookup c knownBlocks of
            Nothing -> (path, b, cs)
            Just path' -> go path' c ds

emptyBlockPath :: BlockPath
emptyBlockPath = BlockPath LCA.empty Map.empty

extendBlockPath :: [BlockHash] -> BlockPath -> BlockPath
extendBlockPath spine st = foldl' delta st spine
  where delta (BlockPath path blocks) blkHash = BlockPath path' blocks'
          where path'   = LCA.cons blkHash () path
                blocks' = Map.insert blkHash path' blocks

accumPrehistory :: BlockSpine blk => blk -> Prehistory blk -> Prehistory blk
accumPrehistory blk hist@(Prehistory branches blocks) =
  case Map.lookup blkHash blocks of
    Just _ -> hist
    Nothing ->
      case Map.lookup predHash blocks of
        Nothing -> hist
        Just path -> Prehistory branches' blocks'
          where
            branches' = Map.insert blkHash blk (Map.delete predHash branches)
            blocks'   = Map.insert blkHash (LCA.cons blkHash () path) blocks
  where
    blkHash  = blk ^. hash
    predHash = blk ^. predecessor

accumBalance :: MonadState Balances m => Block -> m ()
accumBalance = modify . (<>) . getBalanceChanges

scanBranch ::
  ( MonadIO m, MonadLogger m
  , MonadReader ctx m , HasNodeRPC ctx
  , MonadError e m, AsRpcError e
  )
  => Block -> RawLevel -> RawLevel -> (Block -> m a) -> m ()
scanBranch branch start stop k = do
  let headLvl = _blockHeaderFull_level $ _block_header branch
  for_ [start .. stop] $ \n -> do
    blk <- nodeRPC $ rBlockPred (headLvl - n) (_block_chainId branch) (_block_hash branch)
    void $ k blk
