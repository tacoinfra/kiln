{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.History where

import Control.DeepSeq (NFData)
import Control.Lens (Lens, ifor_, view, (%=), (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Except
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Sequence ((<|))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import GHC.Generics (Generic)

import qualified Data.LCA.Online.Polymorphic as LCA

import Tezos.NodeRPC
import Tezos.NodeRPC.Network
import Tezos.NodeRPC.Sources
import Tezos.Types

data CachedHistory a = CachedHistory
  -- what i really need here is a cover tree (or some other metric index)
  -- a plausible alternative is to only keep the fittest n branches
  -- investigate: https://github.com/mikeizbicki/HLearn/blob/master/src/HLearn/Data/SpaceTree/CoverTree.hs
  { _cachedHistory_branches :: !(Map BlockHash VeryBlockLike)
  , _cachedHistory_blocks :: !(Map BlockHash (LCA.Path BlockHash a))
  , _cachedHistory_minLevel :: !RawLevel
  } deriving (Show, Typeable, Generic)
instance NFData a => NFData (CachedHistory a)

makeLenses 'CachedHistory

emptyCache :: CachedHistory a
emptyCache = CachedHistory Map.empty Map.empty 1

class HasCachedHistory s t a b | s -> a, t -> b where
  cachedHistory :: Lens s t (CachedHistory a) (CachedHistory b)

instance HasCachedHistory (CachedHistory a) (CachedHistory b) a b where
  cachedHistory = id


type ProgressFn f = BlockHash -> BlockHash -> Int -> Int -> f ()
-- add a block to cached history.  If there are multipe blocks between the
-- added block and the deepest allowed root, the summary for those blocks will
-- be mempty
accumHistory
  ::
  ( BlockLike b
  , MonadIO m, MonadLogger m
  , MonadState s m, Monoid a, HasCachedHistory s s a a
  , MonadReader r m, HasPublicNodeContext r
  , MonadError e m, AsPublicNodeError e
  )
  => ProgressFn m -> ChainId -> (forall b0. BlockLike b0 => b0 -> a) -> b -> m a
accumHistory progress chainId f blk = do
  minLevel <- gets $ _cachedHistory_minLevel . view cachedHistory
  let blkHash = view hash blk
  let predHash = view predecessor blk
  -- check to see if we already have history for the predecessor block
  gets (Map.lookup predHash . _cachedHistory_blocks . view cachedHistory) >>= \case
    -- we have the predecessor, nothing more to do.
    Just _branch -> do
      return ()

    -- we don't have the predecessor. If the requested block has a low
    -- enough level, we can use it as a root, otherwise we need to restore a
    -- full branch of blocks.
    Nothing -> when (view level blk > minLevel) $ do
      -- we will now proceed to restore the missing history.  We ask a node for
      -- enough block-hashes to reach from the new block to "the root" at
      -- minLevel
      let levels = view level blk - minLevel
      branches <- gets $ _cachedHistory_branches . view cachedHistory
      descendents <- getHistory chainId blk levels $ Map.keysSet branches
      -- make sure we have a root node
      let rootHash = Seq.index (blkHash <| descendents) (length descendents) -- 1
      -- log ("got branch", length descendents, "expect", levels, rootHash)
      rootBlk <- getBlock chainId rootHash
      cachedHistory %= accumHistoryImpl (rootBlk ^. hash) (rootBlk ^. predecessor) (f rootBlk)
      -- scan insert the intermediate nodes
      let preds = Seq.reverse descendents -- [1,2,3,4]
      let blks = Seq.drop 1 preds -- [2,3,4]
      let newBranchLength = length blks
      ifor_ (Seq.zip blks preds) $ \i (blkHash', predhash') -> do
        progress blkHash blkHash' i newBranchLength
        cachedHistory %= accumHistoryImpl blkHash' predhash' mempty
          -- insert the top node

  if view level blk >= minLevel
    then do
      cachedHistory %= exposeBranch blk . accumHistoryImpl blkHash predHash (f blk)
      blkBranch <- gets $ (Map.! blkHash) . view (cachedHistory . cachedHistory_blocks)
      -- log ("after", length $ LCA.toList blkBranch)
      return $ LCA.measure blkBranch
    else return mempty

exposeBranch :: BlockLike b => b -> CachedHistory a -> CachedHistory a
exposeBranch blk c = c { _cachedHistory_branches
  = Map.delete (blk ^. predecessor)
  $ Map.insert (blk ^. hash) (mkVeryBlockLike blk)
  $ _cachedHistory_branches c }

accumHistoryImpl
  :: Monoid a => BlockHash -> BlockHash -> a -> CachedHistory a -> CachedHistory a
accumHistoryImpl blkHash predHash acc c = case Map.lookup blkHash (_cachedHistory_blocks c) of
  Just _ -> c -- why dont we replace acc?  It'd have to be updated in every path that contains it, O(n log h) work.  this way we're only O(log n)
  Nothing -> CachedHistory
      { _cachedHistory_blocks = Map.insert blkHash newPath blocks
      , _cachedHistory_branches = Map.delete predHash branches
      , _cachedHistory_minLevel = _cachedHistory_minLevel c
      }
    where
      blocks = _cachedHistory_blocks c
      branches = _cachedHistory_branches c
      newPath = LCA.cons blkHash acc $ maybe LCA.empty id $ Map.lookup predHash blocks

accumBalance :: MonadState Balances m => Block -> m ()
accumBalance = modify . (<>) . getBalanceChanges

scanBranch ::
  ( MonadIO m, MonadLogger m
  , MonadReader ctx m , HasNodeRPC ctx
  , MonadError e m, AsRpcError e
  )
  => Block -> RawLevel -> RawLevel -> (Block -> m a) -> m ()
scanBranch branch start stop k = do
  let headLvl = _blockHeader_level $ _block_header branch
  for_ [start .. stop] $ \n -> do
    blk <- nodeRPC $ rBlockPred (_block_chainId branch) (_block_hash branch) (headLvl - n)
    void $ k blk
