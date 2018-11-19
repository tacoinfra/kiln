{-# LANGUAGE BangPatterns #-}
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

import Control.Concurrent.MVar (MVar, modifyMVar, readMVar)
import Control.DeepSeq (NFData)
import Control.Lens (Lens, ifor_, view, (%=), (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Except
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup ((<>))
import Data.Sequence ((<|))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
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
  cachedHistory :: Lens s t (MVar (CachedHistory a)) (MVar (CachedHistory b))

instance HasCachedHistory (MVar (CachedHistory a)) (MVar (CachedHistory b)) a b where
  cachedHistory = id


type ProgressFn f = BlockHash -> BlockHash -> Int -> Int -> f ()


-- | Data type with the right instances for 'MonadReader' constraints required by 'accumHistory'.
data AccumHistoryContext a = AccumHistoryContext
  { _accumHistoryContext_cachedHistory :: !(MVar (CachedHistory a))
  , _accumHistoryContext_publicNodeContext :: !PublicNodeContext
  } deriving (Typeable, Generic)
makeLenses 'AccumHistoryContext
instance HasCachedHistory (AccumHistoryContext a) (AccumHistoryContext b) a b where
  cachedHistory = accumHistoryContext_cachedHistory
instance HasPublicNodeContext (AccumHistoryContext a) where
  publicNodeContext = accumHistoryContext_publicNodeContext
instance HasNodeRPC (AccumHistoryContext a) where
  nodeRPCContext = accumHistoryContext_publicNodeContext . nodeRPCContext

-- add a block to cached history.  If there are multipe blocks between the
-- added block and the deepest allowed root, the summary for those blocks will
-- be mempty
accumHistory
  :: forall a b e r m.
    ( BlockLike b
    , MonadIO m, MonadLogger m
    , MonadReader r m, Monoid a, HasCachedHistory r r a a, HasPublicNodeContext r
    , MonadError e m, AsPublicNodeError e
    )
  => ProgressFn IO -> ChainId -> (forall b0. BlockLike b0 => b0 -> a) -> b -> m a
accumHistory progress chainId f blk = do
  cacheHistoryVar <- asks (view cachedHistory)
  history <- liftIO $ readMVar cacheHistoryVar
  let minLevel = _cachedHistory_minLevel history
  let blkHash = view hash blk
  let predHash = view predecessor blk

  let
    updateBranches :: forall n. MonadState (CachedHistory a) n => n a
    updateBranches = if view level blk >= minLevel
      then do
        modify $ exposeBranch blk . accumHistoryImpl blkHash predHash (f blk)
        hist :: CachedHistory a <- get
        let blkBranch = _cachedHistory_blocks hist Map.! blkHash
        return $ LCA.measure blkBranch
      else return mempty

  -- check to see if we already have history for the predecessor block
  if view level blk > minLevel && Map.notMember predHash (_cachedHistory_blocks history)
    then do
      -- we will now proceed to restore the missing history.  We ask a node for
      -- enough block-hashes to reach from the new block to "the root" at
      -- minLevel
      let !levels = view level blk - minLevel
      let !branches = _cachedHistory_branches history
      !descendents <- getHistory chainId blk levels $ Map.keysSet branches -- this gives, e.g. [4,3,2,1]

      -- make sure we have a root node
      let !rootHash = Seq.index (blkHash <| descendents) (length descendents) -- gets 1 from [blkHash,4,3,2,1]
      !rootBlk <- getBlock chainId rootHash

      -- scan insert the intermediate nodes
      let !preds = Seq.reverse descendents -- [1,2,3,4]
      let !blks = Seq.drop 1 preds -- [2,3,4]

      liftIO $ modifyMVar cacheHistoryVar $ \hist -> do
        fmap swap $ flip runStateT hist $ do
          modify $ accumHistoryImpl (rootBlk ^. hash) (rootBlk ^. predecessor) (f rootBlk)
          let newBranchLength = length blks
          ifor_ (Seq.zip blks preds) $ \i (blkHash', predhash') -> do
            liftIO $ progress blkHash blkHash' i newBranchLength
            modify $ accumHistoryImpl blkHash' predhash' mempty
          updateBranches
    else
      liftIO $ modifyMVar cacheHistoryVar $ \hist ->
        fmap swap $ runStateT updateBranches hist


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
