{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Tezos.History where

import Control.Lens (Lens, view, (.=))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable
import Data.Map(Map)
import Data.Semigroup ((<>))
import Data.Set(Set)
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.LCA.Online.Polymorphic as LCA

import Tezos.NodeRPC
import Tezos.Types

data CachedHistory a = CachedHistory
  -- what i really need here is a cover tree (or some other metric index)
  -- a plausible alternative is to only keep the fittest n branches
  -- investigate: https://github.com/mikeizbicki/HLearn/blob/master/src/HLearn/Data/SpaceTree/CoverTree.hs
  { _cachedHistory_branches :: Set BlockHash
  , _cachedHistory_blocks :: Map BlockHash (LCA.Path BlockHash a)
  } deriving (Show, Typeable)

emptyCache :: CachedHistory a
emptyCache = CachedHistory Set.empty Map.empty

class HasCachedHistory s t a b | s -> a, t -> b where
  cachedHistory :: Lens s t (CachedHistory a) (CachedHistory b)

instance HasCachedHistory (CachedHistory a) (CachedHistory b) a b where
  cachedHistory = id

accumHistory
  :: (MonadState s m, Monoid a, HasCachedHistory s s a a)
  => (Block -> m [(BlockHash, a)]) -> (Block -> a) -> Block -> m ()
accumHistory bad f blk = do
  CachedHistory branches blocks <- gets $ view cachedHistory
  let hash = (_block_hash blk)
  let predHash = (_blockHeader_predecessor $ _block_header blk)
  branch' <- case Map.lookup predHash blocks of
    -- TODO: this does a linear scan of all known blocks looking for the best
    -- choice for sharing.  this is terrible.
    --
    -- we could go about limiting the number of branches we track (with, say, a
    -- fitness bounded max-heap) which would probably be fine for this
    -- application
    Nothing -> do
      newBranch <- LCA.fromList <$> bad blk
      let branchPaths = (blocks Map.!) <$> toList branches
      return $ case LCA.nearest newBranch branchPaths of
        Nothing -> newBranch
        Just neighbor -> LCA.graft neighbor newBranch
    Just branch -> return $ LCA.cons hash (f blk) branch

  let blocks' = Map.insert hash branch' blocks
  -- once in sync, add the new blk to branches, remove its predecessor
  let branches' = Set.delete predHash . Set.insert hash $ branches

  cachedHistory .= CachedHistory branches' blocks'

accumBalance :: MonadState Balances m => Block -> m ()
accumBalance = modify . (<>) . getBalanceChanges

bootstrapHistory ::
  ( MonadReader ctx m, HasNodeRPC ctx
  , MonadError e m, AsRpcError e
  , MonadIO m)
  => RawLevel -> Block -> m [BlockHash]
bootstrapHistory minLevel blk = do 
  let levels = (_blockHeader_level $ _block_header blk) - minLevel
  let blkHash = (_block_hash blk)
  result <- nodeRPC $ RBlocks (DynamicParamChainId_ChainId $ _block_chainId blk) levels $ Set.singleton blkHash
  case Map.lookup blkHash result of
    Nothing -> error "sulk"
    Just descendents -> return $ blkHash : toList descendents

scanBranch ::
  ( MonadIO m
  , MonadReader ctx m , HasNodeRPC ctx
  , MonadError e m, AsRpcError e
  )
  => Block -> RawLevel -> RawLevel -> (Block -> m a) -> m ()
scanBranch branch start stop k = do
  let headLvl = _blockHeader_level $ _block_header branch
  let branch' n = blockHashIdPred' (_block_chainId branch) (_block_hash branch) (headLvl - n)
  for_ [start .. stop] $ \n -> do
    blk <- nodeRPC $ RBlock $ branch' n
    void $ k blk
