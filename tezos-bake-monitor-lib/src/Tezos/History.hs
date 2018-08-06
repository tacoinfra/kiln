{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Tezos.History where

import Control.Lens.TH
import Control.Lens -- (Lens, view, (.=))
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
import qualified Data.Sequence as Seq

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

makeLenses 'CachedHistory

emptyCache :: CachedHistory a
emptyCache = CachedHistory Set.empty Map.empty

class HasCachedHistory s t a b | s -> a, t -> b where
  cachedHistory :: Lens s t (CachedHistory a) (CachedHistory b)

instance HasCachedHistory (CachedHistory a) (CachedHistory b) a b where
  cachedHistory = id

-- add a block to cached history.  If there are multipe blocks between the
-- added block and the deepest allowed root, the summary for those blocks will
-- be mempty
accumHistory
  ::
  ( BlockLike b
  , MonadIO m
  , MonadState s m, Monoid a, HasCachedHistory s s a a
  , MonadReader r m, HasNodeRPC r
  , MonadError e m, AsRpcError e
  )
  => ChainId -> RawLevel -> (forall b0. BlockLike b0 => b0 -> a) -> b -> m a
accumHistory chainId minLevel f blk = do
  let blkHash = (view hash blk)
  let predHash = (view predecessor blk)
  let chainIdParam = DynamicParamChainId_ChainId chainId

  CachedHistory _branches blocks <- gets $ view cachedHistory

  -- extend a branch to include the new block.
  case Map.lookup predHash blocks of
    Just _branch -> return () --
    Nothing -> when (not $ view level blk > minLevel) $ do
      -- we will now proceed to restore the missing history
      let levels = (view level blk) - minLevel
      result <- nodeRPC $ RBlocks chainIdParam levels $ Set.singleton blkHash
      case Map.lookup blkHash result of
        Nothing -> throwError $ (^. re asRpcError) $ RpcError_UnexpectedStatus 404 "node did not return a branch containing requested block"
        Just descendents -> do
          -- make sure we have a root node
          let rootHash = Seq.index (blkHash <| descendents) (length descendents)
          rootBlk <- nodeRPC $ RBlock $ blockHashId' chainId rootHash
          cachedHistory %= accumHistoryImpl blkHash predHash (f rootBlk)
          -- scan insert the intermediate nodes
          let preds = Seq.drop 1 $ Seq.reverse descendents
          let blks = Seq.drop 1 $ preds |> blkHash
          for_ (Seq.zipWith accumHistoryImpl blks preds) $ \accum -> do
            cachedHistory %= accum mempty
          -- insert the top node

  cachedHistory %= accumHistoryImpl blkHash predHash (f blk)
  gets $ LCA.measure . (Map.! blkHash) . view (cachedHistory . cachedHistory_blocks)

accumHistoryImpl
  :: Monoid a => BlockHash -> BlockHash -> a -> CachedHistory a -> CachedHistory a
accumHistoryImpl blkHash predHash acc c = case Map.lookup blkHash (_cachedHistory_blocks c) of
  Just _ -> c
  Nothing -> CachedHistory
      { _cachedHistory_blocks = Map.insert blkHash newPath $ blocks
      , _cachedHistory_branches = Set.delete predHash . Set.insert blkHash $ branches
      }
    where
      blocks = _cachedHistory_blocks c
      branches = _cachedHistory_branches c
      newPath = LCA.cons blkHash acc $ maybe LCA.empty id $ Map.lookup predHash blocks

accumBalance :: MonadState Balances m => Block -> m ()
accumBalance = modify . (<>) . getBalanceChanges

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
