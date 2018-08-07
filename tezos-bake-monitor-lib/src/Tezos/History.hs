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
  -- let log x = liftIO $ print (blkHash, x)
  -- log ("begin", predHash)

  -- check to see if we already have history for the predecessor block
  (gets $ Map.lookup predHash . _cachedHistory_blocks . view cachedHistory) >>= \case
    -- we have the predecessor, nothing more to do.
    Just _branch -> do
      -- log "already have predecessor"
      return ()

    -- we don't have the predecessor. If the requested block has a low
    -- enough level, we can use it as a root, otherwise we need to restore a
    -- full branch of blocks.
    Nothing -> when (view level blk > minLevel) $ do
      -- log "need predecessor"
      -- we will now proceed to restore the missing history.  We ask a node for
      -- enough block-hashes to reach from the new block to "the root" at
      -- minLevel
      let levels = (view level blk) - minLevel
      result <- nodeRPC $ RBlocks chainIdParam levels $ Set.singleton blkHash
      case Map.lookup blkHash result of
        Nothing -> throwError $ (^. re asRpcError) $ RpcError_UnexpectedStatus 404 "node did not return a branch containing requested block"
        Just descendents -> do -- suposing something like {5:[4,3,2,1]}
          -- make sure we have a root node
          let rootHash = Seq.index (blkHash <| descendents) (length descendents) -- 1
          -- log ("got branch", length descendents, "expect", levels, rootHash)
          rootBlk <- nodeRPC $ RBlock $ blockHashId' chainId rootHash
          cachedHistory %= accumHistoryImpl (rootBlk ^. hash) (rootBlk ^. predecessor) (f rootBlk)
          -- scan insert the intermediate nodes
          let preds = Seq.reverse descendents -- [1,2,3,4]
          let blks = Seq.drop 1 $ preds -- [2,3,4]
          for_ (Seq.zip blks preds) $ \(blkHash', predhash') -> do
            -- log ("inserting",blkHash', predhash')
            cachedHistory %= accumHistoryImpl blkHash' predhash' mempty
          -- insert the top node

  cachedHistory %= accumHistoryImpl blkHash predHash (f blk)
  blkBranch <- gets $ (Map.! blkHash) . view (cachedHistory . cachedHistory_blocks)
  -- log ("after", fst <$> LCA.toList blkBranch)
  return $ LCA.measure blkBranch

accumHistoryImpl
  :: Monoid a => BlockHash -> BlockHash -> a -> CachedHistory a -> CachedHistory a
accumHistoryImpl blkHash predHash acc c = case Map.lookup blkHash (_cachedHistory_blocks c) of
  Just _ -> c -- why dont we replace acc?  It'd have to be updated in every path that contains it, O(n log h) work.  this way we're only O(log n)
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
