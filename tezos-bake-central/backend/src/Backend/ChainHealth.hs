{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.ChainHealth (scanForkInfo) where

import Control.Concurrent.STM (atomically)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader (MonadReader)
import Data.Time (UTCTime)
import Safe (maximumByMay)

import Backend.CachedNodeRPC
import Backend.STM (atomicallyWith)
import Common.Schema
import Common.Verification
import ExtraPrelude
import Tezos.Types

-- problem:: many baked blocks do not appear on chain
-- problem:: any parent of seen blocks do not appear on chain
-- problem:: blocks are not seen frequently

-- rough sketch,
-- %seen <- consider some block (say, the most recent, seen block or the most recent baked block)
-- %lvl, %parent <-  ask the node for %seen level, and the ID of its parent. (level - 1)
-- %1 ask the same node for the head and its level (level')
-- %2 ask the same node for head$(level' - level - 1)
-- if %0 != %2; sulk

checkChainHealth
  :: (Monad m , MonadIO m, MonadReader s m, HasNodeDataSource s, BlockLike b)
  => UTCTime
  -> Int -- ^ max unseen age, in seconds
  -> b
  -> m ForkInfo
checkChainHealth _now _delay seenBaked = do
  nds <- asks (^. nodeDataSource)
  seen <- runExceptT $ do
    -- try really hard to get seenBaked into history
    seen <- nodeQueryDataSource $ NodeQuery_Block (seenBaked ^. hash)
    -- look for the head to give the newly seen block a chance to become the head
    headBlock :: WithProtocolHash VeryBlockLike
      <- maybe (throwError $ ForkStatus_BadNode CacheError_NotEnoughHistory) pure
         =<< liftIO (atomically $ dataSourceHead nds)
    ancestor <- maybe (throwError ForkStatus_Forked) pure =<< atomicallyWith (branchPoint (headBlock ^. hash) (seenBaked ^. hash))
    -- TODO: compare the time between now and the blocks we're looking at to throw ForkStatus_Too{Old,New}
    if (seen ^. predecessor) == (ancestor ^. predecessor)
      then return () -- ForkStatus_Good
      else throwError ForkStatus_Forked
  return $ ForkInfo seen (seenBaked ^. timestamp) (seenBaked ^. hash)

scanForkInfo :: forall m r.
  ( MonadIO m
  , MonadReader r m, HasNodeDataSource r
  )
  => UTCTime -> Report -> m [ForkInfo]
scanForkInfo now rpt = do
  let
    check :: forall b. BlockLike b => b -> m ForkInfo
    check = checkChainHealth now 30
  baked <- traverse check $ maximumByMay (compare `on` _event_time) $ _report_baked rpt
  seen <- traverse check $ maximumByMay (compare `on` _event_time) $ _report_seen rpt
  return $ catMaybes [baked, seen]
