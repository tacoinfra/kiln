{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.ChainHealth (scanForkInfo) where

import Control.Monad (void)
import Control.Lens (view, (^.))
import Control.Monad.Except (MonadError, throwError, catchError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Data.Function (on)
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Time (UTCTime, addUTCTime)
import qualified Network.HTTP.Client as Http
import Safe (maximumByMay)
import Say (say)

import Tezos.NodeRPC
import Tezos.Lenses
import Tezos.Types
import Common (tshow)
import Common.Schema
import Common.Verification
import Backend.CachedNodeRPC

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
checkChainHealth now delay seenBaked = do
  seen <- runExceptT $ do
    -- try really hard to get seenBaked into history
    seen <- nodeQueryDataSource $ NodeQuery_Block $ seenBaked ^. hash
    -- look for the head to give the newly seen block a chance to become the head
    head :: VeryBlockLike <- maybe (throwError $ ForkStatus_BadNode $ RpcError_HttpException "NO HISTORY") pure =<< dataSourceHead
    ancestor <- maybe (throwError $ ForkStatus_Forked) pure =<< branchPoint (head ^. hash) (seenBaked ^. hash)
    -- TODO: compare the time between now and the blocks we're looking at to throw ForkStatus_Too{Old,New}
    if (seen ^. predecessor) == (ancestor ^. predecessor)
      then return () -- ForkStatus_Good
      else throwError ForkStatus_Forked
  -- let node = either (const $ mkNode addr) id status'
  return $ ForkInfo seen (seenBaked ^. timestamp) (seenBaked ^. hash)

scanForkInfo :: forall m r.
  ( MonadIO m
  , MonadReader r m, HasNodeDataSource r
  )
  => UTCTime -> Report -> m [ForkInfo]
scanForkInfo now rpt = do
  let
    check :: forall b. BlockLike b => b -> m ForkInfo
    check b = checkChainHealth now 30 b
  baked <- traverse check $ maximumByMay (compare `on` _event_time) $ _report_baked rpt
  seen <- traverse check $ maximumByMay (compare `on` _event_time) $ _report_seen rpt
  return $ catMaybes [baked, seen]

