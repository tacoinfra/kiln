{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.ChainHealth (scanForkInfo, obtainNode) where

import Control.Monad (void)
import Control.Lens (view, (^.))
import Control.Monad.Error (MonadError, throwError, catchError)
import Control.Monad.Except (runExceptT)
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

-- problem:: many baked blocks do not appear on chain
-- problem:: any parent of seen blocks do not appear on chain
-- problem:: blocks are not seen frequently

-- rough sketch,
-- %seen <- consider some block (say, the most recent, seen block or the most recent baked block)
-- %lvl, %parent <-  ask the node for %seen level, and the ID of its parent. (level - 1)
-- %1 ask the same node for the head and its level (level')
-- %2 ask the same node for head$(level' - level - 1)
-- if %0 != %2; sulk

scanForkInfo :: MonadIO m => Http.Manager -> UTCTime -> Report -> Node -> m [ForkInfo]
scanForkInfo httpMgr now rpt node = do
  let addr = _node_address node
  let ctx = NodeRPCContext httpMgr addr
  flip runReaderT ctx $ traverse (checkChainHealth now 30) $ catMaybes
    [ fmap fromBaked $ maximumByMay (compare `on` _event_time) $ _report_baked rpt
    , fmap fromSeen $ maximumByMay (compare `on` _event_time) $ _report_seen rpt
    ]

fromBaked :: Event BakedEvent -> ChainHealthBlock
fromBaked e = ChainHealthBlock (_event_time e) (_bakedEvent_hash $ _event_detail e)

fromSeen :: Event SeenEvent -> ChainHealthBlock
fromSeen e = ChainHealthBlock (_event_time e) (_seenEvent_hash $ _event_detail e)

data ChainHealthBlock = ChainHealthBlock
  { _chainHealthBlock_time :: UTCTime
  , _chainHealthBlock_blockHash :: BlockHash
  }
checkChainHealth
  :: (Monad m , MonadIO m, MonadReader s m, HasNodeRPC s)
  => UTCTime
  -> Int -- ^ max unseen age, in seconds
  -> ChainHealthBlock
  -> m ForkInfo
checkChainHealth now delay seenBaked = do
  addr <- asks (_nodeRPCContext_node . view nodeRPCContext)
  status' <- runExceptT $ do
    (headInfo, node) <- obtainNode
    seen <- (nodeRPC (RBlock $ blockHashId $ _chainHealthBlock_blockHash seenBaked)) `catchError` \case
      ForkStatus_BadNode (RpcError_UnexpectedStatus 404 _) -> 
        let maxTime = addUTCTime (- fromIntegral delay) now
        in if _chainHealthBlock_time seenBaked >= maxTime
           then throwError ForkStatus_TooNew
           else throwError ForkStatus_TooOld
      bad -> throwError bad
    let ancestorBlockHash = blockHashIdPred
          (_block_hash headInfo)
          (headInfo ^. block_header . blockHeader_level
           - seen ^. block_header . blockHeader_level)
    ancestor <- nodeRPC (RBlock ancestorBlockHash)
    if (seen ^. block_header . blockHeader_predecessor)
        == (ancestor ^. block_header . blockHeader_predecessor)
      then return node -- ForkStatus_Good
      else throwError ForkStatus_Forked
  let status = void $ status'
  let node = either (const $ mkNode addr) id status'
  return $ ForkInfo node status (_chainHealthBlock_time seenBaked) (_chainHealthBlock_blockHash seenBaked)


-- Obtains a Node datastructure for the node specified by the environment, and a head block, if successful
-- TODO: This won't work in the typical case of node rpc on localhost with
-- monitor on a different host.  We'll leave it for now since it's "useful",
-- but this should probably be reported by the client rather than queried by
-- the monitor
obtainNode ::
  ( MonadIO m
  , MonadReader s m, HasNodeRPC s
  , MonadError e m, AsRpcError e
  ) => m (Block, Node)
obtainNode = do
  addr <- asks (_nodeRPCContext_node . view nodeRPCContext)
  headInfo <- nodeRPC (RBlock headId)
  connections <- nodeRPC RConnections
  networkStat <- nodeRPC RNetworkStat
  return (headInfo, Node
    { _node_address = addr
    , _node_identity = Nothing -- TODO
    , _node_headLevel = Just $ headInfo ^. block_header . blockHeader_level
    , _node_headBlockHash = Just $ headInfo ^. block_hash
    , _node_peerCount = Just $ connections
    , _node_networkStat = networkStat
    , _node_fitness = Just $ headInfo ^. block_header . blockHeader_fitness
    , _node_deleted = False
    })

