{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.ChainHealth (scanForkInfo, obtainNode) where

import Control.Lens (view, (^.))
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

type ForkInfo = ForkInfoF RpcError


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
    (mHeadBlockInfo, node) <- obtainNode
    status <- case mHeadBlockInfo of
      Left bad -> return $ ForkStatus_BadNode bad
      Right headInfo ->
        runExceptT (nodeRPC (RBlock $ blockHashId $ _chainHealthBlock_blockHash seenBaked)) >>= \case
          Left (RpcError_UnexpectedStatus 404 _) -> do
            let maxTime = addUTCTime (- fromIntegral delay) now
            return $ if _chainHealthBlock_time seenBaked >= maxTime
              then ForkStatus_TooNew
              else ForkStatus_TooOld
          Left bad -> do
            return $ ForkStatus_BadNode bad
          Right seen -> do
            let ancestorBlockHash = blockHashIdPred
                  (_block_hash headInfo)
                  (headInfo ^. block_header . blockHeader_level
                   - seen ^. block_header . blockHeader_level)
            runExceptT (nodeRPC (RBlock ancestorBlockHash)) >>= \case
              Left bad -> do
                say "no ancestor"
                return $ ForkStatus_BadNode bad
              Right ancestor -> do
                return $ if
                    (seen ^. block_header . blockHeader_predecessor) ==
                    (ancestor ^. block_header . blockHeader_predecessor)
                  then ForkStatus_Good
                  else ForkStatus_Forked
    return $ ForkInfo node status (_chainHealthBlock_time seenBaked) (_chainHealthBlock_blockHash seenBaked)

-- Obtains a Node datastructure for the node specified by the environment, and a head block, if successful
-- TODO: This won't work in the typical case of node rpc on localhost with
-- monitor on a different host.  We'll leave it for now since it's "useful",
-- but this should probably be reported by the client rather than queried by
-- the monitor
obtainNode :: (MonadIO m, MonadReader s m, HasNodeRPC s) => m (RpcResponse Block, Node)
obtainNode = do
  addr <- asks (_nodeRPCContext_node . view nodeRPCContext)
  (info, level, headHash, fitness) <- runExceptT (nodeRPC (RBlock headId)) >>= \case
    Left bad -> do
      say "Couldn't get head block."
      return (Left bad, Nothing, Nothing, Nothing)
    Right headInfo -> do
      return
        ( Right headInfo
        , Just $ headInfo ^. block_header . blockHeader_level
        , Just $ headInfo ^. block_hash
        , Just $ headInfo ^. block_header . blockHeader_fitness
        )
  connections <- runExceptT (nodeRPC RConnections) >>= \case
    Left bad -> do
      say $ "Couldn't get connection information for node " <> tshow addr <> ": " <> tshow bad
      return Nothing
    Right n -> return (Just n)
  networkStat <- runExceptT (nodeRPC RNetworkStat) >>= \case
    Left bad -> do
      say $ "Couldn't get network status information for node " <> tshow addr <> ": " <> tshow bad
      return (NetworkStat 0 0 0 0)
    Right ns -> return ns
  return (info, Node
    { _node_address = addr
    , _node_identity = Nothing -- TODO
    , _node_headLevel = level
    , _node_headBlockHash = headHash
    , _node_peerCount = connections
    , _node_networkStat = networkStat
    , _node_fitness = fitness
    , _node_deleted = False
    })
