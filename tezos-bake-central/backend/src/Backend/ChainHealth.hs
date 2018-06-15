{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.ChainHealth (scanForkInfo, validateForkyBlocks, obtainNode) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Semigroup ((<>))
import Data.Time (UTCTime, addUTCTime)
import qualified Network.HTTP.Client as Http

import Backend.NodeRPC
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
  let ctx = NodeRPCContext httpMgr $ _node_address node -- "http://127.0.0.1:18731"
  -- traverse (flip runReaderT ctx . checkChainHealth now 30) $ concat [_report_baked rpt, _report_last_seen rpt]
  runNodeRPCT ctx . mapM (checkChainHealth now 30) $ _report_baked rpt

checkChainHealth
  :: ( Monad m , MonadTezosNode m, MonadIO m )
  => UTCTime
  -> Int -- ^ max unseen age, in seconds
  -> Baked
  -> m ForkInfo
checkChainHealth now delay seenBaked = do
    (mHeadBlockInfo, node) <- obtainNode
    status <- case mHeadBlockInfo of
      Left bad -> return $ ForkStatus_BadNode bad
      Right headInfo ->
        nodeRPC (RBlock $ blockHashId $ _bakedEvent_hash $ _event_detail seenBaked) >>= \case
          Left (RpcError_UnexpectedStatus 404 _) -> do
            let maxTime = addUTCTime (- fromIntegral delay) now
            return $ if _event_time seenBaked >= maxTime
              then ForkStatus_TooNew
              else ForkStatus_TooOld
          Left bad -> do
            return $ ForkStatus_BadNode bad
          Right seen -> do
            let ancestorBlockHash = BlockId (BlockIdHash_BlockHash $ _blockInfo_hash headInfo)
                                            (Just $ _blockInfo_level headInfo - _blockInfo_level seen)
            nodeRPC (RBlock ancestorBlockHash) >>= \case
              Left bad -> do
                liftIO $ putStrLn "no ancestor"
                return $ ForkStatus_BadNode bad
              Right ancestor -> do
                return $ if _blockInfo_predecessor seen == _blockInfo_predecessor ancestor
                  then ForkStatus_Good
                  else ForkStatus_Forked
    return $ ForkInfo node status seenBaked

-- Obtains a Node datastructure for the node specified by the environment, and a head block, if successful
obtainNode :: (MonadIO m, MonadTezosNode m) => m (RpcResponse BlockInfo, Node)
obtainNode = do
  addr <- nodeAddress
  (info, level) <- nodeRPC (RBlock headId) >>= \case
    Left bad -> do
      liftIO $ putStrLn "Couldn't get head block."
      return (Left bad, Nothing)
    Right headInfo -> do
      -- liftIO $ putStrLn ("head:" <> show head)
      return (Right headInfo, Just $ _blockInfo_level headInfo)
  connections <- nodeRPC RConnections >>= \case
    Left _bad -> do
      liftIO $ putStrLn $ "Couldn't get connection information for node " <> show addr
      return Nothing
    Right n -> return (Just n)
  networkStat <- nodeRPC RNetworkStat >>= \case
    Left _bad -> do
      liftIO $ putStrLn $ "Couldn't get network status information for node " <> show addr
      return (NetworkStat 0 0 0 0)
    Right ns -> return ns
  return (info, Node { _node_address = addr, _node_headLevel = level, _node_peerCount = connections, _node_networkStat = networkStat})
