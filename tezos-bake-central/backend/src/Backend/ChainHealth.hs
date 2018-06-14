{-# LANGUAGE LambdaCase #-}
module Backend.ChainHealth (scanForkInfo, validateForkyBlocks) where

import Control.Monad.Trans
import Data.Time
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Backend.NodeRPC

import Common.Schema
import Common.Verification

-- type ForkStatus = ForkStatusF (RpcResponse Void)
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

-- factorResponse :: RpcResponse a -> Either (RpcResponse Void) a
-- factorResponse (RpcResponse_HttpException bad) = Left $ RpcResponse_HttpException bad
-- factorResponse (RpcResponse_UnexpectedStatus bad) = Left $ RpcResponse_UnexpectedStatus bad
-- factorResponse (RpcResponse_NonJSON clue bad) = Left $ RpcResponse_NonJSON clue bad
-- factorResponse (RpcResponse_Success ok) = Right ok

scanForkInfo :: MonadIO m => UTCTime -> Report -> Node -> m [ForkInfo]
scanForkInfo now rpt node = do
  httpMgr <- liftIO $ newManager tlsManagerSettings
  let ctx = NodeRPCContext httpMgr $ _node_address node -- "http://127.0.0.1:18731"
  -- traverse (flip runReaderT ctx . checkChainHealth now 30) $ concat [_report_baked rpt, _report_last_seen rpt]
  runNodeRPCT ctx . mapM (checkChainHealth now 30) $ _report_baked rpt

checkChainHealth
  :: ( Monad m , MonadTezosNode m )
  => UTCTime
  -> Int -- ^ max unseen age, in seconds
  -> Baked
  -> m ForkInfo
checkChainHealth now delay seenBaked = do
    let _seenBlockLevel = blockLevel seenBaked
    addr <- nodeAddress
    (level, status) <- nodeRPC (Block headId) >>= \case
      Left bad -> return (Nothing, ForkStatus_BadNode bad)
      Right headInfo -> do
        status <- nodeRPC (Block $ blockHashId $ _bakedEvent_hash $ _event_detail seenBaked) >>= \case
          Left (RpcError_UnexpectedStatus 404 _) -> do
            let maxTime = addUTCTime (- fromIntegral delay) now
            return $ if _event_time seenBaked >= maxTime
              then ForkStatus_TooNew
              else ForkStatus_TooOld
          Left bad -> do
            return $ ForkStatus_BadNode bad
          Right seen -> do
            let ancestorBlockHash = BlockId (BlockIdHash_BlockHash $ _blockInfo_hash headInfo) (Just $ _blockInfo_level headInfo - _blockInfo_level seen)
            nodeRPC (Block ancestorBlockHash) >>= \case
              Left bad -> return $ ForkStatus_BadNode bad
              Right ancestor -> do
                return $ if _blockInfo_predecessor seen == _blockInfo_predecessor ancestor
                  then ForkStatus_Good
                  else ForkStatus_Forked
        return (Just $ _blockInfo_level headInfo, status)
    return $ ForkInfo (Node addr level) status seenBaked
