{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend.ChainHealth (scanForkInfo, validateForkyBlocks) where

import Common.Schema
import Tezos.BakeMonitor.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Data.Either.Validation
import Control.Monad.Trans
import Data.Time
import Tezos.NodeRPC
import Network.HTTP.Types.Status(Status(..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Monoid

-- problem:: many baked blocks do not appear on chain
-- problem:: any parent of seen blocks do not appear on chain
-- problem:: blocks are not seen frequently

-- rough sketch, 
-- %seen <- consider some block (say, the most recent, seen block or the most recent baked block)
-- %lvl, %parent <-  ask the node for %seen level, and the ID of its parent. (level - 1)
-- %1 ask the same node for the head and its level (level')
-- %2 ask the same node for head$(level' - level - 1)
-- if %0 != %2; sulk

data ForkInfo = ForkInfo
  { _forkInfo_node :: Node
  , _forkInfo_forkStatus :: ForkStatus
  , _forkInfo_baked :: Baked
  }

data ForkStatus
  = ForkStatus_Good
  | ForkStatus_TooNew
  | ForkStatus_TooOld
  | ForkStatus_Forked
  | ForkStatus_BadNode (RpcResponse Void)

showForkStatus :: ForkStatus -> Text
showForkStatus = T.pack . \case
  ForkStatus_Good -> "good"
  ForkStatus_TooNew -> "new"
  ForkStatus_TooOld -> "block not in chain"
  ForkStatus_Forked -> "forked"
  ForkStatus_BadNode _ -> "no response from node"

onBadForkState :: (ForkInfo -> a) -> ForkInfo -> Validation a ()
onBadForkState k fi = case _forkInfo_forkStatus fi of
  ForkStatus_TooOld -> Failure $ k fi
  ForkStatus_Forked -> Failure $ k fi
  _ -> Success ()

showBadFork :: ForkInfo -> [Error]
showBadFork (ForkInfo node status baked) = pure $ Error (_baked_time baked) $ T.concat
          [ "node: ", _node_address node
          , " BAKER STATE:" , showForkStatus status
          , " for block:", unBlockHash $ _baked_hash baked
          , " @ ",  T.pack $ show $ _baked_time baked
          , "\n"
          ]

validateForkyBlocks :: Applicative f => ([Error] -> f ()) -> [ForkInfo] -> f ()
validateForkyBlocks f xs = case traverse (onBadForkState (showBadFork)) xs of
  Success _ -> pure ()
  Failure bad -> f bad

factorResponse :: RpcResponse a -> Either (RpcResponse Void) a
factorResponse (RpcResponse_HttpException bad) = Left $ RpcResponse_HttpException bad
factorResponse (RpcResponse_UnexpectedStatus bad) = Left $ RpcResponse_UnexpectedStatus bad
factorResponse (RpcResponse_NonJSON clue bad) = Left $ RpcResponse_NonJSON clue bad
factorResponse (RpcResponse_Success ok) = Right ok

scanForkInfo :: MonadIO m => UTCTime -> Report -> Node -> m [ForkInfo]
scanForkInfo now rpt node = do
  httpMgr <- liftIO $ newManager tlsManagerSettings
  let ctx = NodeRPCContext httpMgr $ _node_address node -- "http://127.0.0.1:18731"
  -- traverse (flip runReaderT ctx . checkChainHealth now 30) $ concat [_report_lastBaked rpt, _report_last_seen rpt]
  runNodeRPCT ctx . mapM (checkChainHealth now 30) $ _report_lastBaked rpt

checkChainHealth
  :: MonadIO m
  => UTCTime
  -> Int -- ^ max unseen age, in seconds
  -> Baked
  -> NodeRPCT m ForkInfo
checkChainHealth now delay seenBaked = do
    addr <- nodeAddress
    (level, status) <- (factorResponse <$> (doRPC $ Block $ BlockHash "head")) >>= \case
      Left bad -> (liftIO $ putStrLn "no head") >> (return (Nothing, ForkStatus_BadNode bad))
      Right headInfo -> do
        -- liftIO $ putStrLn ("head:" <> show head)
        status <- (factorResponse <$> (doRPC $ Block $ _baked_hash seenBaked)) >>= \case
          Left (RpcResponse_UnexpectedStatus (Status 404 _)) -> do
            let maxTime = addUTCTime (- fromIntegral delay) now
            return $ if (_baked_time seenBaked >= maxTime)
              then ForkStatus_TooNew
              else ForkStatus_TooOld
          Left bad -> do
            liftIO $ putStrLn "not seen"
            return $ ForkStatus_BadNode bad
          Right seen -> do
            -- liftIO $ putStrLn ("seen:" <> show seen)
            let ancestorBlockHash = BlockHash $ (unBlockHash $ _blockInfo_hash headInfo) <> "~" <> T.pack (show (_blockInfo_level headInfo - _blockInfo_level seen))
            (factorResponse <$> (doRPC $ Block $ ancestorBlockHash)) >>= \case
              Left bad -> (liftIO $ putStrLn "no ancestor") >> (return $ ForkStatus_BadNode bad)
              Right ancestor -> do
                -- liftIO $ putStrLn ("ancestor:" <> show ancestor)
                return $ if _blockInfo_predecessor seen == _blockInfo_predecessor ancestor
                  then ForkStatus_Good
                  else ForkStatus_Forked
        return (Just $ _blockInfo_level headInfo, status)
    return $ ForkInfo (Node addr level) status seenBaked
