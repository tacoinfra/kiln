{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Semigroup ((<>))

import Tezos.BakeMonitor.Types
import Tezos.NodeRPC

main :: IO ()
main = do
  httpMgr <- liftIO $ newManager tlsManagerSettings
  let ctx = NodeRPCContext httpMgr "http://127.0.0.1:18731"
  resp <- runNodeRPCT ctx $ nodeRPC (Block $ BlockHash "head")
  case resp of
    RpcResponse_HttpException bad -> error $ show bad
    RpcResponse_UnexpectedStatus bad -> error $ show bad
    RpcResponse_NonJSON clue bad -> error $ (clue <> "\n" <> show bad)
    RpcResponse_Success ok -> print ok
