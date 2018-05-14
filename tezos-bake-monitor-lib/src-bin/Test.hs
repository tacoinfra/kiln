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
  let step :: Show a => NodeRPCRequest a -> IO ()
      step x = (runNodeRPCT ctx . nodeRPC $ x) >>= \case
          RpcResponse_HttpException bad -> error $ show bad
          RpcResponse_UnexpectedStatus bad -> error $ show bad
          RpcResponse_NonJSON clue bad -> error $ (clue <> "\n" <> show bad)
          RpcResponse_Success ok -> print ok
  putStrLn "head" >> (step $ Block $ BlockHash "head")
  putStrLn "constants" >> step ProtoConstants
