{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Semigroup ((<>))

import Tezos.NodeRPC

main :: IO ()
main = do
  httpMgr <- liftIO $ newManager tlsManagerSettings
  let ctx = NodeRPCContext httpMgr "http://127.0.0.1:18731"
  let step :: Show a => NodeRPCRequest a -> IO ()
      step x = (runNodeRPCT ctx . nodeRPC $ x) >>= \case
          Left (RpcError_HttpException bad) -> error $ show bad
          Left (RpcError_UnexpectedStatus code bad) -> error $ (show code <> show bad)
          Left (RpcError_NonJSON clue bad) -> error $ (clue <> "\n" <> show bad)
          Right ok -> print ok
  putStrLn "head" >> (step $ RBlock "BLqHVrWsqEw6Qp3iPYzGc461nhkXGxGtwnf9P6Cqk4TTd7NgpXP")
  putStrLn "constants" >> step RProtoConstants
