{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Control.Monad.Except
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Semigroup ((<>))

import Tezos.NodeRPC

main :: IO ()
main = do
  httpMgr <- liftIO $ newManager tlsManagerSettings
  let ctx = NodeRPCContext httpMgr "http://127.0.0.1:8732"
  let step :: Show a => NodeRPCRequest a -> IO ()
      step x = (runReaderT (runExceptT $ nodeRPC x) ctx) >>= \case
          Left (RpcError_HttpException bad) -> error $ show bad
          Left (RpcError_UnexpectedStatus code bad) -> error $ (show code <> show bad)
          Left (RpcError_NonJSON clue bad) -> error $ (clue <> "\n" <> show bad)
          Right ok -> print ok
  putStrLn "head" >> (step $ RBlock headId)
  putStrLn "constants" >> (step $ RProtoConstants headId)
