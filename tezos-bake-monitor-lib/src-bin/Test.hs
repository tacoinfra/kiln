{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Control.Monad.Except
import Data.Foldable
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Semigroup ((<>))
import System.Environment
import qualified Data.Text as T

import System.ProgressBar

import Tezos.NodeRPC
import Tezos.Types


main :: IO ()
main = do
  nodeAddr:_ <- getArgs
  httpMgr <- liftIO $ newManager tlsManagerSettings
  let ctx = NodeRPCContext httpMgr $ T.pack nodeAddr
  let step :: Show a => NodeRPCRequest a -> IO a
      step x = (runReaderT (runExceptT $ nodeRPC x) ctx) >>= \case
          Left (RpcError_HttpException bad) -> error $ ("\n" <>) $ show bad
          Left (RpcError_UnexpectedStatus code bad) -> error $ ("\n" <>)$ (show code <> show bad)
          Left (RpcError_NonJSON clue bad) -> error $ ("\n" <>) $ (clue <> "\n" <> show bad)
          Right ok -> return ok
  headBlk <- (step $ RBlock headId)
  let headLvl = _blockHeader_level $ _block_header headBlk
  flip traverse_ [3 .. headLvl] $ \n -> do
    blk <- step $ RBlock (blockHashIdPred (_block_hash headBlk) (headLvl - n))
    autoProgressBar (const "scan") (const $ show $ _block_hash blk) 80 (Progress (fromIntegral n) (fromIntegral headLvl))
  void $ putStrLn "constants" >> (step $ RProtoConstants headId)
