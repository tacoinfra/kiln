{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((^.))
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as Map
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Semigroup ((<>))
import System.Environment
import qualified Data.Text as T

import System.ProgressBar

import Tezos.NodeRPC
import Tezos.Types
import Tezos.History

import qualified Data.LCA.Online.Polymorphic as LCA


scanProgress :: MonadIO m => Block -> Block -> m ()
scanProgress branch this = do
  liftIO $ autoProgressBar (const "scan") (showProgress this) 80 
      (Progress
        (fromIntegral $ _blockHeader_level $ _block_header this)
        (fromIntegral $ _blockHeader_level $ _block_header branch))

showProgress :: Block -> Progress -> String
showProgress blk (Progress x y) = T.unpack $ T.concat
  [ T.pack $ show x
  , "/"
  , T.pack $ show y
  , "@"
  , toBase58Text (_block_hash blk)
  ]

-- onRPCError :: (HasNodeRPC ctx, MonadReader ctx m, MonadIO m) => NodeRPCRequest a -> m a
onRPCError :: RpcError -> a
onRPCError = \case
  RpcError_HttpException bad ->         error $ ("\n" <>) $ show bad
  RpcError_UnexpectedStatus code bad -> error $ ("\n" <>)$ (show code <> show bad)
  RpcError_NonJSON clue bad ->          error $ ("\n" <>) $ (clue <> "\n" <> show bad)
--     Right ok -> ok

accum :: Block -> StateT (CachedHistory Fitness) (ExceptT RpcError (ReaderT NodeRPCContext IO)) ()
accum = void . accumHistory "NetXdQprcVkpaWU" 2000 (^. fitness)-- getBalanceChanges

main :: IO ()
main = do
  nodeAddr:_ <- getArgs
  httpMgr <- liftIO $ newManager tlsManagerSettings
  let ctx = NodeRPCContext httpMgr $ T.pack nodeAddr
  runTest ctx $ do
    headBlk <- (nodeRPC $ RBlock headId)

    b <- flip execStateT emptyCache $ scanBranch headBlk 2000 2100 $ \blk -> do
      accum blk
      scanProgress headBlk blk
    let (xHash, xPath):_ = Map.toList ( _cachedHistory_blocks b )
    let xLevel :: Int = 2000 + (fromIntegral $ length xPath)
    xBlk <- nodeRPC $ RBlock $ blockHashId' (_block_chainId headBlk) xHash
    liftIO $ print $ [toBase58Text xHash, T.pack $ show xLevel, T.pack $ show $ _blockHeader_level $ _block_header xBlk]
    -- let tfBaker5 = "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r"
    -- liftIO $ putStrLn "bake5"
    -- step (RContract (branch 100) tfBaker5) >>= liftIO . print

    liftIO $ putStrLn "constants"
    void $ nodeRPC $ RProtoConstants headId

runTest :: NodeRPCContext -> ExceptT RpcError (ReaderT NodeRPCContext IO) () -> IO ()
runTest ctx action = either onRPCError id <$> runReaderT (runExceptT action) ctx

