{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens ((^.))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment

import System.ProgressBar

import Tezos.History
import Tezos.NodeRPC
import Tezos.Types
import Tezos.NodeRPC.Sources


scanProgress :: MonadIO m => BlockHash -> BlockHash -> Int -> Int -> m ()
scanProgress _ currentHash i n = when (i `mod` 100 == 0) $ liftIO $ autoProgressBar (const "scan") (showProgress currentHash) 80
  (Progress (fromIntegral i) (fromIntegral n))

showProgress :: BlockHash -> Progress -> String
showProgress blk (Progress x y) = T.unpack $ T.concat
  [ T.pack $ show x
  , "/"
  , T.pack $ show y
  , "@"
  , toBase58Text blk
  ]

-- onRPCError :: (HasNodeRPC ctx, MonadReader ctx m, MonadIO m) => NodeRPCRequest a -> m a
onRPCError :: PublicNodeError -> a
onRPCError = \case
  PublicNodeError_FeatureNotSupported ->                           error "\nfeature not supported"
  PublicNodeError_RpcError (RpcError_HttpException bad) ->         error $ ("\n" <>) $ show bad
  PublicNodeError_RpcError (RpcError_UnexpectedStatus code bad) -> error $ ("\n" <>) (show code <> show bad)
  PublicNodeError_RpcError (RpcError_NonJSON clue bad) ->          error $ ("\n" <>) (clue <> "\n" <> show bad)
--     Right ok -> ok

accum :: ChainId -> Block -> StateT (CachedHistory Fitness) (ExceptT PublicNodeError (ReaderT PublicNodeContext (LoggingT IO))) ()
accum chainId = void . accumHistory scanProgress chainId (^. fitness) -- getBalanceChanges

main :: IO ()
main = do
  nodeAddr:_ <- getArgs
  httpMgr <- liftIO $ newManager tlsManagerSettings
  let ctx = NodeRPCContext httpMgr $ T.pack nodeAddr
  runTest ctx $ do
    chainId <- nodeRPC rChain
    headBlk <- nodeRPC $ rHead chainId

    b <- flip execStateT emptyCache $ scanBranch headBlk 50000 50001 $ \blk -> do
      accum chainId blk
      -- scanProgress headBlk blk
    let (xHash, xPath):_ = Map.toList ( _cachedHistory_blocks b )
    let xLevel :: Int = 2000 + fromIntegral (length xPath)
    xBlk <- nodeRPC $ rBlock chainId xHash
    liftIO $ print [toBase58Text xHash, T.pack $ show xLevel, T.pack $ show $ _blockHeader_level $ _block_header xBlk]
    -- let tfBaker5 = "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r"
    -- liftIO $ putStrLn "bake5"
    -- step (RContract (branch 100) tfBaker5) >>= liftIO . print

    liftIO $ putStrLn "constants"
    void $ nodeRPC $ rProtoConstants chainId (_block_hash headBlk)

runTest :: NodeRPCContext -> ExceptT PublicNodeError (ReaderT PublicNodeContext (LoggingT IO)) () -> IO ()
runTest ctx action = runStderrLoggingT $ either onRPCError id <$> runReaderT (runExceptT action) (PublicNodeContext ctx Nothing)
