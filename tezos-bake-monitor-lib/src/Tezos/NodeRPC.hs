{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.NodeRPC where

import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status(Status(..))
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Tezos.Types

type RpcResponse = Either RpcError
data RpcError
  = RpcError_HttpException Text
  | RpcError_UnexpectedStatus Int BS.ByteString
  | RpcError_NonJSON String LBS.ByteString
  deriving (Eq, Ord, Show, Typeable)

rpcResponse_HttpException :: Text -> Either RpcError a
rpcResponse_HttpException = Left . RpcError_HttpException

rpcResponse_UnexpectedStatus :: Int -> BS.ByteString -> Either RpcError a
rpcResponse_UnexpectedStatus x y = Left $ RpcError_UnexpectedStatus x y

rpcResponse_NonJSON :: String -> LBS.ByteString -> Either RpcError a
rpcResponse_NonJSON x y = Left $ RpcError_NonJSON x y

data NodeRPCContext = NodeRPCContext
  { _nodeRPCContext_httpManager :: Manager
  , _nodeRPCContext_node :: Text
  }

newtype NodeRPCT m a = NodeRPCT (ReaderT NodeRPCContext m a)
  deriving (Functor, Applicative, Monad, MonadIO)

runNodeRPCT :: NodeRPCContext -> NodeRPCT m a -> m a
runNodeRPCT c (NodeRPCT x) = runReaderT x c

class MonadTezosNode m where
  nodeRPC :: NodeRPCRequest a -> m (RpcResponse a)
  nodeAddress :: m Text

instance MonadIO m => MonadTezosNode (NodeRPCT m) where
  nodeRPC = \case
    RComplete (BlockPrefix pfx) -> nodeRPCImpl ("/blocks/head/complete/" <> pfx)
    RBlock hash -> nodeRPCImpl ("/blocks/" <> toBase58Text hash)
    RProtoConstants -> nodeRPCImpl ("/blocks/head/proto/constants")
  nodeAddress = NodeRPCT $ asks _nodeRPCContext_node

newtype BlockPrefix = BlockPrefix Text
  deriving (Eq, Show, Typeable)

data NodeRPCRequest a where
  RComplete :: BlockPrefix -> NodeRPCRequest [BlockHash]
  RBlock :: BlockHash -> NodeRPCRequest Block
  RProtoConstants :: NodeRPCRequest ProtoInfo

nodeRPCImpl :: (MonadIO m, FromJSON a) => Text -> NodeRPCT m (RpcResponse a)
nodeRPCImpl = nodeRPCImpl' eitherDecode

nodeRPCImpl' :: (MonadIO m) => (LBS.ByteString -> Either String a) -> Text -> NodeRPCT m (RpcResponse a)
nodeRPCImpl' decoder rpcSelector = NodeRPCT $ do
  mgr <- asks _nodeRPCContext_httpManager
  node <- asks _nodeRPCContext_node

  let rpcUrl = node <> rpcSelector

  let rpcBoilerplate req = req
        { method = "POST"
        , requestBody = "{}"
        , requestHeaders =
          [ (hContentType, "application/json")
          , (hUserAgent, "tezos-bake-monitor")
          , (hAccept, "*/*")
          ]
        }
  let request = rpcBoilerplate $ parseRequest_ $ T.unpack $ rpcUrl
  result' <- liftIO $ try $ httpLbs request mgr
  case result' of
    Left (err :: HttpException) -> return (rpcResponse_HttpException $ T.pack $ show err)
    Right result -> case responseStatus result of
      Status 200 _ -> do
        let body = responseBody result
        return $ case decoder body of
          Left err -> rpcResponse_NonJSON err body
          Right v -> Right v
      Status code phrase -> return $ rpcResponse_UnexpectedStatus code phrase

