{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Backend.NodeRPC where

import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable
import Data.ByteString.Lazy as LBS
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status(Status(..))
import qualified Data.Text as T

import Common.Schema
import Focus.Backend.DB.PsqlSimple(PostgresRaw)

data RpcResponse a =
    RpcResponse_HttpException HttpException
  | RpcResponse_UnexpectedStatus Status
  | RpcResponse_NonJSON String LBS.ByteString
  | RpcResponse_Success a
  deriving (Functor, Foldable, Traversable)


data NodeRPCContext = NodeRPCContext
  { _nodeRPCContext_httpManager :: Manager
  , _nodeRPCContext_node :: Text
  }

newtype NodeRPCT m a = NodeRPCT (ReaderT NodeRPCContext m a)
  deriving (Functor, Applicative, Monad, MonadIO, PostgresRaw)


runNodeRPCT :: NodeRPCContext -> NodeRPCT m a -> m a
runNodeRPCT c (NodeRPCT x) = runReaderT x c

class MonadTezosNode m where
  nodeRPC :: NodeRPCRequest a -> m (RpcResponse a)
  nodeAddress :: m Text

instance MonadIO m => MonadTezosNode (NodeRPCT m) where
  nodeRPC = \case
    Complete (BlockPrefix pfx) -> nodeRPCImpl ("/blocks/head/complete/" <> pfx)
    Block (BlockHash hash) -> nodeRPCImpl ("/blocks/" <> hash)
    ProtoConstants -> nodeRPCImpl ("/blocks/head/proto/constants")
    Contract (BlockHash block) (PublicKeyHash publicKey) -> nodeRPCImpl ("/blocks/" <> block <> "/proto/context/contracts/" <> publicKey)
  nodeAddress = NodeRPCT $ asks _nodeRPCContext_node

newtype BlockPrefix = BlockPrefix Text
  deriving (Eq, Show, Generic, Typeable)

data NodeRPCRequest a where
  Complete :: BlockPrefix -> NodeRPCRequest [BlockHash]
  Block :: BlockHash -> NodeRPCRequest BlockInfo
  ProtoConstants :: NodeRPCRequest ProtoInfo
  Contract :: BlockHash -> PublicKeyHash -> NodeRPCRequest Account

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
    Left err -> return (RpcResponse_HttpException err)
    Right result -> case responseStatus result of
      Status 200 _ -> do
        let body = responseBody result
        return $ case decoder body of
          Left err -> RpcResponse_NonJSON err body
          Right v -> RpcResponse_Success v
      Status code phrase -> return . RpcResponse_UnexpectedStatus $ Status code phrase
