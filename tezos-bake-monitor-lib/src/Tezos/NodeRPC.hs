{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Tezos.NodeRPC where


import Control.Exception
import Control.Monad.Reader
import Control.Lens.Combinators (preview)
import Data.Aeson
import Data.Fixed
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable
import Data.Aeson.Lens (key, _Integer)
import Data.ByteString.Lazy as LBS
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status(Status(..))
import qualified Data.Text as T

import Tezos.BakeMonitor.Types

data NodeRPCContext = NodeRPCContext
  { _nodeRPCContext_httpManager :: Manager
  , _nodeRPCContext_node :: Text
  }


type NodeRPCT m = ReaderT NodeRPCContext m


newtype BlockPrefix = BlockPrefix Text
  deriving (Eq, Show, Generic, Typeable)


data NodeRPCRequest a where
  Complete :: BlockPrefix -> NodeRPCRequest [BlockHash]
  Block :: BlockHash -> NodeRPCRequest BlockInfo
  ProtoConstants :: NodeRPCRequest ProtoInfo

doRPCImpl :: (MonadIO m, FromJSON a) => Text -> NodeRPCT m (RpcResponse a)
doRPCImpl = doRPCImpl' eitherDecode

doRPCImpl' :: (MonadIO m) => (LBS.ByteString -> Either String a) -> Text -> NodeRPCT m (RpcResponse a)
doRPCImpl' decoder rpcSelector = do
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


doRPC :: MonadIO m => NodeRPCRequest a -> NodeRPCT m (RpcResponse a)
doRPC = \case
  Complete (BlockPrefix pfx) -> doRPCImpl ("/blocks/head/complete/" <> pfx)
  Block (BlockHash hash) -> doRPCImpl ("/blocks/" <> hash)
  ProtoConstants -> flip doRPCImpl' ("/blocks/head/proto/constants") $ \bs -> do
    -- TODO: just make this the *Json instance
    v <- eitherDecode bs
    let readKey :: Text -> Either String Micro
        readKey k = maybe (Left $ "missing:" <> T.unpack k) Right $ ((/10^6) . fromInteger) <$> preview (key k . _Integer) (v :: Value)
    bsd <- readKey "block_security_deposit"
    esd <- readKey "endorsement_security_deposit"
    br <- readKey "block_reward"
    er <- readKey "endorsement_reward"
    return $ ProtoInfo
      { _protoInfo_blockSecurityDeposit = bsd
      , _protoInfo_endorsementSecurityDeposit = esd
      , _protoInfo_blockReward = br
      , _protoInfo_endorsementReward = er
      }



