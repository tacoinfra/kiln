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
import Data.Aeson
import Data.Foldable
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
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

doRPCImpl :: (MonadIO m, FromJSON a) => Text -> NodeRPCT m (RpcResponse a)
doRPCImpl rpcSelector = do
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
        return $ case decode body of
          Nothing -> RpcResponse_NonJSON body
          Just v -> RpcResponse_Success v
      Status code phrase -> return . RpcResponse_UnexpectedStatus $ Status code phrase


doRPC :: MonadIO m => NodeRPCRequest a -> NodeRPCT m (RpcResponse a)
doRPC = \case
  Complete (BlockPrefix pfx) -> (fmap.fmap) BlockHash <$> doRPCImpl ("/blocks/head/complete/" <> pfx)



client :: IO ()
client = do
  httpMgr <- liftIO $ newManager tlsManagerSettings
  let ctx = NodeRPCContext httpMgr "http://127.0.0.1:18731"
  x <- flip runReaderT ctx $ doRPC $ Complete $ BlockPrefix "asdf"
  traverse_ print x

