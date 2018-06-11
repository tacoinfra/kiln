{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.NodeRPC where

import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (Status (..))
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw)

import Common.PublicKeyHash
import Common.Schema

-- data RpcResponse a =
--     RpcResponse_HttpException HttpException
--   | RpcResponse_UnexpectedStatus Status
--   | RpcResponse_NonJSON String LBS.ByteString
--   | RpcResponse_Success a
--   deriving (Functor, Foldable, Traversable)


data NodeRPCContext = NodeRPCContext
  { _nodeRPCContext_httpManager :: Manager
  , _nodeRPCContext_node :: Text
  }
  deriving (Typeable)

newtype NodeRPCT m a = NodeRPCT { unNodeRPCT :: ReaderT NodeRPCContext m a }
  deriving (Functor, Applicative, Monad, MonadIO, PostgresRaw, Typeable)


runNodeRPCT :: NodeRPCContext -> NodeRPCT m a -> m a
runNodeRPCT c (NodeRPCT x) = runReaderT x c


instance MonadIO m => MonadTezosNode (NodeRPCT m) where
  nodeRPC = \case
    Complete (BlockPrefix pfx) -> nodeRPCImpl ("/blocks/head/complete/" <> pfx)
    Block hash -> nodeRPCImpl ("/blocks/" <> showBlockId hash)
    ProtoConstants -> nodeRPCImpl "/blocks/head/proto/constants"
    Contract block publicKey -> nodeRPCImpl ("/blocks/" <> showBlockId block <> "/proto/context/contracts/" <> toPublicKeyHashText publicKey)
  nodeAddress = NodeRPCT $ asks _nodeRPCContext_node

rpcError_HttpException :: HttpException -> RpcResponse a
rpcError_HttpException err = Left $ RpcError_HttpException $ T.pack $ show err

rpcResponse_NonJSON :: String -> LBS.ByteString -> RpcResponse a
rpcResponse_NonJSON err body = Left $ RpcError_NonJSON err body

rpcResponse_UnexpectedStatus :: Int -> BS.ByteString -> RpcResponse a
rpcResponse_UnexpectedStatus code phrase = Left $ RpcError_UnexpectedStatus code phrase

nodeRPCImpl :: (MonadIO m, FromJSON a) => Text -> NodeRPCT m (RpcResponse a)
nodeRPCImpl = nodeRPCImpl' eitherDecode

nodeRPCImpl' :: forall m a. (MonadIO m) => (LBS.ByteString -> Either String a) -> Text -> NodeRPCT m (RpcResponse a)
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
  let request = rpcBoilerplate $ parseRequest_ $ T.unpack rpcUrl
  result' <- liftIO $ try $ httpLbs request mgr
  let logFailure :: RpcResponse a -> ReaderT NodeRPCContext m (RpcResponse a)
      logFailure (Left bad) = do
        liftIO $ putStrLn $ "NODERPC ERROR:" <> show rpcUrl <> " >> " <> show bad
        return $ Left bad
      logFailure ok = return ok
  logFailure =<< case result' of
    Left (err :: HttpException) -> return (rpcError_HttpException err)
    Right result -> case responseStatus result of
      Status 200 _ -> do
        let body = responseBody result
        return $ case decoder body of
          Left err -> rpcResponse_NonJSON err body
          Right v -> return v
      Status code phrase -> return $ rpcResponse_UnexpectedStatus code phrase
