{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.NodeRPC where

import Control.Concurrent
import Control.Exception
import Control.Lens.Cons (unsnoc, uncons)
import Control.Monad.Reader
import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Foldable(traverse_)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method (Method, methodGet, methodPost)
import Network.HTTP.Types.Status (Status (..))
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw)
import Say (say, sayShow)

import Common.TaggedHash
import Common (tshow)
import Common.PublicKeyHash
import Common.Schema


data NodeRPCContext = NodeRPCContext
  { _nodeRPCContext_httpManager :: Manager
  , _nodeRPCContext_node :: Text
  } deriving (Typeable)

newtype NodeRPCT m a = NodeRPCT { unNodeRPCT :: ReaderT NodeRPCContext m a }
  deriving (Functor, Applicative, Monad, MonadIO, PostgresRaw, Typeable)

runNodeRPCT :: NodeRPCContext -> NodeRPCT m a -> m a
runNodeRPCT c (NodeRPCT x) = runReaderT x c



instance MonadIO m => MonadTezosNode (NodeRPCT m) where
  nodeRPC = \case
    RComplete (BlockPrefix pfx) -> nodeRPCImpl methodPost (blockIdToUrl headId <> "/complete/" <> pfx)
    RBlock hash -> nodeRPCImpl methodGet (blockIdToUrl hash)
    RBlocks chain len heads -> fmap byHead <$> nodeRPCImpl methodGet ("/chains/" <> chainIdToUrl chain <> "/blocks?length=" <> tshow len <> foldMap blk2param heads)
      where
        byHead :: [Seq BlockHash] -> Map.Map BlockHash (Seq BlockHash)
        byHead = foldMap $ maybe mempty (uncurry Map.singleton) . uncons
        blk2param :: BlockHash -> Text
        blk2param blkHash = "&head=" <> toBase58Text blkHash
    RProtoConstants blk -> nodeRPCImpl methodGet (blockIdToUrl blk <> "/context/constants")
    RContract block publicKey -> nodeRPCImpl methodGet (blockIdToUrl block <> "/context/contracts/" <> toPublicKeyHashText publicKey)
    RConnections -> do
      (vs :: RpcResponse [Value]) <- nodeRPCImpl methodGet "/network/connections"
      return $ fmap (fromIntegral . Prelude.length) vs
    RBakingRights block cycles -> nodeRPCImpl methodGet $ blockIdToUrl block <> "/helpers/baking_rights"
        <> (if null cycles then "" else "?" <> T.intercalate "&" ["cycle=" <> tshow n | n <- cycles])
    REndorsingRights block cycles -> nodeRPCImpl methodGet $ blockIdToUrl block <> "/helpers/endorsing_rights"
        <> (if null cycles then "" else "?" <> T.intercalate "&" ["cycle=" <> tshow n | n <- cycles])
    RNetworkStat -> nodeRPCImpl methodGet "/network/stat"

    RMonitorHeads f chain -> nodeRPCChunkedImpl f methodGet ("/monitor/heads/" <> chainIdToUrl chain)

  nodeAddress = NodeRPCT $ asks _nodeRPCContext_node



rpcError_HttpException :: HttpException -> RpcResponse a
rpcError_HttpException err = Left $ RpcError_HttpException $ T.pack $ show err

rpcResponse_NonJSON :: String -> LBS.ByteString -> RpcResponse a
rpcResponse_NonJSON err body = Left $ RpcError_NonJSON err body

rpcResponse_UnexpectedStatus :: Int -> BS.ByteString -> RpcResponse a
rpcResponse_UnexpectedStatus code phrase = Left $ RpcError_UnexpectedStatus code phrase


nodeRPCImpl :: (MonadIO m, FromJSON a) => Method -> Text -> NodeRPCT m (RpcResponse a)
nodeRPCImpl = nodeRPCImpl' eitherDecode

nodeRPCImpl' :: forall m a. (MonadIO m) => (LBS.ByteString -> Either String a) -> Method -> Text -> NodeRPCT m (RpcResponse a)
nodeRPCImpl' decoder method_ rpcSelector = NodeRPCT $ do
  mgr <- asks _nodeRPCContext_httpManager
  node <- asks _nodeRPCContext_node
  sayShow (node, method_, rpcSelector)

  let rpcUrl = node <> rpcSelector

  let rpcBoilerplate req = req
        { method = method_
        , requestBody = if method_ == methodGet then "" else "{}"
        , requestHeaders =
          [ (hContentType, "application/json")
          , (hUserAgent, "tezos-bake-monitor")
          , (hAccept, "*/*")
          ]
        }
  let request = rpcBoilerplate $ parseRequest_ $ T.unpack rpcUrl
  result' <- liftIO $ try @HttpException $ httpLbs request mgr
  let logFailure :: RpcResponse a -> ReaderT NodeRPCContext m (RpcResponse a)
      logFailure (Left bad) = do
        say $ "NODERPC ERROR:" <> tshow rpcUrl <> " >> " <> tshow bad
        return $ Left bad
      logFailure ok = return ok
  logFailure =<< case result' of
    Left err -> return (rpcError_HttpException err)
    Right result -> case responseStatus result of
      Status 200 _ -> do
        let body = responseBody result
        return $ case decoder body of
          Left err -> rpcResponse_NonJSON err body
          Right v -> return v
      Status code phrase -> return $ rpcResponse_UnexpectedStatus code phrase


nodeRPCChunkedImpl
  :: forall m a. (MonadIO m, FromJSON a)
  => (RpcResponse a -> IO ())
  -> Method
  -> Text
  -> NodeRPCT m (RpcResponse (IO ()))
nodeRPCChunkedImpl = nodeRPCChunkedImpl' eitherDecode

nodeRPCChunkedImpl'
  :: forall m a. (MonadIO m)
  => (LBS.ByteString -> Either String a)
  -> (RpcResponse a -> IO ())
  -> Method
  -> Text
  -> NodeRPCT m (RpcResponse (IO ()))
nodeRPCChunkedImpl' decoder callback method_ rpcSelector = NodeRPCT $ do
  sayShow (method_, rpcSelector)
  mgr <- asks _nodeRPCContext_httpManager
  node <- asks _nodeRPCContext_node
  let cb' :: LBS.ByteString -> IO ()
      cb' chunk = callback $ first (\err -> RpcError_NonJSON err chunk) $ decoder chunk

  let rpcUrl = node <> rpcSelector
  sayShow rpcUrl

  let rpcBoilerplate req = req
        { method = method_
        , requestBody = if method_ == methodGet then "" else "{}"
        , requestHeaders =
          [ (hContentType, "application/json")
          , (hUserAgent, "tezos-bake-monitor")
          , (hAccept, "*/*")
          ]
        }
  let request = rpcBoilerplate $ parseRequest_ $ T.unpack rpcUrl
  (liftIO $ try @HttpException$ responseOpen request mgr) >>= \case
    Left err -> return (rpcError_HttpException err)
    Right response -> do
      sayShow $ void response
      let
        bodyReader = responseBody response
        worker :: LBS.ByteString -> IO ()
        worker leftover = do
            (try @HttpException $ brRead bodyReader) >>= \case
              Left err -> callback $ rpcError_HttpException err
              Right chunk -> do
                say "asdf"
                sayShow chunk
                say "asdf"
                let Just (xs, x) = unsnoc $ LBS.split (fromIntegral $ ord '\n') (leftover <> LBS.fromStrict chunk)
                traverse_ cb' xs

                if (BS.length chunk == 0) -- thats it man, no more stuff after this
                  then cb' x
                  else worker x
      thread <- liftIO $ forkIO $ worker mempty
      return $ Right $ killThread thread
