{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Network.Http.Client based request handler
module Tezos.NodeRPC.Network where

import Control.Concurrent
import Control.Exception
import Data.Bifunctor
import Data.Char (ord)
import Data.Foldable
import Control.Lens (Lens', uncons, unsnoc, view)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader
import Data.Aeson
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Typeable
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method (Method, methodGet, methodPost)
import Network.HTTP.Types.Status(Status(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Tezos.NodeRPC.Types
import Tezos.Types

-- newtype NodeRPCT m a = NodeRPCT (ReaderT NodeRPCContext m a)
--   deriving (Functor, Applicative, Monad, MonadIO)

nodeRPC :: forall m e a s.
  ( MonadIO m
  , MonadReader s m , HasNodeRPC s
  , MonadError e m , AsRpcError e
  )
  => NodeRPCRequest a -> m a
nodeRPC = \case
  RComplete (BlockPrefix pfx) -> nodeRPCImpl methodPost (blockIdToUrl headId <> "/complete/" <> pfx)
  RBlock blockHash -> nodeRPCImpl methodGet (blockIdToUrl blockHash)
  RBlocks chain (RawLevel len) heads -> byHead <$> nodeRPCImpl methodGet ("/chains/" <> chainIdToUrl chain <> "/blocks?length=" <> (T.pack $ show len) <> foldMap blk2param heads)
    where
      byHead :: [Seq BlockHash] -> Map.Map BlockHash (Seq BlockHash)
      byHead = foldMap $ maybe mempty (uncurry Map.singleton) . uncons
      blk2param :: BlockHash -> Text
      blk2param blkHash = "&head=" <> toBase58Text blkHash
  RProtoConstants blk -> nodeRPCImpl methodGet (blockIdToUrl blk <> "/context/constants")
  RContract block publicKey -> nodeRPCImpl methodGet (blockIdToUrl block <> "/context/contracts/" <> toPublicKeyHashText publicKey)
  RConnections -> do
    vs :: [Value] <- nodeRPCImpl methodGet "/network/connections"
    return $ fromIntegral $ Prelude.length vs
  RBakingRights block params -> nodeRPCImpl methodGet $ blockIdToUrl block <> "/helpers/baking_rights"
      <> (if null params then "" else "?" <> T.intercalate "&" (dynamicParamRightsRangeToQueryArg <$> toList params))
  REndorsingRights block params -> nodeRPCImpl methodGet $ blockIdToUrl block <> "/helpers/endorsing_rights"
      <> (if null params then "" else "?" <> T.intercalate "&" (dynamicParamRightsRangeToQueryArg <$> toList params))
  RNetworkStat -> nodeRPCImpl methodGet "/network/stat"

  RMonitorHeads f chain -> nodeRPCChunkedImpl f methodGet ("/monitor/heads/" <> chainIdToUrl chain)

  where
    dynamicParamRightsRangeToQueryArg = \case
      Left (RawLevel x) -> "level=" <> (T.pack $ show x)
      Right (Cycle x) -> "cycle=" <> (T.pack $ show x)

data NodeRPCContext = NodeRPCContext
  { _nodeRPCContext_httpManager :: Manager
  , _nodeRPCContext_node :: Text
  } deriving (Typeable)
-- TODO: use $makeClassy
class HasNodeRPC s where
  nodeRPCContext :: Lens' s NodeRPCContext
instance HasNodeRPC NodeRPCContext where nodeRPCContext = id

nodeRPCImpl :: forall m a s e.
  ( MonadIO m
  , FromJSON a
  , MonadReader s m , HasNodeRPC s
  , MonadError e m , AsRpcError e
  )
  => Method -> Text -> m a
nodeRPCImpl = nodeRPCImpl' eitherDecode

nodeRPCImpl' :: forall m a s e.
  ( MonadIO m
  , MonadReader s m, HasNodeRPC s
  , MonadError e m, AsRpcError e
  )
  => (LBS.ByteString -> Either String a) -> Method -> Text -> m a
nodeRPCImpl' decoder method_ rpcSelector = do
  mgr <- asks (_nodeRPCContext_httpManager . view nodeRPCContext)
  node <- asks (_nodeRPCContext_node . view nodeRPCContext)
  -- sayShow (node, method_, rpcSelector)

  let rpcUrl = node <> rpcSelector
  liftIO $ T.putStrLn rpcUrl

  let rpcBoilerplate req = req
        { method = method_
        , requestBody = if method_ == methodGet then "" else "{}"
        , requestHeaders =
          [ (hContentType, "application/json")
          , (hUserAgent, "tezos-bake-monitor")
          , (hAccept, "*/*")
          ]
        }
  let
    request = rpcBoilerplate $ parseRequest_ $ T.unpack rpcUrl
    throwLoggedError e = {-sayErr ("NODERPC ERROR: " <> (T.pack $ show rpcUrl) <> " >> " <> (T.pack $ show e)) *>-} throwError e

  liftIO (try @HttpException $ httpLbs request mgr) >>= \case
    Left err -> throwLoggedError $ rpcResponse_HttpException $ (T.pack $ show err)
    Right result -> case responseStatus result of
      Status 200 _ -> do
        let body = responseBody result
        case decoder body of
          Left err -> throwLoggedError $ rpcResponse_NonJSON err body
          Right v -> return v
      Status code phrase -> do
        liftIO $ print $ responseStatus result
        liftIO $ LBS.putStrLn $ responseBody result

        throwLoggedError $ rpcResponse_UnexpectedStatus code phrase

nodeRPCChunkedImpl :: forall m a s e.
  ( MonadIO m, FromJSON a
  , MonadReader s m, HasNodeRPC s
  , MonadError e m, AsRpcError e
  )
  => (RpcResponse a -> IO ())
  -> Method
  -> Text
  -> m (IO ())
nodeRPCChunkedImpl = nodeRPCChunkedImpl' eitherDecode

nodeRPCChunkedImpl' :: forall m a s e.
  ( MonadIO m
  , MonadReader s m, HasNodeRPC s
  , MonadError e m, AsRpcError e
  )
  => (LBS.ByteString -> Either String a)
  -> (RpcResponse a -> IO ())
  -> Method
  -> Text
  -> m (IO ())
nodeRPCChunkedImpl' decoder callback method_ rpcSelector = do
  -- sayShow (method_, rpcSelector)

  mgr <- asks (_nodeRPCContext_httpManager . view nodeRPCContext)
  node <- asks (_nodeRPCContext_node . view nodeRPCContext)

  let
    cb' :: LBS.ByteString -> IO ()
    cb' chunk = callback $ first (\err -> rpcResponse_NonJSON err chunk) $ decoder chunk

    rpcUrl = node <> rpcSelector

  -- sayShow rpcUrl

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
  liftIO (try @HttpException $ responseOpen request mgr) >>= \case
    Left err -> throwError $ rpcResponse_HttpException $ (T.pack $ show err)
    Right response -> do
      -- sayShow $ void response
      let
        bodyReader = responseBody response
        worker :: LBS.ByteString -> IO ()
        worker leftover = do
          try @HttpException (brRead bodyReader) >>= \case
            Left err -> callback $ Left $ rpcResponse_HttpException $ (T.pack $ show err)
            Right chunk -> do
              -- sayShow chunk
              let Just (xs, x) = unsnoc $ LBS.split (fromIntegral $ ord '\n') (leftover <> LBS.fromStrict chunk)
              traverse_ cb' xs

              if BS.length chunk == 0 -- thats it man, no more stuff after this
                then cb' x
                else worker x
      thread <- liftIO $ forkIO $ worker mempty
      return $ killThread thread

