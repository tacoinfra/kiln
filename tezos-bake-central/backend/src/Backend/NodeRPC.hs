{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.NodeRPC where

import Control.Concurrent
import Control.Exception
import Control.Lens (Lens', uncons, unsnoc, view)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Foldable (toList, traverse_)
import Data.Functor (void)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method (Method, methodGet, methodPost)
import Network.HTTP.Types.Status (Status (..))
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw)
import Say (sayErr, sayShow)

import Common (tshow)
import Common.PublicKeyHash
import Common.Schema
import Common.TaggedHash


data NodeRPCContext = NodeRPCContext
  { _nodeRPCContext_httpManager :: Manager
  , _nodeRPCContext_node :: Text
  } deriving (Typeable)

newtype NodeRPCT m a = NodeRPCT { unNodeRPCT :: ReaderT NodeRPCContext m a }
  deriving (Functor, Applicative, Monad, MonadIO, PostgresRaw, Typeable)

class HasNodeRPC s where
  nodeRPCContext :: Lens' s NodeRPCContext

instance HasNodeRPC NodeRPCContext where nodeRPCContext = id

nodeRPC :: forall m a s. (MonadIO m, MonadReader s m, HasNodeRPC s, MonadError RpcError m) => NodeRPCRequest a -> m a
nodeRPC = \case
  RComplete (BlockPrefix pfx) -> nodeRPCImpl methodPost (blockIdToUrl headId <> "/complete/" <> pfx)
  RBlock hash -> nodeRPCImpl methodGet (blockIdToUrl hash)
  RBlocks chain (RawLevel len) heads -> byHead <$> nodeRPCImpl methodGet ("/chains/" <> chainIdToUrl chain <> "/blocks?length=" <> tshow len <> foldMap blk2param heads)
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
      Left (RawLevel x) -> "level=" <> tshow x
      Right (Cycle x) -> "cycle=" <> tshow x

nodeRPCImpl
  :: forall m a s. (MonadIO m, FromJSON a, MonadReader s m, HasNodeRPC s, MonadError RpcError m)
  => Method -> Text -> m a
nodeRPCImpl = nodeRPCImpl' eitherDecode

nodeRPCImpl'
  :: forall m a s. (MonadIO m, MonadReader s m, HasNodeRPC s, MonadError RpcError m)
  => (LBS.ByteString -> Either String a) -> Method -> Text -> m a
nodeRPCImpl' decoder method_ rpcSelector = do
  mgr <- asks (_nodeRPCContext_httpManager . view nodeRPCContext)
  node <- asks (_nodeRPCContext_node . view nodeRPCContext)
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
  let
    request = rpcBoilerplate $ parseRequest_ $ T.unpack rpcUrl
    throwLoggedError e = sayErr ("NODERPC ERROR: " <> tshow rpcUrl <> " >> " <> tshow e) *> throwError e

  liftIO (try @HttpException $ httpLbs request mgr) >>= \case
    Left err -> throwLoggedError $ RpcError_HttpException $ tshow err
    Right result -> case responseStatus result of
      Status 200 _ -> do
        let body = responseBody result
        case decoder body of
          Left err -> throwLoggedError $ RpcError_NonJSON err body
          Right v -> return v
      Status code phrase -> throwLoggedError $ RpcError_UnexpectedStatus code phrase


nodeRPCChunkedImpl
  :: forall m a s. (MonadIO m, FromJSON a, MonadReader s m, HasNodeRPC s, MonadError RpcError m)
  => (RpcResponse a -> IO ())
  -> Method
  -> Text
  -> m (IO ())
nodeRPCChunkedImpl = nodeRPCChunkedImpl' eitherDecode

nodeRPCChunkedImpl'
  :: forall m a s. (MonadIO m, MonadReader s m, HasNodeRPC s, MonadError RpcError m)
  => (LBS.ByteString -> Either String a)
  -> (RpcResponse a -> IO ())
  -> Method
  -> Text
  -> m (IO ())
nodeRPCChunkedImpl' decoder callback method_ rpcSelector = do
  sayShow (method_, rpcSelector)

  mgr <- asks (_nodeRPCContext_httpManager . view nodeRPCContext)
  node <- asks (_nodeRPCContext_node . view nodeRPCContext)

  let
    cb' :: LBS.ByteString -> IO ()
    cb' chunk = callback $ first (\err -> RpcError_NonJSON err chunk) $ decoder chunk

    rpcUrl = node <> rpcSelector

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
  liftIO (try @HttpException $ responseOpen request mgr) >>= \case
    Left err -> throwError $ RpcError_HttpException $ tshow err
    Right response -> do
      sayShow $ void response
      let
        bodyReader = responseBody response
        worker :: LBS.ByteString -> IO ()
        worker leftover = do
          try @HttpException (brRead bodyReader) >>= \case
            Left err -> callback $ Left $ RpcError_HttpException $ tshow err
            Right chunk -> do
              sayShow chunk
              let Just (xs, x) = unsnoc $ LBS.split (fromIntegral $ ord '\n') (leftover <> LBS.fromStrict chunk)
              traverse_ cb' xs

              if BS.length chunk == 0 -- thats it man, no more stuff after this
                then cb' x
                else worker x
      thread <- liftIO $ forkIO $ worker mempty
      return $ killThread thread
