{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Network.Http.Client based request handler
module Tezos.NodeRPC.Network
  ( HasNodeRPC (..)
  , NodeRPCContext (..)
  , QueryNodeImpl (nodeRPC)
  , nodeRPCImpl
  ) where

import Control.Exception.Safe (try)
import Control.Lens (Lens', re, uncons, unsnoc, view, (^.))
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (ord)
import Data.Foldable (fold, toList)
import Data.Function (fix)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (for)
import Data.Typeable (Typeable)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.HTTP.Types.Method as Http (Method, methodGet)
import qualified Network.HTTP.Types.Status as Http (Status (..))

import Tezos.NodeRPC.Class
import Tezos.NodeRPC.Types
import Tezos.Types

newtype QueryNodeImpl a = QueryNodeImpl {
  nodeRPC :: forall m e s. (MonadIO m, MonadReader s m , HasNodeRPC s, MonadError e m , AsRpcError e) => m a
  }

instance QueryChain QueryNodeImpl where
  rChain = QueryNodeImpl $ _block_chainId <$> nodeRPCImpl Http.methodGet "/chains/main/blocks/head"

instance QueryBlock QueryNodeImpl where
  type BlockType QueryNodeImpl = Block
  --rComplete (BlockPrefix pfx) = QueryNodeImpl $ nodeRPCImpl methodPost (blockIdToUrl headId <> "/complete/" <> pfx)
  rHead chainId = QueryNodeImpl $ nodeRPCImpl Http.methodGet $ "/chains/" <> toBase58Text chainId <> "/blocks/head"
  rBlock chainId blockHash = QueryNodeImpl $ nodeRPCImpl Http.methodGet $ chainBlockUrl chainId blockHash

instance QueryHistory QueryNodeImpl where
  rBlockPred chainId blockHash (RawLevel levelsBack) = QueryNodeImpl $ nodeRPCImpl Http.methodGet $ chainBlockUrl chainId blockHash <> "~" <> T.pack (show levelsBack)
  rBlocks chainId (RawLevel len) heads = QueryNodeImpl $ byHead <$> nodeRPCImpl Http.methodGet ("/chains/" <> toBase58Text chainId <> "/blocks?length=" <> T.pack (show len) <> foldMap blk2param heads)
    where
      byHead :: [Seq BlockHash] -> Map.Map BlockHash (Seq BlockHash)
      byHead = foldMap $ maybe mempty (uncurry Map.singleton) . uncons
      blk2param :: BlockHash -> Text
      blk2param blkHash = "&head=" <> toBase58Text blkHash
  rProtoConstants chainId blockHash = QueryNodeImpl $ nodeRPCImpl Http.methodGet $ chainBlockUrl chainId blockHash <> "/context/constants"
  rContract chainId blockHash contractId = QueryNodeImpl $ nodeRPCImpl Http.methodGet (chainBlockUrl chainId blockHash <> "/context/contracts/" <> toContractIdText contractId)
  rBakingRights chainId blockHash params = QueryNodeImpl $ nodeRPCImpl Http.methodGet $ chainBlockUrl chainId blockHash <> "/helpers/baking_rights"
      <> (if null params then "" else "?" <> T.intercalate "&" (dynamicParamRightsRangeToQueryArg <$> toList params))
  rEndorsingRights chainId blockHash params = QueryNodeImpl $ nodeRPCImpl Http.methodGet $ chainBlockUrl chainId blockHash <> "/helpers/endorsing_rights"
      <> (if null params then "" else "?" <> T.intercalate "&" (dynamicParamRightsRangeToQueryArg <$> toList params))

instance QueryNode QueryNodeImpl where
  rConnections = QueryNodeImpl $ do
    vs :: [Aeson.Value] <- nodeRPCImpl Http.methodGet "/network/connections"
    return $ fromIntegral $ Prelude.length vs
  rNetworkStat = QueryNodeImpl $ nodeRPCImpl Http.methodGet "/network/stat"

instance MonitorHeads QueryNodeImpl where
  rMonitorHeads chainId f = QueryNodeImpl $ nodeRPCChunkedImpl f Http.methodGet ("/monitor/heads/" <> toBase58Text chainId)


chainBlockUrl :: ChainId -> BlockHash -> Text
chainBlockUrl chainId blockHash = "/chains/" <> toBase58Text chainId <> "/blocks/" <> toBase58Text blockHash

dynamicParamRightsRangeToQueryArg :: Either RawLevel Cycle -> Text
dynamicParamRightsRangeToQueryArg = \case
  Left (RawLevel x) -> "level=" <> T.pack (show x)
  Right (Cycle x) -> "cycle=" <> T.pack (show x)

data NodeRPCContext = NodeRPCContext
  { _nodeRPCContext_httpManager :: !Http.Manager
  , _nodeRPCContext_node :: !Text
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
  => Http.Method -> Text -> m a
nodeRPCImpl = nodeRPCImpl' Aeson.eitherDecode

nodeRPCImpl' :: forall m a s e.
  ( MonadIO m
  , MonadReader s m, HasNodeRPC s
  , MonadError e m, AsRpcError e
  )
  => (LBS.ByteString -> Either String a) -> Http.Method -> Text -> m a
nodeRPCImpl' decoder method_ rpcSelector = do
  mgr <- asks (_nodeRPCContext_httpManager . view nodeRPCContext)
  node <- asks (_nodeRPCContext_node . view nodeRPCContext)
  -- sayShow (node, method_, rpcSelector)

  let rpcUrl = T.dropWhileEnd (=='/') node <> rpcSelector
  liftIO $ T.putStrLn rpcUrl

  let rpcBoilerplate req = req
        { Http.method = method_
        , Http.requestBody = if method_ == Http.methodGet then "" else "{}"
        , Http.requestHeaders =
          [(Http.hContentType, "application/json") | method_ /= Http.methodGet]
          ++ [ (Http.hUserAgent, "tezos-bake-monitor")
             , (Http.hAccept, "*/*") -- TODO: Probably should pinned to JSON and use "application/json"
             ]
        }
  let
    request = rpcBoilerplate $ Http.parseRequest_ $ T.unpack rpcUrl
    throwLoggedError e = {-sayErr ("NODERPC ERROR: " <> (T.pack $ show rpcUrl) <> " >> " <> (T.pack $ show e)) *>-} throwError e

  liftIO (try @IO @Http.HttpException $ Http.httpLbs request mgr) >>= \case
    Left err -> throwLoggedError $ rpcResponse_HttpException (T.pack $ show err)
    Right result -> case Http.responseStatus result of
      Http.Status 200 _ -> do
        let body = Http.responseBody result
        case decoder body of
          Left err -> throwLoggedError $ rpcResponse_NonJSON err body
          Right v -> return v
      Http.Status code phrase -> do
        liftIO $ print $ Http.responseStatus result
        liftIO $ LBS8.putStrLn $ Http.responseBody result

        throwLoggedError $ rpcResponse_UnexpectedStatus code phrase

nodeRPCChunkedImpl :: forall a r s e m.
  ( MonadIO m, FromJSON a
  , MonadReader s m, HasNodeRPC s
  , MonadError e m, AsRpcError e
  , Monoid r
  )
  => (a -> IO r)
  -> Http.Method
  -> Text
  -> m r
nodeRPCChunkedImpl = nodeRPCChunkedImpl' Aeson.eitherDecode


nodeRPCChunkedImpl' :: forall a r s e m.
  ( MonadIO m
  , MonadReader s m, HasNodeRPC s
  , MonadError e m, AsRpcError e
  , Monoid r
  )
  => (LBS.ByteString -> Either String a)
  -> (a -> IO r)
  -> Http.Method
  -> Text
  -> m r
nodeRPCChunkedImpl' decoder callback method_ rpcSelector = do
  mgr <- asks (_nodeRPCContext_httpManager . view nodeRPCContext)
  node <- asks (_nodeRPCContext_node . view nodeRPCContext)

  let
    rpcUrl = node <> rpcSelector
    rpcBoilerplate req = req
      { Http.method = method_
      , Http.requestBody = if method_ == Http.methodGet then "" else "{}"
      , Http.requestHeaders =
           [(Http.hContentType, "application/json") | method_ /= Http.methodGet]
        ++ [ (Http.hUserAgent, "tezos-bake-monitor")
           , (Http.hAccept, "*/*")
           ]
      }
    request = rpcBoilerplate $ Http.parseRequest_ $ T.unpack rpcUrl

  res :: Either Http.HttpException (Either RpcError r) <- liftIO $ try @_ @Http.HttpException $
    Http.withResponse request mgr $ \response -> runExceptT $ do
      flip fix mempty $ \self (leftover, r) -> do
        let
          callbackWithDecode bytes = case decoder bytes of
            Left e -> throwError $ RpcError_NonJSON e bytes
            Right v -> liftIO $ callback v

          callbackMany xs = (r `mappend`) . fold <$> for xs callbackWithDecode

        chunk <- liftIO $ Http.brRead (Http.responseBody response)

        let messages = LBS.split (fromIntegral $ ord '\n') (leftover <> LBS.fromStrict chunk)

        if BS.length chunk == 0 then
          callbackMany messages
        else case unsnoc messages of
          Nothing -> pure r
          Just (xs, x) -> do
            r' <- callbackMany xs
            self (x, r')

  case res of
    Left (httpErr :: Http.HttpException) -> throwError $ rpcResponse_HttpException (T.pack $ show httpErr)
    Right (Left rpcError) -> throwError $ rpcError ^. re asRpcError
    Right (Right r) -> pure r
