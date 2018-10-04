{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | 'Network.HTTP.Client' based implemention of 'RpcQuery'
module Tezos.NodeRPC.Network where

import Control.Exception.Safe (try)
import Control.Lens (Lens', re, unsnoc, view, (^.))
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugS, logInfoS)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Foldable (fold)
import Data.Function (fix)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.HTTP.Types.Method as Http (Method, methodGet)
import qualified Network.HTTP.Types.Status as Http (Status (..))

import Paths_tezos_bake_monitor_lib (version)
import Tezos.NodeRPC.Class
import Tezos.NodeRPC.Types

nodeRPC
  :: (MonadIO m, MonadLogger m, MonadReader s m , HasNodeRPC s, MonadError e m , AsRpcError e)
  => RpcQuery a -> m a
nodeRPC                         (RpcQuery decoder method resource)    = nodeRPCImpl' decoder method resource

nodeRPCChunked
  :: (MonadIO m, MonadLogger m, MonadReader s m , HasNodeRPC s, MonadError e m , AsRpcError e, Monoid r)
  => PlainNodeStream a -> (a -> IO r) -> m r
nodeRPCChunked (PlainNodeStream (RpcQuery decoder method resource)) k = nodeRPCChunkedImpl' decoder k method resource



data NodeRPCContext = NodeRPCContext
  { _nodeRPCContext_httpManager :: !Http.Manager
  , _nodeRPCContext_node :: !Text
  } deriving (Typeable)
-- TODO: use $makeClassy
class HasNodeRPC s where
  nodeRPCContext :: Lens' s NodeRPCContext
instance HasNodeRPC NodeRPCContext where nodeRPCContext = id

nodeRPCImpl :: forall m a s e.
  ( MonadIO m, MonadLogger m
  , FromJSON a
  , MonadReader s m , HasNodeRPC s
  , MonadError e m , AsRpcError e
  )
  => Http.Method -> Text -> m a
nodeRPCImpl = nodeRPCImpl' Aeson.eitherDecode

nodeRPCImpl' :: forall m a s e.
  ( MonadIO m, MonadLogger m
  , MonadReader s m, HasNodeRPC s
  , MonadError e m, AsRpcError e
  )
  => (LBS.ByteString -> Either String a) -> Http.Method -> Text -> m a
nodeRPCImpl' decoder method_ rpcSelector = do
  mgr <- asks (_nodeRPCContext_httpManager . view nodeRPCContext)
  node <- asks (_nodeRPCContext_node . view nodeRPCContext)
  -- sayShow (node, method_, rpcSelector)

  let rpcUrl = T.dropWhileEnd (=='/') node <> rpcSelector
  $(logDebugS) "NODERPC" $ rpcUrl

  let
    request = rpcBoilerplate method_ $ Http.parseRequest_ $ T.unpack rpcUrl
    throwLoggedError e = {-sayErr ("NODERPC ERROR: " <> (T.pack $ show rpcUrl) <> " >> " <> (T.pack $ show e)) *>-} throwError e

  liftIO (try @_ @Http.HttpException $ Http.httpLbs request mgr) >>= \case
    Left err -> throwLoggedError $ rpcResponse_HttpException (T.pack $ show err)
    Right result -> case Http.responseStatus result of
      Http.Status 200 _ -> do
        let body = Http.responseBody result
        case decoder body of
          Left err -> throwLoggedError $ rpcResponse_NonJSON err body
          Right v -> return v
      Http.Status code phrase -> do
        $(logInfoS) "NODERPC" $ T.pack $ show $ Http.responseStatus result
        $(logDebugS) "NODERPC" $ T.pack $ show $ Http.responseBody result -- TODO: find a better way to log this

        throwLoggedError $ rpcResponse_UnexpectedStatus code phrase

nodeRPCChunkedImpl :: forall a r s e m.
  ( MonadIO m, MonadLogger m, FromJSON a
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
  ( MonadIO m, MonadLogger m
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

    request = rpcBoilerplate method_ $ Http.parseRequest_ $ T.unpack rpcUrl

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


rpcBoilerplate :: Http.Method -> Http.Request -> Http.Request
rpcBoilerplate method_ req = req
  { Http.method = method_
  , Http.requestBody = if method_ == Http.methodGet then "" else "{}"
  , Http.requestHeaders =
    [(Http.hContentType, "application/json") | method_ /= Http.methodGet]
    ++ [ (Http.hUserAgent, "tezos-bake-monitor-lib/" <> T.encodeUtf8 (T.pack $ showVersion version))
       , (Http.hAccept, "*/*") -- TODO: Probably should pinned to JSON and use "application/json"
       ]
  }
