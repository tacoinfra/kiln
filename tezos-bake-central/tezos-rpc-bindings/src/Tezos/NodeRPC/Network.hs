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
{-# LANGUAGE DoAndIfThenElse #-}

-- | 'Network.HTTP.Client' based implemention of 'RpcQuery'
module Tezos.NodeRPC.Network where

import Control.Exception.Safe (throwIO, try)
import Control.Lens (Lens', re, unsnoc, view, (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugS)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Foldable (fold)
import Data.Function (fix)
import Data.Ratio (numerator, denominator)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (NominalDiffTime)
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.HTTP.Types.Method as Http (Method, methodGet)
import qualified Network.HTTP.Types.Status as Http (Status (..))
import System.Timeout(timeout)

import Paths_tezos_rpc_bindings (version)
import Tezos.NodeRPC.Class
import Tezos.Common.NodeRPC.Types

nodeRPC
  :: (MonadIO m, MonadLogger m, MonadReader s m , HasNodeRPC s, MonadError e m , AsRpcError e)
  => RpcQuery a -> m a
nodeRPC                         (RpcQuery decoder body method resource)    = nodeRPCImpl' decoder body method resource

nodeRPCChunked
  :: (MonadIO m, MonadReader s m , HasNodeRPC s, MonadError e m , AsRpcError e, Monoid r)
  => PlainNodeStream a -> (a -> IO r) -> Maybe NominalDiffTime -> m r
nodeRPCChunked (PlainNodeStream (RpcQuery decoder body method resource)) k mbTimeout =
  nodeRPCChunkedImpl' decoder body k method resource mbTimeout



data NodeRPCContext = NodeRPCContext
  { _nodeRPCContext_httpManager :: Http.Manager
  , _nodeRPCContext_node :: Text
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
  => Aeson.Encoding -> Http.Method -> Text -> m a
nodeRPCImpl = nodeRPCImpl' Aeson.eitherDecode'

nodeRPCImpl' :: forall m a s e.
  ( MonadIO m, MonadLogger m
  , MonadReader s m, HasNodeRPC s
  , MonadError e m, AsRpcError e
  )
  => (LBS.ByteString -> Either String a) -> Aeson.Encoding -> Http.Method -> Text -> m a
nodeRPCImpl' decoder requestBody method_ rpcSelector = do
  mgr <- asks (_nodeRPCContext_httpManager . view nodeRPCContext)
  node <- asks (_nodeRPCContext_node . view nodeRPCContext)
  -- sayShow (node, method_, rpcSelector)

  let rpcUrl = T.dropWhileEnd (=='/') node <> rpcSelector
  $(logDebugS) "NODERPC" rpcUrl

  let
    request = rpcBoilerplate method_ requestBody $ Http.parseRequest_ $ T.unpack rpcUrl

  liftIO (try @_ @Http.HttpException $ Http.httpLbs request mgr) >>= \case
    Left err -> throwError $ rpcResponse_HttpException rpcUrl (T.pack $ show err)
    Right result -> case Http.responseStatus result of
      Http.Status 200 _ -> do
        let body = Http.responseBody result
        case decoder body of
          Left err -> throwError $ rpcResponse_NonJSON rpcUrl err body
          Right v -> return v
      Http.Status 401 _ -> throwError $ rpcResponse_RestrictedEndpoint rpcUrl
      Http.Status code phrase -> do
        $(logDebugS) "NODERPC" $ "Non-200 response for request: " <>
          T.pack (show request) <> " - " <> T.pack (show $ Http.responseBody result)
        throwError $ rpcResponse_UnexpectedStatus rpcUrl code phrase

nodeRPCChunkedImpl :: forall a r s e m.
  ( MonadIO m, FromJSON a
  , MonadReader s m, HasNodeRPC s
  , MonadError e m, AsRpcError e
  , Monoid r
  )
  => Aeson.Encoding
  -> (a -> IO r)
  -> Http.Method
  -> Text
  -> Maybe NominalDiffTime
  -> m r
nodeRPCChunkedImpl = nodeRPCChunkedImpl' Aeson.eitherDecode'


nodeRPCChunkedImpl' :: forall a r s e m.
  ( MonadIO m
  , MonadReader s m, HasNodeRPC s
  , MonadError e m, AsRpcError e
  , Monoid r
  )
  => (LBS.ByteString -> Either String a)
  -> Aeson.Encoding
  -> (a -> IO r)
  -> Http.Method
  -> Text
  -> Maybe NominalDiffTime
  -> m r
nodeRPCChunkedImpl' decoder requestBody callback method_ rpcSelector mbTimeout = do
  mgr <- asks (_nodeRPCContext_httpManager . view nodeRPCContext)
  node <- asks (_nodeRPCContext_node . view nodeRPCContext)

  let
    rpcUrl = node <> rpcSelector

    request = rpcBoilerplate method_ requestBody $ Http.parseRequest_ $ T.unpack rpcUrl

  res :: Either Http.HttpException (Either RpcError r) <- liftIO $ try @_ @Http.HttpException $
    Http.withResponse request mgr $ \response -> runExceptT $ do
      flip fix mempty $ \self (leftover, r) -> do
        let
          callbackWithDecode bytes = case decoder bytes of
            Left e -> throwError $ RpcError_NonJSON rpcUrl e bytes
            Right v -> liftIO $ callback v

          callbackMany xs = (r `mappend`) . fold <$> for xs callbackWithDecode

        -- brRead will block the thread in case the connection was broken, so we're waiting
        -- given @timeout'@ before considering that request failed due to timeout
        let readResponseBody = Http.brRead (Http.responseBody response)
        mbChunk <- liftIO $ maybe (Just <$> readResponseBody)
          (\timeout' -> timeout (fromIntegral $ nominalDiffTimeToMicroseconds timeout') readResponseBody)
          mbTimeout

        chunk <- maybe (liftIO $ throwIO $ Http.HttpExceptionRequest request Http.ResponseTimeout) pure mbChunk
        let messages = LBS.split (fromIntegral $ ord '\n') (leftover <> LBS.fromStrict chunk)
        if BS.length chunk == 0 then
          callbackMany messages
        else case unsnoc messages of
          Nothing -> pure r
          Just (xs, x) -> do
            r' <- callbackMany xs
            self (x, r')

  case res of
    Left (httpErr :: Http.HttpException) -> throwError $ rpcResponse_HttpException rpcUrl (T.pack $ show httpErr)
    Right (Left rpcError) -> throwError $ rpcError ^. re asRpcError
    Right (Right r) -> pure r

  where
    nominalDiffTimeToMicroseconds :: NominalDiffTime -> Integer
    nominalDiffTimeToMicroseconds n = (numerator ratio * microsecondsInSecond) `div` denominator ratio
      where
        microsecondsInSecond = 10^(6 :: Integer)
        ratio = toRational n

rpcBoilerplate :: Http.Method -> Aeson.Encoding -> Http.Request -> Http.Request
rpcBoilerplate method_ body req = req
  { Http.method = method_
  , Http.requestBody = if method_ == Http.methodGet then "" else Http.RequestBodyLBS $ Aeson.encodingToLazyByteString body
  , Http.requestHeaders =
    [(Http.hContentType, "application/json") | method_ /= Http.methodGet]
    ++ [ (Http.hUserAgent, "tezos-noderpc/" <> T.encodeUtf8 (T.pack $ showVersion version))
       , (Http.hAccept, "*/*") -- TODO: Probably should pinned to JSON and use "application/json"
       ]
  }

concat <$> traverse makeLenses
  [ 'NodeRPCContext
  ]
