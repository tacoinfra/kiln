{-# LANGUAGE CPP #-}

module Tezos.Common.NodeRPC.Types where

import Control.Lens (Prism', re, (^.))
#if !(MIN_VERSION_base(4,9,0))
import Data.Semigroup
#endif
import Control.Exception.Safe (Exception)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Typeable (Typeable)

type RpcResponse = Either RpcError
data RpcError
  = RpcError_HttpException Text Text
  | RpcError_UnexpectedStatus Text Int BS.ByteString
  | RpcError_NonJSON Text String LBS.ByteString
  | RpcError_RestrictedEndpoint Text
  deriving (Eq, Ord, Show, Typeable)
instance Exception RpcError

class AsRpcError e where
  asRpcError :: Prism' e RpcError

instance AsRpcError RpcError where
  asRpcError = id

rpcResponse_HttpException :: (AsRpcError e) => Text -> Text -> e
rpcResponse_HttpException url x = RpcError_HttpException url x ^. re asRpcError

rpcResponse_UnexpectedStatus :: (AsRpcError e) => Text -> Int -> BS.ByteString -> e
rpcResponse_UnexpectedStatus url x y = RpcError_UnexpectedStatus url x y ^. re asRpcError

rpcResponse_NonJSON :: (AsRpcError e) => Text -> String -> LBS.ByteString -> e
rpcResponse_NonJSON url x y = RpcError_NonJSON url x y ^. re asRpcError

rpcResponse_RestrictedEndpoint :: (AsRpcError e) => Text -> e
rpcResponse_RestrictedEndpoint url = RpcError_RestrictedEndpoint url ^. re asRpcError
