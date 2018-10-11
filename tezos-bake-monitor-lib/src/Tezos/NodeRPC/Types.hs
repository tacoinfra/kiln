{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.NodeRPC.Types where

import Control.Lens (Prism', re, (^.))
import Control.Lens.TH (makeLenses)
import Data.Int (Int32)
#if !(MIN_VERSION_base(4,9,0))
import Data.Semigroup
#endif
import Control.Exception.Safe (Exception)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Typeable (Typeable)

import Tezos.Json (deriveTezosJson)
import Tezos.Types

type RpcResponse = Either RpcError
data RpcError
  = RpcError_HttpException Text
  | RpcError_UnexpectedStatus Int BS.ByteString
  | RpcError_NonJSON String LBS.ByteString
  deriving (Eq, Ord, Show, Typeable)
instance Exception RpcError

class AsRpcError e where
  asRpcError :: Prism' e RpcError

instance AsRpcError RpcError where
  asRpcError = id

rpcResponse_HttpException :: (AsRpcError e) => Text -> e
rpcResponse_HttpException x = RpcError_HttpException x ^. re asRpcError

rpcResponse_UnexpectedStatus :: (AsRpcError e) => Int -> BS.ByteString -> e
rpcResponse_UnexpectedStatus x y = RpcError_UnexpectedStatus x y ^. re asRpcError

rpcResponse_NonJSON :: (AsRpcError e) => String -> LBS.ByteString -> e
rpcResponse_NonJSON x y = RpcError_NonJSON x y ^. re asRpcError

data NetworkStat = NetworkStat
  { _networkStat_totalSent      :: TezosWord64 -- bytes
  , _networkStat_totalRecv      :: TezosWord64 -- bytes
  , _networkStat_currentInflow  :: Int32 -- bytes/s
  , _networkStat_currentOutflow :: Int32 -- bytes/s
  } deriving (Eq, Ord, Show, Typeable)

newtype BlockPrefix = BlockPrefix Text
  deriving (Eq, Show, Typeable)

concat <$> traverse deriveTezosJson
  [ ''NetworkStat
  ]

concat <$> traverse makeLenses
 [ 'NetworkStat
 ]

