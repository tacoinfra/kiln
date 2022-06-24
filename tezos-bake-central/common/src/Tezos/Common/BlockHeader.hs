{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Tezos.Common.BlockHeader where

import Control.DeepSeq (NFData)
import Control.Lens (makeLenses)
import Data.Time
import Data.Typeable
import Data.Word (Word8)
import GHC.Generics

import Tezos.Common.Base58Check
import Tezos.Common.Fitness
import Tezos.Common.Json
import Tezos.Common.Level

data BlockHeaderFull = BlockHeaderFull
  { _blockHeaderFull_level :: RawLevel
  , _blockHeaderFull_proto :: Word8
  , _blockHeaderFull_predecessor :: BlockHash
  , _blockHeaderFull_timestamp :: UTCTime
  , _blockHeaderFull_fitness :: Fitness
  }
  deriving (Show, Eq, Ord, Generic, Typeable)
instance NFData BlockHeaderFull

-- This is JSON we get from /block/header RPC
data BlockHeader = BlockHeader
  { _blockHeader_level :: RawLevel
  , _blockHeader_hash :: BlockHash
  , _blockHeader_proto :: Word8
  , _blockHeader_protocol :: ProtocolHash
  , _blockHeader_chainId :: ChainId
  , _blockHeader_predecessor :: BlockHash
  , _blockHeader_timestamp :: UTCTime
  , _blockHeader_fitness :: Fitness
  }
  deriving (Show, Eq, Ord, Generic, Typeable)
instance NFData BlockHeader

concat <$> traverse deriveTezosFromJson [ ''BlockHeader, ''BlockHeaderFull]
concat <$> traverse makeLenses [ 'BlockHeader, 'BlockHeaderFull]
