{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.BlockHeader where

import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Time
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)

import Tezos.Base16ByteString
import Tezos.Base58Check
import Tezos.Fitness
import Tezos.Json
import Tezos.Level
import Tezos.Signature

-- TODO: split this into ShellHeader/AlphaProtoHeader/etc
-- AKA: raw_block_header, "block_header.alpha.full_header"
data BlockHeader = BlockHeader
  { _blockHeader_level :: !RawLevel
  , _blockHeader_proto :: !Word8
  , _blockHeader_predecessor :: !BlockHash
  , _blockHeader_timestamp :: !UTCTime
  , _blockHeader_validationPass :: !Word8
  , _blockHeader_operationsHash :: !OperationListListHash
  , _blockHeader_fitness :: !Fitness
  , _blockHeader_context :: !ContextHash
  , _blockHeader_priority :: !Priority
  , _blockHeader_proofOfWorkNonce :: !(Base16ByteString ByteString)
  , _blockHeader_seedNonceHash :: !(Maybe NonceHash)
  , _blockHeader_signature :: !(Maybe Signature)
  }
  deriving (Show, Eq, Ord, Generic, Typeable)
instance NFData BlockHeader

newtype Priority = Priority { unPriority :: Word16 }
  deriving (Eq, Ord, Generic, Typeable, Show, FromJSON, ToJSON, NFData, Hashable)


concat <$> traverse deriveTezosJson [ ''BlockHeader ]
concat <$> traverse makeLenses [ 'BlockHeader ]
