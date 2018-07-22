{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.BlockHeader where

import Data.Word
import Data.Time
import Data.Typeable
import GHC.Generics
import Data.ByteString (ByteString)

import Tezos.Base16ByteString
import Tezos.Base58Check
import Tezos.Fitness
import Tezos.Level
import Tezos.Signature
import Tezos.Json

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
  , _blockHeader_priority :: !Word16
  , _blockHeader_proofOfWorkNonce :: !(Base16ByteString ByteString)
  , _blockHeader_seedNonceHash :: !(Maybe NonceHash)
  , _blockHeader_signature :: !(Maybe Signature)
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

concat <$> traverse deriveTezosJson [ ''BlockHeader ]
