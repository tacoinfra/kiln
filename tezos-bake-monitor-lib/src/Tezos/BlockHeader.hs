{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.BlockHeader where

import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary.Get (isolate)
import Data.Bits (Bits)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Time
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)

import Tezos.Base16ByteString
import Tezos.Base58Check
import Tezos.Binary ((<**))
import qualified Tezos.Binary as B
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

data BlockHeaderShell = BlockHeaderShell
  { _blockHeaderShell_level :: !RawLevel
  , _blockHeaderShell_proto :: !Word8
  , _blockHeaderShell_predecessor :: !BlockHash
  , _blockHeaderShell_timestamp :: !UTCTime
  , _blockHeaderShell_validationPass :: !Word8
  , _blockHeaderShell_operationsHash :: !OperationListListHash
  , _blockHeaderShell_fitness :: !Fitness
  , _blockHeaderShell_context :: !ContextHash
  }
  deriving (Show, Eq, Ord, Generic, Typeable)
instance NFData BlockHeaderShell

newtype Priority = Priority { unPriority :: Word16 }
  deriving (Eq, Ord, Generic, Typeable, Show, FromJSON, ToJSON, NFData, Hashable, Enum, Num, Integral, Real, Bits, B.TezosBinary)

instance B.TezosUnsignedBinary BlockHeader where
  putUnsigned = shellHeaderEncoding <** contentsEncoding
    where
      shellHeaderEncoding = B.puts _blockHeader_level <** B.puts _blockHeader_proto
        <** B.puts _blockHeader_predecessor <** B.puts _blockHeader_timestamp
        <** B.puts _blockHeader_validationPass <** B.puts _blockHeader_operationsHash
        <** B.puts _blockHeader_fitness <** B.puts _blockHeader_context
      contentsEncoding = B.puts _blockHeader_priority <** B.puts _blockHeader_proofOfWorkNonce
        <** B.puts _blockHeader_seedNonceHash
  getUnsigned = thenContentsDecoding shellHeaderDecoding
    where
      shellHeaderDecoding = pure BlockHeader
        <*> B.get -- level
        <*> B.get -- proto
        <*> B.get -- predecessor
        <*> B.get -- timestamp
        <*> B.get -- validationPass
        <*> B.get -- operationsHash
        <*> B.get -- fitness
        <*> B.get -- context
      thenContentsDecoding shellHeader = shellHeader
        <*> B.get -- priority
        <*> isolate 8 B.get -- proofOfWorkNonce
                            -- Length of this is a 'fixed' protocol parameter, meaning it
                            -- can change at protocol upgrades, but it hasn't done so yet.
                            -- May need to look out for this in the future. 
        <*> B.get -- seedNonceHash
        <*> pure Nothing -- no signature yet, because it's unsigned

concat <$> traverse deriveTezosJson [ ''BlockHeader, ''BlockHeaderShell]
concat <$> traverse makeLenses [ 'BlockHeader, 'BlockHeaderShell ]
