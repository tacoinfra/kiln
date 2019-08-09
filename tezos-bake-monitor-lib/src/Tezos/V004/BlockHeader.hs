{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.V004.BlockHeader where

import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary.Get (isolate)
import Data.Bits (Bits)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Hashable (Hashable)
import Data.Time
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)

import Tezos.Common.Base16ByteString
import Tezos.Common.Base58Check
import Tezos.Common.Binary ((<**))
import qualified Tezos.Common.Binary as B
import Tezos.V004.Fitness
import Tezos.Common.Json
import Tezos.V004.Level
import Tezos.V004.Signature

-- This is the JSON embedded in Block
-- and this is called 'full', because its called $block_header.alpha.full_header in RPC spec
data BlockHeaderFull = BlockHeaderFull
  { _blockHeaderFull_level :: !RawLevel
  , _blockHeaderFull_proto :: !Word8
  , _blockHeaderFull_predecessor :: !BlockHash
  , _blockHeaderFull_timestamp :: !UTCTime
  , _blockHeaderFull_validationPass :: !Word8
  , _blockHeaderFull_operationsHash :: !OperationListListHash
  , _blockHeaderFull_fitness :: !Fitness
  , _blockHeaderFull_context :: !ContextHash
  , _blockHeaderFull_priority :: !Priority
  , _blockHeaderFull_proofOfWorkNonce :: !(Base16ByteString ByteString)
  , _blockHeaderFull_seedNonceHash :: !(Maybe NonceHash)
  , _blockHeaderFull_signature :: !(Maybe Signature)
  }
  deriving (Show, Eq, Ord, Generic, Typeable)
instance NFData BlockHeaderFull

-- This is JSON we get from /block/header RPC
data BlockHeader = BlockHeader
  { _blockHeader_level :: !RawLevel
  , _blockHeader_hash :: !BlockHash
  , _blockHeader_proto :: !Word8
  , _blockHeader_protocol :: !ProtocolHash
  , _blockHeader_chainId :: !ChainId
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

-- Version-specific fragment of the block header.
-- RPC reference: GET ../<block_id>/header/protocol_data
data BlockHeaderProto = BlockHeaderProto
  { _blockHeaderProto_protocol :: !ProtocolHash
  , _blockHeaderProto_priority :: !Priority
  , _blockHeaderProto_proofOfWorkNonce :: !(Base16ByteString ByteString)
  , _blockHeaderProto_seedNonceHash :: !(Maybe NonceHash)
  , _blockHeaderProto_signature :: !(Maybe Signature)
  }
  deriving (Show, Eq, Ord, Generic, Typeable)
instance NFData BlockHeaderProto

-- This is embedded in other structures like Checkpoint
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

instance B.TezosUnsignedBinary BlockHeaderFull where
  putUnsigned = shellHeaderEncoding <** contentsEncoding
    where
      shellHeaderEncoding = B.puts _blockHeaderFull_level <** B.puts _blockHeaderFull_proto
        <** B.puts _blockHeaderFull_predecessor <** B.puts _blockHeaderFull_timestamp
        <** B.puts _blockHeaderFull_validationPass <** B.puts _blockHeaderFull_operationsHash
        <** B.puts _blockHeaderFull_fitness <** B.puts _blockHeaderFull_context
      contentsEncoding = B.puts _blockHeaderFull_priority <** B.puts _blockHeaderFull_proofOfWorkNonce
        <** B.puts _blockHeaderFull_seedNonceHash
  getUnsigned = thenContentsDecoding shellHeaderDecoding
    where
      shellHeaderDecoding = pure BlockHeaderFull
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

instance B.TezosBinary BlockHeaderFull where
  put bh = B.putUnsigned bh *> traverse_ B.put (_blockHeaderFull_signature bh)
  get = do
    bh <- B.getUnsigned
    sig <- B.get
    pure $ bh { _blockHeaderFull_signature = Just sig }

concat <$> traverse deriveTezosJson [ ''BlockHeader, ''BlockHeaderFull, ''BlockHeaderProto, ''BlockHeaderShell]
concat <$> traverse makeLenses [ 'BlockHeader, 'BlockHeaderFull, 'BlockHeaderProto, 'BlockHeaderShell ]
