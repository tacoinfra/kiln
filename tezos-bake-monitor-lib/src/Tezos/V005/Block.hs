{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Tezos.V005.Block (module Tezos.V005.Block, module Old) where

import Control.Lens.TH (makeLenses)
import Data.Sequence (Seq)
import Data.Time
import Data.Typeable (Typeable)
import Data.Word
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Tezos.Common.Base58Check
import Tezos.Common.Json

import Tezos.V005.BalanceUpdate
import Tezos.V005.BlockHeader
import Tezos.V005.Fitness
import Tezos.V005.Level
import Tezos.V005.Operation

import Tezos.V004.Block as Old hiding
  ( Block(..), MonitorBlock(..), toBlockHeader
  , block_hash, block_header, block_chainId, block_metadata , block_protocol, block_operations
  , monitorBlock_hash, monitorBlock_level, monitorBlock_proto, monitorBlock_predecessor, monitorBlock_timestamp
  , monitorBlock_fitness, monitorBlock_context, monitorBlock_operationsHash, monitorBlock_validationPass
  )

-- | "description": "All the information about a block.",
data Block = Block
  { _block_protocol :: !ProtocolHash --  "protocol": { "type": "string", "enum": [ "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" ] },
  , _block_chainId :: !ChainId --  "chain_id": { "$ref": "#/definitions/Chain_id" },
  , _block_hash :: !BlockHash -- "hash": { "$ref": "#/definitions/block_hash" },
  , _block_header :: !BlockHeaderFull --  "header": { "$ref": "#/definitions/raw_block_header" }, (we actually get $block_header.alpha.full_header)
  , _block_metadata :: !BlockMetadata --  "metadata": { "$ref": "#/definitions/block_header_metadata" },
  , _block_operations :: !(Seq (Seq Operation)) --  "operations": { "type": "array", "items": { "type": "array", "items": { "$ref": "#/definitions/operation" } } }
  } deriving (Eq, Ord, Show, Generic, Typeable)


data MonitorBlock = MonitorBlock
  { _monitorBlock_hash :: !BlockHash
  , _monitorBlock_level :: !RawLevel
  , _monitorBlock_proto :: !Word8
  , _monitorBlock_predecessor :: !BlockHash
  , _monitorBlock_timestamp :: !UTCTime
  , _monitorBlock_validationPass :: !Word8
  , _monitorBlock_operationsHash :: !OperationListListHash
  , _monitorBlock_fitness :: !Fitness
  , _monitorBlock_context :: !ContextHash
  -- , _monitorBlock_protocolData :: Base16ByteString ??? -- Certainly NOT a blockheader...
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance NFData MonitorBlock

toBlockHeader :: Block -> BlockHeader
toBlockHeader blk = BlockHeader
  { _blockHeader_level = _blockHeaderFull_level blkH
  , _blockHeader_hash = _block_hash blk
  , _blockHeader_proto = _blockHeaderFull_proto blkH
  , _blockHeader_protocol = _block_protocol blk
  , _blockHeader_chainId = _block_chainId blk
  , _blockHeader_predecessor = _blockHeaderFull_predecessor blkH
  , _blockHeader_timestamp = _blockHeaderFull_timestamp blkH
  , _blockHeader_validationPass = _blockHeaderFull_validationPass blkH
  , _blockHeader_operationsHash = _blockHeaderFull_operationsHash blkH
  , _blockHeader_fitness = _blockHeaderFull_fitness blkH
  , _blockHeader_context = _blockHeaderFull_context blkH
  , _blockHeader_priority = _blockHeaderFull_priority blkH
  , _blockHeader_proofOfWorkNonce = _blockHeaderFull_proofOfWorkNonce blkH
  , _blockHeader_seedNonceHash = _blockHeaderFull_seedNonceHash blkH
  , _blockHeader_signature = _blockHeaderFull_signature blkH
  }
  where blkH = _block_header blk

concat <$> traverse deriveTezosJson
  [ ''Block
  , ''MonitorBlock
  ]

concat <$> traverse makeLenses
  [ 'Block
  , 'MonitorBlock
  ]

instance BlockLike Block where
  hash = block_hash
  predecessor = block_header . blockHeaderFull_predecessor
  level = block_header . blockHeaderFull_level
  fitness = block_header . blockHeaderFull_fitness
  timestamp = block_header . blockHeaderFull_timestamp

instance HasProtocolHash Block where
  protocolHash = block_protocol

instance HasChainId Block where
  chainIdL = block_chainId

instance BlockLike MonitorBlock where
  hash = monitorBlock_hash
  predecessor = monitorBlock_predecessor
  level = monitorBlock_level
  fitness = monitorBlock_fitness
  timestamp = monitorBlock_timestamp

instance HasBlockHeaderFull Block where
  blockHeaderFull = block_header

instance HasBlockMetadata Block where
  blockMetadata = block_metadata

instance HasBalanceUpdates Block where
  balanceUpdates f blk = blk' <$> md' <*> ops'
    where
      blk' x y = blk {_block_metadata = x, _block_operations = y}
      md' = (blockMetadata_balanceUpdates . traverse) f $ _block_metadata blk
      ops' = (traverse . traverse . balanceUpdates) f $ _block_operations blk
