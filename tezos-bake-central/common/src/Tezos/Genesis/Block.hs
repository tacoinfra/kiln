{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Tezos.Genesis.Block where

import Control.Lens.TH (makeLenses)
import Data.Foldable (fold)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Tezos.Common.Accusation
import Tezos.Common.Base58Check
import Tezos.Common.Block
import Tezos.Common.Json (deriveTezosFromJson)

import Tezos.Common.BlockHeader
import Tezos.Oxford.Operation

-- | "description": "All the information about a block.",
data Block = Block
  { _block_protocol :: ProtocolHash --  "protocol": { "type": "string", "enum": [ "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" ] },
  , _block_chainId :: ChainId --  "chain_id": { "$ref": "#/definitions/Chain_id" },
  , _block_hash :: BlockHash -- "hash": { "$ref": "#/definitions/block_hash" },
  , _block_header :: BlockHeaderFull --  "header": { "$ref": "#/definitions/raw_block_header" }, (we actually get $block_header.alpha.full_header)
  , _block_metadata :: BlockMetadata --  "metadata": { "$ref": "#/definitions/block_header_metadata" },
  , _block_operations :: Seq (Seq Operation) --  "operations": { "type": "array", "items": { "type": "array", "items": { "$ref": "#/definitions/operation" } } }
  } deriving (Eq, Ord, Show, Generic, Typeable)

-- | "block_header_metadata": {
data BlockMetadata = BlockMetadata
  { _blockMetadata_protocol :: ProtocolHash
  } deriving (Show, Eq, Ord, Generic, Typeable)

concat <$> traverse deriveTezosFromJson
  [ ''Block
  , ''BlockMetadata
  ]

concat <$> traverse makeLenses
  [ 'Block
  , 'BlockMetadata
  ]

toBlockHeader :: Block -> BlockHeader
toBlockHeader blk = BlockHeader
  { _blockHeader_level = _blockHeaderFull_level blkH
  , _blockHeader_hash = _block_hash blk
  , _blockHeader_proto = _blockHeaderFull_proto blkH
  , _blockHeader_protocol = _block_protocol blk
  , _blockHeader_chainId = _block_chainId blk
  , _blockHeader_predecessor = _blockHeaderFull_predecessor blkH
  , _blockHeader_timestamp = _blockHeaderFull_timestamp blkH
  , _blockHeader_fitness = _blockHeaderFull_fitness blkH
  }
  where blkH = _block_header blk

instance BlockLike Block where
  hash = block_hash
  predecessor = block_header . blockHeaderFull_predecessor
  level = block_header . blockHeaderFull_level
  fitness = block_header . blockHeaderFull_fitness
  timestamp = block_header . blockHeaderFull_timestamp

instance HasProtocolHash Block where
  protocolHash = block_protocol

instance HasBlockHeaderFull Block where
  blockHeaderFull = block_header

instance HasChainId Block where
  chainIdL = block_chainId

instance MayHaveAccusations Block where
  getAccusations = getAccusations . fold . Seq.lookup 2 . _block_operations
