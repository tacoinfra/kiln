{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Tezos.Mumbai.Block where

import Control.DeepSeq (NFData)
import Control.Lens (Lens')
import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import Data.Foldable (fold)
import Data.Hashable (Hashable)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Tezos.Common.Accusation
import Tezos.Common.Base58Check
import Tezos.Common.Block
import Tezos.Common.Json (deriveTezosJson, deriveTezosFromJson)
import Tezos.Common.Level
import Tezos.Common.PublicKeyHash

import Tezos.Common.BlockHeader
import Tezos.Mumbai.Level
import Tezos.Mumbai.Operation

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
  { _blockMetadata_protocol :: ProtocolHash --  "protocol": { "type": "string", "enum": [ "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" ] },
  , _blockMetadata_baker :: PublicKeyHash --  "baker": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _blockMetadata_levelInfo :: LevelInfo --  "level_info": { ... },
  , _blockMetadata_votingPeriodInfo :: VotingPeriodInfo --  "voting_period_info": { ... },
  , _blockMetadata_deactivated :: Seq PublicKeyHash--  "deactivated": { "type": "array", "items": { "$ref": "#/definitions/Signature.Public_key_hash" } },
  , _blockMetadata_proposer :: Maybe PublicKeyHash --  "proposer": { "$ref": "#/definitions/Signature.Public_key_hash" },
  } deriving (Show, Eq, Ord, Generic, Typeable)

data BranchInfo = BranchInfo
  { _branchInfo_block :: WithProtocolHash VeryBlockLike
  , _branchInfo_cycle :: Cycle
  , _branchInfo_cyclePosition :: RawLevel
  } deriving (Eq, Show, Typeable, Generic)
instance NFData BranchInfo

data VotingPeriodInfo = VotingPeriodInfo
  { _votingPeriodInfo_votingPeriod :: VotingPeriod -- "voting_period" : { ... },
  , _votingPeriodInfo_position :: RawLevel -- "position": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _votingPeriodInfo_remaining :: Maybe RawLevel -- "remaining": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  } deriving (Show, Eq, Ord, Generic, Typeable)

data VotingPeriod = VotingPeriod
  { _votingPeriod_kind :: VotingPeriodKind -- "kind" : { ... };
  , _votingPeriod_index :: RawLevel -- "index": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  } deriving (Show, Eq, Ord, Generic, Typeable)

-- | "voting_period_kind": {
data VotingPeriodKind
  = VotingPeriodKind_Proposal --  { "type": "string", "enum": [ "proposal" ] },
  | VotingPeriodKind_Exploration --  { "type": "string", "enum": [ "exploration" ] },
  | VotingPeriodKind_Cooldown --  { "type": "string", "enum": [ "cooldown" ] },
  | VotingPeriodKind_Promotion --  { "type": "string", "enum": [ "promotion" ] }
  | VotingPeriodKind_Adoption --  { "type": "string", "enum": [ "adoption" ] }
  deriving (Eq, Ord, Read, Show, Generic, Typeable, Bounded, Enum)
instance Hashable VotingPeriodKind
instance NFData VotingPeriodKind

instance Aeson.ToJSONKey VotingPeriodKind
instance Aeson.FromJSONKey VotingPeriodKind

concat <$> traverse deriveTezosJson
  [ ''BranchInfo
  , ''VotingPeriodKind
  ]

concat <$> traverse deriveTezosFromJson
  [ ''Block
  , ''BlockMetadata
  , ''VotingPeriod
  , ''VotingPeriodInfo
  ]

concat <$> traverse makeLenses
  [ 'Block
  , 'BlockMetadata
  , 'BranchInfo
  , 'VotingPeriod
  , 'VotingPeriodInfo
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

class HasBlockMetadata a where
  blockMetadata :: Lens' a BlockMetadata

instance HasBlockMetadata Block where
  blockMetadata = block_metadata

instance HasChainId Block where
  chainIdL = block_chainId

instance MayHaveAccusations Block where
  getAccusations = getAccusations . fold . Seq.lookup 2 . _block_operations

instance BlockLike BranchInfo where
  hash = branchInfo_block . hash
  predecessor = branchInfo_block . predecessor
  fitness = branchInfo_block . fitness
  level = branchInfo_block . level
  timestamp = branchInfo_block . timestamp

instance HasProtocolHash BranchInfo where
  protocolHash = branchInfo_block . protocolHash
