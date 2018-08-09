{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.Block where

-- import Data.Attoparsec.ByteString
import Control.Lens (Lens')
import Control.Lens.TH (makeLenses)
import Data.Sequence (Seq)
import Data.Time
import Data.Typeable
import Data.Word

import Tezos.BalanceUpdate
import Tezos.Base58Check
import Tezos.BlockHeader
import Tezos.Fitness
import Tezos.Json
import Tezos.Level
import Tezos.Operation
import Tezos.PublicKeyHash
import Tezos.TestChainStatus

-- | "description": "All the information about a block.",
data Block = Block
  { _block_protocol :: ProtocolHash --  "protocol": { "type": "string", "enum": [ "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" ] },
  , _block_chainId :: ChainId --  "chain_id": { "$ref": "#/definitions/Chain_id" },
  , _block_hash :: BlockHash -- "hash": { "$ref": "#/definitions/block_hash" },
  , _block_header :: BlockHeader --  "header": { "$ref": "#/definitions/raw_block_header" },
  , _block_metadata :: BlockMetadata --  "metadata": { "$ref": "#/definitions/block_header_metadata" },
  , _block_operations :: Seq (Seq Operation) --  "operations": { "type": "array", "items": { "type": "array", "items": { "$ref": "#/definitions/operation" } } }
  }
  deriving (Show, Eq, Ord, Typeable)

data MaxOperationListLength = MaxOperationListLength --  "max_operation_list_length": {
  { _maxOperationListLength_maxSize :: !Int --  "max_size": { "type": "integer", "minimum": -1073741824, "maximum": 1073741823 },
  , _maxOperationListLength_maxOp :: !(Maybe Int)--  "max_op": { "type": "integer", "minimum": -1073741824, "maximum": 1073741823 }
  }
  deriving (Show, Eq, Ord, Typeable)



-- | "voting_period_kind": {
data VotingPeriodKind
  =  VotingPeriodKind_Proposal --  { "type": "string", "enum": [ "proposal" ] },
  |  VotingPeriodKind_TestingVote --  { "type": "string", "enum": [ "testing_vote" ] },
  |  VotingPeriodKind_Testing --  { "type": "string", "enum": [ "testing" ] },
  |  VotingPeriodKind_PromotionVote --  { "type": "string", "enum": [ "promotion_vote" ] }
  deriving (Show, Eq, Ord, Typeable)

-- | "block_header_metadata": {
data BlockMetadata = BlockMetadata
  { _blockMetadata_protocol :: !ProtocolHash --  "protocol": { "type": "string", "enum": [ "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" ] },
  , _blockMetadata_nextProtocol :: !ProtocolHash --  "next_protocol": { "type": "string", "enum": [ "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" ] },
  , _blockMetadata_testChainStatus :: !TestChainStatus --  "test_chain_status": { "$ref": "#/definitions/test_chain_status" },
  , _blockMetadata_maxOperationsTtl :: !Int --  "max_operations_ttl": { "type": "integer", "minimum": -1073741824, "maximum": 1073741823 },
  , _blockMetadata_maxOperationDataLength :: !Int --  "max_operation_data_length": { "type": "integer", "minimum": -1073741824, "maximum": 1073741823 },
  , _blockMetadata_maxBlockHeaderLength :: !Int --  "max_block_header_length": { "type": "integer", "minimum": -1073741824, "maximum": 1073741823 },
  , _blockMetadata_maxOperationListLength :: !(Seq MaxOperationListLength) --  "max_operation_list_length": { ... },
  , _blockMetadata_baker :: !PublicKeyHash --  "baker": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _blockMetadata_level :: !Level --  "level": { ... },
  , _blockMetadata_votingPeriodKind :: !VotingPeriodKind --  "voting_period_kind": { ... },
  , _blockMetadata_nonceHash :: !(Maybe CycleNonce) --  "nonce_hash": { "oneOf": [ { "$ref": "#/definitions/cycle_nonce" }, { "type": "null" } ] },
  , _blockMetadata_consumedGas :: !TezosWord64 --  "consumed_gas": { "$ref": "#/definitions/positive_bignum" },
  , _blockMetadata_deactivated :: !(Seq PublicKeyHash)--  "deactivated": { "type": "array", "items": { "$ref": "#/definitions/Signature.Public_key_hash" } },
  , _blockMetadata_balanceUpdates :: !(Seq BalanceUpdate) --  "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  }
  deriving (Show, Eq, Ord, Typeable)

data MonitorBlock = MonitorBlock
  { _monitorBlock_hash :: BlockHash
  , _monitorBlock_level :: RawLevel
  , _monitorBlock_proto :: Word8
  , _monitorBlock_predecessor :: BlockHash
  , _monitorBlock_timestamp :: UTCTime
  , _monitorBlock_validationPass :: Word8
  , _monitorBlock_operationsHash :: OperationListListHash
  , _monitorBlock_fitness :: Fitness
  , _monitorBlock_context :: ContextHash
  -- , _monitorBlock_protocolData :: Base16ByteString ??? -- Certainly NOT a blockheader...
  } deriving (Eq, Ord, Show)

concat <$> traverse deriveTezosJson
  [ ''Block
  , ''BlockMetadata
  , ''MaxOperationListLength
  , ''VotingPeriodKind
  , ''MonitorBlock
  ]

concat <$> traverse makeLenses
 [ 'Block
 , 'BlockMetadata
 , 'MaxOperationListLength --  "max_operation_list_length": {
 , 'MonitorBlock
 ]


class BlockLike b where
  -- chain :: Lens' b ChainId
  hash :: Lens' b BlockHash
  predecessor :: Lens' b BlockHash
  level :: Lens' b RawLevel
  fitness :: Lens' b Fitness
  timestamp :: Lens' b UTCTime

instance BlockLike Block where
  hash = block_hash
  predecessor = block_header . blockHeader_predecessor
  level = block_header . blockHeader_level
  fitness = block_header . blockHeader_fitness
  timestamp = block_header . blockHeader_timestamp

instance BlockLike MonitorBlock where
  hash = monitorBlock_hash
  predecessor = monitorBlock_predecessor
  level = monitorBlock_level
  fitness = monitorBlock_fitness
  timestamp = monitorBlock_timestamp

instance HasBalanceUpdates Block where
  balanceUpdates f blk = blk' <$> md' <*> ops'
    where
      blk' x y = blk {_block_metadata = x, _block_operations = y}
      md' = (blockMetadata_balanceUpdates . traverse) f $ _block_metadata blk
      ops' = (traverse . traverse . balanceUpdates) f $ _block_operations blk

