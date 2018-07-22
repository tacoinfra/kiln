{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE InstanceSigs #-}

module Tezos.Block where

-- import Data.Attoparsec.ByteString
import Data.Sequence (Seq)
import Data.Typeable

import Tezos.BalanceUpdate
import Tezos.Base58Check
import Tezos.BlockHeader
import Tezos.Json
import Tezos.Level
import Tezos.Operation
import Tezos.PublicKeyHash
import Tezos.TestChainStatus

-- | "description": "All the information about a block.",
data Block = Block
  { _block_protocol :: ProtocolHash -- ^ "protocol": { "type": "string", "enum": [ "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" ] },
  , _block_chainId :: ChainId -- ^ "chain_id": { "$ref": "#/definitions/Chain_id" }, "hash": { "$ref": "#/definitions/block_hash" },
  , _block_header :: BlockHeader -- ^ "header": { "$ref": "#/definitions/raw_block_header" },
  , _block_metadata :: BlockHeaderMetadata -- ^ "metadata": { "$ref": "#/definitions/block_header_metadata" },
  , _block_operations :: Seq (Seq Operation) -- ^ "operations": { "type": "array", "items": { "type": "array", "items": { "$ref": "#/definitions/operation" } } }
  }
  deriving (Show, Eq, Ord, Typeable)


data MaxOperationListLength = MaxOperationListLength -- ^ "max_operation_list_length": {
  { _maxOperationListLength_maxSize :: !Int -- ^ "max_size": { "type": "integer", "minimum": -1073741824, "maximum": 1073741823 },
  , _maxOperationListLength_maxOp :: !(Maybe Int)-- ^ "max_op": { "type": "integer", "minimum": -1073741824, "maximum": 1073741823 }
  }
  deriving (Show, Eq, Ord, Typeable)



-- | "voting_period_kind": {
data VotingPeriodKind
  =  VotingPeriodKind_Proposal -- ^ { "type": "string", "enum": [ "proposal" ] },
  |  VotingPeriodKind_TestingVote -- ^ { "type": "string", "enum": [ "testing_vote" ] },
  |  VotingPeriodKind_Testing -- ^ { "type": "string", "enum": [ "testing" ] },
  |  VotingPeriodKind_PromotionVote -- ^ { "type": "string", "enum": [ "promotion_vote" ] }
  deriving (Show, Eq, Ord, Typeable)

-- | "block_header_metadata": {
data BlockHeaderMetadata = BlockHeaderMetadata
  { _blockHeaderMetadata_protocol :: !ProtocolHash -- ^ "protocol": { "type": "string", "enum": [ "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" ] },
  , _blockHeaderMetadata_nextProtocol :: !ProtocolHash -- ^ "next_protocol": { "type": "string", "enum": [ "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" ] },
  , _blockHeaderMetadata_testChainStatus :: !TestChainStatus -- ^ "test_chain_status": { "$ref": "#/definitions/test_chain_status" },
  , _blockHeaderMetadata_maxOperationsTtl :: !Int -- ^ "max_operations_ttl": { "type": "integer", "minimum": -1073741824, "maximum": 1073741823 },
  , _blockHeaderMetadata_maxOperationDataLength :: !Int -- ^ "max_operation_data_length": { "type": "integer", "minimum": -1073741824, "maximum": 1073741823 },
  , _blockHeaderMetadata_maxBlockHeaderLength :: !Int -- ^ "max_block_header_length": { "type": "integer", "minimum": -1073741824, "maximum": 1073741823 },
  , _blockHeaderMetadata_maxOperationListLength :: !(Seq MaxOperationListLength) -- ^ "max_operation_list_length": { ... },
  , _blockHeaderMetadata_baker :: !PublicKeyHash -- ^ "baker": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _blockHeaderMetadata_level :: !Level -- ^ "level": { ... },
  , _blockHeaderMetadata_votingPeriodKind :: !VotingPeriodKind -- ^ "voting_period_kind": { ... },
  , _blockHeaderMetadata_nonceHash :: !(Maybe CycleNonce) -- ^ "nonce_hash": { "oneOf": [ { "$ref": "#/definitions/cycle_nonce" }, { "type": "null" } ] },
  , _blockHeaderMetadata_consumedGas :: !TezosWord64 -- ^ "consumed_gas": { "$ref": "#/definitions/positive_bignum" },
  , _blockHeaderMetadata_deactivated :: !(Seq PublicKeyHash)-- ^ "deactivated": { "type": "array", "items": { "$ref": "#/definitions/Signature.Public_key_hash" } },
  , _blockHeaderMetadata_balanceUpdates :: !(Seq BalanceUpdate) -- ^ "balance_updates": { "$ref": "#/definitions/operation_metadata.alpha.balance_updates" }
  }
  deriving (Show, Eq, Ord, Typeable)

concat <$> traverse deriveTezosJson
  [ ''Block
  , ''BlockHeaderMetadata
  , ''MaxOperationListLength
  , ''VotingPeriodKind
  ]
