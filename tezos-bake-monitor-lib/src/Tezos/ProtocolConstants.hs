{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.ProtocolConstants where

import Data.Typeable
import Data.Word

import Tezos.Tez
import Tezos.Json
import Tezos.PeriodSequence
import Tezos.Level

data ProtoInfo = ProtoInfo
  { _protoInfo_proofOfWorkNonceSize :: !Word8 -- "proof_of_work_nonce_size": { "type": "integer", "minimum": 0, "maximum": 255 },
  , _protoInfo_nonceLength :: !Word8 -- "nonce_length": { "type": "integer", "minimum": 0, "maximum": 255 },
  , _protoInfo_maxRevelationsPerBlock :: !Word8 -- "max_revelations_per_block": { "type": "integer", "minimum": 0, "maximum": 255 },
  , _protoInfo_maxOperationDataLength :: !Int -- "max_operation_data_length": { "type": "integer", "minimum": -1073741824, "maximum": 1073741823 },
  , _protoInfo_preservedCycles :: !Cycle -- "preserved_cycles": { "type": "integer", "minimum": 0, "maximum": 255 },
  , _protoInfo_blocksPerCycle :: !RawLevel -- "blocks_per_cycle": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _protoInfo_blocksPerCommitment :: !RawLevel -- "blocks_per_commitment": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _protoInfo_blocksPerRollSnapshot :: !RawLevel -- "blocks_per_roll_snapshot": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _protoInfo_blocksPerVotingPeriod :: !RawLevel -- "blocks_per_voting_period": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _protoInfo_timeBetweenBlocks :: !PeriodSequence -- "time_between_blocks": { "type": "array", "items": { "$ref": "#/definitions/int64" } },
  -- this doesn't compute, this param has type Word16, but the 'slots' parameter in `block/metadata' only has room for 256 endorsements
  , _protoInfo_endorsersPerBlock :: !Word16 -- "endorsers_per_block": { "type": "integer", "minimum": 0, "maximum": 65535 },
  , _protoInfo_hardGasLimitPerOperation :: !TezosWord64 -- "hard_gas_limit_per_operation": { "$ref": "#/definitions/bignum" },
  , _protoInfo_hardGasLimitPerBlock :: !TezosWord64 -- "hard_gas_limit_per_block": { "$ref": "#/definitions/bignum" },
  , _protoInfo_proofOfWorkThreshold :: !TezosWord64 -- "proof_of_work_threshold": { "$ref": "#/definitions/int64" },
  , _protoInfo_tokensPerRoll :: !Tez -- "tokens_per_roll": { "$ref": "#/definitions/mutez" },
  , _protoInfo_michelsonMaximumTypeSize :: !Word16 -- "michelson_maximum_type_size": { "type": "integer", "minimum": 0, "maximum": 65535 },
  , _protoInfo_seedNonceRevelationTip :: !Tez -- "seed_nonce_revelation_tip": { "$ref": "#/definitions/mutez" },
  , _protoInfo_originationBurn :: !Tez -- "origination_burn": { "$ref": "#/definitions/mutez" },
  , _protoInfo_blockSecurityDeposit :: !Tez -- "block_security_deposit": { "$ref": "#/definitions/mutez" },
  , _protoInfo_endorsementSecurityDeposit :: !Tez -- "endorsement_security_deposit": { "$ref": "#/definitions/mutez" },
  , _protoInfo_blockReward :: !Tez -- "block_reward": { "$ref": "#/definitions/mutez" },
  , _protoInfo_endorsementReward :: !Tez -- "endorsement_reward": { "$ref": "#/definitions/mutez" },
  , _protoInfo_costPerByte :: !Tez -- "cost_per_byte": { "$ref": "#/definitions/mutez" },
  , _protoInfo_hardStorageLimitPerOperation :: !TezosWord64 -- "hard_storage_limit_per_operation": { "$ref": "#/definitions/bignum" }
  } deriving (Eq, Ord, Show, Typeable)
deriveTezosJson ''ProtoInfo
