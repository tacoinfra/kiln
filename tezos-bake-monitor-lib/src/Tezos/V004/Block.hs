{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Tezos.V004.Block where

import Control.Lens (Lens', (^.))
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (parseJSON), ToJSON)
import qualified Data.Aeson as Aeson
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq)
import Data.Time
import Data.Typeable (Typeable)
import Data.Word
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Tezos.V004.BalanceUpdate
import Tezos.Common.Base58Check
import Tezos.V004.BlockHeader
import Tezos.V004.Fitness
import Tezos.Common.Json
import Tezos.V004.Level
import Tezos.V004.Operation
import Tezos.V004.PublicKeyHash
import Tezos.V004.TestChainStatus

-- | "description": "All the information about a block.",
data Block = Block
  { _block_protocol :: !ProtocolHash --  "protocol": { "type": "string", "enum": [ "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" ] },
  , _block_chainId :: !ChainId --  "chain_id": { "$ref": "#/definitions/Chain_id" },
  , _block_hash :: !BlockHash -- "hash": { "$ref": "#/definitions/block_hash" },
  , _block_header :: !BlockHeaderFull --  "header": { "$ref": "#/definitions/raw_block_header" }, (we actually get $block_header.alpha.full_header)
  , _block_metadata :: !BlockMetadata --  "metadata": { "$ref": "#/definitions/block_header_metadata" },
  , _block_operations :: !(Seq (Seq Operation)) --  "operations": { "type": "array", "items": { "type": "array", "items": { "$ref": "#/definitions/operation" } } }
  } deriving (Eq, Ord, Show, Generic, Typeable)

data MaxOperationListLength = MaxOperationListLength --  "max_operation_list_length": {
  { _maxOperationListLength_maxSize :: !Int --  "max_size": { "type": "integer", "minimum": -1073741824, "maximum": 1073741823 },
  , _maxOperationListLength_maxOp :: !(Maybe Int)--  "max_op": { "type": "integer", "minimum": -1073741824, "maximum": 1073741823 }
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance Hashable MaxOperationListLength
instance NFData MaxOperationListLength



-- | "voting_period_kind": {
data VotingPeriodKind
  = VotingPeriodKind_Proposal --  { "type": "string", "enum": [ "proposal" ] },
  | VotingPeriodKind_TestingVote --  { "type": "string", "enum": [ "testing_vote" ] },
  | VotingPeriodKind_Testing --  { "type": "string", "enum": [ "testing" ] },
  | VotingPeriodKind_PromotionVote --  { "type": "string", "enum": [ "promotion_vote" ] }
  deriving (Eq, Ord, Read, Show, Generic, Typeable, Bounded, Enum)
instance Hashable VotingPeriodKind
instance NFData VotingPeriodKind

instance Aeson.ToJSONKey VotingPeriodKind
instance Aeson.FromJSONKey VotingPeriodKind

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
  } deriving (Show, Eq, Ord, Generic, Typeable)

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

data VeryBlockLike = VeryBlockLike
  { _veryBlockLike_hash :: !BlockHash
  , _veryBlockLike_predecessor :: !BlockHash
  , _veryBlockLike_fitness :: !Fitness
  , _veryBlockLike_level :: !RawLevel
  , _veryBlockLike_timestamp :: !UTCTime
  } deriving (Eq, Ord, Show, Typeable, Generic)
instance NFData VeryBlockLike

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

-- | Simple wrapper for adding a protocol hash to some other value.
--
-- The Aeson instances are smart (too smart). If the underlying type
-- is not an object or an object with a conflicting key with the one
-- added by this type, the JSON encoding will be two-layered.
-- Otherwise, the JSON encoding will simply add an additional key
-- for the protocol information.
data WithProtocolHash a = WithProtocolHash
  { _withProtocolHash_value :: !a
  , _withProtocolHash_protocolHash :: !ProtocolHash
  } deriving (Eq, Ord, Show, Read, Generic, Typeable)
instance NFData a => NFData (WithProtocolHash a)

instance ToJSON a => ToJSON (WithProtocolHash a) where
  toJSON (WithProtocolHash a protoHash) = case Aeson.toJSON a of
    v@(Aeson.Object o)
      | "protocol" `HashMap.member` o -> fallback v
      | otherwise -> Aeson.Object $ HashMap.insert "protocol" (Aeson.toJSON protoHash) o
    v -> fallback v
    where
      fallback v = Aeson.object ["value" Aeson..= v, "protocol" Aeson..= Aeson.toJSON protoHash]

instance FromJSON a => FromJSON (WithProtocolHash a) where
  parseJSON json = Aeson.withObject "WithProtocolHash" (\o -> do
    protoHash <- o Aeson..: "protocol"
    (if HashMap.size o == 2 then o Aeson..:? "value" else pure Nothing) >>= \case
      Nothing -> WithProtocolHash <$> Aeson.parseJSON json <*> pure protoHash
      Just val -> pure $ WithProtocolHash val protoHash) json


concat <$> traverse deriveTezosJson
  [ ''Block
  , ''BlockMetadata
  , ''MaxOperationListLength
  , ''MonitorBlock
  , ''VotingPeriodKind
  , ''VeryBlockLike
  ]

concat <$> traverse makeLenses
  [ 'Block
  , 'BlockMetadata
  , 'MaxOperationListLength --  "max_operation_list_length": {
  , 'MonitorBlock
  , 'VeryBlockLike
  , 'WithProtocolHash
  ]

class BlockLike b where
  -- chain :: Lens' b ChainId
  hash :: Lens' b BlockHash
  predecessor :: Lens' b BlockHash
  level :: Lens' b RawLevel
  fitness :: Lens' b Fitness
  timestamp :: Lens' b UTCTime

class HasProtocolHash a where
  protocolHash :: Lens' a ProtocolHash

-- TODO: Can this be in blocklike instead?
class HasChainId a where
  -- chainId clashed with a lot of code, so lets avoid that for now
  chainIdL :: Lens' a ChainId

class HasBlockMetadata a where
  blockMetadata :: Lens' a BlockMetadata
  
class HasBlockHeaderFull a where
  blockHeaderFull :: Lens' a BlockHeaderFull
  
instance BlockLike Block where
  hash = block_hash
  predecessor = block_header . blockHeaderFull_predecessor
  level = block_header . blockHeaderFull_level
  fitness = block_header . blockHeaderFull_fitness
  timestamp = block_header . blockHeaderFull_timestamp

instance HasProtocolHash Block where
  protocolHash = block_protocol

instance HasProtocolHash BlockHeader where
  protocolHash = blockHeader_protocol

instance HasBlockHeaderFull Block where
  blockHeaderFull = block_header

instance HasBlockMetadata Block where
  blockMetadata = block_metadata

instance HasChainId Block where
  chainIdL = block_chainId

instance BlockLike BlockHeader where
  hash = blockHeader_hash
  predecessor = blockHeader_predecessor
  level = blockHeader_level
  fitness = blockHeader_fitness
  timestamp = blockHeader_timestamp

instance BlockLike MonitorBlock where
  hash = monitorBlock_hash
  predecessor = monitorBlock_predecessor
  level = monitorBlock_level
  fitness = monitorBlock_fitness
  timestamp = monitorBlock_timestamp

instance BlockLike VeryBlockLike where
  hash = veryBlockLike_hash
  predecessor = veryBlockLike_predecessor
  fitness = veryBlockLike_fitness
  level = veryBlockLike_level
  timestamp = veryBlockLike_timestamp

instance HasProtocolHash BlockMetadata where
  protocolHash = blockMetadata_protocol

instance BlockLike a => BlockLike (WithProtocolHash a) where
  hash = withProtocolHash_value . hash
  predecessor = withProtocolHash_value . predecessor
  fitness = withProtocolHash_value . fitness
  level = withProtocolHash_value . level
  timestamp = withProtocolHash_value . timestamp
instance HasProtocolHash (WithProtocolHash a) where
  protocolHash = withProtocolHash_protocolHash

instance HasBalanceUpdates Block where
  balanceUpdates f blk = blk' <$> md' <*> ops'
    where
      blk' x y = blk {_block_metadata = x, _block_operations = y}
      md' = (blockMetadata_balanceUpdates . traverse) f $ _block_metadata blk
      ops' = (traverse . traverse . balanceUpdates) f $ _block_operations blk

mkVeryBlockLike :: BlockLike b => b -> VeryBlockLike
mkVeryBlockLike blk = VeryBlockLike
  { _veryBlockLike_hash = blk ^. hash
  , _veryBlockLike_predecessor = blk ^. predecessor
  , _veryBlockLike_fitness = blk ^. fitness
  , _veryBlockLike_level = blk ^. level
  , _veryBlockLike_timestamp = blk ^. timestamp
  }

