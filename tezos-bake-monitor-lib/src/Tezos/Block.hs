{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Tezos.Block where

import Control.Applicative ((<|>))
import Control.Lens (Lens', coerced, (^.), _1, _2)
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (parseJSON), ToJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as BS16
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HashMap
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Typeable (Typeable)
import Data.Word
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Sequence as Seq

import Tezos.BalanceUpdate
import Tezos.Base16ByteString (Base16ByteString (..))
import Tezos.Base58Check
import Tezos.BlockHeader
import Tezos.Fitness
import Tezos.Json
import Tezos.Level
import Tezos.Operation
import Tezos.PublicKeyHash
import Tezos.ShortByteString (toShort, fromShort)
import Tezos.Signature (Signature)
import Tezos.TestChainStatus
import Tezos.Tez (Tez)

-- | "description": "All the information about a block.",
data Block = Block
  { _block_protocol :: !ProtocolHash --  "protocol": { "type": "string", "enum": [ "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" ] },
  , _block_chainId :: !ChainId --  "chain_id": { "$ref": "#/definitions/Chain_id" },
  , _block_hash :: !BlockHash -- "hash": { "$ref": "#/definitions/block_hash" },
  , _block_header :: !BlockHeader --  "header": { "$ref": "#/definitions/raw_block_header" },
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

data TzScanBlock = TzScanBlock
  { _tzScanBlock_hash :: !BlockHash
  , _tzScanBlock_predecessorHash :: !BlockHash
  , _tzScanBlock_fitness :: !TzScanFitness
  , _tzScanBlock_timestamp :: !UTCTime
  , _tzScanBlock_validationPass :: !Word8
  -- , _tzScanBlock_operations :: Seq (Seq Operation)
  , _tzScanBlock_protocol :: !TzScanProtocol
  , _tzScanBlock_testProtocol :: !TzScanProtocol
  , _tzScanBlock_network :: !ChainId
  -- , _tzScanBlock_testNetwork_ :: !Text
  -- , _tzScanBlock_testNetworkExpiration" :: !Text
  , _tzScanBlock_baker :: !TzScanBaker
  , _tzScanBlock_nbOperations :: !(Maybe Word64)
  , _tzScanBlock_priority :: !Int
  , _tzScanBlock_level :: !RawLevel
  , _tzScanBlock_commitedNonceHash :: !TzScanNonceHash
  , _tzScanBlock_pow_nonce :: !(Base16ByteString ByteString)
  , _tzScanBlock_proto :: !Word8
  --, _tzScanBlock_data :: !Operation -- TODO: Not sure how to parse this
  , _tzScanBlock_signature :: !(Maybe Signature)
  -- , _tzScanBlock_volume :: !Integer -- TODO: unkown type
  , _tzScanBlock_fees :: !Tez
  -- , _tzScanBlock_distanceLevel :: !Integer -- TODO: unknown type
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance NFData TzScanBlock

newtype TzScanFitness = TzScanFitness Fitness
  deriving (Eq, Ord, Show, Generic, Typeable)
instance Hashable TzScanFitness
instance NFData TzScanFitness
instance FromJSON TzScanFitness where
  parseJSON = Aeson.withText "block fitness string" $ \txt -> pure $ TzScanFitness $ FitnessF $ Seq.fromList
    (Base16ByteString . toShort . fst . BS16.decode . T.encodeUtf8 <$> T.splitOn " " txt)
instance ToJSON TzScanFitness where
  toJSON (TzScanFitness (FitnessF xs)) = Aeson.toJSON $ T.intercalate " " $ toList $ T.decodeUtf8 . BS16.encode . fromShort . unbase16ByteString <$> xs
  toEncoding (TzScanFitness (FitnessF xs)) = Aeson.toEncoding $ T.intercalate " " $ toList $ T.decodeUtf8 . BS16.encode . fromShort . unbase16ByteString <$> xs

newtype TzScanProtocol = TzScanProtocol
  { -- _tzScanProtocol_name :: !Text -- TODO: What even is this?
  _tzScanProtocol_hash :: ProtocolHash
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance Hashable TzScanProtocol
instance NFData TzScanProtocol

newtype TzScanBaker = TzScanBaker
  { _tzScanBaker_tz :: PublicKeyHash
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance Hashable TzScanBaker
instance NFData TzScanBaker

newtype TzScanNonceHash = TzScanNonceHash (Maybe NonceHash)
  deriving (Eq, Ord, Show, Generic, Typeable, ToJSON)
instance Hashable TzScanNonceHash
instance NFData TzScanNonceHash
instance FromJSON TzScanNonceHash where
  parseJSON v = TzScanNonceHash <$> (parseJSON v <|> pure Nothing)

data VeryBlockLike = VeryBlockLike
  { _veryBlockLike_hash :: !BlockHash
  , _veryBlockLike_predecessor :: !BlockHash
  , _veryBlockLike_fitness :: !Fitness
  , _veryBlockLike_level :: !RawLevel
  , _veryBlockLike_timestamp :: !UTCTime
  } deriving (Eq, Ord, Show, Typeable, Generic)
instance NFData VeryBlockLike


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
  , ''TzScanBaker
  , ''TzScanBlock
  , ''TzScanProtocol
  , ''VotingPeriodKind
  , ''VeryBlockLike
  ]

concat <$> traverse makeLenses
  [ 'Block
  , 'BlockMetadata
  , 'MaxOperationListLength --  "max_operation_list_length": {
  , 'MonitorBlock
  , 'TzScanBaker
  , 'TzScanBlock
  , 'TzScanProtocol
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

instance BlockLike Block where
  hash = block_hash
  predecessor = block_header . blockHeader_predecessor
  level = block_header . blockHeader_level
  fitness = block_header . blockHeader_fitness
  timestamp = block_header . blockHeader_timestamp
instance HasProtocolHash Block where
  protocolHash = block_protocol

instance BlockLike (BlockHash, BlockHeader) where
  hash = _1
  predecessor = _2 . blockHeader_predecessor
  level = _2 . blockHeader_level
  fitness = _2 . blockHeader_fitness
  timestamp = _2 . blockHeader_timestamp

instance BlockLike MonitorBlock where
  hash = monitorBlock_hash
  predecessor = monitorBlock_predecessor
  level = monitorBlock_level
  fitness = monitorBlock_fitness
  timestamp = monitorBlock_timestamp

instance BlockLike TzScanBlock where
  hash = tzScanBlock_hash
  predecessor = tzScanBlock_predecessorHash
  level = tzScanBlock_level
  fitness = tzScanBlock_fitness . coerced
  timestamp = tzScanBlock_timestamp
instance HasProtocolHash TzScanBlock where
  protocolHash = tzScanBlock_protocol . coerced

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

