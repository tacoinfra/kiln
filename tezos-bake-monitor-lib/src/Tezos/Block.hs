{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.Block where

import Control.Lens (Lens')
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (parseJSON), ToJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as BS16
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Typeable (Typeable)
import qualified Data.Vector as Vector
import Data.Word
import GHC.Generics (Generic)

import Tezos.BalanceUpdate
import Tezos.Base16ByteString (Base16ByteString (..))
import Tezos.Base58Check
import Tezos.BlockHeader
import Tezos.Fitness
import Tezos.Json
import Tezos.Level
import Tezos.Operation
import Tezos.PublicKeyHash
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



-- | "voting_period_kind": {
data VotingPeriodKind
  = VotingPeriodKind_Proposal --  { "type": "string", "enum": [ "proposal" ] },
  | VotingPeriodKind_TestingVote --  { "type": "string", "enum": [ "testing_vote" ] },
  | VotingPeriodKind_Testing --  { "type": "string", "enum": [ "testing" ] },
  | VotingPeriodKind_PromotionVote --  { "type": "string", "enum": [ "promotion_vote" ] }
  deriving (Eq, Ord, Show, Generic, Typeable)

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
  } deriving (Show, Eq, Ord, Typeable)

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
  } deriving (Eq, Ord, Show, Typeable)


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
  , _tzScanBlock_nbOperations :: !Word64
  , _tzScanBlock_priority :: !Int
  , _tzScanBlock_level :: !RawLevel
  , _tzScanBlock_commitedNonceHash :: !(Maybe NonceHash)
  , _tzScanBlock_pow_nonce :: !(Base16ByteString ByteString)
  , _tzScanBlock_proto :: !Word8
  , _tzScanBlock_data :: !Operation
  , _tzScanBlock_signature :: !(Maybe Signature)
  -- , _tzScanBlock_volume :: !Integer -- TODO: unkown type
  , _tzScanBlock_fees :: !Tez
  -- , _tzScanBlock_distanceLevel :: !Integer -- TODO: unknown type
  }

newtype TzScanFitness = TzScanFitness Fitness
instance FromJSON TzScanFitness where
  parseJSON = Aeson.withText "block fitness string" $ \txt -> TzScanFitness <$>
    Aeson.parseJSON (Aeson.Array $ Vector.fromList $ Aeson.String <$> T.splitOn " " txt)
instance ToJSON TzScanFitness where
  toJSON (TzScanFitness (FitnessF xs)) = Aeson.toJSON $ T.intercalate " " $ toList $ T.decodeUtf8 . BS16.encode . unbase16ByteString <$> xs
  toEncoding (TzScanFitness (FitnessF xs)) = Aeson.toEncoding $ T.intercalate " " $ toList $ T.decodeUtf8 . BS16.encode . unbase16ByteString <$> xs


data TzScanProtocol = TzScanProtocol
  { _tzScanProtocol_name :: !Text
  , _tzScanProtocol_hash :: !ProtocolHash
  }

newtype TzScanBaker = TzScanBaker
  { _tzScanBaker_tz :: PublicKeyHash
  }

concat <$> traverse deriveTezosJson
  [ ''Block
  , ''BlockMetadata
  , ''MaxOperationListLength
  , ''MonitorBlock
  , ''TzScanBaker
  , ''TzScanBlock
  , ''TzScanProtocol
  , ''VotingPeriodKind
  ]

concat <$> traverse makeLenses
 [ 'Block
 , 'BlockMetadata
 , 'MaxOperationListLength --  "max_operation_list_length": {
 , 'MonitorBlock
 , 'TzScanBaker
 , 'TzScanBlock
 , 'TzScanProtocol
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

