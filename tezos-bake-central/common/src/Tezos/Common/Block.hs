{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Tezos.Common.Block where

import Control.DeepSeq (NFData)
import Control.Lens (Lens', (^.), makeLenses)
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Time
import Data.Typeable
import Data.Word (Word8)
import GHC.Generics

import Tezos.Common.Base58Check
import Tezos.Common.BlockHeader
import Tezos.Common.Fitness
import Tezos.Common.Json
import Tezos.Common.Level

class BlockLike b where
  hash :: Lens' b BlockHash
  predecessor :: Lens' b BlockHash
  level :: Lens' b RawLevel
  fitness :: Lens' b Fitness
  timestamp :: Lens' b UTCTime

class HasProtocolHash a where
  protocolHash :: Lens' a ProtocolHash

class HasChainId a where
  chainIdL :: Lens' a ChainId

class HasBlockHeaderFull a where
  blockHeaderFull :: Lens' a BlockHeaderFull

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
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance NFData MonitorBlock

data VeryBlockLike = VeryBlockLike
  { _veryBlockLike_hash :: BlockHash
  , _veryBlockLike_predecessor :: BlockHash
  , _veryBlockLike_fitness :: Fitness
  , _veryBlockLike_level :: RawLevel
  , _veryBlockLike_timestamp :: UTCTime
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
  { _withProtocolHash_value :: a
  , _withProtocolHash_protocolHash :: ProtocolHash
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

deriveTezosFromJson ''MonitorBlock
deriveTezosJson ''VeryBlockLike
concat <$> traverse makeLenses
  [ 'MonitorBlock
  , 'VeryBlockLike
  , 'WithProtocolHash
  ]

instance BlockLike a => BlockLike (WithProtocolHash a) where
  hash = withProtocolHash_value . hash
  predecessor = withProtocolHash_value . predecessor
  fitness = withProtocolHash_value . fitness
  level = withProtocolHash_value . level
  timestamp = withProtocolHash_value . timestamp

instance HasProtocolHash (WithProtocolHash a) where
  protocolHash = withProtocolHash_protocolHash

instance BlockLike VeryBlockLike where
  hash = veryBlockLike_hash
  predecessor = veryBlockLike_predecessor
  fitness = veryBlockLike_fitness
  level = veryBlockLike_level
  timestamp = veryBlockLike_timestamp

instance BlockLike MonitorBlock where
  hash = monitorBlock_hash
  predecessor = monitorBlock_predecessor
  level = monitorBlock_level
  fitness = monitorBlock_fitness
  timestamp = monitorBlock_timestamp

mkVeryBlockLike :: BlockLike b => b -> VeryBlockLike
mkVeryBlockLike blk = VeryBlockLike
  { _veryBlockLike_hash = blk ^. hash
  , _veryBlockLike_predecessor = blk ^. predecessor
  , _veryBlockLike_fitness = blk ^. fitness
  , _veryBlockLike_level = blk ^. level
  , _veryBlockLike_timestamp = blk ^. timestamp
  }

instance HasProtocolHash BlockHeader where
  protocolHash = blockHeader_protocol

instance BlockLike BlockHeader where
  hash = blockHeader_hash
  predecessor = blockHeader_predecessor
  level = blockHeader_level
  fitness = blockHeader_fitness
  timestamp = blockHeader_timestamp
