{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tezos.BakeMonitor.Types where

import Data.Proxy

import Control.Applicative
import Control.Lens.TH
import Data.Aeson (ToJSON(..), FromJSON(..), fieldLabelModifier)
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LBS
import Data.Scientific
import Data.Fixed
import Data.Function
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock
import Data.Typeable
import Data.Word
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types.Status(Status(..))
import qualified Cases
import Data.Int(Int64)

data Ident = Ident
  { _ident_hash :: Text
  , _ident_nickname :: Maybe Text
  }
  deriving (Eq, Show, Generic, Typeable)

instance FromJSON Ident
instance ToJSON Ident

data Report = Report
  { _report_counts :: Count
  , _report_lastBaked :: [Baked]
  , _report_errors :: [Error]
  , _report_failedBaker :: [LT.Text]
  , _report_lastSeen :: Maybe UTCTime
  , _report_tezzies :: Maybe Tezzies
  }
  deriving (Eq, Show, Generic, Typeable)

instance FromJSON Report
instance ToJSON Report

newtype PublicKeyHash = PublicKeyHash Text
  deriving (Eq, Ord, Show, Generic, Typeable, ToJSON, FromJSON)

newtype Tezzies = Tezzies { getTezzies :: Micro }
  deriving (Eq, Ord, Show, Generic, Typeable, Enum, Fractional, Num, Real, RealFrac)

getMicroTezzies :: Tezzies -> Int64
getMicroTezzies
  = (floor :: Fixed E6 -> Int64)
  . ((fromInteger $ resolution (Proxy :: Proxy E6)) * )
  . getTezzies

microTezzies :: forall a. Integral a => a -> Tezzies
microTezzies
  = Tezzies
  . (/ (fromInteger $ resolution (Proxy :: Proxy E6)))
  . (fromIntegral :: a -> Fixed E6)

-- | the instance for Data.Fixed.Micro defined in Data.Aeson is perfectly
-- cromulent, its just not what we need.  tezos encodes these values as
-- integers.
instance ToJSON Tezzies where
  toJSON = toJSON . getMicroTezzies
  toEncoding = toEncoding . getMicroTezzies

instance FromJSON Tezzies where
  parseJSON x = (microTezzies . (floor :: Scientific -> Int64)) <$> parseJSON x
            <|> (microTezzies . (read :: String -> Int64)) <$> parseJSON x

newtype PeriodSequenceF a = PeriodSequence (NonEmpty a)
  deriving (Eq, Ord, Show, Generic, Typeable, ToJSON, FromJSON, Functor)

instance Foldable PeriodSequenceF where
  foldMap f (PeriodSequence xs) = go xs where
    go (x :| []) = fix (f x `mappend`)
    go (x :| (y:ys)) = f x `mappend` go (y :| ys)

type PeriodSequence = PeriodSequenceF Int

data ProtoInfo = ProtoInfo
  { _protoInfo_blockReward :: Tezzies
  , _protoInfo_blockSecurityDeposit :: Tezzies
  , _protoInfo_blocksPerCommitment :: Int
  , _protoInfo_blocksPerCycle :: Int
  , _protoInfo_blocksPerRollSnapshot :: Int
  , _protoInfo_blocksPerVotingPeriod :: Int
  , _protoInfo_dictatorPubkey :: Text -- PublicKeyHash
  , _protoInfo_endorsementReward :: Tezzies
  , _protoInfo_endorsementSecurityDeposit :: Tezzies
  , _protoInfo_endorsersPerBlock :: Int
  , _protoInfo_firstFreeBakingSlot :: Int
  , _protoInfo_maxOperationDataLength :: Int
  , _protoInfo_michelsonMaximumTypeSize :: Int
  , _protoInfo_originationBurn :: Tezzies
  , _protoInfo_preservedCycles :: Int
  , _protoInfo_proofOfWorkThreshold :: Int
  , _protoInfo_seedNonceRevelationTip :: Tezzies
  , _protoInfo_timeBetweenBlocks :: PeriodSequence -- repeating sequence of seconds
  , _protoInfo_tokensPerRoll :: Tezzies
  -- TODO: these didn't show up in my quick greppings, so I don't know the types too exactly.
  -- , _protoInfo_instructionsPerTransaction :: 40000
  -- , _protoInfo_maxRevelationsPerBlock :: 32,
  -- , _protoInfo_nonceLength :: 32,
  -- , _protoInfo_proofOfWorkNonceSize :: 8
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

data Count = Count
  { _count_selected :: !Integer
  , _count_injected :: !Integer
  , _count_errors :: !Integer
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance Monoid Count where
  mempty = Count 0 0 0
  Count s i e `mappend` Count s' i' e' = Count (s + s') (i + i') (e + e')

instance FromJSON Count
instance ToJSON Count

newtype BlockHash = BlockHash {unBlockHash :: Text}
  deriving (Eq, Ord, Show, ToJSON, FromJSON, Generic, Typeable)

data Baked = Baked
  { _baked_seq :: !Integer
  , _baked_hash :: BlockHash
  , _baked_time :: UTCTime
  , _baked_level :: Word64
  }
  deriving (Eq, Show, Generic, Typeable)

instance FromJSON Baked
instance ToJSON Baked

data Error = Error
  { _error_time :: UTCTime
  , _error_text :: Text
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance FromJSON Error
instance ToJSON Error

data RpcResponse a =
    RpcResponse_HttpException HttpException
  | RpcResponse_UnexpectedStatus Status
  | RpcResponse_NonJSON String LBS.ByteString
  | RpcResponse_Success a
  deriving (Functor, Foldable, Traversable)

-- there are tons of fields i am not trying to parse here
data BlockInfo = BlockInfo
  { _blockInfo_hash :: BlockHash
  , _blockInfo_level :: Word64
  , _blockInfo_proto :: Word64
  , _blockInfo_predecessor :: BlockHash
  }
  deriving (Eq, Show, Generic, Typeable)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (length "_blockInfo_")} ''BlockInfo)

$(deriveJSON defaultOptions{fieldLabelModifier = T.unpack . Cases.snakify . T.pack . drop (length "_protoInfo_")} ''ProtoInfo)

makeLenses 'BlockInfo

makeLenses 'Report
makeLenses 'Count
makeLenses 'Baked
makeLenses 'Error
