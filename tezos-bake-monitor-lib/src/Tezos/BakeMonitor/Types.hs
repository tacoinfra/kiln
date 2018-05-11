{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tezos.BakeMonitor.Types where

import Control.Lens.TH
import Data.Aeson (ToJSON(..), FromJSON(..), fieldLabelModifier)
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LBS
import Data.Fixed
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time.Clock
import Data.Typeable
import Data.Word
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types.Status(Status(..))

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
  , _report_tezzies :: Maybe Micro
  }
  deriving (Eq, Show, Generic, Typeable)

instance FromJSON Report
instance ToJSON Report

data ProtoInfo = ProtoInfo
  { _protoInfo_endorsementSecurityDeposit :: Micro
  , _protoInfo_blockSecurityDeposit :: Micro
  , _protoInfo_blockReward :: Micro
  , _protoInfo_endorsementReward :: Micro
  , _protoInfo_preservedCycles :: Word64
  , _protoInfo_blocksPerCycle :: Word64
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance FromJSON ProtoInfo
instance ToJSON ProtoInfo

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
makeLenses 'BlockInfo

makeLenses 'Report
makeLenses 'Count
makeLenses 'Baked
makeLenses 'Error
