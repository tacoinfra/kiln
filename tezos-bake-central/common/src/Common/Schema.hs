{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Schema where



import Control.Applicative
import Control.Lens.TH
import Data.Aeson hiding (Error)
-- import Data.Aeson (ToJSON(..), FromJSON(..), fieldLabelModifier)
import Data.Aeson.TH
import Data.Fixed
import Data.Function
import Data.Int(Int64)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy
import Data.Scientific
import Data.Text (Text)
import Data.Time
-- import Data.Time.Clock
import Data.Typeable
import Data.Word
import Focus.Schema
import GHC.Generics
-- import Network.HTTP.Client hiding (Proxy)
-- import Network.HTTP.Types.Status(Status(..))
import qualified Cases
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
-- import qualified Data.Text.Lazy as LT
-- import Tezos.BakeMonitor.Types


-- moved from tezos-bake-monitor-lig:Tezos.BakeMonitor.Types since we shouldn't need it anymore.
data Ident = Ident
  { _ident_hash :: Text
  , _ident_nickname :: Maybe Text
  }
  deriving (Eq, Show, Generic, Typeable)

instance FromJSON Ident
instance ToJSON Ident

-- data Report = Report
--   { _report_counts :: Count
--   , _report_lastBaked :: [Baked]
--   , _report_errors :: [Error]
--   , _report_failedBaker :: [LT.Text]
--   , _report_lastSeen :: Maybe UTCTime
--   , _report_tezzies :: Maybe Tezzies
--   }
--   deriving (Eq, Show, Generic, Typeable)
-- 
-- instance FromJSON Report
-- instance ToJSON Report

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

-- I dont really think this is correct
mkCount :: Report -> Count
mkCount rpt = Count
  { _count_selected = 0
  , _count_injected = 0
  , _count_errors = 0
  }

instance Monoid Count where
  mempty = Count 0 0 0
  Count s i e `mappend` Count s' i' e' = Count (s + s') (i + i') (e + e')

instance FromJSON Count
instance ToJSON Count

newtype BlockHash = BlockHash {unBlockHash :: Text}
  deriving (Eq, Ord, Show, ToJSON, FromJSON, Generic, Typeable)


type Baked = Event BakedEvent
-- data Baked = Baked
--   { _baked_seq :: !Integer
--   , _baked_hash :: BlockHash
--   , _baked_time :: UTCTime
--   , _baked_level :: Word64
--   }
--   deriving (Eq, Show, Generic, Typeable)

-- instance FromJSON Baked
-- instance ToJSON Baked

data Error = Error
  { _error_time :: UTCTime
  , _error_text :: Text
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance FromJSON Error
instance ToJSON Error

-- there are tons of fields i am not trying to parse here
data BlockInfo = BlockInfo
  { _blockInfo_hash :: BlockHash
  , _blockInfo_level :: Word64
  , _blockInfo_proto :: Word64
  , _blockInfo_predecessor :: BlockHash
  }
  deriving (Eq, Show, Generic, Typeable)



type ClientAddress = Text

data Client = Client
  { _client_address :: ClientAddress
  , _client_updated :: Maybe UTCTime
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId Client
instance FromJSON Client
instance ToJSON Client

data PendingReward = PendingReward
  { _pendingReward_client :: Id Client
  , _pendingReward_hash :: Text -- needed because we need to be able to tell that we're not adding the same reward twice
  , _pendingReward_level :: Word64
  , _pendingReward_amount :: Micro
  }
  deriving (Eq, Show, Generic, Typeable)

instance HasId PendingReward
instance FromJSON PendingReward
instance ToJSON PendingReward

data ClientInfo = ClientInfo
  { _clientInfo_client :: Id Client
  , _clientInfo_report :: Json Report
  }
  deriving (Eq, Show, Generic, Typeable)

instance HasId ClientInfo
instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Node = Node
  { _node_address :: ClientAddress
  , _node_headLevel :: Maybe Word64
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId Node
instance FromJSON Node
instance ToJSON Node

data Parameters = Parameters
  { _parameters_node :: Id Node
  , _parameters_protoInfo :: ProtoInfo
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId Parameters
instance FromJSON Parameters
instance ToJSON Parameters

data Level = Level
  { _level_cycle :: Int
  , _level_cyclePosition :: Int
  , _level_expectedCommitment :: Bool
  , _level_level :: Int
  , _level_levelPosition :: Int
  , _level_votingPeriod :: Int
  , _level_votingPeriodPosition :: Int
  }
  deriving (Show, Eq, Ord, Typeable, Generic)


data BakedEvent = BakedEvent
  { _bakedEvent_hash :: BlockHash
  -- , operations :: ...
  -- , signedHeader :: ...
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

type ChainId = Text
type Protocol = Text
type Fitness = [Text]

data SeenEvent = SeenEvent
  { _seenEvent_chainId :: ChainId
  , _seenEvent_fitness :: Fitness
  , _seenEvent_hash :: BlockHash
  , _seenEvent_level :: Level
  , _seenEvent_predecessor :: BlockHash
  , _seenEvent_protocol :: Protocol
  , _seenEvent_timestamp :: UTCTime
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

data Event e = Event
  { _event_detail :: e
  , _event_seq :: Int
  , _event_time :: UTCTime
  , _event_worker :: Text
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

data ErrorEvent = ErrorEvent
  { _errorEvent_message :: Text
  , _errorEvent_trace :: [Value]
  }
  deriving (Show, Eq, Typeable, Generic)

mkErr :: Event ErrorEvent -> Error
mkErr err = Error
  { _error_time = _event_time err
  , _error_text = T.pack $ show $ _event_detail err
  }


data Report = Report
  { _report_baked :: [Event BakedEvent]
  -- , _report_endorsed :: []
  , _report_errors :: [Event ErrorEvent]
  , _report_seen :: [Event SeenEvent]
  , _report_startTime :: UTCTime
  }
  deriving (Show, Eq, Typeable, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (length "_blockInfo_")} ''BlockInfo)

$(deriveJSON defaultOptions{fieldLabelModifier = T.unpack . Cases.snakify . T.pack . drop (length "_protoInfo_")} ''ProtoInfo)
$(deriveJSON defaultOptions{fieldLabelModifier = T.unpack . Cases.snakify . T.pack . drop (length "_level_")} ''Level)
$(deriveJSON defaultOptions{fieldLabelModifier = T.unpack . Cases.snakify . T.pack . drop (length "_report_")} ''Report)
$(deriveJSON defaultOptions{fieldLabelModifier = T.unpack . Cases.snakify . T.pack . drop (length "_event_")} ''Event)
$(deriveJSON defaultOptions{fieldLabelModifier = T.unpack . Cases.snakify . T.pack . drop (length "_bakedEvent_")} ''BakedEvent)
$(deriveJSON defaultOptions{fieldLabelModifier = T.unpack . Cases.snakify . T.pack . drop (length "_seenEvent_")} ''SeenEvent)
$(deriveJSON defaultOptions{fieldLabelModifier = T.unpack . Cases.snakify . T.pack . drop (length "_errorEvent_")} ''ErrorEvent)

makeLenses 'BlockInfo

makeLenses 'Report
makeLenses 'Count
makeLenses 'Event
makeLenses 'BakedEvent
makeLenses 'SeenEvent
makeLenses 'ErrorEvent
makeLenses 'Error



