{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Schema where



import Control.Applicative
import Control.Lens.TH
import Data.Aeson hiding (Error)
import Data.Aeson.TH
import Data.Fixed
import Data.Function
import Data.Int
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy
import Data.Scientific
import Data.Text (Text)
import Data.Time
import Data.Typeable
import Data.Word
import Focus.Schema
import GHC.Generics
import qualified Cases
import qualified Data.Text as T
import Data.Sequence(Seq())
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

import qualified Data.ByteString.Base16 as BS
import Data.Monoid


-- moved from tezos-bake-monitor-lig:Tezos.BakeMonitor.Types since we shouldn't need it anymore.
data Ident = Ident
  { _ident_hash :: Text
  , _ident_nickname :: Maybe Text
  }
  deriving (Eq, Show, Generic, Typeable)

instance FromJSON Ident
instance ToJSON Ident

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
-- integers.  Like the FromJSON instance below, it "may" be neccesary to encode
-- values larger than `2^31/resolution` as strings, but that's not handled
-- currently
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
mkCount _ = Count
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
  , _bakedEvent_signedHeader :: Base16ByteString BS.ByteString
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

type ChainId = Text
type Protocol = Text


data FitnessF a = Fitness { unFitness :: Seq a }
  deriving (Eq, Show, Generic, Typeable, Functor, Foldable, Traversable)

-- | Not sure why GND doesn't work for this...
instance ToJSON a => ToJSON (FitnessF a) where
  toJSON = toJSON . unFitness
  toEncoding = toEncoding . unFitness

instance FromJSON a => FromJSON (FitnessF a) where
  parseJSON = fmap Fitness . parseJSON

type Fitness = FitnessF (Base16ByteString BS.ByteString)

instance Ord a => Ord (FitnessF a) where
  compare = (compare `on` length) <> (compare `on` unFitness)

newtype Base16ByteString a = Base16ByteString { unbase16ByteString :: a }
  deriving (Eq, Ord, Show, Generic, Typeable, Functor, Foldable, Traversable)

instance ToJSON (Base16ByteString BS.ByteString) where
  toJSON (Base16ByteString x) = toJSON $ T.decodeUtf8 $ BS.encode x
  toEncoding (Base16ByteString x) = toEncoding $ T.decodeUtf8 $ BS.encode x

instance FromJSON (Base16ByteString BS.ByteString) where
  parseJSON x = do
    hexesText <- parseJSON x
    let (bytes, rest) = BS.decode $ T.encodeUtf8 hexesText
    if (BS.length rest > 0)
    then fail $ "unmatched characters" <> show rest
    else return $ Base16ByteString bytes



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
  , _errorEvent_trace :: Json [Value]
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

data BlockHeader = BlockHeader
  { _blockHeader_level :: Int32
  , _blockHeader_proto :: Word8
  , _blockHeader_predecessor :: BS.ByteString
  , _blockHeader_timestamp :: UTCTime
  , _blockHeader_validationPass :: Word8
  , _blockHeader_operationsHash :: BS.ByteString
  , _blockHeader_fitness :: Fitness
  , _blockHeader_context :: BS.ByteString
  , _blockHeader_priority :: Word16
  , _blockHeader_proofOfWorkNonce :: Word64
  , _blockHeader_seedNonceHash :: Maybe BS.ByteString
  }


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



