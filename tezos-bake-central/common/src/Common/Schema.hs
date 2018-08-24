{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Common.Schema where

import qualified Cases
import Control.Lens (views, (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (runExcept)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Semigroup (Semigroup, Sum (..), getSum, (<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Version (Version)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Rhyolite.Schema (Email, HasId, Id, Json)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.Json
import Tezos.NodeRPC
import Tezos.NodeRPC.Sources (DataSource)
import Tezos.Types

instance Aeson.ToJSON Uri.URI where
  toJSON = Aeson.toJSON . Uri.render
  toEncoding = Aeson.toEncoding . Uri.render
instance Aeson.FromJSON Uri.URI where
  parseJSON x = maybe (fail "Invalid URI") pure . Uri.mkURI =<< Aeson.parseJSON x

sumFees :: PublicKeyHash -> Operation -> Tez
sumFees delegate = getSum . views balanceUpdates getFee
  where
    getFee :: BalanceUpdate -> Sum Tez
    getFee (BalanceUpdate_Freezer x) | _freezerUpdate_delegate x == delegate = Sum (_freezerUpdate_change x)
    getFee _ = Sum 0

type Baked = Event BakedEvent

data Error = Error
  { _error_time :: UTCTime
  , _error_text :: Text
  } deriving (Eq, Ord, Show, Generic, Typeable)

type ClientAddress = Text


-- TODO: move to ~-lib
knownProtocols :: [ProtocolHash]
knownProtocols =
  [ "PrihK96nBAFSxVL1GLJTVhu9YnzkMFiBeuJRPA8NwuZVZCE1L6i" -- GENESIS
  , "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" -- BETANET
  ]

data Client = Client
  { _client_address :: !URI
  , _client_updated :: !(Maybe UTCTime)
  , _client_deleted :: !Bool
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId Client

data PendingReward = PendingReward
  { _pendingReward_delegate :: !(Id Delegate)
  , _pendingReward_hash :: !Text -- needed because we need to be able to tell that we're not adding the same reward twice
  , _pendingReward_level :: !TezosWord64
  , _pendingReward_amount :: !Tez
  } deriving (Eq, Show, Generic, Typeable)
instance HasId PendingReward

data ClientInfo = ClientInfo
  { _clientInfo_client :: !(Id Client)
  , _clientInfo_report :: !(Json Report)
  , _clientInfo_config :: !(Json ClientConfig)
  -- , _clientInfo_node :: Id Node
  } deriving (Eq, Show, Generic, Typeable)
instance HasId ClientInfo

data Node = Node
  { _node_address :: !URI
  , _node_identity :: !(Maybe CryptoboxPublicKeyHash)
  , _node_headLevel :: !(Maybe RawLevel)
  , _node_headBlockHash :: !(Maybe BlockHash)
  , _node_headBlockBakedAt :: !(Maybe UTCTime)
  , _node_peerCount :: !(Maybe Word64)
  , _node_networkStat :: !NetworkStat
  , _node_fitness :: !(Maybe Fitness)
  , _node_deleted :: !Bool
  , _node_lastHeartbeat :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId Node


parseChainOrError :: Text -> Either NamedChain ChainId
parseChainOrError x = case runExcept (parseChain x) :: Either Text (Either NamedChain ChainId) of
  Left (e :: Text) -> error $ T.unpack $ "Invalid chain '" <> x <> "': " <> e
  Right v -> v


data PublicNodeHead = PublicNodeHead
  { _publicNodeHead_source :: !(Json DataSource)
  , _publicNodeHead_headLevel :: !RawLevel
  , _publicNodeHead_headBlockHash :: !BlockHash
  , _publicNodeHead_headBlockFitness :: !Fitness
  , _publicNodeHead_headBlockBakedAt :: !UTCTime
  , _publicNodeHead_updated :: !UTCTime
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId PublicNodeHead

mkNode :: URI -> Node
mkNode addr = Node
  { _node_address = addr
  , _node_identity = Nothing -- TODO
  , _node_headLevel = Nothing
  , _node_headBlockHash = Nothing
  , _node_headBlockBakedAt = Nothing
  , _node_peerCount = Nothing
  , _node_networkStat = NetworkStat 0 0 0 0
  , _node_fitness = Nothing
  , _node_deleted = False
  , _node_lastHeartbeat = Nothing
  }

data Parameters = Parameters
  { _parameters_protoInfo :: !ProtoInfo
  , _parameters_chain :: !ChainId
  , _parameters_headTimestamp :: !UTCTime
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId Parameters

data BakedEventOperation = BakedEventOperation
  { _bakedEventOperation_branch :: BlockHash
  , _bakedEventOperation_data :: Operation
  } deriving (Show, Eq, Ord, Typeable, Generic)

data BakedEvent = BakedEvent
  { _bakedEvent_hash :: BlockHash
  , _bakedEvent_operations :: [[BakedEventOperation]]
  , _bakedEvent_signedHeader :: BlockHeader
  , _bakedEvent_delegate :: !PublicKeyHash
  } deriving (Show, Eq, Ord, Typeable, Generic)

data SeenEvent = SeenEvent
  { _seenEvent_hash :: BlockHash
  -- , _seenEvent_chainId :: ChainId
  , _seenEvent_fitness :: Fitness
  , _seenEvent_level :: !RawLevel
  , _seenEvent_predecessor :: BlockHash
  -- , _seenEvent_protocol :: Protocol
  , _seenEvent_timestamp :: UTCTime
  } deriving (Show, Eq, Ord, Typeable, Generic)

data Event e = Event
  { _event_detail :: e
  , _event_seq :: Int
  , _event_time :: UTCTime
  , _event_worker :: Text
  } deriving (Show, Eq, Ord, Typeable, Generic)

data ErrorEvent = ErrorEvent
  { _errorEvent_message :: Text
  , _errorEvent_trace :: Json [Aeson.Value]
  } deriving (Show, Eq, Typeable, Generic)

data EndorseEvent = EndorseEvent
  { _endorseEvent_hash :: BlockHash
  , _endorseEvent_level :: Int
  , _endorseEvent_slot :: Int -- todo, pluralize
  , _endorseEvent_delegate :: PublicKeyHash
  , _endorseEvent_name :: String
  , _endorseEvent_oph :: OperationHash
  } deriving (Show, Eq, Typeable, Generic)


mkErr :: Event ErrorEvent -> Error
mkErr err = Error
  { _error_time = _event_time err
  , _error_text = _errorEvent_message $ _event_detail err
  }

data Report = Report
  { _report_baked :: [Event BakedEvent]
  -- , _report_endorsed :: [Event EndorseEvent]
  , _report_errors :: [Event ErrorEvent]
  , _report_seen :: [Event SeenEvent]
  , _report_startTime :: UTCTime
  } deriving (Show, Eq, Typeable, Generic)

blockLevel :: Event BakedEvent -> Int
blockLevel = fromIntegral . _blockHeader_level . _bakedEvent_signedHeader . _event_detail

blockRewards :: Event BakedEvent -> ProtoInfo -> Tez
blockRewards b p = _protoInfo_blockReward p + fees + nonceTip
  where
    blockHeader = _bakedEvent_signedHeader $ _event_detail b
    nonceTip = maybe 0 (const $ _protoInfo_seedNonceRevelationTip p) (_blockHeader_seedNonceHash blockHeader)
    delegate = _bakedEvent_delegate $ _event_detail b
    fees = getSum $ (foldMap.foldMap) (Sum . sumFees delegate . _bakedEventOperation_data) $ _bakedEvent_operations $ _event_detail b

endorsementReward :: Event EndorseEvent -> ProtoInfo -> Tez
endorsementReward b p = Tez $ getTez (_protoInfo_endorsementReward p) / fromIntegral (1 + _endorseEvent_slot (_event_detail b))

-- Used to produce info on the summary tab
instance Semigroup Report where
  u <> v = Report
    { _report_baked = _report_baked u <> _report_baked v
    , _report_errors = _report_errors u <> _report_errors v
    , _report_seen = _report_seen u <> _report_seen v
    , _report_startTime = min (_report_startTime u) (_report_startTime v)
    }

data ClientDaemonWorker
  = ClientDaemonWorker_Baking
  | ClientDaemonWorker_Denunciation
  | ClientDaemonWorker_Endorsement
  deriving (Enum, Show, Eq, Typeable, Generic)

data ClientConfig = ClientConfig
  { _clientConfig_startTime :: UTCTime
  , _clientConfig_delegates :: [PublicKeyHash] -- Ident
  , _clientConfig_workers :: [ClientDaemonWorker]
  , _clientConfig_nodeUri :: ClientAddress
  } deriving (Show, Eq, Typeable, Generic)

data Delegate = Delegate
  { _delegate_publicKeyHash :: !PublicKeyHash
  , _delegate_deleted :: !Bool
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId Delegate

data BakeEfficiency = BakeEfficiency
  { _bakeEfficiency_bakedBlocks :: !Word64
  , _bakeEfficiency_bakingRights :: !Word64
  -- TODO:
  -- , _bakeEfficiency_endorsedBlocks :: !Word64
  -- , _bakeEfficiency_endorsingRights :: !Word64
  -- , _bakeEfficiency_endorsedSlots :: !Word64
  -- , _bakeEfficiency_endorsingSlotRights :: !Word64
  -- , _bakeEfficiency_branch :: !BlockHash
  -- , _bakeEfficiency_range :: !RawLevel
  } deriving (Eq, Ord, Show, Generic, Typeable)

instance Semigroup BakeEfficiency where
  BakeEfficiency x1 x2 <> BakeEfficiency y1 y2 = BakeEfficiency (x1 + y1) (x2 + y2)

instance Monoid BakeEfficiency where
  mempty = BakeEfficiency 0 0
  mappend = (<>)

data VeryBlockLike = VeryBlockLike
  { _veryBlockLike_hash :: !BlockHash
  , _veryBlockLike_predecessor :: !BlockHash
  , _veryBlockLike_fitness :: !Fitness
  , _veryBlockLike_level :: !RawLevel
  , _veryBlockLike_timestamp :: !UTCTime
  } deriving (Eq, Ord, Show, Typeable)

mkVeryBlockLike :: BlockLike b => b -> VeryBlockLike
mkVeryBlockLike blk = VeryBlockLike
  { _veryBlockLike_hash = blk ^. hash
  , _veryBlockLike_predecessor = blk ^. predecessor
  , _veryBlockLike_fitness = blk ^. fitness
  , _veryBlockLike_level = blk ^. level
  , _veryBlockLike_timestamp = blk ^. timestamp
  }

data Notificatee = Notificatee
  { _notificatee_email :: Email
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId Notificatee

data SmtpProtocol
  = SmtpProtocol_Plain
  | SmtpProtocol_Ssl
  | SmtpProtocol_Starttls
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

data MailServerConfig = MailServerConfig
  { _mailServerConfig_hostName :: Text
  , _mailServerConfig_portNumber :: Word16
  , _mailServerConfig_smtpProtocol :: SmtpProtocol
  , _mailServerConfig_userName :: Text
  , _mailServerConfig_password :: Text
  , _mailServerConfig_madeDefaultAt :: UTCTime
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId MailServerConfig

data EndpointType = EndpointType_Node | EndpointType_Client
  deriving (Eq, Ord, Bounded, Enum, Generic, Typeable, Read, Show)

data ErrorLogInaccessibleEndpoint = ErrorLogInaccessibleEndpoint
  { _errorLogInaccessibleEndpoint_log :: !(Id ErrorLog)
  , _errorLogInaccessibleEndpoint_type :: !EndpointType
  , _errorLogInaccessibleEndpoint_address :: !URI
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogInaccessibleEndpoint

data ErrorLogBakerNoHeartbeat = ErrorLogBakerNoHeartbeat
  { _errorLogBakerNoHeartbeat_log :: !(Id ErrorLog)
  , _errorLogBakerNoHeartbeat_lastLevel :: !RawLevel
  , _errorLogBakerNoHeartbeat_lastBlockHash :: !BlockHash
  , _errorLogBakerNoHeartbeat_client :: !(Id Client)
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogBakerNoHeartbeat

data ClientWorker = ClientWorker_Baking | ClientWorker_Endorsing
  deriving (Eq, Ord, Bounded, Enum, Generic, Typeable, Read, Show)

data ErrorLogMultipleBakersForSameDelegate = ErrorLogMultipleBakersForSameDelegate
  { _errorLogMultipleBakersForSameDelegate_log :: !(Id ErrorLog)
  , _errorLogMultipleBakersForSameDelegate_publicKeyHash :: !PublicKeyHash
  , _errorLogMultipleBakersForSameDelegate_client :: !(Id Client)
  , _errorLogMultipleBakersForSameDelegate_worker :: !ClientWorker
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogMultipleBakersForSameDelegate

data ErrorLogNodeOnFork = ErrorLogNodeOnFork
  { _errorLogNodeOnFork_log :: !(Id ErrorLog)
  , _errorLogNodeOnFork_node :: !(Id Node)
  , _errorLogNodeOnFork_tooOld :: !Bool
  , _errorLogNodeOnFork_bakedBlock :: !BlockHash
  , _errorLogNodeOnFork_bakedBlockTime :: !UTCTime
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogNodeOnFork

data UpgradeCheckError
  = UpgradeCheckError_UpstreamUnreachable
  | UpgradeCheckError_UpstreamMissing
  | UpgradeCheckError_UpstreamUnparseable
  deriving (Eq, Ord, Generic, Typeable, Enum, Bounded, Read, Show)

data ErrorLogUpgradeNotice = ErrorLogUpgradeNotice
  { _errorLogUpgradeNotice_log :: !(Id ErrorLog)
  , _errorLogUpgradeNotice_error :: !(Maybe UpgradeCheckError)
  , _errorLogUpgradeNotice_newVersion :: !(Maybe Version)
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogUpgradeNotice

data ErrorLog = ErrorLog
  { _errorLog_started :: !UTCTime
  , _errorLog_stopped :: !(Maybe UTCTime)
  , _errorLog_lastSeen :: !UTCTime
  , _errorLog_noticeSentAt :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLog

data CachedProtocolConstants = CachedProtocolConstants
  { _cachedProtocolConstants_chainId :: !ChainId
  , _cachedProtocolConstants_protocol :: !ProtocolHash
  , _cachedProtocolConstants_blocksPerCycle :: !RawLevel
  , _cachedProtocolConstants_preservedCycles :: !Cycle
  } deriving (Eq, Generic, Ord, Show, Typeable)
instance HasId CachedProtocolConstants

data GenericCacheEntry = GenericCacheEntry
  { _genericCacheEntry_chainId :: !ChainId
  , _genericCacheEntry_key :: !(Json Aeson.Value)
  , _genericCacheEntry_value :: !(Json Aeson.Value)
  } deriving (Eq, Generic, Show, Typeable)
instance HasId GenericCacheEntry


-- We build instances carefully so that they agree exactly with the JSON produced by the tezos ocaml apps
concat <$> traverse (deriveJSON Aeson.defaultOptions
      { Aeson.fieldLabelModifier = T.unpack . Cases.snakify . T.pack . dropWhile ('_' /=) . tail
      , Aeson.constructorTagModifier = T.unpack . Cases.snakify . T.pack . dropWhile ('_' /=)
      })
  [ ''BakeEfficiency
  , ''BakedEvent
  , ''BakedEventOperation
  , ''ClientConfig
  , ''ClientDaemonWorker
  , ''ClientInfo
  , ''ClientWorker
  , ''Delegate
  , ''EndorseEvent
  , ''EndpointType
  , ''ErrorEvent
  , ''ErrorLog
  , ''ErrorLogBakerNoHeartbeat
  , ''ErrorLogInaccessibleEndpoint
  , ''ErrorLogMultipleBakersForSameDelegate
  , ''ErrorLogNodeOnFork
  , ''ErrorLogUpgradeNotice
  , ''Event
  , ''Node
  , ''PublicNodeHead
  , ''Report
  , ''SeenEvent
  , ''SmtpProtocol
  , ''UpgradeCheckError
  ]

concat <$> traverse makeLenses
  [ 'BakedEvent
  , 'BakedEventOperation
  , 'BakeEfficiency
  , 'CachedProtocolConstants
  , 'Delegate
  , 'EndorseEvent
  , 'Error
  , 'ErrorEvent
  , 'ErrorLog
  , 'ErrorLogBakerNoHeartbeat
  , 'ErrorLogInaccessibleEndpoint
  , 'ErrorLogMultipleBakersForSameDelegate
  , 'ErrorLogNodeOnFork
  , 'Event
  , 'MailServerConfig
  , 'PublicNodeHead
  , 'Report
  , 'SeenEvent
  , 'VeryBlockLike
  ]

instance BlockLike VeryBlockLike where
   hash = veryBlockLike_hash
   predecessor = veryBlockLike_predecessor
   fitness = veryBlockLike_fitness
   level = veryBlockLike_level
   timestamp = veryBlockLike_timestamp

instance BlockLike (Event BakedEvent) where
   hash = event_detail . bakedEvent_hash
   predecessor = event_detail . bakedEvent_signedHeader . blockHeader_predecessor
   fitness = event_detail . bakedEvent_signedHeader . blockHeader_fitness
   level = event_detail . bakedEvent_signedHeader . blockHeader_level
   timestamp = event_time

instance BlockLike (Event SeenEvent) where
   hash = event_detail . seenEvent_hash
   predecessor = event_detail . seenEvent_predecessor
   fitness = event_detail . seenEvent_fitness
   level = event_detail . seenEvent_level
   timestamp = event_time
