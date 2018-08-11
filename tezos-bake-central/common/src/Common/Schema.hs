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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module Common.Schema where

import qualified Cases
import Control.Lens (views, (^.))
import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as AppendMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Semigroup, Sum (..), getSum, (<>))
import Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (Generic)
import Rhyolite.Schema (Email, HasId, Id, Json)
import Tezos.Json
import Tezos.NodeRPC
import Tezos.Types


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
  { _client_address :: !ClientAddress
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
  { _node_address :: !ClientAddress
  , _node_identity :: !(Maybe CryptoboxPublicKeyHash)
  , _node_headLevel :: !(Maybe RawLevel)
  , _node_headBlockHash :: !(Maybe BlockHash)
  , _node_peerCount :: !(Maybe Word64)
  , _node_networkStat :: !NetworkStat
  , _node_fitness :: !(Maybe Fitness)
  , _node_deleted :: !Bool
  , _node_lastHeartbeat :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId Node

data TzScan = TzScan
  { _tzScan_chainId :: !ChainId
  , _tzScan_headLevel :: !(Maybe RawLevel)
  , _tzScan_headBlockHash :: !(Maybe BlockHash)
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId TzScan

mkNode :: ClientAddress -> Node
mkNode addr = Node
  { _node_address = addr
  , _node_identity = Nothing -- TODO
  , _node_headLevel = Nothing
  , _node_headBlockHash = Nothing
  , _node_peerCount = Nothing
  , _node_networkStat = NetworkStat 0 0 0 0
  , _node_fitness = Nothing
  , _node_deleted = False
  , _node_lastHeartbeat = Nothing
  }

data Parameters = Parameters
  { _parameters_node :: Id Node
  , _parameters_protoInfo :: ProtoInfo
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

-- instance BlockLike (Event BakedEvent) where

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

bakingRightsMap :: Foldable f => f BakingRights -> AppendMap PublicKeyHash (Map RawLevel Priority) -- map from delegate to
bakingRightsMap = foldMap $ \(BakingRights lvl delegate prio _) -> AppendMap.singleton delegate (Map.singleton lvl prio)

data BakeEfficiency = BakeEfficiency
  { _bakeEfficiency_bakedBlocks :: !Word64
  , _bakeEfficiency_bakingRights :: !Word64
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


data DelegateStats = DelegateStats
  { _delegateStats_delegate :: !(Id Delegate)
  , _delegateStats_efficiency :: !BakeEfficiency
  , _delegateStats_accountBalance :: !(Maybe Tez) -- "2052452947621"
  , _delegateStats_accountSpendable :: !(Maybe Bool) -- true
  , _delegateStats_accountSetable :: !(Maybe Bool)
  , _delegateStats_accountValue :: !(Maybe PublicKeyHash)
  , _delegateStats_accountCounter :: !(Maybe TezosWord64) -- 1540
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId DelegateStats

-- | convert the databasey DelegateStats to more jsoney (BakeEfficiency, Account)
unDelegateStats :: PublicKeyHash -> DelegateStats -> Maybe (BakeEfficiency, Account)
unDelegateStats publicKeyHash stats =
  let efficiency = _delegateStats_efficiency stats
      accountDelegate =
        AccountDelegate
          <$> _delegateStats_accountSetable stats
          <*> pure (_delegateStats_accountValue stats)
      account = Account publicKeyHash
        <$> _delegateStats_accountBalance stats
        <*> _delegateStats_accountSpendable stats
        <*> accountDelegate
        <*> pure Nothing -- TOOD: something?
        <*> _delegateStats_accountCounter stats
  in ((efficiency,) <$> account)

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
  , _errorLogInaccessibleEndpoint_address :: !ClientAddress
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

data ErrorLog = ErrorLog
  { _errorLog_started :: !UTCTime
  , _errorLog_stopped :: !(Maybe UTCTime)
  , _errorLog_lastSeen :: !UTCTime
  , _errorLog_noticeSentAt :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLog

data CachedProtocolConstants = CachedProtocolConstants
  { _cachedProtocolConstants_protocol :: !ProtocolHash
  , _cachedProtocolConstants_blocksPerCycle :: !RawLevel
  , _cachedProtocolConstants_preservedCycles :: !Cycle
  } deriving (Eq, Generic, Ord, Show, Typeable)
instance HasId CachedProtocolConstants

-- |  a tag for rights
data CachedChainCycle = CachedChainCycle
  { _cachedChainCycle_chainId :: !ChainId
  , _cachedChainCycle_constants :: !(Id CachedProtocolConstants)
  , _cachedChainCycle_cycle :: !Cycle
  , _cachedChainCycle_hash :: !BlockHash -- Hash of *first* block in cycle.
  , _cachedChainCycle_predecessor :: !BlockHash -- Hash of first block in *previous* cycle.
  } deriving (Eq, Generic, Ord, Show, Typeable)
instance HasId CachedChainCycle

data CachedBlock = CachedBlock
  { _cachedBlock_chain :: !(Id CachedChainCycle)
  , _cachedBlock_baker :: !PublicKeyHash
  , _cachedBlock_endorsers :: !(Json (Seq PublicKeyHash))
  , _cachedBlock_cyclePosition :: !RawLevel
  , _cachedBlock_hash :: !BlockHash
  , _cachedBlock_predecessor :: !BlockHash
  , _cachedBlock_fitness :: !Fitness
  , _cachedBlock_level :: !RawLevel
  , _cachedBlock_timestamp :: !UTCTime
  } deriving (Eq, Generic, Ord, Show, Typeable)
instance HasId CachedBlock


-- rights for block at level `level` for cycle `cycle`+`cycle.constants.preservedCycles`
data CachedBlockRights = CachedBlockRights
  { _cachedBlockRights_cycle :: !(Id CachedChainCycle)
  , _cachedBlockRights_forCycle :: !Word32
  , _cachedBlockRights_bakers :: !(Json (Seq BakingRights))
  , _cachedBlockRights_endorsers :: !(Json (Seq EndorsingRights))
  } deriving (Eq, Generic, Ord, Show, Typeable)
instance HasId CachedBlockRights

data CycleHistory = CycleHistory
  { _cycleHistory_ancestor :: !(Id CachedChainCycle)
  , _cycleHistory_descendant :: !(Id CachedChainCycle)
  , _cycleHistory_distance :: !Word32
  } deriving (Eq, Generic, Ord, Show, Typeable)
instance HasId CycleHistory



-- We build instances carefully so that they agree exactly with the JSON produced by the tezos ocaml apps
concat <$> traverse (deriveJSON Aeson.defaultOptions
      { Aeson.fieldLabelModifier = T.unpack . Cases.snakify . T.pack . dropWhile ('_' /=) . tail
      , Aeson.constructorTagModifier = T.unpack . Cases.snakify . T.pack . dropWhile ('_' /=)
      })
  [ ''BakeEfficiency
  , ''BakedEvent
  , ''BakedEventOperation
  , ''ClientConfig , ''ClientDaemonWorker
  , ''ClientInfo
  , ''ClientWorker
  , ''Delegate
  , ''DelegateStats
  , ''EndorseEvent
  , ''EndpointType
  , ''ErrorEvent
  , ''ErrorLog
  , ''ErrorLogBakerNoHeartbeat
  , ''ErrorLogInaccessibleEndpoint
  , ''ErrorLogMultipleBakersForSameDelegate
  , ''ErrorLogNodeOnFork
  , ''Event
  , ''Node
  , ''Report
  , ''SeenEvent
  , ''SmtpProtocol
  ]

concat <$> traverse makeLenses
  [ 'BakeEfficiency
  , 'BakedEvent
  , 'BakedEventOperation
  , 'Delegate
  , 'DelegateStats
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
  , 'Report
  , 'SeenEvent
  , 'CachedChainCycle
  , 'CachedBlockRights
  , 'CachedBlock
  , 'CachedProtocolConstants
  , 'VeryBlockLike
  ]

instance BlockLike CachedBlock where
  hash = cachedBlock_hash
  predecessor = cachedBlock_predecessor
  level = cachedBlock_level
  fitness = cachedBlock_fitness
  timestamp = cachedBlock_timestamp

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

