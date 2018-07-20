{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
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

module Common.Schema where

import qualified Cases
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as AppendMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Fixed (Micro)
import Data.Function (fix)
import Data.Functor.Identity (Identity)
import Data.Int (Int32, Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Semigroup, Sum (..), getSum, (<>))
import Data.Sequence as Seq
import Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import Rhyolite.Schema (Email, HasId, Id, Json)

import Common (tshow)
import Common.Base16ByteString (Base16ByteString (..))
import Common.BlockHeader (BlockHeader (..))
import Common.Fitness (Fitness)
import Common.Json (TezosWord64)
import Common.Operation (ProtoOperation, sumFees)
import Common.PublicKeyHash (PublicKeyHash)
import Common.TaggedHash (BlockHash, ChainId, ContextHash, CryptoboxPublicKeyHash, OperationHash,
                          OperationListListHash, ProtocolHash, toBase58Text)
import Common.Tez (Tez (..))

-- import GADT.JSON (deriveGadtJson)

-- TODO:  all of the hashey things in Tezos are some mystery hash in base58
-- with a wonky 1.5-2 letter prefix.  maybe capture that and get read/show
-- instances once and for all...

newtype PeriodSequenceF a = PeriodSequence (NonEmpty a)
  deriving (Eq, Ord, Show, Generic, Typeable, ToJSON, FromJSON, Functor)

instance Foldable PeriodSequenceF where
  foldMap f (PeriodSequence xs) = go xs where
    go (x :| []) = fix (f x `mappend`)
    go (x :| (y:ys)) = f x `mappend` go (y :| ys)

type PeriodSequence = PeriodSequenceF TezosWord64

data ProtoInfo = ProtoInfo
  { _protoInfo_blockReward :: Tez
  , _protoInfo_blockSecurityDeposit :: Tez
  , _protoInfo_blocksPerCommitment :: !RawLevel
  , _protoInfo_blocksPerCycle :: !RawLevel
  , _protoInfo_blocksPerRollSnapshot :: !RawLevel
  , _protoInfo_blocksPerVotingPeriod :: !RawLevel
  , _protoInfo_endorsementReward :: Tez
  , _protoInfo_endorsementSecurityDeposit :: Tez
  , _protoInfo_endorsersPerBlock :: Int
  , _protoInfo_maxOperationDataLength :: Int
  , _protoInfo_michelsonMaximumTypeSize :: Int
  , _protoInfo_originationBurn :: Tez
  , _protoInfo_preservedCycles :: !Cycle
  , _protoInfo_proofOfWorkThreshold :: TezosWord64
  , _protoInfo_seedNonceRevelationTip :: Tez
  , _protoInfo_timeBetweenBlocks :: PeriodSequence -- repeating sequence of seconds
  , _protoInfo_tokensPerRoll :: Tez
  -- TODO: these didn't show up in my quick greppings, so I don't know the types too exactly.
  -- , _protoInfo_instructionsPerTransaction :: 40000
  -- , _protoInfo_maxRevelationsPerBlock :: 32,
  -- , _protoInfo_nonceLength :: 32,
  -- , _protoInfo_proofOfWorkNonceSize :: 8
  } deriving (Eq, Ord, Show, Generic, Typeable)

type Baked = Event BakedEvent

data Error = Error
  { _error_time :: UTCTime
  , _error_text :: Text
  } deriving (Eq, Ord, Show, Generic, Typeable)

data BlockInfo = BlockInfo
  { _blockInfo_protocol :: ProtocolHash
  , _blockInfo_chainId :: ChainId
  , _blockInfo_hash :: BlockHash
  , _blockInfo_header :: BlockInfoHeader
  , _blockInfo_metadata :: BlockInfoMetadata
  --, _blockInfo_operations :: [[ProtoOperation]]
  } deriving (Eq, Show, Generic, Typeable)

data BlockInfoHeader = BlockInfoHeader
  { _blockInfoHeader_level :: RawLevel
  , _blockInfoHeader_proto :: Word8
  , _blockInfoHeader_predecessor :: BlockHash
  , _blockInfoHeader_timestamp :: UTCTime
  , _blockInfoHeader_validationPass :: Word8
  , _blockInfoHeader_operationsHash :: OperationListListHash
  , _blockInfoHeader_fitness :: Fitness
  , _blockInfoHeader_context :: ContextHash
  , _blockInfoHeader_priority :: Priority
  -- , _blockInfoHeader_proofOfWorkNonce
  -- , _blockInfoHeader_signature
  } deriving (Eq, Show, Generic, Typeable)

data BlockInfoMetadata = BlockInfoMetadata
  { _blockInfoMetadata_protocol :: ProtocolHash
  , _blockInfoMetadata_nextProtocol :: ProtocolHash
  -- , _blockInfoMetadata_testChainStatus
  -- , _blockInfoMetadata_maxOperationsTtl
  -- , _blockInfoMetadata_maxOperationDataLength
  -- , _blockInfoMetadata_maxBlockHeaderLength
  -- , _blockInfoMetadata_maxOperationListLength
  , _blockInfoMetadata_baker :: PublicKeyHash
  , _blockInfoMetadata_level :: Level
  --, _blockInfoMetadata_votingPeriodKind
  } deriving (Eq, Show, Generic, Typeable)

type ClientAddress = Text


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

data NetworkStat = NetworkStat
  { _networkStat_totalSent :: TezosWord64 -- bytes
  , _networkStat_totalRecv :: TezosWord64 -- bytes
  , _networkStat_currentInflow :: Int32 -- bytes/s
  , _networkStat_currentOutflow :: Int32 -- bytes/s
  } deriving (Eq, Ord, Show, Generic, Typeable)

data Node = Node
  { _node_address :: !ClientAddress
  , _node_identity :: !(Maybe CryptoboxPublicKeyHash)
  , _node_headLevel :: !(Maybe RawLevel)
  , _node_headBlockHash :: !(Maybe BlockHash)
  , _node_peerCount :: !(Maybe Word64)
  , _node_networkStat :: !NetworkStat
  , _node_fitness :: !(Maybe Fitness)
  , _node_deleted :: !Bool
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId Node

data Parameters = Parameters
  { _parameters_node :: Id Node
  , _parameters_protoInfo :: ProtoInfo
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId Parameters

data Level = Level
  { _level_cycle :: Cycle
  , _level_cyclePosition :: RawLevel
  , _level_expectedCommitment :: Bool
  , _level_level :: RawLevel
  , _level_levelPosition :: Int
  , _level_votingPeriod :: Int
  , _level_votingPeriodPosition :: Int
  } deriving (Show, Eq, Ord, Typeable, Generic)

data BakedEventOperation = BakedEventOperation
  { _bakedEventOperation_branch :: BlockHash
  , _bakedEventOperation_data :: Base16ByteString ProtoOperation
  } deriving (Show, Eq, Ord, Typeable, Generic)

data BakedEvent = BakedEvent
  { _bakedEvent_hash :: BlockHash
  , _bakedEvent_operations :: [[BakedEventOperation]]
  , _bakedEvent_signedHeader :: Base16ByteString BlockHeader
  } deriving (Show, Eq, Ord, Typeable, Generic)

data SeenEvent = SeenEvent
  { _seenEvent_hash :: BlockHash
  -- , _seenEvent_chainId :: ChainId
  -- , _seenEvent_fitness :: Fitness
  , _seenEvent_level :: Word64
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
  , _endorseEvent_slot :: Int
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

-- TODO: handle parsing errors
blockLevel :: Event BakedEvent -> Int
blockLevel = fromIntegral . _blockHeader_level . unbase16ByteString . _bakedEvent_signedHeader . _event_detail

blockRewards :: Event BakedEvent -> ProtoInfo -> Tez
blockRewards b p = _protoInfo_blockReward p + fees + nonceTip
  where
    blockHeader = unbase16ByteString $ _bakedEvent_signedHeader $ _event_detail b
    nonceTip = maybe 0 (const $ _protoInfo_seedNonceRevelationTip p) (_blockHeader_seedNonceHash blockHeader)
    fees = getSum $ (foldMap.foldMap) (Sum . sumFees . unbase16ByteString . _bakedEventOperation_data) $ _bakedEvent_operations $ _event_detail b

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


data AccountDelegate = AccountDelegate
  { _accountDelegate_setable :: !Bool
  , _accountDelegate_value :: !(Maybe PublicKeyHash)
  } deriving (Show, Eq, Ord, Generic, Typeable)

data Account = Account
  { _account_manager :: !PublicKeyHash -- "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
  , _account_balance :: !Tez -- "2052452947621"
  , _account_spendable :: !Bool -- true
  , _account_delegate :: !AccountDelegate
  , _account_counter :: !TezosWord64 -- 1540
  } deriving (Show, Eq, Ord, Generic, Typeable)

newtype BlockPrefix = BlockPrefix Text
  deriving (Eq, Show, Generic, Typeable)

data BlockId = BlockId
  { _blockId_chainId :: DynamicParamChainId
  , _blockId_blockHash :: DynamicParamBlockHash
  , _blockId_predecessor :: Maybe RawLevel -- ^ Number predecessors prior to block
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

data DynamicParamBlockHash
  = DynamicParamBlockHash_BlockHash BlockHash
  | DynamicParamBlockHash_Genesis
  | DynamicParamBlockHash_Head
  | DynamicParamBlockHash_TestHead
  deriving (Eq, Ord, Show, Generic, Typeable)

data DynamicParamChainId
  = DynamicParamChainId_ChainId ChainId
  | DynamicParamChainId_Main
  | DynamicParamChainId_Test
  deriving (Eq, Ord, Show, Generic, Typeable)

-- Smart constructors for "dynamic" url patterns in NodeRPC
blockHashId :: BlockHash -> BlockId
blockHashId x = BlockId DynamicParamChainId_Main (DynamicParamBlockHash_BlockHash x) Nothing

blockHashId' :: ChainId -> BlockHash -> BlockId
blockHashId' chain x = BlockId (DynamicParamChainId_ChainId chain) (DynamicParamBlockHash_BlockHash x) Nothing

blockHashIdPred :: BlockHash -> RawLevel -> BlockId
blockHashIdPred x = BlockId DynamicParamChainId_Main (DynamicParamBlockHash_BlockHash x) . Just

blockHashIdPred' :: ChainId -> BlockHash -> RawLevel -> BlockId
blockHashIdPred' chain x = BlockId (DynamicParamChainId_ChainId chain) (DynamicParamBlockHash_BlockHash x) . Just

genesisId :: BlockId
genesisId = BlockId DynamicParamChainId_Main DynamicParamBlockHash_Genesis Nothing

genesisId' :: ChainId -> BlockId
genesisId' chain = BlockId (DynamicParamChainId_ChainId chain) DynamicParamBlockHash_Genesis Nothing

chainHeadId :: ChainId -> BlockId
chainHeadId chain = BlockId (DynamicParamChainId_ChainId chain) DynamicParamBlockHash_Head Nothing

headId :: BlockId
headId = BlockId DynamicParamChainId_Main DynamicParamBlockHash_Head Nothing

headId' :: ChainId -> BlockId
headId' chain = BlockId (DynamicParamChainId_ChainId chain) DynamicParamBlockHash_Head Nothing

testHeadId :: BlockId
testHeadId = BlockId DynamicParamChainId_Main DynamicParamBlockHash_TestHead Nothing

chainIdToUrl :: DynamicParamChainId -> Text
chainIdToUrl chainId = case chainId of
  DynamicParamChainId_ChainId x -> toBase58Text x
  DynamicParamChainId_Main -> "main"
  DynamicParamChainId_Test -> "test"

blockIdToUrl :: BlockId -> Text
blockIdToUrl (BlockId chainId blockId offset) = "/chains/" <> chainIdToUrl chainId <> "/blocks/" <> blockId' <> offset'
  where
    blockId' = case blockId of
      DynamicParamBlockHash_BlockHash x -> toBase58Text x
      DynamicParamBlockHash_Genesis -> "genesis"
      DynamicParamBlockHash_Head -> "head"
      DynamicParamBlockHash_TestHead -> "test_head"
    offset' = maybe "" (("~" <>) . tshow . unRawLevel) offset


-- ACTUALLY, 2^30 max (from Ocaml types)
newtype RawLevel = RawLevel { unRawLevel :: Int32 }
  deriving (Eq, Ord, Show, Enum, Typeable, Generic, FromJSON, ToJSON, Num, Integral, Real)
newtype Cycle = Cycle { unCycle :: Int32 }
  deriving (Eq, Ord, Show, Enum, Typeable, Generic, FromJSON, ToJSON, Num, Integral, Real)

newtype Priority = Priority { unPriority :: Word64 }
  deriving (Eq, Ord, Generic, Typeable, Show, FromJSON, ToJSON)

data NodeRPCRequest a where
  RComplete :: BlockPrefix -> NodeRPCRequest [BlockHash]
  RBlock :: BlockId -> NodeRPCRequest BlockInfo
  RBlocks :: DynamicParamChainId -> RawLevel -> Set BlockHash -> NodeRPCRequest (Map BlockHash (Seq BlockHash)) -- the predecessors of the requested block.
  RProtoConstants :: BlockId -> NodeRPCRequest ProtoInfo
  RContract :: BlockId -> PublicKeyHash -> NodeRPCRequest Account
  RConnections :: NodeRPCRequest Word64 -- just a count for now, but there's more data there we may someday be interested in

  -- This only produces results when the cycles requested are between within
  -- $PRESERVED_CYCLES of the BlockId requested. for older data, use an older block as context
  RBakingRights :: BlockId -> Set (Either RawLevel Cycle) -> NodeRPCRequest (Seq BakingRights)
  REndorsingRights :: BlockId -> Set (Either RawLevel Cycle) -> NodeRPCRequest (Seq EndorsingRights)
  RNetworkStat :: NodeRPCRequest NetworkStat

  RMonitorHeads :: (RpcResponse MonitorBlock -> IO ()) -> DynamicParamChainId -> NodeRPCRequest (IO ())

bakingRightsMap :: Foldable f => f BakingRights -> AppendMap PublicKeyHash (Map RawLevel Priority) -- map from delegate to
bakingRightsMap = foldMap $ \(BakingRights lvl delegate prio _) -> AppendMap.singleton delegate (Map.singleton lvl prio)

data RpcError
  = RpcError_HttpException Text
  | RpcError_UnexpectedStatus Int BS.ByteString
  | RpcError_NonJSON String LBS.ByteString
  deriving (Eq, Ord, Show, Generic, Typeable)

type RpcResponse = Either RpcError


data BakeEfficiency = BakeEfficiency
  { _bakeEfficiency_bakedBlocks :: !Word64
  , _bakeEfficiency_bakingRights :: !Word64
  } deriving (Eq, Ord, Show, Generic, Typeable)

instance Semigroup BakeEfficiency where
  BakeEfficiency x1 x2 <> BakeEfficiency y1 y2 = BakeEfficiency (x1 + y1) (x2 + y2)

instance Monoid BakeEfficiency where
  mempty = BakeEfficiency 0 0
  mappend = (<>)

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
  , _errorLogBakerNoHeartbeat_lastLevel :: !Word64
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
  -- , _monitorBlock_protocolData :: Base16ByteString ??? -- Certainly NOT a blockheader...
  } deriving (Eq, Generic, Ord, Show)

data BakingRights = BakingRights
  { _bakingRights_level :: !RawLevel
  , _bakingRights_delegate :: !PublicKeyHash
  , _bakingRights_priority :: !Priority
  , _bakingRights_estimatedTime :: !(Maybe UTCTime)
  } deriving (Eq, Generic, Ord, Show)

data EndorsingRights = EndorsingRights
  { _endorsingRights_level :: !RawLevel
  , _endorsingRights_delegate :: !PublicKeyHash
  , _endorsingRights_slots :: !(Seq Word8)
  , _endorsingRights_estimatedTime :: !(Maybe UTCTime)
  } deriving (Eq, Generic, Ord, Show)

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
  [ ''Account
  , ''AccountDelegate
  , ''BakeEfficiency
  , ''BakedEvent
  , ''BakedEventOperation
  , ''BlockId
  , ''BlockInfo
  , ''BlockInfoHeader
  , ''BlockInfoMetadata
  , ''BakingRights
  , ''EndorsingRights
  , ''ClientConfig
  , ''ClientDaemonWorker
  , ''ClientInfo
  , ''ClientWorker
  , ''Delegate
  , ''DelegateStats
  , ''DynamicParamBlockHash
  , ''DynamicParamChainId
  , ''EndorseEvent
  , ''EndpointType
  , ''ErrorEvent
  , ''ErrorLog
  , ''ErrorLogBakerNoHeartbeat
  , ''ErrorLogInaccessibleEndpoint
  , ''ErrorLogMultipleBakersForSameDelegate
  , ''ErrorLogNodeOnFork
  , ''Event
  , ''Level
  , ''MonitorBlock
  , ''NetworkStat
  , ''Node
  , ''ProtoInfo
  , ''Report
  , ''SeenEvent
  , ''SmtpProtocol
  ]

concat <$> traverse makeLenses
  [ 'BakeEfficiency
  , 'BakedEvent
  , 'BakedEventOperation
  , 'BlockInfo
  , 'BlockInfoHeader
  , 'BlockInfoMetadata
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
  , 'Level
  , 'MailServerConfig
  , 'Report
  , 'SeenEvent
  ]
