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
{-# LANGUAGE TypeFamilies #-}


module Common.Schema where

import qualified Cases
import Control.Lens.TH
import Data.Aeson hiding (Error)
import Data.Aeson.TH
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Fixed
import Data.Function
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Typeable
import Data.Word
import GHC.Generics
import Rhyolite.Schema

import Common.Base16ByteString
import Common.BlockHeader
import Common.Fitness
import Common.Json (TezosWord64)
import Common.Operation
import Common.PublicKeyHash
import Common.TaggedHash
import Common.Tez

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
  --, _protoInfo_firstFreeBakingSlot :: Int
  , _protoInfo_maxOperationDataLength :: Int
  , _protoInfo_michelsonMaximumTypeSize :: Int
  , _protoInfo_originationBurn :: Tezzies
  , _protoInfo_preservedCycles :: Int
  , _protoInfo_proofOfWorkThreshold :: TezosWord64
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

type Baked = Event BakedEvent

data Error = Error
  { _error_time :: UTCTime
  , _error_text :: Text
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance FromJSON Error
instance ToJSON Error

data BlockInfo = BlockInfo
  { _blockInfo_protocol :: ProtocolHash
  , _blockInfo_chainId :: ChainId
  , _blockInfo_hash :: BlockHash
  , _blockInfo_header :: BlockInfoHeader
  , _blockInfo_metadata :: BlockInfoMetadata
  --, _blockInfo_operations :: [[ProtoOperation]]
  }

data BlockInfoHeader = BlockInfoHeader
  { _blockInfoHeader_level :: TezosWord64
  , _blockInfoHeader_proto :: Word8
  , _blockInfoHeader_predecessor :: BlockHash
  , _blockInfoHeader_timestamp :: UTCTime
  , _blockInfoHeader_validationPass :: Word8
  , _blockInfoHeader_operationsHash :: OperationListListHash
  , _blockInfoHeader_fitness :: Fitness
  , _blockInfoHeader_context :: ContextHash
  -- , _blockInfoHeader_priority
  -- , _blockInfoHeader_proofOfWorkNonce
  -- , _blockInfoHeader_signature
  }

data BlockInfoMetadata = BlockInfoMetadata
  { _blockInfoMetadata_protocol :: ProtocolHash
  , _blockInfoMetadata_nextProtocol :: ProtocolHash
  -- , _blockInfoMetadata_testChainStatus
  -- , _blockInfoMetadata_maxOperationsTtl
  -- , _blockInfoMetadata_maxOperationDataLength
  -- , _blockInfoMetadata_maxBlockHeaderLength
  -- , _blockInfoMetadata_maxOperationListLength
  , _blockInfoMetadata_baker :: PublicKeyHash
  -- , _blockInfoMetadata_level
  --, _blockInfoMetadata_votingPeriodKind
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
  , _pendingReward_level :: TezosWord64
  , _pendingReward_amount :: Micro
  }
  deriving (Eq, Show, Generic, Typeable)

instance HasId PendingReward

data ClientInfo = ClientInfo
  { _clientInfo_client :: Id Client
  , _clientInfo_report :: Json Report
  , _clientInfo_config :: Json ClientConfig
  , _clientInfo_balance :: Maybe Tezzies
  -- , _clientInfo_node :: Id Node
  }
  deriving (Eq, Show, Generic, Typeable)

instance HasId ClientInfo

data NetworkStat = NetworkStat
  { _networkStat_totalSent :: TezosWord64 -- bytes
  , _networkStat_totalRecv :: TezosWord64 -- bytes
  , _networkStat_currentInflow :: Int32 -- bytes/s
  , _networkStat_currentOutflow :: Int32 -- bytes/s
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

data Node = Node
  { _node_address :: !ClientAddress
  , _node_identity :: !(Maybe CryptoboxPublicKeyHash)
  , _node_headLevel :: !(Maybe Word64)
  , _node_peerCount :: !(Maybe Word64)
  , _node_networkStat :: !NetworkStat
  , _node_fitness :: !(Maybe Fitness)
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId Node

data Parameters = Parameters
  { _parameters_node :: Id Node
  , _parameters_protoInfo :: ProtoInfo
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId Parameters

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

data BakedEventOperation = BakedEventOperation
  { _bakedEventOperation_branch :: BlockHash
  , _bakedEventOperation_data :: Base16ByteString ProtoOperation
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

data BakedEvent = BakedEvent
  { _bakedEvent_hash :: BlockHash
  , _bakedEvent_operations :: [[BakedEventOperation]]
  , _bakedEvent_signedHeader :: Base16ByteString BlockHeader
  }
  deriving (Show, Eq, Ord, Typeable, Generic)



data SeenEvent = SeenEvent
  { _seenEvent_hash :: BlockHash
  -- , _seenEvent_chainId :: ChainId
  -- , _seenEvent_fitness :: Fitness
  , _seenEvent_level :: Word64
  , _seenEvent_predecessor :: BlockHash
  -- , _seenEvent_protocol :: Protocol
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

data EndorseEvent = EndorseEvent
  { _endorseEvent_hash :: BlockHash
  , _endorseEvent_level :: Int
  , _endorseEvent_slot :: Int
  , _endorseEvent_delegate :: PublicKeyHash
  , _endorseEvent_name :: String
  , _endorseEvent_oph :: OperationHash
  }
  deriving (Show, Eq, Typeable, Generic)

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
  }
  deriving (Show, Eq, Typeable, Generic)

-- TODO: handle parsing errors
blockLevel :: Event BakedEvent -> Int
blockLevel = fromIntegral . _blockHeader_level . unbase16ByteString . _bakedEvent_signedHeader . _event_detail

blockRewards :: Event BakedEvent -> ProtoInfo -> Tezzies
blockRewards b p = _protoInfo_blockReward p + fees + nonceTip
  where
    blockHeader = unbase16ByteString $ _bakedEvent_signedHeader $ _event_detail b
    nonceTip = maybe 0 (const $ _protoInfo_seedNonceRevelationTip p) (_blockHeader_seedNonceHash blockHeader)
    fees = getSum $ (foldMap.foldMap) (Sum . sumFees . unbase16ByteString . _bakedEventOperation_data) $ _bakedEvent_operations $ _event_detail b

endorsementReward :: Event EndorseEvent -> ProtoInfo -> Tezzies
endorsementReward b p = Tezzies $ getTezzies (_protoInfo_endorsementReward p) / fromIntegral (1 + _endorseEvent_slot (_event_detail b))

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
  }
  deriving (Show, Eq, Typeable, Generic)

data Account = Account
  { _account_manager :: PublicKeyHash -- "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
  , _account_balance :: Tezzies -- "2052452947621"
  , _account_spendable :: Bool -- true
  -- , _account_delegate :: {"setable":false,"value":"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"}
  , _account_counter :: TezosWord64 -- 1540
  }

newtype BlockPrefix = BlockPrefix Text
  deriving (Eq, Show, Generic, Typeable, ToJSON, FromJSON)

data BlockId = BlockId
  { _blockId_chainId :: DynamicParamChainId
  , _blockId_blockHash :: DynamicParamBlockHash
  , _blockId_predecessor :: Maybe Word64 -- ^ Number predecessors prior to block
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

blockHashIdPred :: BlockHash -> Word64 -> BlockId
blockHashIdPred x = BlockId DynamicParamChainId_Main (DynamicParamBlockHash_BlockHash x) . Just

genesisId :: BlockId
genesisId = BlockId DynamicParamChainId_Main DynamicParamBlockHash_Genesis Nothing

headId :: BlockId
headId = BlockId DynamicParamChainId_Main DynamicParamBlockHash_Head Nothing

testHeadId :: BlockId
testHeadId = BlockId DynamicParamChainId_Main DynamicParamBlockHash_TestHead Nothing

blockIdToUrl :: BlockId -> Text
blockIdToUrl (BlockId chainId blockId offset) = "/chains/" <> chainId' <> "/blocks/" <> blockId' <> offset'
  where
    chainId' = case chainId of
      DynamicParamChainId_ChainId x -> toBase58Text x
      DynamicParamChainId_Main -> "main"
      DynamicParamChainId_Test -> "test"
    blockId' = case blockId of
      DynamicParamBlockHash_BlockHash x -> toBase58Text x
      DynamicParamBlockHash_Genesis -> "genesis"
      DynamicParamBlockHash_Head -> "head"
      DynamicParamBlockHash_TestHead -> "test_head"
    offset' = maybe "" (("~" <>) . T.pack . show) offset

data NodeRPCRequest a where
  RComplete :: BlockPrefix -> NodeRPCRequest [BlockHash]
  RBlock :: BlockId -> NodeRPCRequest BlockInfo
  RProtoConstants :: NodeRPCRequest ProtoInfo
  RContract :: BlockId -> PublicKeyHash -> NodeRPCRequest Account
  RConnections :: NodeRPCRequest Word64 -- just a count for now, but there's more data there we may someday be interested in
  RNetworkStat :: NodeRPCRequest NetworkStat


data RpcError =
    RpcError_HttpException Text
  | RpcError_UnexpectedStatus Int BS.ByteString
  | RpcError_NonJSON String LBS.ByteString
  deriving (Eq, Ord, Show, Generic, Typeable)


type RpcResponse = Either RpcError

class MonadTezosNode m where
  nodeRPC :: NodeRPCRequest a -> m (RpcResponse a)
  nodeAddress :: m Text


data Notificatee = Notificatee
  { _notificatee_email :: Email
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

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
  } deriving (Eq, Generic, Ord, Show)

instance HasId MailServerConfig


-- We build instances carefully so that they agree exactly with the JSON produced by the tezos ocaml apps
concat <$> traverse (deriveJSON defaultOptions
      { fieldLabelModifier =     T.unpack . Cases.snakify . T.pack . dropWhile ('_' /=) . tail
      , constructorTagModifier = T.unpack . Cases.snakify . T.pack . dropWhile ('_' /=)
      })
  [ ''Account
  , ''BakedEvent
  , ''BakedEventOperation
  , ''BlockId
  , ''BlockInfo
  , ''BlockInfoHeader
  , ''BlockInfoMetadata
  , ''ClientConfig
  , ''ClientDaemonWorker
  , ''ClientInfo
  , ''DynamicParamBlockHash
  , ''DynamicParamChainId
  , ''EndorseEvent
  , ''ErrorEvent
  , ''Event
  , ''Level
  , ''NetworkStat
  , ''Node
  , ''ProtoInfo
  , ''Report
  , ''SeenEvent
  , ''SmtpProtocol
  ]

concat <$> traverse makeLenses
  [ 'BakedEvent
  , 'BakedEventOperation
  , 'BlockInfo
  , 'BlockInfoHeader
  , 'BlockInfoMetadata
  , 'EndorseEvent
  , 'Error
  , 'ErrorEvent
  , 'Event
  , 'MailServerConfig
  , 'Report
  , 'SeenEvent
  ]
