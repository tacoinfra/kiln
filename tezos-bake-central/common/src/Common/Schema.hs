{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Needed for nested `deriveArgDict`
{-# LANGUAGE UndecidableInstances #-}

-- TODO do everywhere
{-# OPTIONS_GHC -Wall -fno-warn-orphans -Werror #-}

-- 'deriveJSONGADT' produces seemingly redundant pattern matches.
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- GHC is confused about type families.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Common.Schema
  ( module Common.Schema

  -- Re-exports
  , Id
  ) where

import Control.Exception.Safe (Exception, SomeException)
import Control.Lens hiding (universe)
import Control.Monad.Except (runExcept)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonE
import Data.Aeson.TH (deriveJSON)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Aeson.GADT (deriveJSONGADT)
import Data.GADT.Compare.TH (deriveGEq, deriveEqTagIdentity)
import Data.GADT.Compare.TH (deriveGCompare, deriveOrdTagIdentity)
import Data.GADT.Show.TH (deriveGShow, deriveShowTagIdentity)
import Data.Dependent.Sum.Orphans ()
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Semigroup, Sum (..), getSum, (<>))
import Data.Sequence (Seq)
import Data.Some (Some(..))
import Data.Text (Text)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime)
import Data.Typeable (Typeable)
import Data.Universe
import Data.Universe.Helpers (universeDef)
import Data.Universe.TH (deriveSomeUniverse)
import Data.Version (Version)
import Data.Word
import GHC.Generics (Generic)
import Language.Haskell.TH (Name)
import Rhyolite.Schema (Email, HasId (..), Id, Json)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.NodeRPC.Sources (PublicNode)
import Tezos.NodeRPC.Types (NetworkStat (..), RpcError, AsRpcError (asRpcError))
import Tezos.Operation
import Tezos.Types

import Common (defaultTezosCompatJsonOptions)
import ExtraPrelude

data ClientError
  = ClientError_NodeNotReady
  | ClientError_RequestDeclinedByLedger
  | ClientError_LedgerDisconnected
  | ClientError_Other Text
  deriving (Eq, Ord, Show, Generic, Typeable)
instance Aeson.ToJSON ClientError
instance Aeson.FromJSON ClientError

-- | Required Tezos Baking app version
requiredTezosBakingAppVersion :: Text
requiredTezosBakingAppVersion = "2.0.0"

data CacheError
  = CacheError_RpcError !RpcError
  | CacheError_NoSuitableNode
  | CacheError_NotEnoughHistory
  | CacheError_Timeout !NominalDiffTime
  | CacheError_SomeException !SomeException
  | CacheError_UnrevealedPublicKey !ContractId
  deriving (Show, Generic, Typeable)
instance Exception CacheError
makePrisms ''CacheError

class AsCacheError e where
  asCacheError :: Prism' e CacheError

instance AsRpcError CacheError where
  asRpcError = _CacheError_RpcError

instance AsCacheError CacheError where
  asCacheError = id

instance Aeson.ToJSON Uri.URI where
  toJSON = Aeson.toJSON . Uri.render
  toEncoding = Aeson.toEncoding . Uri.render
instance Aeson.FromJSON Uri.URI where
  parseJSON x = maybe (fail "Invalid URI") pure . Uri.mkURI =<< Aeson.parseJSON x

sumFees :: PublicKeyHash -> Operation -> Tez
sumFees baker = getSum . views balanceUpdates getFee
  where
    getFee :: BalanceUpdate -> Sum Tez
    getFee (BalanceUpdate_Freezer x) | _freezerUpdate_delegate x == baker = Sum (_freezerUpdate_change x)
    getFee _ = Sum 0

type Baked = Event BakedEvent

data Error = Error
  { _error_time :: UTCTime
  , _error_text :: Text
  } deriving (Eq, Ord, Show, Generic, Typeable)

mkErr :: Event ErrorEvent -> Error
mkErr err = Error
  { _error_time = _event_time err
  , _error_text = _errorEvent_message $ _event_detail err
  }

-- TODO: move to ~-lib
knownProtocols :: [ProtocolHash]
knownProtocols =
  [ "PrihK96nBAFSxVL1GLJTVhu9YnzkMFiBeuJRPA8NwuZVZCE1L6i" -- GENESIS
  , "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" -- MAINNET
  ]

data BlockBaker = BlockBaker
  { _blockBaker_publicKeyHash :: !PublicKeyHash
  , _blockBaker_priority :: !Priority
  , _blockBaker_endorsements :: !(Map PublicKeyHash (Seq Word8))
  } deriving (Eq, Ord, Show, Typeable, Generic)

getBakerFromBlock :: Block -> BlockBaker
getBakerFromBlock block = BlockBaker
  { _blockBaker_publicKeyHash = block ^. block_metadata . blockMetadata_baker
  , _blockBaker_priority = block ^. block_header . blockHeader_priority
  , _blockBaker_endorsements = block ^. block_operations
    . traverse
    . traverse
    . operation_contents
    . traverse
    . _OperationContents_Endorsement
    . operationContentsEndorsement_metadata
    . to f
  }
  where
    f :: EndorsementMetadata -> Map PublicKeyHash (Seq Word8)
    f em = Map.singleton
      (_endorsementMetadata_delegate em)
      (_endorsementMetadata_slots em)

data DeletableRow a = DeletableRow
  { _deletableRow_data :: !a
  , _deletableRow_deleted :: !Bool
  } deriving (Eq, Ord, Show, Generic, Typeable)

--------------------------------------------------------------------------------
-- Baker Daemon
--------------------------------------------------------------------------------

-- Just for the surrogate key for now.ils_ = BakerDetails (WithId PublicKeyHash Baker
data BakerDaemon = BakerDaemon
  deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId BakerDaemon

-- data BakerDaemonExternal = BakerDaemonExternal (WithId (Id BakerDaemon) (Deletable BakerDaemonExternal'))

data BakerDaemonExternal = BakerDaemonExternal
  { _bakerDaemonExternal_id :: !(Id BakerDaemon)
  , _bakerDaemonExternal_data :: !(DeletableRow BakerDaemonExternalData)
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId BakerDaemonExternal where
  -- Should be the same as `IdData BakerDaemonExternalData` always.
  type IdData BakerDaemonExternal = Id BakerDaemon

data BakerDaemonExternalData = BakerDaemonExternalData
  { _bakerDaemonExternalData_address :: !URI
  , _bakerDaemonExternalData_alias :: !(Maybe Text)
  , _bakerDaemonExternalData_updated :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId BakerDaemonExternalData where
  type IdData BakerDaemonExternalData = Id BakerDaemon

-- data BakerDaemonDetails = BakerDaemonDetails (WithId (Id BakerDaemon) BakerDaemonDetails')

data BakerDaemonInfo = BakerDaemonInfo
  { _bakerDaemonInfo_id :: !(Id BakerDaemon)
  , _bakerDaemonInfo_data :: BakerDaemonInfoData
  } deriving (Eq, Ord, Show, Generic, Typeable)

data BakerDaemonInfoData = BakerDaemonInfoData
  { _bakerDaemonInfoData_report :: !(Json Report)
  , _bakerDaemonInfoData_config :: !(Json ClientConfig)
  -- , _bakerDaemonInfo_node :: Id Node
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId BakerDaemonInfoData where
  type IdData BakerDaemonInfoData = Id BakerDaemon

data BakerDaemonInternal = BakerDaemonInternal
  { _bakerDaemonInternal_id :: !(Id BakerDaemon)
  , _bakerDaemonInternal_data :: !(DeletableRow BakerDaemonInternalData)
  } deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId BakerDaemonInternal where
  type IdData BakerDaemonInternal = Id BakerDaemon

data ConnectedLedger = ConnectedLedger
  { _connectedLedger_ledgerIdentifier :: !(Maybe LedgerIdentifier)
  , _connectedLedger_bakingAppVersion :: !(Maybe Text)
  , _connectedLedger_updated :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance Aeson.ToJSON ConnectedLedger
instance Aeson.FromJSON ConnectedLedger

data LedgerAccount = LedgerAccount
  { _ledgerAccount_publicKeyHash :: !(Maybe PublicKeyHash)
  , _ledgerAccount_secretKey :: !SecretKey
  , _ledgerAccount_balance :: !(Maybe Tez)
  , _ledgerAccount_shouldImport :: !Bool
  , _ledgerAccount_imported :: !Bool
  , _ledgerAccount_shouldSetupToBake :: !Bool
  , _ledgerAccount_shouldRegisterFee :: !(Maybe Tez) -- ^ Contains the fee if the user wishes to register
  , _ledgerAccount_shouldSetHWM :: !(Maybe RawLevel) -- ^ Contains the block level if we need to set the HWM
  } deriving (Eq, Ord, Show, Generic, Typeable)

-- This can be lifted into 'LedgerAccount' if we need to support more than one
-- baker in the future
kilnLedgerAlias :: Text
kilnLedgerAlias = "ledger_kiln"

data BakerDaemonInternalData = BakerDaemonInternalData
  { _bakerDaemonInternalData_alias :: !Text
  , _bakerDaemonInternalData_publicKeyHash :: !(Maybe PublicKeyHash)
  , _bakerDaemonInternalData_insufficientFunds :: !Bool
  , _bakerDaemonInternalData_protocol :: !ProtocolHash
  , _bakerDaemonInternalData_bakerProcessData :: !(Id ProcessData)
  , _bakerDaemonInternalData_endorserProcessData :: !(Id ProcessData)
  , _bakerDaemonInternalData_altProtocol :: !(Maybe ProtocolHash)
  , _bakerDaemonInternalData_altBakerProcessData :: !(Id ProcessData)
  , _bakerDaemonInternalData_altEndorserProcessData :: !(Id ProcessData)
  } deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId BakerDaemonInternalData where
  type IdData BakerDaemonInternalData = Id BakerDaemon

--------------------------------------------------------------------------------
-- Node
--------------------------------------------------------------------------------

-- Just for the surrogate key for now.ils_ = BakerDetails (WithId PublicKeyHash Baker
data Node = Node
  deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId Node

instance HasId a => HasId (DeletableRow a) where
  type IdData (DeletableRow a) = IdData a

-- data NodeExternal = NodeExternal (WithId (Id Node) (Deletable NodeExternal'))

data NodeExternal = NodeExternal
  { _nodeExternal_id :: !(Id Node)
  , _nodeExternal_data :: !(DeletableRow NodeExternalData)
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId NodeExternal where
  -- Should be the same as `IdData NodeExternalData` always.
  type IdData NodeExternal = Id Node

data NodeExternalData = NodeExternalData
  { _nodeExternalData_address :: !URI
  , _nodeExternalData_alias :: !(Maybe Text)
  , _nodeExternalData_minPeerConnections :: !(Maybe Int)
  } deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId NodeExternalData where
  type IdData NodeExternalData = Id Node

-- data NodeInternal = NodeInternal (WithId (Id Node) (Deletable NodeInternal'))

data NodeInternal = NodeInternal
  { _nodeInternal_id :: !(Id Node)
  , _nodeInternal_data :: !(DeletableRow (Id ProcessData))
  } deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId NodeInternal where
  type IdData NodeInternal = Id Node

data ProcessState
   = ProcessState_Stopped
   | ProcessState_Initializing
   | ProcessState_GeneratingIdentity -- only applicable to Node
   | ProcessState_Starting
   | ProcessState_Running
   | ProcessState_Failed
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Enum, Bounded)

data ProcessControl
  = ProcessControl_Run
  | ProcessControl_Stop
  | ProcessControl_Restart
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Enum, Bounded)

data ProcessData = ProcessData
  { _processData_control :: !ProcessControl
  , _processData_state :: !ProcessState -- the state the process is actually in.
  , _processData_updated :: !(Maybe UTCTime) -- the time the process' state was last set.
  , _processData_backend :: !(Maybe Int) -- a "unique" process id
  } deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId ProcessData

-- data NodeDetails = NodeDetails (WithId (Id Node) NodeDetails')

data NodeDetails = NodeDetails
  { _nodeDetails_id :: !(Id Node)
  , _nodeDetails_data :: NodeDetailsData
  } deriving (Eq, Ord, Show, Generic, Typeable)

-- TODO don't need Maybes here probably.
data NodeDetailsData = NodeDetailsData
  { _nodeDetailsData_identity :: !(Maybe CryptoboxPublicKeyHash)
  , _nodeDetailsData_headLevel :: !(Maybe RawLevel)
  , _nodeDetailsData_headBlockHash :: !(Maybe BlockHash)
  , _nodeDetailsData_headBlockPred :: !(Maybe BlockHash)
  , _nodeDetailsData_headBlockBakedAt :: !(Maybe UTCTime)
  , _nodeDetailsData_peerCount :: !(Maybe Word64)
  , _nodeDetailsData_networkStat :: !NetworkStat
  , _nodeDetailsData_fitness :: !(Maybe Fitness)
  , _nodeDetailsData_updated :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show, Typeable, Generic)
instance HasId NodeDetailsData where
  type IdData NodeDetailsData = Id Node

mkNodeDetails :: NodeDetailsData
mkNodeDetails = NodeDetailsData
  { _nodeDetailsData_identity = Nothing -- TODO
  , _nodeDetailsData_headLevel = Nothing
  , _nodeDetailsData_headBlockHash = Nothing
  , _nodeDetailsData_headBlockPred = Nothing
  , _nodeDetailsData_headBlockBakedAt = Nothing
  , _nodeDetailsData_peerCount = Nothing
  , _nodeDetailsData_networkStat = NetworkStat 0 0 0 0
  , _nodeDetailsData_fitness = Nothing
  , _nodeDetailsData_updated = Nothing
  }

getNodeHeadBlock :: NodeDetailsData -> Maybe VeryBlockLike
getNodeHeadBlock n = VeryBlockLike
  <$> _nodeDetailsData_headBlockHash n
  <*> _nodeDetailsData_headBlockPred n
  <*> _nodeDetailsData_fitness n
  <*> _nodeDetailsData_headLevel n
  <*> _nodeDetailsData_headBlockBakedAt n

--------------------------------------------------------------------------------

parseChainOrError :: Text -> Either NamedChain ChainId
parseChainOrError x = case runExcept (parseChain x) :: Either Text (Either NamedChain ChainId) of
  Left e -> error $ T.unpack $ "Invalid chain '" <> x <> "': " <> e
  Right v -> v

newtype NamedChainOrChainId = NamedChainOrChainId { getNamedChainOrChainId :: Either NamedChain ChainId }
  deriving (Eq, Ord, Show, Generic, Typeable, Aeson.ToJSON, Aeson.FromJSON)

instance Aeson.FromJSONKey NamedChainOrChainId where
  fromJSONKey = Aeson.FromJSONKeyTextParser $ either (fail . T.unpack) (pure . NamedChainOrChainId) . parseChain

instance Aeson.ToJSONKey NamedChainOrChainId where
  toJSONKey = Aeson.ToJSONKeyText f (AesonE.text . f)
    where f = showChain . getNamedChainOrChainId

data PublicNodeConfig = PublicNodeConfig
  { _publicNodeConfig_source :: !PublicNode
  , _publicNodeConfig_enabled :: !Bool
  , _publicNodeConfig_updated :: !UTCTime
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId PublicNodeConfig

data PublicNodeHead = PublicNodeHead
  { _publicNodeHead_source :: !PublicNode
  , _publicNodeHead_chain :: !NamedChainOrChainId
  , _publicNodeHead_headBlock :: !VeryBlockLike
  , _publicNodeHead_updated :: !UTCTime
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId PublicNodeHead

data Parameters = Parameters
  { _parameters_chain :: !ChainId
  , _parameters_protoInfo :: !ProtoInfo
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
  , _bakedEvent_baker :: !PublicKeyHash
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

instance Ord ErrorEvent where
  compare = compare `on` _errorEvent_message

data EndorseEvent = EndorseEvent
  { _endorseEvent_hash :: BlockHash
  , _endorseEvent_level :: Int
  , _endorseEvent_slot :: Int -- todo, pluralize
  , _endorseEvent_baker :: PublicKeyHash
  , _endorseEvent_name :: String
  , _endorseEvent_oph :: OperationHash
  } deriving (Show, Eq, Typeable, Generic)

data Report = Report
  { _report_baked :: [Event BakedEvent]
  -- , _report_endorsed :: [Event EndorseEvent]
  , _report_errors :: [Event ErrorEvent]
  , _report_seen :: [Event SeenEvent]
  , _report_startTime :: UTCTime
  } deriving (Show, Eq, Ord, Typeable, Generic)

data Accusation = Accusation
  { _accusation_hash :: !OperationHash -- ^ hash of the accusation operation
  , _accusation_blockHash :: !BlockHash -- ^ hash of the block where the accusation was included
  , _accusation_chain :: !ChainId -- ^ chainId of the network where the accusation occurred
  , _accusation_level :: !RawLevel -- ^ level where accusation was incorporated in the blockchain
  , _accusation_baker :: !PublicKeyHash -- ^ PKH of baker who was accused
  , _accusation_occurredLevel :: !RawLevel -- ^ level at which the baker double baked or double endorsed
  , _accusation_isBake :: !Bool -- ^ is this a double bake?  (as opposed to double endorsement...)
  } deriving (Show, Eq, Ord, Typeable, Generic)
instance HasId Accusation where
  type IdData Accusation = (OperationHash, BlockHash)

data Amendment = Amendment
  { _amendment_period :: !VotingPeriodKind
  , _amendment_chainId :: !ChainId
  , _amendment_votingPeriod :: !RawLevel
  , _amendment_start :: !UTCTime -- ^ Start time
  , _amendment_startLevel :: !RawLevel -- ^ Start level
  , _amendment_position :: !RawLevel -- ^ Blocks passed in this period
  } deriving (Eq, Ord, Generic, Typeable, Show)

data PeriodProposal = PeriodProposal
  { _periodProposal_hash :: !ProtocolHash
  , _periodProposal_chainId :: !ChainId
  , _periodProposal_votingPeriod :: !RawLevel
  , _periodProposal_votes :: !Int
  } deriving (Eq, Ord, Generic, Typeable, Show)

data PeriodVote = PeriodVote
  { _periodVote_proposal :: !ProtocolHash
  , _periodVote_chainId :: !ChainId
  , _periodVote_votingPeriod :: !RawLevel
  , _periodVote_ballots :: !Ballots
  , _periodVote_quorum :: !Int -- Percent * 100, e.g. 80.02% would be 8002
  , _periodVote_totalRolls :: !Int -- Total number of rolls of delegates who are eligible to vote
  } deriving (Eq, Ord, Generic, Typeable, Show)

data PeriodTestingVote = PeriodTestingVote
  { _periodTestingVote_periodVote :: PeriodVote
  } deriving (Eq, Ord, Generic, Typeable, Show)

-- | Like Tezos.TestChainStatus, but for a single column
data TestChainStatus = TestChainStatus_NotRunning | TestChainStatus_Forking | TestChainStatus_Running
  deriving (Eq, Ord, Generic, Typeable, Read, Show, Enum, Bounded)
instance Aeson.ToJSON TestChainStatus
instance Aeson.FromJSON TestChainStatus

data PeriodTesting = PeriodTesting
  { _periodTesting_proposal :: !ProtocolHash
  , _periodTesting_chainId :: !ChainId
  , _periodTesting_testChainId :: !(Maybe ChainId)
  , _periodTesting_votingPeriod :: !RawLevel
  , _periodTesting_startingLevel :: !(Maybe RawLevel)
  , _periodTesting_status :: !TestChainStatus
  } deriving (Eq, Ord, Generic, Typeable, Show)

data PeriodPromotionVote = PeriodPromotionVote
  { _periodPromotionVote_periodVote :: PeriodVote
  } deriving (Eq, Ord, Generic, Typeable, Show)

data BlockTodo = BlockTodo
  { _blockTodo_hash :: !BlockHash
  , _blockTodo_level :: !Int
  , _blockTodo_chain :: !ChainId
  , _blockTodo_claimedBy :: !(Maybe Int) -- TODO WIP do backends have IDs?  they probably should if they're going to claim jobs...
  , _blockTodo_claimedAt :: !(Maybe UTCTime)
  , _blockTodo_parsedParent :: !Bool
  , _blockTodo_parsedAccusations :: !Bool
  } deriving (Show, Eq, Ord, Typeable, Generic)

blockLevel :: Event BakedEvent -> Int
blockLevel = fromIntegral . _blockHeader_level . _bakedEvent_signedHeader . _event_detail

blockRewards :: Event BakedEvent -> ProtoInfo -> Tez
blockRewards b p = _protoInfo_blockReward p + fees + nonceTip
  where
    blockHeader = _bakedEvent_signedHeader $ _event_detail b
    nonceTip = maybe 0 (const $ _protoInfo_seedNonceRevelationTip p) (_blockHeader_seedNonceHash blockHeader)
    baker = _bakedEvent_baker $ _event_detail b
    fees = getSum $ (foldMap.foldMap) (Sum . sumFees baker . _bakedEventOperation_data) $ _bakedEvent_operations $ _event_detail b

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
  deriving (Ord, Enum, Show, Eq, Typeable, Generic)

data ClientConfig = ClientConfig
  { _clientConfig_startTime :: UTCTime
  , _clientConfig_bakers :: [PublicKeyHash] -- Ident
  , _clientConfig_workers :: [ClientDaemonWorker]
  , _clientConfig_nodeUri :: !URI
  } deriving (Show, Eq, Ord, Typeable, Generic)

-- newtype Baker_ = Baker (WithId PublicKeyHash (Deletable Baker'))

data Baker = Baker
  { _baker_publicKeyHash :: !PublicKeyHash
  , _baker_data :: !(DeletableRow BakerData)
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId Baker where
  -- Should be the same as `IdData BakerData` always.
  type IdData Baker = PublicKeyHash

data BakerData = BakerData
  { _bakerData_alias :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId BakerData where
  type IdData BakerData = PublicKeyHash

-- delegatedContracts isn't interesting to kiln at this time.  Even if it were,
-- we'd probably want to cache it seperately  (it changes way slower anyhow)
data CacheDelegateInfo = CacheDelegateInfo
  { _cacheDelegateInfo_balance :: !Tez
  , _cacheDelegateInfo_frozenBalance :: !Tez
  , _cacheDelegateInfo_frozenBalanceByCycle :: !(Seq FrozenBalanceByCycle)
  , _cacheDelegateInfo_stakingBalance :: !Tez
  -- , _cacheDelegateInfo_delegatedContracts :: !(Seq.Seq ContractId)
  , _cacheDelegateInfo_delegatedBalance :: !Tez
  , _cacheDelegateInfo_deactivated :: !Bool
  , _cacheDelegateInfo_gracePeriod :: !Cycle
  } deriving (Eq, Ord, Show, Generic, Typeable)

-- newtype BakerDetails = BakerDetails (WithId PublicKeyHash BakerDetails')

data BakerDetails = BakerDetails
  { _bakerDetails_publicKeyHash :: !PublicKeyHash
  -- Used to say what block we examined for delegate info, and also for missed baking and endorsing. It would be the same thing per worker
  , _bakerDetails_branch :: !VeryBlockLike
  , _bakerDetails_delegateInfo :: !(Maybe (Json CacheDelegateInfo))
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId BakerDetails where
  type IdData BakerDetails = PublicKeyHash

data BakerRightsCycleProgress = BakerRightsCycleProgress
  { _bakerRightsCycleProgress_chainId :: !ChainId
  , _bakerRightsCycleProgress_branch :: !BlockHash -- The hash of the first block in the cycle that confers rights.
  -- | we reuse this table to also give us clues about which cycles we've ever
  -- tried to cache, so we can start caching before any delegates have been
  -- configured.
  , _bakerRightsCycleProgress_publicKeyHash :: !PublicKeyHash
  , _bakerRightsCycleProgress_cycle :: !Cycle -- the cycle in which rights are determined: if this is 6, the associated rights are in cycle 12
  , _bakerRightsCycleProgress_progress :: !RawLevel
    -- ranging over the first level in this cycle to the last
    -- this indicates that the amount already computed is from
    -- the first level in the cycle to 'progress', inclusive
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId BakerRightsCycleProgress

data RightKind = RightKind_Baking | RightKind_Endorsing
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance Aeson.FromJSONKey RightKind
instance Aeson.ToJSONKey RightKind

-- It's an explicit choice not to include either the priority; this reduces the
-- amount of reduntant data since we only really care about expected returns
-- rather than all possible.  For the same reason we /do/ include endorsement
-- slots, since that affects expected returns.
data BakerRight = BakerRight
  { _bakerRight_branch :: !(Id BakerRightsCycleProgress)
  , _bakerRight_level :: !RawLevel
  , _bakerRight_right :: !RightKind
  , _bakerRight_slots :: !(Maybe Int)
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId BakerRight


data BakeEfficiency = BakeEfficiency
  { _bakeEfficiency_bakedBlocks :: !Word64
  , _bakeEfficiency_bakingRights :: !Word64
  -- TODO:
  -- { _bakeEfficiency_bakerSucecss :: Sum Int
  -- , _bakeEfficiency_bakerTotal :: Sum Int
  -- , _bakeEfficiency_endorseOperationSuccess
  -- , _bakeEfficiency_endorseOperationTotal
  -- , _bakeEfficiency_endorseSlotsSuccess
  -- , _bakeEfficiency_endorseSlotsTotal
  -- , _bakeEfficiency_bakingRights :: !Word64
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

data Notificatee = Notificatee
  { _notificatee_email :: Email
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId Notificatee
instance Aeson.ToJSON Notificatee

data AlertNotificationMethod
  = AlertNotificationMethod_Email
  | AlertNotificationMethod_Telegram
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance Aeson.ToJSONKey AlertNotificationMethod where
  toJSONKey = Aeson.ToJSONKeyText tshow (AesonE.text . tshow)

-- show match show!
instance Aeson.FromJSONKey AlertNotificationMethod where
  fromJSONKey = Aeson.FromJSONKeyTextParser $ \case
    "AlertNotificationMethod_Email" -> pure AlertNotificationMethod_Email
    "AlertNotificationMethod_Telegram" -> pure AlertNotificationMethod_Telegram
    _ -> fail "unknown alert notification method"

data SmtpProtocol
  = SmtpProtocol_Plain
  | SmtpProtocol_Ssl
  | SmtpProtocol_Starttls
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance Universe SmtpProtocol where universe = universeDef
instance Finite SmtpProtocol

data MailServerConfig = MailServerConfig
  { _mailServerConfig_hostName :: Text
  , _mailServerConfig_portNumber :: Word16
  , _mailServerConfig_smtpProtocol :: SmtpProtocol
  , _mailServerConfig_userName :: Text
  , _mailServerConfig_password :: Text
  -- TODO this `madeDefaultAt` seems to be for old design
  , _mailServerConfig_madeDefaultAt :: UTCTime
  , _mailServerConfig_enabled :: !Bool
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId MailServerConfig

data ErrorLogNetworkUpdate = ErrorLogNetworkUpdate
  { _errorLogNetworkUpdate_log :: !(Id ErrorLog)
  , _errorLogNetworkUpdate_namedChain :: !NamedChain
  , _errorLogNetworkUpdate_commit :: !Text
  , _errorLogNetworkUpdate_gitLabProjectId :: !Text
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogNetworkUpdate where
  type IdData ErrorLogNetworkUpdate = Id ErrorLog

data ErrorLogInaccessibleNode = ErrorLogInaccessibleNode
  { _errorLogInaccessibleNode_log :: !(Id ErrorLog)
  , _errorLogInaccessibleNode_node :: !(Id Node)
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogInaccessibleNode where
  type IdData ErrorLogInaccessibleNode = Id ErrorLog

data ErrorLogNodeWrongChain = ErrorLogNodeWrongChain
  { _errorLogNodeWrongChain_log :: !(Id ErrorLog)
  , _errorLogNodeWrongChain_node :: !(Id Node)
  , _errorLogNodeWrongChain_expectedChainId :: !ChainId
  , _errorLogNodeWrongChain_actualChainId :: !ChainId
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogNodeWrongChain where
  type IdData ErrorLogNodeWrongChain = Id ErrorLog

data ErrorLogNodeInvalidPeerCount = ErrorLogNodeInvalidPeerCount
  { _errorLogNodeInvalidPeerCount_log :: !(Id ErrorLog)
  , _errorLogNodeInvalidPeerCount_node :: !(Id Node)
  , _errorLogNodeInvalidPeerCount_minPeerCount :: !Int
  , _errorLogNodeInvalidPeerCount_actualPeerCount :: !Word64
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogNodeInvalidPeerCount where
  type IdData ErrorLogNodeInvalidPeerCount = Id ErrorLog

-- | Bakers in the daemon sense, not delegate sense
data ErrorLogBakerNoHeartbeat = ErrorLogBakerNoHeartbeat
  { _errorLogBakerNoHeartbeat_log :: !(Id ErrorLog)
  , _errorLogBakerNoHeartbeat_lastLevel :: !RawLevel
  , _errorLogBakerNoHeartbeat_lastBlockHash :: !BlockHash
  , _errorLogBakerNoHeartbeat_client :: !(Id BakerDaemon)
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogBakerNoHeartbeat where
  type IdData ErrorLogBakerNoHeartbeat = Id ErrorLog

data ClientWorker = ClientWorker_Baking | ClientWorker_Endorsing
  deriving (Eq, Ord, Bounded, Enum, Generic, Typeable, Read, Show)

data ErrorLogMultipleBakersForSameBaker = ErrorLogMultipleBakersForSameBaker
  { _errorLogMultipleBakersForSameBaker_log :: !(Id ErrorLog)
  , _errorLogMultipleBakersForSameBaker_publicKeyHash :: !PublicKeyHash
  , _errorLogMultipleBakersForSameBaker_client :: !(Id BakerDaemon)
  , _errorLogMultipleBakersForSameBaker_worker :: !ClientWorker
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogMultipleBakersForSameBaker where
  type IdData ErrorLogMultipleBakersForSameBaker = Id ErrorLog

data ErrorLogBakerDeactivated = ErrorLogBakerDeactivated
  { _errorLogBakerDeactivated_log :: !(Id ErrorLog)
  , _errorLogBakerDeactivated_publicKeyHash :: !PublicKeyHash
  , _errorLogBakerDeactivated_preservedCycles :: !Cycle
  , _errorLogBakerDeactivated_fitness :: !Fitness
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogBakerDeactivated where
  type IdData ErrorLogBakerDeactivated = Id ErrorLog

data ErrorLogBakerDeactivationRisk = ErrorLogBakerDeactivationRisk
  { _errorLogBakerDeactivationRisk_log :: !(Id ErrorLog)
  , _errorLogBakerDeactivationRisk_publicKeyHash :: !PublicKeyHash
  , _errorLogBakerDeactivationRisk_gracePeriod :: !Cycle
  , _errorLogBakerDeactivationRisk_latestCycle :: !Cycle
  , _errorLogBakerDeactivationRisk_preservedCycles :: !Cycle
  , _errorLogBakerDeactivationRisk_fitness :: !Fitness
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogBakerDeactivationRisk where
  type IdData ErrorLogBakerDeactivationRisk = Id ErrorLog

data ErrorLogBakerAccused = ErrorLogBakerAccused
  { _errorLogBakerAccused_log :: !(Id ErrorLog)
  , _errorLogBakerAccused_op :: !(Id Accusation)
  , _errorLogBakerAccused_baker :: !(Id Baker)
  , _errorLogBakerAccused_cycle :: !Cycle
  , _errorLogBakerAccused_level :: !RawLevel
  , _errorLogBakerAccused_accusedCycle :: !Cycle
  , _errorLogBakerAccused_accusedLevel :: !RawLevel
  , _errorLogBakerAccused_right :: !RightKind
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogBakerAccused where
  type IdData ErrorLogBakerAccused = Id ErrorLog

data ErrorLogBadNodeHead = ErrorLogBadNodeHead
  { _errorLogBadNodeHead_log :: !(Id ErrorLog)
  , _errorLogBadNodeHead_node :: !(Id Node)
  , _errorLogBadNodeHead_lca :: !(Maybe (Json VeryBlockLike))
  , _errorLogBadNodeHead_nodeHead :: !(Json VeryBlockLike)
  , _errorLogBadNodeHead_latestHead :: !(Json VeryBlockLike)
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogBadNodeHead where
  type IdData ErrorLogBadNodeHead = Id ErrorLog

-- we wilfully ignore the branch issue; we mostly don't care on which branch you
-- did or didn't take your rights.
--
-- in particular, there's two ways to "resolve" this type of alert, either a
-- new uncle occurs in which the baker /did/ exercise their rights, or the user
-- manually acknowledges the error.  If the network is branch hopping; its
-- possible for a user to acknowledge a miss, then for the same level missed to
-- be re-reported;  we explicitly ignore that possibility.
data ErrorLogBakerMissed = ErrorLogBakerMissed
  { _errorLogBakerMissed_log :: !(Id ErrorLog)
  , _errorLogBakerMissed_baker :: !(Id Baker)
  , _errorLogBakerMissed_right :: !RightKind
  , _errorLogBakerMissed_level :: !RawLevel
  , _errorLogBakerMissed_fitness :: !Fitness
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogBakerMissed where
  type IdData ErrorLogBakerMissed = Id ErrorLog

data ErrorLogInsufficientFunds = ErrorLogInsufficientFunds
  { _errorLogInsufficientFunds_log :: !(Id ErrorLog)
  , _errorLogInsufficientFunds_baker :: !(Id Baker)
  , _errorLogInsufficientFunds_detected :: !UTCTime
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogInsufficientFunds where
  type IdData ErrorLogInsufficientFunds = Id ErrorLog

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

data UpgradeCheckError
  = UpgradeCheckError_UpstreamUnreachable
  | UpgradeCheckError_UpstreamMissing
  | UpgradeCheckError_UpstreamUnparseable
  deriving (Eq, Ord, Generic, Typeable, Enum, Bounded, Read, Show)

data UpstreamVersion = UpstreamVersion
  { _upstreamVersion_error :: !(Maybe UpgradeCheckError)
  , _upstreamVersion_version :: !(Maybe Version)
  , _upstreamVersion_updated :: !UTCTime
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId UpstreamVersion

-- | Stores settings for limiting notifications. Missing records for a
-- 'RightKind' indicate not to do any filtering
data RightNotificationSettings = RightNotificationSettings
  { _rightNotificationSettings_rightKind :: !RightKind
  , _rightNotificationSettings_limit :: !RightNotificationLimit
  } deriving (Eq, Ord, Show, Typeable, Generic)

data RightNotificationLimit = RightNotificationLimit
  { _rightNotificationLimit_amount :: !Int -- ^ How many rights have to be missed before notifying
  , _rightNotificationLimit_withinMinutes :: !Int -- ^ Time window (minutes) for counting missed rights
  } deriving (Eq, Ord, Show, Typeable, Generic)

data TelegramConfig = TelegramConfig
  { _telegramConfig_botName :: !(Maybe Text)
  , _telegramConfig_botApiKey :: !Text
  , _telegramConfig_created :: !UTCTime
  , _telegramConfig_updated :: !UTCTime
  , _telegramConfig_enabled :: !Bool
  , _telegramConfig_validated :: !(Maybe Bool)
  } deriving (Eq, Generic, Ord, Show, Typeable)
instance HasId TelegramConfig

data TelegramRecipient = TelegramRecipient
  { _telegramRecipient_config :: !(Id TelegramConfig)
  , _telegramRecipient_userId :: !Int64
  , _telegramRecipient_chatId :: !Int64
  , _telegramRecipient_firstName :: !Text
  , _telegramRecipient_lastName :: !(Maybe Text)
  , _telegramRecipient_username :: !(Maybe Text)
  , _telegramRecipient_created :: !UTCTime
  , _telegramRecipient_deleted :: !Bool
  } deriving (Eq, Generic, Ord, Show, Typeable)
instance HasId TelegramRecipient

data TelegramMessageQueue = TelegramMessageQueue
  { _telegramMessageQueue_recipient :: !(Id TelegramRecipient)
  , _telegramMessageQueue_message :: !Text
  , _telegramMessageQueue_created :: !UTCTime
  } deriving (Eq, Generic, Ord, Show, Typeable)
instance HasId TelegramMessageQueue

-- Re-ordering these can yield errors
-- https://ghc.haskell.org/trac/ghc/ticket/8740 (fixed in GHC 8.6)
data LogTag a where
  LogTag_NetworkUpdate :: LogTag ErrorLogNetworkUpdate
  LogTag_Node :: NodeLogTag a -> LogTag a
  LogTag_Baker :: BakerLogTag a -> LogTag a
  LogTag_BakerNoHeartbeat :: LogTag ErrorLogBakerNoHeartbeat
  --  | Misc baker /daemon/ error.

deriving instance Eq (LogTag a)
deriving instance Ord (LogTag a)
deriving instance Show (LogTag a)

data NodeLogTag a where
  NodeLogTag_InaccessibleNode :: NodeLogTag ErrorLogInaccessibleNode
  NodeLogTag_NodeWrongChain :: NodeLogTag ErrorLogNodeWrongChain
  NodeLogTag_NodeInvalidPeerCount :: NodeLogTag ErrorLogNodeInvalidPeerCount
  NodeLogTag_BadNodeHead :: NodeLogTag ErrorLogBadNodeHead

deriving instance Eq (NodeLogTag a)
deriving instance Ord (NodeLogTag a)
deriving instance Show (NodeLogTag a)

-- TODO: we now have a slightly confusing bit of vocabulary.  we have the on
-- chain entity: Delegates, and the background process tezos-baker both
-- referred to by the name "Baker".  that's confusing; especially when some
-- things refer to both;  "MultipleBakersForSameBaker" refer to two instances
-- of a background process and a delegate. we should really rename one or both
-- to minimize confusion between these two ideas.
data BakerLogTag a where
  BakerLogTag_MultipleBakersForSameBaker :: BakerLogTag ErrorLogMultipleBakersForSameBaker
  BakerLogTag_BakerMissed :: BakerLogTag ErrorLogBakerMissed
  BakerLogTag_BakerDeactivated :: BakerLogTag ErrorLogBakerDeactivated
  BakerLogTag_BakerDeactivationRisk :: BakerLogTag ErrorLogBakerDeactivationRisk
  BakerLogTag_BakerAccused :: BakerLogTag ErrorLogBakerAccused
  BakerLogTag_InsufficientFunds :: BakerLogTag ErrorLogInsufficientFunds

deriving instance Eq (BakerLogTag a)
deriving instance Ord (BakerLogTag a)
deriving instance Show (BakerLogTag a)

fmap concat $ sequence (map (deriveJSON defaultTezosCompatJsonOptions)
  [ ''Accusation
  , ''Amendment
  , ''AlertNotificationMethod
  , ''BakeEfficiency
  , ''BakedEvent
  , ''BakedEventOperation
  , ''Baker
  , ''BakerDaemon
  , ''BakerDaemonExternal
  , ''BakerDaemonExternalData
  , ''BakerDaemonInfo
  , ''BakerDaemonInfoData
  , ''BakerDaemonInternalData
  , ''BakerData
  , ''BakerDetails
  , ''BakerRight
  , ''BakerRightsCycleProgress
  , ''BlockBaker
  , ''BlockTodo
  , ''CacheDelegateInfo
  , ''ClientConfig
  , ''ClientDaemonWorker
  , ''ClientWorker
  , ''DeletableRow
  , ''EndorseEvent
  , ''ErrorEvent
  , ''ErrorLog
  , ''ErrorLogBadNodeHead
  , ''ErrorLogBakerAccused
  , ''ErrorLogBakerDeactivated
  , ''ErrorLogBakerDeactivationRisk
  , ''ErrorLogBakerMissed
  , ''ErrorLogBakerNoHeartbeat
  , ''ErrorLogInaccessibleNode
  , ''ErrorLogInsufficientFunds
  , ''ErrorLogMultipleBakersForSameBaker
  , ''ErrorLogNetworkUpdate
  , ''ErrorLogNodeInvalidPeerCount
  , ''ErrorLogNodeWrongChain
  , ''Event
  , ''MailServerConfig
  , ''Node
  , ''NodeDetails
  , ''NodeDetailsData
  , ''NodeExternal
  , ''NodeExternalData
  , ''NodeInternal
  , ''Parameters
  , ''PeriodTestingVote
  , ''PeriodPromotionVote
  , ''PeriodProposal
  , ''PeriodTesting
  , ''PeriodVote
  , ''ProcessControl
  , ''ProcessData
  , ''ProcessState
  , ''PublicNodeConfig
  , ''PublicNodeHead
  , ''Report
  , ''RightKind
  , ''RightNotificationLimit
  , ''RightNotificationSettings
  , ''SeenEvent
  , ''SmtpProtocol
  , ''TelegramConfig
  , ''TelegramMessageQueue
  , ''TelegramRecipient
  , ''UpgradeCheckError
  , ''UpstreamVersion
  ] ++ map makeLenses
  [ 'Accusation
  , 'Amendment
  , 'BakeEfficiency
  , 'BakedEvent
  , 'BakedEventOperation
  , 'Baker
  , 'BakerDaemon
  , 'BakerDaemonExternal
  , 'BakerDaemonExternalData
  , 'BakerDaemonInfo
  , 'BakerDaemonInfoData
  , 'BakerDaemonInternalData
  , 'BakerData
  , 'BakerDetails
  , 'BakerRight
  , 'BakerRightsCycleProgress
  , 'BlockBaker
  , 'BlockTodo
  , 'CachedProtocolConstants
  , 'DeletableRow
  , 'EndorseEvent
  , 'Error
  , 'ErrorEvent
  , 'ErrorLog
  , 'ErrorLogBadNodeHead
  , 'ErrorLogBakerAccused
  , 'ErrorLogBakerDeactivated
  , 'ErrorLogBakerDeactivationRisk
  , 'ErrorLogBakerMissed
  , 'ErrorLogBakerNoHeartbeat
  , 'ErrorLogInaccessibleNode
  , 'ErrorLogInsufficientFunds
  , 'ErrorLogMultipleBakersForSameBaker
  , 'ErrorLogNetworkUpdate
  , 'ErrorLogNodeInvalidPeerCount
  , 'ErrorLogNodeWrongChain
  , 'Event
  , 'MailServerConfig
  , 'Node
  , 'NodeDetails
  , 'NodeDetailsData
  , 'NodeExternal
  , 'NodeExternalData
  , 'NodeInternal
  , 'Parameters
  , 'PeriodTestingVote
  , 'PeriodPromotionVote
  , 'PeriodProposal
  , 'PeriodTesting
  , 'PeriodVote
  , 'ProcessData
  , 'PublicNodeConfig
  , 'PublicNodeHead
  , 'Report
  , 'RightNotificationLimit
  , 'RightNotificationSettings
  , 'SeenEvent
  , 'TelegramConfig
  , 'TelegramMessageQueue
  , 'TelegramRecipient
  , 'UpstreamVersion
  ] ++ map makePrisms
  [ ''UpgradeCheckError
  ])

return []

fmap concat $ for [''NodeLogTag, ''BakerLogTag] $ \t -> concat <$> sequence
  [ deriveJSONGADT t
  , deriveArgDict t
  , deriveGEq t
  , deriveGCompare t
  , deriveGShow t
  , deriveEqTagIdentity t
  , deriveOrdTagIdentity t
  , deriveShowTagIdentity t
  ]

-- Do this is second because it is downstream
fmap concat $ for [''LogTag] $ \t -> concat <$> sequence
  [ deriveJSONGADT t
  , deriveArgDict t
  , deriveGEq t
  , deriveGCompare t
  , deriveGShow t
  , deriveEqTagIdentity t
  , deriveOrdTagIdentity t
  , deriveShowTagIdentity t
  ]

deriveSomeUniverse ''NodeLogTag
deriveSomeUniverse ''BakerLogTag
-- need Cale to fix this
-- deriveSomeUniverse ''LogTag
instance Universe (Some LogTag) where
  universe = [This LogTag_NetworkUpdate] <> fmap (\(This x) -> This (LogTag_Node x)) universe <> fmap (\(This x) -> This (LogTag_Baker x)) universe <> [This LogTag_BakerNoHeartbeat]

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

instance BlockLike PublicNodeHead where
  hash = publicNodeHead_headBlock . hash
  predecessor = publicNodeHead_headBlock . predecessor
  fitness = publicNodeHead_headBlock . fitness
  level = publicNodeHead_headBlock . level
  timestamp = publicNodeHead_headBlock . timestamp


aliasedIdentification :: (a -> Maybe Text) -> (a -> Text) -> a -> (Text, Maybe Text)
aliasedIdentification getMain getFallback x =
  let fallback = getFallback x
  in maybe (fallback, Nothing) (, Just fallback) $ getMain x

bakerIdentification :: Baker -> (Text, Maybe Text)
bakerIdentification = aliasedIdentification
  (view $ baker_data . deletableRow_data . bakerData_alias)
  (toPublicKeyHashText . _baker_publicKeyHash)

errorLogNames :: [Name]
errorLogNames =
  [ ''ErrorLogBadNodeHead
  , ''ErrorLogBakerAccused
  , ''ErrorLogBakerDeactivated
  , ''ErrorLogBakerDeactivationRisk
  , ''ErrorLogBakerMissed
  , ''ErrorLogBakerNoHeartbeat
  , ''ErrorLogInaccessibleNode
  , ''ErrorLogInsufficientFunds
  , ''ErrorLogMultipleBakersForSameBaker
  , ''ErrorLogNetworkUpdate
  , ''ErrorLogNodeInvalidPeerCount
  , ''ErrorLogNodeWrongChain
  ]
