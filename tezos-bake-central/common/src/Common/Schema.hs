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
{-# LANGUAGE PackageImports #-}
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

import Control.Applicative
import Control.Exception.Safe (Exception, SomeException)
import Control.Lens hiding (universe)
import Control.Monad.Except (runExcept)
import qualified Data.Aeson as Aeson
import Data.Aeson.GADT (deriveJSONGADT)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.TH as Aeson
import qualified Data.Aeson.Encoding as AesonE
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Sum.Orphans ()
import Data.GADT.Compare.TH (deriveGCompare)
import Data.GADT.Compare.TH (deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Semigroup, Sum (..), getSum, (<>))
import Data.Sequence (Seq)
import Data.Some (Some(..))
import Data.Text (Text)
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime)
import Data.Typeable (Typeable)
import Data.Universe
import Data.Universe.Helpers (universeDef)
import Data.Universe.Some
import Data.Version (Version)
import Data.Word
import Database.Id.Class
import GHC.Generics (Generic)
import "template-haskell" Language.Haskell.TH (Name)
import Rhyolite.Schema (Email, Json)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.Common.NodeRPC.Types (RpcError, AsRpcError(asRpcError))
import Tezos.Common.Json (tezosJsonOptions)
import Tezos.Types hiding (TestChainStatus)
import Tezos.V010.NodeRPC.CrossCompat (FrozenBalanceByCycleSeqCrossCompat)

import Common (defaultTezosCompatJsonOptions)
import ExtraPrelude

maxProposalUpvotes :: Int
maxProposalUpvotes = 20

data ClientError
  = ClientError_NodeNotReady
  | ClientError_RequestDeclinedByLedger
  | ClientError_LedgerDisconnected
  | ClientError_Timeout
  | ClientError_Other Text
  deriving (Eq, Ord, Show, Generic, Typeable)
instance Aeson.ToJSON ClientError
instance Aeson.FromJSON ClientError

-- | Required Tezos Baking app version
requiredTezosBakingAppVersion :: Text
requiredTezosBakingAppVersion = "2.0.0"

data UnsuitableNodeReason
  = UnsuitableNodeReason_QueryBeforeSavepoint RawLevel RawLevel
  | UnsuitableNodeReason_MissingBlockInfo
  | UnsuitableNodeReason_MissingSavepoint
  | UnsuitableNodeReason_QueryFailed Text -- TODO This should be CacheError but we've got a cycle that doesn't play ball with TH
  | UnsuitableNodeReason_BranchNotContained BlockHash
  deriving (Show, Generic, Typeable)
makePrisms ''UnsuitableNodeReason


data CacheError
  = CacheError_RpcError !RpcError
  | CacheError_NoSuitableNode Text [(URI,UnsuitableNodeReason)]
  | CacheError_NotEnoughHistory
  | CacheError_Timeout !NominalDiffTime
  | CacheError_SomeException !SomeException
  | CacheError_UnrevealedPublicKey !ContractId
  | CacheError_UnknownProtocol !ProtocolHash
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

sumFees :: PublicKeyHash -> Operation -> TezDelta
sumFees baker = getSum . views balanceUpdates getFee
  where
    getFee :: BalanceUpdate -> Sum TezDelta
    getFee (BalanceUpdate_Freezer x) | _freezerUpdate_delegate x == baker = Sum (_freezerUpdate_change x)
    getFee _ = Sum 0


data Error = Error
  { _error_time :: !UTCTime
  , _error_text :: !Text
  } deriving (Eq, Ord, Show, Generic, Typeable)

data BlockBaker = BlockBaker
  { _blockBaker_publicKeyHash :: !PublicKeyHash
  , _blockBaker_priority :: !Priority
  , _blockBaker_endorsements :: !(Map PublicKeyHash (Seq Word8))
  } deriving (Eq, Ord, Show, Typeable, Generic)

getBakerFromBlock :: Block -> BlockBaker
getBakerFromBlock block = BlockBaker
  { _blockBaker_publicKeyHash = block ^. block_metadata . blockMetadata_baker
  , _blockBaker_priority = block ^. block_header . blockHeaderFull_priority
  , _blockBaker_endorsements = block ^. block_operations
    . traverse
    . traverse
    . operation_contents
    . traverse
    . _OperationContents_EndorsementWithSlot
    . operationContentsEndorsementWithSlot_metadata
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

data BakerDaemonInternal = BakerDaemonInternal
  { _bakerDaemonInternal_id :: !(Id BakerDaemon)
  , _bakerDaemonInternal_data :: !(DeletableRow BakerDaemonInternalData)
  } deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId BakerDaemonInternal where
  type IdData BakerDaemonInternal = Id BakerDaemon

data ConnectedLedger = ConnectedLedger
  { _connectedLedger_ledgerIdentifier :: !(Maybe LedgerIdentifier)
  , _connectedLedger_bakingAppVersion :: !(Maybe Text)
  , _connectedLedger_walletAppVersion :: !(Maybe Text)
  , _connectedLedger_forceConnectivityCheck :: !Bool
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
  , _ledgerAccount_shouldRegister :: !Bool
  , _ledgerAccount_shouldSetHWM :: !(Maybe RawLevel) -- ^ Contains the block level if we need to set the HWM
  , _ledgerAccount_shouldDoVoteProtocol :: !(Maybe (Id PeriodProposal)) -- ^ Proposal to vote for
  , _ledgerAccount_shouldDoVoteBallot :: !(Maybe Ballot) -- ^ If present along with the protocol field, vote with given ballot. If missing, upvote the proposal.
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

newtype TezosVersion = TezosVersion { getTezosVersion :: Either Text NodeVersion }
  deriving (Ord, Read, Show, Typeable)

instance Aeson.FromJSON TezosVersion where
  parseJSON v = TezosVersion <$> (Aeson.withText "TezosNodeVersion" (pure . Left) v <|> fmap Right (Aeson.parseJSON v))

instance Aeson.ToJSON TezosVersion where
  toJSON = either Aeson.String Aeson.toJSON . getTezosVersion

instance Eq TezosVersion where
  TezosVersion (Left c) == TezosVersion (Right nv) = c == _commitInfo_commitHash (_nodeVersion_commitInfo nv)
  TezosVersion (Right nv) == TezosVersion (Left c) = c == _commitInfo_commitHash (_nodeVersion_commitInfo nv)
  TezosVersion a == TezosVersion b = a == b

data NodeVersion = NodeVersion
     { _nodeVersion_version :: !MajorMinorVersion
     , _nodeVersion_networkVersion :: !NetworkVersion
     , _nodeVersion_commitInfo :: !CommitInfo
     } deriving (Eq, Ord, Read, Show)

data MajorMinorVersion = MajorMinorVersion
     { _majorMinorVersion_major :: !Int32
     , _majorMinorVersion_minor :: !Int32
     , _majorMinorVersion_extra :: !(Maybe Int32)
     , _majorMinorVersion_additional_info :: !AdditionalInfo
     } deriving (Eq, Ord, Read, Show)

data AdditionalInfo =
  Development
  | ReleaseCandidate !Int
  {-
  The release candidate number type could be TezosWord64, but that
  type doesn't have a Read instance and I don't think the precision
  inherent in the TezosWord64 type is necessary to have here.
  -}
  | Release
  deriving (Eq, Generic, Ord, Read, Show)

instance Aeson.ToJSON AdditionalInfo where
  toJSON = \case
    Development -> Aeson.String "dev"
    ReleaseCandidate rc -> Aeson.object ["rc" Aeson..= rc]
    Release -> Aeson.String "release"

instance Aeson.FromJSON AdditionalInfo where
  parseJSON v =
    Aeson.withText "Development" (\text -> if text == "dev" then pure Development else empty) v
    <|> Aeson.withObject "ReleaseCandidate" (\ob -> ReleaseCandidate <$> ob Aeson..: "rc") v
    <|> Aeson.withText "Release" (\text -> if text == "release" then pure Release else empty) v

data NetworkVersion = NetworkVersion
  { _networkVersion_chainName :: !Text -- This is not quite synonymous with the usual chainName or chainId.
  , _networkVersion_distributedDbVersion :: !Word16
  , _networkVersion_p2pVersion :: !Word16
  } deriving (Eq, Generic, Ord, Read, Show)

{- These aeson instances and lenses were written because the auto  -}
{- derivation mechanism wasn't behaving properly and it is just easier-}
{- to write what is needed for this simple type.}-}

instance Aeson.ToJSON NetworkVersion where
  toJSON nv = Aeson.object
    [ "chain_name" Aeson..= _networkVersion_chainName nv
    , "distributed_db_version" Aeson..= _networkVersion_distributedDbVersion nv
    , "p2p_version" Aeson..= _networkVersion_p2pVersion nv
    ]

instance Aeson.FromJSON NetworkVersion where
  parseJSON = Aeson.withObject "NetworkVersion" $ \o -> NetworkVersion
    <$> o Aeson..: "chain_name"
    <*> o Aeson..: "distributed_db_version"
    <*> o Aeson..: "p2p_version"

networkVersion_chainName :: Functor f => (Text -> f Text) -> NetworkVersion -> f NetworkVersion
networkVersion_chainName f s = (\u -> s {_networkVersion_chainName = u}) <$> f (_networkVersion_chainName s)

networkVersion_distributedDbVersion :: Functor f => (Word16 -> f Word16) -> NetworkVersion -> f NetworkVersion
networkVersion_distributedDbVersion f s = (\u -> s {_networkVersion_distributedDbVersion = u}) <$> f (_networkVersion_distributedDbVersion s)

networkVersion_p2pVersion :: Functor f => (Word16 -> f Word16) -> NetworkVersion -> f NetworkVersion
networkVersion_p2pVersion f s = (\u -> s {_networkVersion_p2pVersion = u}) <$> f (_networkVersion_p2pVersion s)

data CommitInfo = CommitInfo
     { _commitInfo_commitDate :: !Text
     , _commitInfo_commitHash :: !Text
     } deriving (Eq, Generic, Ord, Read, Show)

data NodeExternalData = NodeExternalData
  { _nodeExternalData_address :: !URI
  , _nodeExternalData_alias :: !(Maybe Text)
  , _nodeExternalData_minPeerConnections :: !(Maybe Int)
  } deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId NodeExternalData where
  type IdData NodeExternalData = Id Node

data NodeInternal = NodeInternal
  { _nodeInternal_id :: !(Id Node)
  , _nodeInternal_data :: !(DeletableRow (Id ProcessData))
  } deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId NodeInternal where
  type IdData NodeInternal = Id Node

-- | WARNING: Never remove or modify these cases since they are stored in the DB directly. Adding is ok.
data NodeProcessState
  = NodeProcessState_ImportingSnapshot
  | NodeProcessState_ImportCanceled
  | NodeProcessState_ImportComplete
  | NodeProcessState_ImportFailed
  | NodeProcessState_ImportTimeout
  | NodeProcessState_GeneratingIdentity
  | NodeProcessState_DownloadingSnapshot
  | NodeProcessState_DownloadCanceled
  | NodeProcessState_DownloadComplete
  | NodeProcessState_DownloadFailed
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

-- | WARNING: Never remove or modify these cases since they are stored in the DB directly. Adding is ok.
data ProcessState
   = ProcessState_Stopped
   | ProcessState_Initializing
   | ProcessState_Node NodeProcessState -- only applicable to Node
   | ProcessState_Starting
   | ProcessState_Running
   | ProcessState_Failed
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

isProcessStateNode :: ProcessState -> Bool
isProcessStateNode = \case
  ProcessState_Node _ -> True
  _ -> False

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

data NodeDetails = NodeDetails
  { _nodeDetails_id :: !(Id Node)
  , _nodeDetails_data :: !NodeDetailsData
  } deriving (Eq, Ord, Show, Generic, Typeable)

-- TODO don't need Maybes here probably.
data NodeDetailsData = NodeDetailsData
  { _nodeDetailsData_identity :: !(Maybe CryptoboxPublicKeyHash)
  , _nodeDetailsData_headLevel :: !(Maybe RawLevel)
  , _nodeDetailsData_headBlockHash :: !(Maybe BlockHash)
  , _nodeDetailsData_headBlockPred :: !(Maybe BlockHash)
  , _nodeDetailsData_headBlockBakedAt :: !(Maybe UTCTime)
  , _nodeDetailsData_savePoint :: !(Maybe RawLevel)
  , _nodeDetailsData_savePointUpdated :: !(Maybe Cycle)
  , _nodeDetailsData_peerCount :: !(Maybe Word64)
  , _nodeDetailsData_networkStat :: !NetworkStat
  , _nodeDetailsData_fitness :: !(Maybe Fitness)
  , _nodeDetailsData_updated :: !(Maybe UTCTime)
  , _nodeDetailsData_synchronisationThreshold :: !Word8
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
  , _nodeDetailsData_savePoint = Nothing
  , _nodeDetailsData_savePointUpdated = Nothing
  , _nodeDetailsData_peerCount = Nothing
  , _nodeDetailsData_networkStat = NetworkStat 0 0 0 0
  , _nodeDetailsData_fitness = Nothing
  , _nodeDetailsData_updated = Nothing
  , _nodeDetailsData_synchronisationThreshold = 4
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

-- TODO:
--   each protocol should have it's own ProtoInfo type
--   remove any funny json (de-)serialization from ProtocolIndex *and* TBML
--   factor out these funny json manipulations into a optional way of funneling a protocol-specific ProtoInfo type into another ProtoInfo type
--   remove _protocolIndex_constants
--   move _protocolIndex_jsonConstants to a postgresql 'jsonb' type
--
-- We can cache ProtoInfo types in memory,  but given the changes to ProtoInfo across protocol versions, it's not really a structured data type we can cleanly unpack into a common record or a fully "structured" SQL schema.
data ProtocolIndex = ProtocolIndex
  { _protocolIndex_chainId :: !ChainId
  , _protocolIndex_hash :: !ProtocolHash
  , _protocolIndex_jsonConstants :: !(Json Aeson.Value)
  , _protocolIndex_constants :: !ProtoInfo
  , _protocolIndex_proto :: !Word8
  , _protocolIndex_firstBlockHash :: !(Maybe BlockHash)
  , _protocolIndex_firstBlockPredecessor :: !(Maybe BlockHash)
  , _protocolIndex_firstBlockLevel :: !(Maybe RawLevel)
  , _protocolIndex_firstBlockFitness :: !(Maybe Fitness)
  , _protocolIndex_firstBlockTimestamp :: !(Maybe UTCTime)
  , _protocolIndex_firstBlockCycle :: !(Maybe Cycle)
  } deriving (Eq, Show, Generic, Typeable)
instance HasId ProtocolIndex where
  type IdData ProtocolIndex = (ChainId, ProtocolHash)

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
instance HasId PeriodProposal

data PeriodVote = PeriodVote
  { _periodVote_ballots :: !Ballots
  , _periodVote_quorum :: !Int -- Percent * 100, e.g. 80.02% would be 8002
  , _periodVote_totalRolls :: !Int -- Total number of rolls of delegates who are eligible to vote
  } deriving (Eq, Ord, Generic, Typeable, Show)

data PeriodTestingVote = PeriodTestingVote
  { _periodTestingVote_proposal :: !(Id PeriodProposal)
  , _periodTestingVote_periodVote :: !PeriodVote
  } deriving (Eq, Ord, Generic, Typeable, Show)

-- | Like Tezos.TestChainStatus, but for a single column
data TestChainStatus = TestChainStatus_NotRunning | TestChainStatus_Forking | TestChainStatus_Running
  deriving (Eq, Ord, Generic, Typeable, Read, Show, Enum, Bounded)
instance Aeson.ToJSON TestChainStatus
instance Aeson.FromJSON TestChainStatus

data PeriodTesting = PeriodTesting
  { _periodTesting_proposal :: !(Id PeriodProposal)
  , _periodTesting_testChainId :: !(Maybe ChainId)
  , _periodTesting_startingLevel :: !(Maybe RawLevel)
  , _periodTesting_status :: !TestChainStatus
  } deriving (Eq, Ord, Generic, Typeable, Show)

data PeriodPromotionVote = PeriodPromotionVote
  { _periodPromotionVote_proposal :: !(Id PeriodProposal)
  , _periodPromotionVote_periodVote :: !PeriodVote
  } deriving (Eq, Ord, Generic, Typeable, Show)

-- There is no actual voting in this period.
data PeriodAdoption = PeriodAdoption
  { _periodAdoption_proposal :: !(Id PeriodProposal)
  , _periodAdoption_periodVote :: !PeriodVote
  }
   deriving (Eq, Ord, Generic, Typeable, Show)

-- Proposal period
data BakerProposal = BakerProposal
  { _bakerProposal_pkh :: !PublicKeyHash
  , _bakerProposal_proposal :: !(Id PeriodProposal)
  , _bakerProposal_included :: !(Maybe BlockHash)
  , _bakerProposal_attempted :: !(Maybe BlockHash)
  } deriving (Eq, Ord, Generic, Typeable, Show)

-- Exploration/promotion period
data BakerVote = BakerVote
  { _bakerVote_pkh :: !PublicKeyHash
  , _bakerVote_proposal :: !(Id PeriodProposal)
  , _bakerVote_ballot :: !Ballot
  , _bakerVote_included :: !(Maybe BlockHash)
  , _bakerVote_attempted :: !(Maybe BlockHash)
  } deriving (Eq, Ord, Generic, Typeable, Show)

data AccusationBlock = AccusationBlock
  { _accusationBlock_hash :: !BlockHash
  , _accusationBlock_level :: !RawLevel
  , _accusationBlock_chain :: !ChainId
  } deriving (Show, Eq, Ord, Typeable, Generic)

data ClientDaemonWorker
  = ClientDaemonWorker_Baking
  | ClientDaemonWorker_Denunciation
  | ClientDaemonWorker_Endorsement
  deriving (Ord, Enum, Show, Eq, Typeable, Generic)

data ClientConfig = ClientConfig
  { _clientConfig_startTime :: !UTCTime
  , _clientConfig_bakers :: ![PublicKeyHash] -- Ident
  , _clientConfig_workers :: ![ClientDaemonWorker]
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
  , _cacheDelegateInfo_frozenBalanceByCycle :: !FrozenBalanceByCycleSeqCrossCompat
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

data BakerRightsProgress = BakerRightsProgress
  { _bakerRightsProgress_chainId :: !ChainId
  -- | we reuse this table to also give us clues about which cycles we've ever
  -- tried to cache, so we can start caching before any delegates have been
  -- configured.
  , _bakerRightsProgress_publicKeyHash :: !PublicKeyHash
  -- | The last level for which rights were successfully gathered and stored.
  , _bakerRightsProgress_progress :: !RawLevel
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId BakerRightsProgress

data RightKind = RightKind_Baking | RightKind_Endorsing
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance Aeson.FromJSONKey RightKind
instance Aeson.ToJSONKey RightKind

-- It's an explicit choice not to include either the priority; this reduces the
-- amount of reduntant data since we only really care about expected returns
-- rather than all possible.  For the same reason we /do/ include endorsement
-- slots, since that affects expected returns.
data BakerRight = BakerRight
  { _bakerRight_branch :: !(Id BakerRightsProgress)
  , _bakerRight_level :: !RawLevel
  , _bakerRight_right :: !RightKind
  , _bakerRight_slots :: !(Maybe Int)
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId BakerRight


data BakeEfficiency = BakeEfficiency
  { _bakeEfficiency_bakedBlocks :: !Word64
  , _bakeEfficiency_bakingRights :: !Word64
  -- TODO:
  -- { _bakeEfficiency_bakerSucecss :: !(Sum Int)
  -- , _bakeEfficiency_bakerTotal :: !(Sum Int)
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

data Notificatee = Notificatee
  { _notificatee_email :: !Email
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
  { _mailServerConfig_hostName :: !Text
  , _mailServerConfig_portNumber :: !Word16
  , _mailServerConfig_smtpProtocol :: !SmtpProtocol
  , _mailServerConfig_userName :: !Text
  , _mailServerConfig_password :: !Text
  -- TODO this `madeDefaultAt` seems to be for old design
  , _mailServerConfig_madeDefaultAt :: !UTCTime
  , _mailServerConfig_enabled :: !Bool
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId MailServerConfig

data ErrorLogNetworkUpdate = ErrorLogNetworkUpdate
  { _errorLogNetworkUpdate_log :: !(Id ErrorLog)
  , _errorLogNetworkUpdate_namedChain :: !NamedChain
  , _errorLogNetworkUpdate_version :: !TezosVersion
  , _errorLogNetworkUpdate_gitLabProjectId :: !Text
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogNetworkUpdate where
  type IdData ErrorLogNetworkUpdate = Id ErrorLog

data ErrorLogBakerLedgerDisconnected = ErrorLogBakerLedgerDisconnected
  { _errorLogBakerLedgerDisconnected_log :: !(Id ErrorLog)
  , _errorLogBakerLedgerDisconnected_baker :: !(Id Baker)
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogBakerLedgerDisconnected where
  type IdData ErrorLogBakerLedgerDisconnected = Id ErrorLog

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

data ErrorLogNodeInsufficientPeers = ErrorLogNodeInsufficientPeers
  { _errorLogNodeInsufficientPeers_log :: !(Id ErrorLog)
  , _errorLogNodeInsufficientPeers_node :: !(Id Node)
  , _errorLogNodeInsufficientPeers_synchronisationThreshold :: !Word8
  , _errorLogNodeInsufficientPeers_actualPeerCount :: !Word64
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogNodeInsufficientPeers where
  type IdData ErrorLogNodeInsufficientPeers = Id ErrorLog

-- | Bakers in the daemon sense, not delegate sense
data ErrorLogBakerNoHeartbeat = ErrorLogBakerNoHeartbeat
  { _errorLogBakerNoHeartbeat_log :: !(Id ErrorLog)
  , _errorLogBakerNoHeartbeat_lastLevel :: !RawLevel
  , _errorLogBakerNoHeartbeat_lastBlockHash :: !BlockHash
  , _errorLogBakerNoHeartbeat_client :: !(Id BakerDaemon)
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogBakerNoHeartbeat where
  type IdData ErrorLogBakerNoHeartbeat = Id ErrorLog

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
  , _errorLogBadNodeHead_bootstrapped :: !Bool
  , _errorLogBadNodeHead_chainStatus :: !SyncState
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
-- manually acknowledges the error.  If the network is branch hopping; it's
-- possible for a user to acknowledge a miss, then for the same level missed to
-- be re-reported;  we explicitly ignore that possibility.
data ErrorLogBakerMissed = ErrorLogBakerMissed
  { _errorLogBakerMissed_log :: !(Id ErrorLog)
  , _errorLogBakerMissed_baker :: !(Id Baker)
  , _errorLogBakerMissed_right :: !RightKind
  , _errorLogBakerMissed_level :: !RawLevel
  , _errorLogBakerMissed_fitness :: !Fitness
  , _errorLogBakerMissed_bakeTime :: !UTCTime
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

data ErrorLogVotingReminder = ErrorLogVotingReminder
  { _errorLogVotingReminder_log :: !(Id ErrorLog)
  , _errorLogVotingReminder_baker :: !(Id Baker)
  , _errorLogVotingReminder_periodKind :: !VotingPeriodKind
  , _errorLogVotingReminder_votingPeriod :: !RawLevel
  , _errorLogVotingReminder_previouslyVoted :: !Bool
  , _errorLogVotingReminder_rangeMax :: !Int
  , _errorLogVotingReminder_periodEndsAt :: !UTCTime
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogVotingReminder where
  type IdData ErrorLogVotingReminder = Id ErrorLog

-- | WARNING: Never remove or modify these cases since they are stored in the DB directly. Adding is ok.
data InternalNodeFailureReason
  = InternalNodeFailureReason_CarthageUpgrade
  | InternalNodeFailureReason_Unknown Text
  deriving (Eq, Ord, Generic, Typeable, Read, Show)
instance Exception InternalNodeFailureReason

data ErrorLogInternalNodeFailed = ErrorLogInternalNodeFailed
  { _errorLogInternalNodeFailed_log :: !(Id ErrorLog)
  , _errorLogInternalNodeFailed_node :: !(Id NodeInternal)
  , _errorLogInternalNodeFailed_reason :: !InternalNodeFailureReason
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogInternalNodeFailed where
  type IdData ErrorLogInternalNodeFailed = Id ErrorLog

data ErrorLog = ErrorLog
  { _errorLog_started :: !UTCTime
  , _errorLog_stopped :: !(Maybe UTCTime)
  , _errorLog_lastSeen :: !UTCTime
  , _errorLog_noticeSentAt :: !(Maybe UTCTime)
  , _errorLog_chainId :: !ChainId
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLog

data UpgradeCheckError
  = UpgradeCheckError_UpstreamUnreachable
  | UpgradeCheckError_UpstreamMissing
  | UpgradeCheckError_UpstreamUnparseable
  deriving (Eq, Ord, Generic, Typeable, Enum, Bounded, Read, Show)

data UpstreamVersion = UpstreamVersion
  { _upstreamVersion_error :: !(Maybe UpgradeCheckError)
  , _upstreamVersion_version :: !(Maybe Version)
  , _upstreamVersion_updated :: !UTCTime
  , _upstreamVersion_dismissed :: !Bool
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

data SnapshotMeta = SnapshotMeta
  { _snapshotMeta_filename :: !Text -- user supplied
  , _snapshotMeta_storePath :: !Text -- where stored
  , _snapshotMeta_uploadTime :: !UTCTime
  , _snapshotMeta_importCompleteTime :: !(Maybe UTCTime)
  , _snapshotMeta_importError :: !(Maybe Text)
  , _snapshotMeta_headBlock :: !(Maybe BlockHash)
  , _snapshotMeta_headBlockPrefix :: !(Maybe Text)
  , _snapshotMeta_headBlockLevel :: !(Maybe RawLevel)
  , _snapshotMeta_headBlockBakeTime :: !(Maybe UTCTime)
  , _snapshotMeta_control :: !ProcessControl
  , _snapshotMeta_mbUri :: !(Maybe URI)
  , _snapshotMeta_downloadError :: !(Maybe Text)
  } deriving (Eq, Generic, Ord, Show, Typeable)
instance HasId SnapshotMeta

data SnapshotImportSource
  = SnapshotImportSource_FileSource
  | SnapshotImportSource_FilePathSource FilePath
  | SnapshotImportSource_UriSource URI
  deriving (Eq, Generic, Ord, Show, Typeable)

data SnapshotImportError
  = SnapshotImportError_FileNotFound
  deriving (Eq, Generic, Ord, Show, Typeable)

data AddInternalNodeError
  = AddInternalNodeError_SnapshotImportError SnapshotImportError
  deriving (Eq, Generic, Ord, Show, Typeable)

-- Re-ordering these can yield errors
-- https://ghc.haskell.org/trac/ghc/ticket/8740 (fixed in GHC 8.6)
data LogTag a where
  LogTag_NetworkUpdate :: LogTag ErrorLogNetworkUpdate
  LogTag_Node :: NodeLogTag a -> LogTag a
  LogTag_Baker :: BakerLogTag a -> LogTag a
  LogTag_InternalNodeFailed :: LogTag ErrorLogInternalNodeFailed
  LogTag_BakerNoHeartbeat :: LogTag ErrorLogBakerNoHeartbeat
  --  | Misc baker /daemon/ error.

deriving instance Eq (LogTag a)
deriving instance Ord (LogTag a)
deriving instance Show (LogTag a)

-- Review CollectiveNodesFailure code when adding a new alert
data NodeLogTag a where
  NodeLogTag_InaccessibleNode :: NodeLogTag ErrorLogInaccessibleNode
  NodeLogTag_NodeWrongChain :: NodeLogTag ErrorLogNodeWrongChain
  NodeLogTag_NodeInsufficientPeers :: NodeLogTag ErrorLogNodeInsufficientPeers
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
  BakerLogTag_BakerLedgerDisconnected :: BakerLogTag ErrorLogBakerLedgerDisconnected
  BakerLogTag_BakerMissed :: BakerLogTag ErrorLogBakerMissed
  BakerLogTag_BakerDeactivated :: BakerLogTag ErrorLogBakerDeactivated
  BakerLogTag_BakerDeactivationRisk :: BakerLogTag ErrorLogBakerDeactivationRisk
  BakerLogTag_BakerAccused :: BakerLogTag ErrorLogBakerAccused
  BakerLogTag_InsufficientFunds :: BakerLogTag ErrorLogInsufficientFunds
  BakerLogTag_VotingReminder :: BakerLogTag ErrorLogVotingReminder

deriving instance Eq (BakerLogTag a)
deriving instance Ord (BakerLogTag a)
deriving instance Show (BakerLogTag a)

fmap concat $ sequence (map (deriveJSON defaultTezosCompatJsonOptions)
  [ ''Accusation
  , ''AccusationBlock
  , ''AddInternalNodeError
  , ''AlertNotificationMethod
  , ''Amendment
  , ''BakeEfficiency
  , ''Baker
  , ''BakerDaemon
  , ''BakerDaemonInternalData
  , ''BakerData
  , ''BakerDetails
  , ''BakerProposal
  , ''BakerRight
  , ''BakerRightsProgress
  , ''BakerVote
  , ''BlockBaker
  , ''CacheDelegateInfo
  , ''DeletableRow
  , ''ErrorLog
  , ''ErrorLogBadNodeHead
  , ''ErrorLogBakerAccused
  , ''ErrorLogBakerDeactivated
  , ''ErrorLogBakerDeactivationRisk
  , ''ErrorLogBakerLedgerDisconnected
  , ''ErrorLogBakerMissed
  , ''ErrorLogBakerNoHeartbeat
  , ''ErrorLogInaccessibleNode
  , ''ErrorLogInsufficientFunds
  , ''ErrorLogInternalNodeFailed
  , ''ErrorLogNetworkUpdate
  , ''ErrorLogNodeInsufficientPeers
  , ''ErrorLogNodeInvalidPeerCount
  , ''ErrorLogNodeWrongChain
  , ''ErrorLogVotingReminder
  , ''InternalNodeFailureReason
  , ''MailServerConfig
  , ''Node
  , ''NodeDetails
  , ''NodeDetailsData
  , ''NodeExternal
  , ''NodeExternalData
  , ''NodeInternal
  , ''NodeProcessState
  , ''PeriodPromotionVote
  , ''PeriodAdoption
  , ''PeriodProposal
  , ''PeriodTesting
  , ''PeriodTestingVote
  , ''PeriodVote
  , ''ProcessControl
  , ''ProcessData
  , ''ProcessState
  , ''RightKind
  , ''RightNotificationLimit
  , ''RightNotificationSettings
  , ''SmtpProtocol
  , ''SnapshotImportError
  , ''SnapshotImportSource
  , ''SnapshotMeta
  , ''TelegramConfig
  , ''TelegramMessageQueue
  , ''TelegramRecipient
  , ''UpgradeCheckError
  , ''UpstreamVersion
  ] ++ map makeLenses
  [ 'Accusation
  , 'AccusationBlock
  , 'Amendment
  , 'BakeEfficiency
  , 'Baker
  , 'BakerDaemon
  , 'BakerDaemonInternalData
  , 'BakerData
  , 'BakerDetails
  , 'BakerRight
  , 'BakerRightsProgress
  , 'BlockBaker
  , 'DeletableRow
  , 'Error
  , 'ErrorLog
  , 'ErrorLogBadNodeHead
  , 'ErrorLogBakerAccused
  , 'ErrorLogBakerDeactivated
  , 'ErrorLogBakerDeactivationRisk
  , 'ErrorLogBakerLedgerDisconnected
  , 'ErrorLogBakerMissed
  , 'ErrorLogBakerNoHeartbeat
  , 'ErrorLogInaccessibleNode
  , 'ErrorLogInsufficientFunds
  , 'ErrorLogInternalNodeFailed
  , 'ErrorLogNetworkUpdate
  , 'ErrorLogNodeInvalidPeerCount
  , 'ErrorLogNodeWrongChain
  , 'ErrorLogVotingReminder
  , 'MailServerConfig
  , 'Node
  , 'NodeDetails
  , 'NodeDetailsData
  , 'NodeExternal
  , 'NodeExternalData
  , 'NodeInternal
  , 'PeriodPromotionVote
  , 'PeriodAdoption
  , 'PeriodProposal
  , 'PeriodTesting
  , 'PeriodTestingVote
  , 'PeriodVote
  , 'ProcessData
  , 'ProtocolIndex
  , 'RightNotificationLimit
  , 'RightNotificationSettings
  , 'SnapshotMeta
  , 'TelegramConfig
  , 'TelegramMessageQueue
  , 'TelegramRecipient
  , 'UpstreamVersion
  ] ++ map makePrisms
  [ ''ErrorLogInternalNodeFailed
  , ''UpgradeCheckError
  ])

fmap concat $ for [''NodeLogTag, ''BakerLogTag] $ \t -> concat <$> sequence
  [ deriveJSONGADT t
  , deriveArgDict t
  , deriveGEq t
  , deriveGCompare t
  , deriveGShow t
  ]

-- Do this is second because it is downstream
fmap concat $ for [''LogTag] $ \t -> concat <$> sequence
  [ deriveJSONGADT t
  , deriveArgDict t
  , deriveGEq t
  , deriveGCompare t
  , deriveGShow t
  ]

instance UniverseSome NodeLogTag where
  universeSome =
    [ Some NodeLogTag_InaccessibleNode
    , Some NodeLogTag_NodeWrongChain
    , Some NodeLogTag_NodeInsufficientPeers
    , Some NodeLogTag_NodeInvalidPeerCount
    , Some NodeLogTag_BadNodeHead
    ]

instance UniverseSome BakerLogTag where
  universeSome =
    [ Some BakerLogTag_BakerMissed
    , Some BakerLogTag_BakerDeactivated
    , Some BakerLogTag_BakerDeactivationRisk
    , Some BakerLogTag_BakerAccused
    , Some BakerLogTag_BakerLedgerDisconnected
    , Some BakerLogTag_InsufficientFunds
    , Some BakerLogTag_VotingReminder
    ]
-- need Cale to fix this
-- deriveSomeUniverse ''LogTag
instance UniverseSome LogTag where
  universeSome =
    [Some LogTag_NetworkUpdate]
    <> fmap (\(Some x) -> Some (LogTag_Node x)) universe
    <> fmap (\(Some x) -> Some (LogTag_Baker x)) universe
    <> [Some LogTag_InternalNodeFailed, Some LogTag_BakerNoHeartbeat]

instance HasProtocolHash ProtocolIndex where
  protocolHash = protocolIndex_hash

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
  , ''ErrorLogBakerLedgerDisconnected
  , ''ErrorLogBakerMissed
  , ''ErrorLogBakerNoHeartbeat
  , ''ErrorLogInaccessibleNode
  , ''ErrorLogInsufficientFunds
  , ''ErrorLogInternalNodeFailed
  , ''ErrorLogNetworkUpdate
  , ''ErrorLogNodeInsufficientPeers
  , ''ErrorLogNodeInvalidPeerCount
  , ''ErrorLogNodeWrongChain
  , ''ErrorLogVotingReminder
  ]

instance Aeson.ToJSON ProtocolIndex where
  toJSON protoIndex =
    case $(Aeson.mkToJSON tezosJsonOptions ''ProtocolIndex) protoIndex of
      Aeson.Object o ->
        case HashMap.lookup "json_constants" o of
          Nothing -> error "the 'impossible' happened: the _protocolIndex_jsonConstants field is missing"
          Just jc -> Aeson.Object $ HashMap.insert "constants" jc $ HashMap.delete "json_constants" o
      _ -> error "the 'impossible' happened: ProtocolIndex is not a JSON object"

instance Aeson.FromJSON ProtocolIndex where
  parseJSON = Aeson.withObject "ProtocolIndex" $ \o -> do
    jc :: Aeson.Value <- o Aeson..: "constants"
    let o' = HashMap.insert "json_constants" jc o
    $(Aeson.mkParseJSON tezosJsonOptions ''ProtocolIndex) (Aeson.Object o')

makePrisms ''AdditionalInfo

concat <$> traverse (deriveJSON $ defaultTezosCompatJsonOptions { Aeson.omitNothingFields = True })
  [ 'NodeVersion
  , 'MajorMinorVersion
  , 'CommitInfo
  ]

concat <$> traverse makeLenses
  [ ''NodeVersion
  , ''MajorMinorVersion
  , ''CommitInfo
  ]
