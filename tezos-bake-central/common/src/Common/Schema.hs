{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- TODO do everywhere
{-# OPTIONS_GHC -Wall -fno-warn-orphans -Werror #-}

-- 'deriveJSONGADT' produces seemingly redundant pattern matches.
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Common.Schema
  ( module Common.Schema

  -- Re-exports
  , Id
  ) where

import Control.Exception.Safe (Exception, SomeException)
import Control.Lens
import Control.Monad.Except (runExcept)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonE
import Data.Aeson.TH (deriveJSON)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Aeson.GADT (deriveJSONGADT)
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Dependent.Sum (DSum)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Semigroup, Sum (..), getSum, (<>))
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime)
import Data.Typeable (Typeable)
import Data.Universe
import Data.Universe.Helpers (universeDef)
import Data.Version (Version)
import Data.Word
import GHC.Generics (Generic)
import Rhyolite.Schema (Email, HasId, IdData, Id, Json)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.Json
import Tezos.NodeRPC.Sources (PublicNode)
import Tezos.NodeRPC.Types (NetworkStat (..), RpcError, AsRpcError (asRpcError))
import Tezos.Operation
import Tezos.Types

import Common (defaultTezosCompatJsonOptions)
import ExtraPrelude

data CacheError
  = CacheError_RpcError !RpcError
  | CacheError_NoSuitableNode
  | CacheError_NotEnoughHistory
  | CacheError_Timeout !NominalDiffTime
  | CacheError_SomeException !SomeException
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

data Client = Client
  { _client_address :: !URI
  , _client_alias :: !(Maybe Text)
  , _client_updated :: !(Maybe UTCTime)
  , _client_deleted :: !Bool
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId Client

data PendingReward = PendingReward
  { _pendingReward_baker :: !(Id Baker)
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
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId ClientInfo

data Node = Node
  { _node_address :: !URI
  , _node_alias :: !(Maybe Text)
  , _node_identity :: !(Maybe CryptoboxPublicKeyHash)
  , _node_headLevel :: !(Maybe RawLevel)
  , _node_headBlockHash :: !(Maybe BlockHash)
  , _node_headBlockPred :: !(Maybe BlockHash)
  , _node_headBlockBakedAt :: !(Maybe UTCTime)
  , _node_peerCount :: !(Maybe Word64)
  , _node_networkStat :: !NetworkStat
  , _node_fitness :: !(Maybe Fitness)
  , _node_deleted :: !Bool
  , _node_updated :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance HasId Node

mkNode :: URI -> Maybe Text -> Node
mkNode addr alias = Node
  { _node_address = addr
  , _node_alias = alias
  , _node_identity = Nothing -- TODO
  , _node_headLevel = Nothing
  , _node_headBlockHash = Nothing
  , _node_headBlockPred = Nothing
  , _node_headBlockBakedAt = Nothing
  , _node_peerCount = Nothing
  , _node_networkStat = NetworkStat 0 0 0 0
  , _node_fitness = Nothing
  , _node_deleted = False
  , _node_updated = Nothing
  }

getNodeHeadBlock :: Node -> Maybe VeryBlockLike
getNodeHeadBlock n = VeryBlockLike
  <$> _node_headBlockHash n
  <*> _node_headBlockPred n
  <*> _node_fitness n
  <*> _node_headLevel n
  <*> _node_headBlockBakedAt n

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

data Baker = Baker
  { _baker_publicKeyHash :: !PublicKeyHash
  , _baker_alias :: !(Maybe Text)
  , _baker_deleted :: !Bool
  } deriving (Eq, Ord, Show, Generic, Typeable)

instance HasId Baker where
  type IdData Baker = PublicKeyHash

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

data BakerDetails = BakerDetails
  { _bakerDetails_publicKeyHash :: !PublicKeyHash
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
-- rather than all possible.  For the same reason we *do* include endorsement
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

data AlertNotificationMethod
  = AlertNotificationMethod_Email
  | AlertNotificationMethod_Telegram
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance Aeson.ToJSONKey AlertNotificationMethod where
  toJSONKey = Aeson.ToJSONKeyText (tshow) (AesonE.text . tshow)

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
instance HasId ErrorLogNetworkUpdate

data ErrorLogInaccessibleNode = ErrorLogInaccessibleNode
  { _errorLogInaccessibleNode_log :: !(Id ErrorLog)
  , _errorLogInaccessibleNode_node :: !(Id Node)
  , _errorLogInaccessibleNode_address :: !URI
  , _errorLogInaccessibleNode_alias :: !(Maybe Text)
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogInaccessibleNode

data ErrorLogNodeWrongChain = ErrorLogNodeWrongChain
  { _errorLogNodeWrongChain_log :: !(Id ErrorLog)
  , _errorLogNodeWrongChain_node :: !(Id Node)
  , _errorLogNodeWrongChain_address :: !URI
  , _errorLogNodeWrongChain_alias :: !(Maybe Text)
  , _errorLogNodeWrongChain_expectedChainId :: !ChainId
  , _errorLogNodeWrongChain_actualChainId :: !ChainId
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogNodeWrongChain

-- | Bakers in the daemon sense, not delegate sense
data ErrorLogBakerNoHeartbeat = ErrorLogBakerNoHeartbeat
  { _errorLogBakerNoHeartbeat_log :: !(Id ErrorLog)
  , _errorLogBakerNoHeartbeat_lastLevel :: !RawLevel
  , _errorLogBakerNoHeartbeat_lastBlockHash :: !BlockHash
  , _errorLogBakerNoHeartbeat_client :: !(Id Client)
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogBakerNoHeartbeat

data ClientWorker = ClientWorker_Baking | ClientWorker_Endorsing
  deriving (Eq, Ord, Bounded, Enum, Generic, Typeable, Read, Show)

data ErrorLogMultipleBakersForSameBaker = ErrorLogMultipleBakersForSameBaker
  { _errorLogMultipleBakersForSameBaker_log :: !(Id ErrorLog)
  , _errorLogMultipleBakersForSameBaker_publicKeyHash :: !PublicKeyHash
  , _errorLogMultipleBakersForSameBaker_client :: !(Id Client)
  , _errorLogMultipleBakersForSameBaker_worker :: !ClientWorker
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogMultipleBakersForSameBaker

data ErrorLogBakerDeactivated = ErrorLogBakerDeactivated
  { _errorLogBakerDeactivated_log :: !(Id ErrorLog)
  , _errorLogBakerDeactivated_publicKeyHash :: !PublicKeyHash
  , _errorLogBakerDeactivated_preservedCycles :: !Cycle
  , _errorLogBakerDeactivated_fitness :: !Fitness
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogBakerDeactivated

data ErrorLogBakerDeactivationRisk = ErrorLogBakerDeactivationRisk
  { _errorLogBakerDeactivationRisk_log :: !(Id ErrorLog)
  , _errorLogBakerDeactivationRisk_publicKeyHash :: !PublicKeyHash
  , _errorLogBakerDeactivationRisk_gracePeriod :: !Cycle
  , _errorLogBakerDeactivationRisk_latestCycle :: !Cycle
  , _errorLogBakerDeactivationRisk_preservedCycles :: !Cycle
  , _errorLogBakerDeactivationRisk_fitness :: !Fitness
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogBakerDeactivationRisk

data ErrorLogBadNodeHead = ErrorLogBadNodeHead
  { _errorLogBadNodeHead_log :: !(Id ErrorLog)
  , _errorLogBadNodeHead_node :: !(Id Node)
  , _errorLogBadNodeHead_lca :: !(Maybe (Json VeryBlockLike))
  , _errorLogBadNodeHead_nodeHead :: !(Json VeryBlockLike)
  , _errorLogBadNodeHead_latestHead :: !(Json VeryBlockLike)
  } deriving (Eq, Ord, Generic, Typeable, Show)
instance HasId ErrorLogBadNodeHead

-- we wilfully ignore the branch issue; we mostly don't care on which branch you
-- did or didn't take your rights.
--
-- in particular, there's two ways to "resolve" this type of alert, either a
-- new uncle occurs in which the baker *did* exercise their rights, or the user
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
instance HasId ErrorLogBakerMissed

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
  , _telegramRecipient_userId :: !Word64
  , _telegramRecipient_chatId :: !Word64
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

data LogTag a where
  LogTag_InaccessibleNode :: LogTag ErrorLogInaccessibleNode
  LogTag_NodeWrongChain :: LogTag ErrorLogNodeWrongChain
  LogTag_BakerNoHeartbeat :: LogTag ErrorLogBakerNoHeartbeat
  LogTag_BadNodeHead :: LogTag ErrorLogBadNodeHead
  LogTag_MultipleBakersForSameBaker :: LogTag ErrorLogMultipleBakersForSameBaker
  LogTag_BakerDeactivated :: LogTag ErrorLogBakerDeactivated
  LogTag_BakerDeactivationRisk :: LogTag ErrorLogBakerDeactivationRisk
  LogTag_BakerMissed :: LogTag ErrorLogBakerMissed
  LogTag_NetworkUpdate :: LogTag ErrorLogNetworkUpdate


data BakerErrorDescriptions = BakerErrorDescriptions
  { _bakerErrorDescriptions_title :: !Text
  , _bakerErrorDescriptions_tile :: !Text
  , _bakerErrorDescriptions_notification :: !Text
  , _bakerErrorDescriptions_problem :: !Text
  , _bakerErrorDescriptions_warning :: !(Maybe Text)
  , _bakerErrorDescriptions_fix :: !Text
  , _bakerErrorDescriptions_resolved :: !(Baker -> (Text, Text))
  , _bakerErrorDescriptions_userResolvable :: !(Maybe (DSum LogTag Identity))
  }


fmap concat $ sequence (map (deriveJSON defaultTezosCompatJsonOptions)
  [ ''BakeEfficiency
  , ''BakedEvent
  , ''BakedEventOperation
  , ''Baker
  , ''BakerDetails
  , ''BakerRight
  , ''BakerRightsCycleProgress
  , ''BlockBaker
  , ''CacheDelegateInfo
  , ''ClientConfig
  , ''ClientDaemonWorker
  , ''ClientInfo
  , ''ClientWorker
  , ''EndorseEvent
  , ''ErrorEvent
  , ''ErrorLog
  , ''ErrorLogBadNodeHead
  , ''ErrorLogBakerMissed
  , ''ErrorLogBakerDeactivated
  , ''ErrorLogBakerDeactivationRisk
  , ''ErrorLogBakerNoHeartbeat
  , ''ErrorLogInaccessibleNode
  , ''ErrorLogMultipleBakersForSameBaker
  , ''ErrorLogNodeWrongChain
  , ''ErrorLogNetworkUpdate
  , ''Event
  , ''MailServerConfig
  , ''Node
  , ''Parameters
  , ''PublicNodeConfig
  , ''PublicNodeHead
  , ''Report
  , ''RightKind
  , ''SeenEvent
  , ''AlertNotificationMethod
  , ''SmtpProtocol
  , ''TelegramConfig
  , ''TelegramMessageQueue
  , ''TelegramRecipient
  , ''UpgradeCheckError
  , ''UpstreamVersion
  ] ++ map makeLenses
  [ 'BakeEfficiency
  , 'BakedEvent
  , 'BakedEventOperation
  , 'Baker
  , 'BakerDetails
  , 'BakerRight
  , 'BakerRightsCycleProgress
  , 'BlockBaker
  , 'CachedProtocolConstants
  , 'EndorseEvent
  , 'Error
  , 'ErrorEvent
  , 'ErrorLog
  , 'ErrorLogBadNodeHead
  , 'ErrorLogBakerMissed
  , 'ErrorLogBakerDeactivated
  , 'ErrorLogBakerDeactivationRisk
  , 'ErrorLogBakerNoHeartbeat
  , 'ErrorLogInaccessibleNode
  , 'ErrorLogMultipleBakersForSameBaker
  , 'ErrorLogNodeWrongChain
  , 'ErrorLogNetworkUpdate
  , 'Event
  , 'MailServerConfig
  , 'Parameters
  , 'PublicNodeConfig
  , 'PublicNodeHead
  , 'Report
  , 'SeenEvent
  , 'TelegramConfig
  , 'TelegramMessageQueue
  , 'TelegramRecipient
  , 'UpstreamVersion
  ] ++ map makePrisms
  [ ''UpgradeCheckError
  ])

return []

deriveArgDict ''LogTag
deriveGCompare ''LogTag
deriveGEq ''LogTag
deriveGShow ''LogTag
deriveJSONGADT ''LogTag




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

nodeIdentification :: Node -> (Text, Maybe Text)
nodeIdentification = aliasedIdentification _node_alias $ Uri.render . _node_address

bakerIdentification :: Baker -> (Text, Maybe Text)
bakerIdentification = aliasedIdentification _baker_alias $ toPublicKeyHashText . _baker_publicKeyHash
