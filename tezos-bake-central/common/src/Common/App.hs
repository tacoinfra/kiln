{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.App
  ( module Common.App

  -- Re-exports
  , AlertNotificationMethod (..)
  ) where

import Control.Lens.TH (makeLenses)
import Data.Some (Some)
import Data.Dependent.Sum
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import Data.Dependent.Sum.Orphans ()
import Data.Dependent.Map (DMap)
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import Data.Time (UTCTime, diffUTCTime)
import qualified Data.Time as Time
import Data.Validation (Validation)
import Data.Word (Word16)
import Data.Witherable (Filterable (mapMaybe))
import Database.Id.Class
import Reflex (Additive, Group (..))
import Reflex.Query.Class (Query (QueryResult, crop))
import Rhyolite.Schema (Email)
import Rhyolite.App (PositivePart (..), standardPositivePart)
import Data.MonoidMap (MonoidMap (..))
import Tezos.Types

import Common (uriHostPortPath)
import Common.Alerts (AlertsFilter (..))
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import Common.Config (FrontendConfig)
import Common.Schema
import Common.Vassal
import ExtraPrelude

type ErrorInfo = (ErrorLog, ErrorLogView)

getErrorInterval :: (ErrorLog, ErrorLogView) -> First ((ErrorLog, ErrorLogView), ClosedInterval (WithInfinity UTCTime))
getErrorInterval ei@(el, _) = First (ei, ClosedInterval
  (Bounded $ _errorLog_started el)
  (maybe UpperInfinity Bounded $ _errorLog_stopped el))

-- | Get or estimate the start time of a period. Return 'Bool' indicates if the date is estimated
getStartTimeForPeriod :: VotingPeriodKind -> Amendment -> Map.Map VotingPeriodKind Amendment -> ProtoInfo -> (Time.UTCTime, Bool)
getStartTimeForPeriod p a as proto
  | Just a' <- Map.lookup p as = (_amendment_start a', False)
  | otherwise = (estimate, True)
  where estimate = Time.addUTCTime (timeBetweenBlocks * blocksPerPeriod * periodDiff) (_amendment_start a)
        blocksPerPeriod = fromIntegral $ getBlocksPerVotingPeriod proto
        timeBetweenBlocks = getTimeBetweenBlocks proto
        periodDiff = fromIntegral $ fromEnum p - fromEnum (_amendment_period a)

-- | Get or estimate the end time of a period. Return 'Bool' indicates if the date is estimated
getEndTimeForPeriod :: VotingPeriodKind -> Amendment -> Map.Map VotingPeriodKind Amendment -> ProtoInfo -> (Time.UTCTime, Bool)
getEndTimeForPeriod p a as proto
  | Just p' <- safeSucc p, Just a' <- Map.lookup p' as = (_amendment_start a', False)
  | otherwise = (estimate, True)
  where estimate = Time.addUTCTime (timeBetweenBlocks * blocksPerPeriod * periodDiff) (_amendment_start a)
        blocksPerPeriod = fromIntegral $ getBlocksPerVotingPeriod proto
        timeBetweenBlocks = getTimeBetweenBlocks proto
        periodDiff = fromIntegral $ fromEnum p - fromEnum (_amendment_period a) + 1

calculatePeriodProgress :: UTCTime -> UTCTime -> UTCTime -> (Double, Time.NominalDiffTime)
calculatePeriodProgress currentTime startTime endTime = (ellapsedFraction, remaining)
  where
    ellapsed = currentTime `diffUTCTime` startTime
    remaining = endTime `diffUTCTime` currentTime
    ellapsedFraction = realToFrac ellapsed / realToFrac (ellapsed + remaining)

type Deletable a = First (Maybe a)

type Deletable' e a = Validation (First e) a

instance FromJSON e => FromJSON a => FromJSON (Validation e a)
instance ToJSON e => ToJSON a => ToJSON (Validation e a)

pattern IthacaProtocolHash :: ProtocolHash
pattern IthacaProtocolHash = "Psithaca2MLRFYargivpo7YvUr7wUDqyxrdhC5CQq78mRvimz6A"

pattern JakartaProtocolHash :: ProtocolHash
pattern JakartaProtocolHash = "PtJakart2xVj7pYXJBXrqHgd82rdkLey5ZeeGwDgPp9rhQUbSqY"

data WorkerType
  = WorkerType_Node
  | WorkerType_Baker
  deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON WorkerType
instance ToJSON WorkerType

data BakerInternalData = BakerInternalData
  { _bakerInternalData_secretKey :: SecretKey
  , _bakerInternalData_processData :: ProcessData
  , _bakerInternalData_endorserProcessData :: ProcessData
  } deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON BakerInternalData
instance ToJSON BakerInternalData

isBakerRunning :: BakerInternalData -> Bool
isBakerRunning = (==) ProcessControl_Run
  . _processData_control
  . _bakerInternalData_processData

data BakerNextRight
  = BakerNextRight_GatheringData
  | BakerNextRight_WaitingForRights
  | BakerNextRight_BakeBlock RawLevel
  | BakerNextRight_KnownNoRights
  deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON BakerNextRight
instance ToJSON BakerNextRight

data BakerSummary = BakerSummary
  { _bakerSummary_baker :: Either BakerData BakerInternalData
  , _bakerSummary_alertCount :: Int
  , _bakerSummary_nextRight :: BakerNextRight
  } deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON BakerSummary
instance ToJSON BakerSummary

data BakerAlert
  = BakerAlert_Alert (DSum BakerLogTag Identity)
  | BakerAlert_GroupedAlert GroupedBakerAlert
  deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON BakerAlert
instance ToJSON BakerAlert

-- When adding a new type of grouped baker alert, make the neccessary changes
-- in 'groupBakerAlerts' function.
data GroupedAlertType
  = GroupedAlertType_MissedBake
  | GroupedAlertType_MissedEndorsementBonus
  deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON GroupedAlertType
instance ToJSON GroupedAlertType

data GroupedBakerAlert = GroupedBakerAlert
  { _groupedBakerAlert_type :: GroupedAlertType
  , _groupedBakerAlert_first :: (RawLevel, UTCTime)
  , _groupedBakerAlert_latest :: (RawLevel, UTCTime)
  , _groupedBakerAlert_right :: Maybe RightKind
  , _groupedBakerAlert_baker :: Id Baker
  , _groupedBakerAlert_logs :: NonEmpty (Id ErrorLog)
  } deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON GroupedBakerAlert
instance ToJSON GroupedBakerAlert

errorLogFromBakerAlert :: BakerAlert -> NonEmpty (Id ErrorLog)
errorLogFromBakerAlert = \case
  BakerAlert_Alert (btag :=> Identity blog) -> errorLogIdForBakerLogTag btag blog :| []
  BakerAlert_GroupedAlert GroupedBakerAlert{..} -> _groupedBakerAlert_logs

-- data NodeSummary = Node Node' AlertCount

data NodeSummary = NodeSummary
  { _nodeSummary_node :: Either NodeExternalData ProcessData
  , _nodeSummary_alertCount :: Int
  } deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON NodeSummary
instance ToJSON NodeSummary

bakerSummaryIdentification :: (IdData BakerData, BakerSummary) -> (Text, Maybe Text)
bakerSummaryIdentification = aliasedIdentification
  (either _bakerData_alias (const $ Just "Kiln Baker") . _bakerSummary_baker . snd)
  (toPublicKeyHashText . fst)

nodeSummaryIdentification :: NodeSummary -> (Text, Maybe Text)
nodeSummaryIdentification = nodeDataIdentification . _nodeSummary_node

nodeDataIdentification :: Either NodeExternalData ProcessData -> (Text, Maybe Text)
nodeDataIdentification = \case
  Left e -> aliasedIdentification
    _nodeExternalData_alias
    (uriHostPortPath . _nodeExternalData_address)
    e
  Right _ -> ("Kiln Node", Nothing)

data LiquidityBakingToggleVote
  = LiquidityBakingToggleVote_Pass
  | LiquidityBakingToggleVote_On
  | LiquidityBakingToggleVote_Off
  deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON LiquidityBakingToggleVote
instance ToJSON LiquidityBakingToggleVote

data SetupState = SetupState
  { _setupState_import :: Maybe (First ImportSecretKeyStep)
  , _setupState_setup :: Maybe (First SetupLedgerToBakeStep)
  , _setupState_register :: Maybe (First RegisterStep)
  , _setupState_setHWM :: Maybe (First SetHWMStep)
  } deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON SetupState
instance ToJSON SetupState
instance Monoid SetupState where
  mempty = SetupState Nothing Nothing Nothing Nothing
instance Semigroup SetupState where
  ss1 <> ss2 = SetupState
    { _setupState_import = _setupState_import ss1 <> _setupState_import ss2
    , _setupState_setup = _setupState_setup ss1 <> _setupState_setup ss2
    , _setupState_register = _setupState_register ss1 <> _setupState_register ss2
    , _setupState_setHWM = _setupState_setHWM ss1 <> _setupState_setHWM ss2
    }

data ImportSecretKeyStep
  = ImportSecretKeyStep_Prompting
  | ImportSecretKeyStep_Done
  | ImportSecretKeyStep_Declined
  | ImportSecretKeyStep_Disconnected
  | ImportSecretKeyStep_Failed Text -- Anything else
  deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON ImportSecretKeyStep
instance ToJSON ImportSecretKeyStep

data SetupLedgerToBakeStep
  = SetupLedgerToBakeStep_Prompting
  | SetupLedgerToBakeStep_Done
  | SetupLedgerToBakeStep_DoneAndRegistered
  | SetupLedgerToBakeStep_Declined
  | SetupLedgerToBakeStep_Disconnected
  | SetupLedgerToBakeStep_Failed -- Anything else
  | SetupLedgerToBakeStep_OutdatedVersion Text
  deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON SetupLedgerToBakeStep
instance ToJSON SetupLedgerToBakeStep

data RegisterStep
  = RegisterStep_Prompting
  | RegisterStep_WaitingForInclusion
  | RegisterStep_Registered
  | RegisterStep_AlreadyRegistered
  | RegisterStep_Declined
  | RegisterStep_Disconnected
  | RegisterStep_NodeNotReady
  | RegisterStep_Failed
  | RegisterStep_NotEnoughFunds Tez -- holds account balance
  deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON RegisterStep
instance ToJSON RegisterStep

data SetHWMStep
  = SetHWMStep_Prompting
  | SetHWMStep_Done
  | SetHWMStep_Declined
  | SetHWMStep_Disconnected
  | SetHWMStep_Failed Text -- Anything else
  deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON SetHWMStep
instance ToJSON SetHWMStep

data VoteStep
  = VoteStep_Prompting
  | VoteStep_Done
  | VoteStep_Declined
  | VoteStep_Disconnected
  | VoteStep_WrongPeriod
  | VoteStep_Failed Text -- Anything else
  deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON VoteStep
instance ToJSON VoteStep

data VoteState = VoteState
  { _voteState_step :: Maybe (First VoteStep)
  } deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON VoteState
instance ToJSON VoteState
instance Monoid VoteState where
  mempty = VoteState Nothing
instance Semigroup VoteState where
  s1 <> s2 = VoteState
    { _voteState_step = _voteState_step s1 <> _voteState_step s2
    }

instance FromJSONKey (DSum LogTag (Const ()))
instance ToJSONKey (DSum LogTag (Const ()))

instance FromJSONKey (Some LogTag)
instance ToJSONKey (Some LogTag)

data BakeViewSelector a = BakeViewSelector
  { _bakeViewSelector_config :: MaybeSelector FrontendConfig a
  , _bakeViewSelector_bakerAddresses :: RangeSelector' PublicKeyHash (Deletable BakerSummary) a
  , _bakeViewSelector_bakerAlerts :: RangeSelector' PublicKeyHash (Deletable (NonEmpty BakerAlert)) a
  -- TODO don't need `Deletable` around `BakerDetails`.
  , _bakeViewSelector_bakerDetails :: RangeSelector' PublicKeyHash (Deletable BakerDetails) a
  , _bakeViewSelector_errors :: MonoidalMap AlertsFilter (ComposeSelector (MapSelector (Some LogTag) ()) (IntervalSelector' UTCTime (Id ErrorLog) (Deletable ErrorInfo)) a)
  , _bakeViewSelector_mailServer :: MaybeSelector (Maybe MailServerView) a
  , _bakeViewSelector_nodeAddresses :: RangeSelector' (Id Node) (Deletable NodeSummary) a -- TODO: rename to 'nodeSummaries' ?
  , _bakeViewSelector_nodeVersions :: RangeSelector' (Id Node) (Maybe TezosVersion) a
  , _bakeViewSelector_nodeDetails :: RangeSelector' (Id Node) NodeDetailsData a
  , _bakeViewSelector_latestTezosRelease :: MaybeSelector (Maybe MajorMinorVersion) a
  , _bakeViewSelector_parameters :: MapSelector ProtocolHash ProtocolIndex a
  , _bakeViewSelector_latestHead :: MaybeSelector BranchInfo a
  , _bakeViewSelector_amendment :: RangeSelector VotingPeriodKind (Deletable Amendment) a
  , _bakeViewSelector_proposals :: RangeSelector' (Id PeriodProposal) (Deletable (PeriodProposal, Maybe Bool)) a
  , _bakeViewSelector_bakerVote :: MaybeSelector (Maybe BakerVote) a
  , _bakeViewSelector_periodTestingVote :: MaybeSelector (Maybe PeriodTestingVote) a
  , _bakeViewSelector_periodTesting :: MaybeSelector (Maybe PeriodTesting) a
  , _bakeViewSelector_periodPromotionVote :: MaybeSelector (Maybe PeriodPromotionVote) a
  , _bakeViewSelector_periodAdoption :: MaybeSelector (Maybe PeriodAdoption) a
  , _bakeViewSelector_upstreamVersion :: MaybeSelector UpstreamVersion a
  , _bakeViewSelector_telegramConfig :: MaybeSelector (Maybe TelegramConfig) a
  , _bakeViewSelector_telegramRecipients :: RangeSelector' (Id TelegramRecipient) (Deletable TelegramRecipient) a
  , _bakeViewSelector_alertCount :: MaybeSelector (DMap LogTag (Const Int)) a
  , _bakeViewSelector_snapshotMeta :: MaybeSelector SnapshotMeta a
  , _bakeViewSelector_connectedLedger :: MaybeSelector (Maybe ConnectedLedger) a
  , _bakeViewSelector_showLedger :: RangeSelector SecretKey (Deletable' Text (PublicKeyHash, Maybe Tez)) a
  , _bakeViewSelector_prompting :: RangeSelector SecretKey (Deletable SetupState) a
  , _bakeViewSelector_votePrompting :: RangeSelector SecretKey (Deletable VoteState) a
  , _bakeViewSelector_rightNotificationSettings :: RangeSelector RightKind (Deletable RightNotificationLimit) a
  , _bakeViewSelector_bakerRegistered :: RangeSelector' PublicKeyHash Bool a
  } deriving (Functor, Generic, Typeable, Traversable, Foldable, Show, Eq, Ord)

instance (Monoid a, Num a, Ord a, Ord k) => PositivePart (BakeViewSelector (MonoidMap k a)) where
  positivePart x = let v = mapMaybe standardPositivePart x in if v == mempty then Nothing else Just v

instance Additive a => Additive (BakeViewSelector a)

data BakeView a = BakeView
  { _bakeView_config :: MaybeView FrontendConfig a
  , _bakeView_bakerAddresses :: RangeView' PublicKeyHash (Deletable BakerSummary) a
  , _bakeView_bakerAlerts :: RangeView' PublicKeyHash (Deletable (NonEmpty BakerAlert)) a
  , _bakeView_bakerDetails :: RangeView' PublicKeyHash (Deletable BakerDetails) a
  -- TODO: I'm more than a little concerned about this approach for dealing
  -- with deletes in IntervalView.  I think in this particular case, we can get
  -- away with it; since we never go from Resolved to Unresolved, so the
  -- relevant selection window should *eventually* roll off for the resolved
  -- things and be dropped anyway.  In other cases, this approach is likely to
  -- leak memory in Reflex (deletes never really get to go away)
  , _bakeView_errors :: MonoidalMap AlertsFilter (ComposeView (MapSelector (Some LogTag) ()) (IntervalSelector' UTCTime (Id ErrorLog) (Deletable ErrorInfo)) a)
  , _bakeView_mailServer :: MaybeView (Maybe MailServerView) a
  , _bakeView_nodeAddresses :: RangeView' (Id Node) (Deletable NodeSummary) a
  , _bakeView_nodeVersions :: RangeView' (Id Node) (Maybe TezosVersion) a
  , _bakeView_nodeDetails :: RangeView' (Id Node) NodeDetailsData a
  , _bakeView_latestTezosRelease :: MaybeView (Maybe MajorMinorVersion) a
  , _bakeView_parameters :: Common.Vassal.View (MapSelector ProtocolHash ProtocolIndex) a
  , _bakeView_latestHead :: MaybeView BranchInfo a
  , _bakeView_amendment :: RangeView VotingPeriodKind (Deletable Amendment) a
  , _bakeView_proposals :: RangeView' (Id PeriodProposal) (Deletable (PeriodProposal, Maybe Bool)) a
  , _bakeView_bakerVote :: MaybeView (Maybe BakerVote) a
  , _bakeView_periodTestingVote :: MaybeView (Maybe PeriodTestingVote) a
  , _bakeView_periodTesting :: MaybeView (Maybe PeriodTesting) a
  , _bakeView_periodPromotionVote :: MaybeView (Maybe PeriodPromotionVote) a
  , _bakeView_periodAdoption :: MaybeView (Maybe PeriodAdoption) a
  , _bakeView_upstreamVersion :: MaybeView UpstreamVersion a
  , _bakeView_telegramConfig :: MaybeView (Maybe TelegramConfig) a
  , _bakeView_telegramRecipients :: RangeView' (Id TelegramRecipient) (Deletable TelegramRecipient) a
  , _bakeView_alertCount :: MaybeView (DMap LogTag (Const Int)) a
  , _bakeView_snapshotMeta :: MaybeView SnapshotMeta a
  -- , _bakeView_graphs       :: AppendMap (Id BakerDaemon) (First (Maybe (Micro, Text)), a)
  -- , _bakeView_summaryGraph :: Single (Maybe (Micro, Text)) a
  , _bakeView_connectedLedger :: MaybeView (Maybe ConnectedLedger) a
  , _bakeView_showLedger :: RangeView SecretKey (Deletable' Text (PublicKeyHash, Maybe Tez)) a
  , _bakeView_prompting :: RangeView SecretKey (Deletable SetupState) a
  , _bakeView_votePrompting :: RangeView SecretKey (Deletable VoteState) a
  , _bakeView_rightNotificationSettings :: RangeView RightKind (Deletable RightNotificationLimit) a
  , _bakeView_bakerRegistered :: RangeView' PublicKeyHash Bool a
  } deriving (Functor, Generic, Typeable, Traversable, Foldable, Show, Eq)

data MailServerView = MailServerView
  { _mailServerView_hostName :: Text
  , _mailServerView_portNumber :: Word16
  , _mailServerView_smtpProtocol :: SmtpProtocol
  , _mailServerView_userName :: Text
  , _mailServerView_enabled :: Bool
  , _mailServerView_notificatees :: [Email]
  } deriving (Eq, Ord, Generic, Typeable, Read, Show)
instance FromJSON MailServerView
instance ToJSON MailServerView

type BakerErrorLogView = DSum BakerLogTag Identity
type NodeErrorLogView = DSum NodeLogTag Identity
type ErrorLogView = DSum LogTag Identity

nodeErrorViewOnly :: ErrorLogView -> Maybe NodeErrorLogView
nodeErrorViewOnly = \case
  LogTag_Node nlt :=> v -> Just $ nlt :=> v
  _ -> Nothing

nodeIdForNodeErrorLogView :: NodeErrorLogView -> Id Node
nodeIdForNodeErrorLogView (tag :=> Identity v) = ($ v) $ case tag of
  NodeLogTag_InaccessibleNode -> _errorLogInaccessibleNode_node
  NodeLogTag_NodeWrongChain -> _errorLogNodeWrongChain_node
  NodeLogTag_NodeInsufficientPeers -> _errorLogNodeInsufficientPeers_node
  NodeLogTag_NodeInvalidPeerCount -> _errorLogNodeInvalidPeerCount_node
  NodeLogTag_BadNodeHead -> _errorLogBadNodeHead_node

bakerErrorViewOnly :: ErrorLogView -> Maybe BakerErrorLogView
bakerErrorViewOnly = \case
  LogTag_Baker blt :=> v -> Just $ blt :=> v
  _ -> Nothing

bakerIdForBakerErrorLogView :: BakerErrorLogView -> PublicKeyHash
bakerIdForBakerErrorLogView (tag :=> Identity v) = bakerIdForBakerLogTag tag v

bakerIdForBakerLogTag :: BakerLogTag t -> t -> PublicKeyHash
bakerIdForBakerLogTag = \case
  BakerLogTag_BakerLedgerDisconnected -> unId . _errorLogBakerLedgerDisconnected_baker
  BakerLogTag_BakerMissed -> unId . _errorLogBakerMissed_baker
  BakerLogTag_MissedEndorsementBonus -> unId . _errorLogBakerMissedEndorsementBonus_baker
  BakerLogTag_BakerDeactivated -> _errorLogBakerDeactivated_publicKeyHash
  BakerLogTag_BakerDeactivationRisk -> _errorLogBakerDeactivationRisk_publicKeyHash
  BakerLogTag_BakerAccused -> unId . _errorLogBakerAccused_baker
  BakerLogTag_InsufficientFunds -> unId . _errorLogInsufficientFunds_baker
  BakerLogTag_VotingReminder -> unId . _errorLogVotingReminder_baker

errorLogIdForBakerLogTag :: BakerLogTag t -> t -> Id ErrorLog
errorLogIdForBakerLogTag = \case
  BakerLogTag_BakerLedgerDisconnected -> _errorLogBakerLedgerDisconnected_log
  BakerLogTag_BakerMissed -> _errorLogBakerMissed_log
  BakerLogTag_MissedEndorsementBonus -> _errorLogBakerMissedEndorsementBonus_log
  BakerLogTag_BakerDeactivated -> _errorLogBakerDeactivated_log
  BakerLogTag_BakerDeactivationRisk -> _errorLogBakerDeactivationRisk_log
  BakerLogTag_BakerAccused -> _errorLogBakerAccused_log
  BakerLogTag_InsufficientFunds -> _errorLogInsufficientFunds_log
  BakerLogTag_VotingReminder -> _errorLogVotingReminder_log

errorLogIdForNodeLogTag :: NodeLogTag t -> t -> Id ErrorLog
errorLogIdForNodeLogTag = \case
  NodeLogTag_InaccessibleNode -> _errorLogInaccessibleNode_log
  NodeLogTag_NodeWrongChain -> _errorLogNodeWrongChain_log
  NodeLogTag_BadNodeHead -> _errorLogBadNodeHead_log
  NodeLogTag_NodeInsufficientPeers -> _errorLogNodeInsufficientPeers_log
  NodeLogTag_NodeInvalidPeerCount -> _errorLogNodeInvalidPeerCount_log

errorLogIdForErrorLogView :: ErrorLogView -> Id ErrorLog
errorLogIdForErrorLogView (tag :=> Identity v) = ($ v) $ case tag of
  LogTag_Node nlt -> errorLogIdForNodeLogTag nlt
  LogTag_Baker blt -> errorLogIdForBakerLogTag blt
  LogTag_BakerNoHeartbeat -> _errorLogBakerNoHeartbeat_log
  LogTag_NetworkUpdate -> _errorLogNetworkUpdate_log
  LogTag_InternalNodeFailed -> _errorLogInternalNodeFailed_log

mailServerConfigToView :: MailServerConfig -> [Email] -> MailServerView
mailServerConfigToView x ns = MailServerView
  { _mailServerView_hostName = _mailServerConfig_hostName x
  , _mailServerView_portNumber = _mailServerConfig_portNumber x
  , _mailServerView_smtpProtocol = _mailServerConfig_smtpProtocol x
  , _mailServerView_userName = _mailServerConfig_userName x
  , _mailServerView_enabled = _mailServerConfig_enabled x
  , _mailServerView_notificatees = ns
  }

cropBakeView :: (Semigroup a) => BakeViewSelector a -> BakeView b -> BakeView a
cropBakeView vs v = BakeView
  { _bakeView_config = cropView (_bakeViewSelector_config vs) (_bakeView_config v)
  , _bakeView_parameters = cropView (_bakeViewSelector_parameters vs) (_bakeView_parameters v)
  , _bakeView_nodeAddresses = cropView (_bakeViewSelector_nodeAddresses vs) (_bakeView_nodeAddresses v)
  , _bakeView_nodeVersions = cropView (_bakeViewSelector_nodeVersions vs) (_bakeView_nodeVersions v)
  , _bakeView_nodeDetails = cropView (_bakeViewSelector_nodeDetails vs) (_bakeView_nodeDetails v)
  , _bakeView_latestTezosRelease = cropView (_bakeViewSelector_latestTezosRelease vs) (_bakeView_latestTezosRelease v)
  , _bakeView_bakerAddresses = cropView (_bakeViewSelector_bakerAddresses vs) (_bakeView_bakerAddresses v)
  , _bakeView_bakerAlerts = cropView (_bakeViewSelector_bakerAlerts vs) (_bakeView_bakerAlerts v)
  , _bakeView_bakerDetails = cropView (_bakeViewSelector_bakerDetails vs) (_bakeView_bakerDetails v)
  , _bakeView_mailServer = cropView (_bakeViewSelector_mailServer vs) (_bakeView_mailServer v)
  , _bakeView_errors = MMap.intersectionWith cropView (_bakeViewSelector_errors vs) (_bakeView_errors v)
  , _bakeView_latestHead = cropView (_bakeViewSelector_latestHead vs) (_bakeView_latestHead v)
  , _bakeView_amendment = cropView (_bakeViewSelector_amendment vs) (_bakeView_amendment v)
  , _bakeView_proposals = cropView (_bakeViewSelector_proposals vs) (_bakeView_proposals v)
  , _bakeView_bakerVote = cropView (_bakeViewSelector_bakerVote vs) (_bakeView_bakerVote v)
  , _bakeView_periodTestingVote = cropView (_bakeViewSelector_periodTestingVote vs) (_bakeView_periodTestingVote v)
  , _bakeView_periodTesting = cropView (_bakeViewSelector_periodTesting vs) (_bakeView_periodTesting v)
  , _bakeView_periodPromotionVote = cropView (_bakeViewSelector_periodPromotionVote vs) (_bakeView_periodPromotionVote v)
  , _bakeView_periodAdoption = cropView (_bakeViewSelector_periodAdoption vs) (_bakeView_periodAdoption v)
  , _bakeView_upstreamVersion = cropView (_bakeViewSelector_upstreamVersion vs) (_bakeView_upstreamVersion v)
  , _bakeView_telegramConfig = cropView (_bakeViewSelector_telegramConfig vs) (_bakeView_telegramConfig v)
  , _bakeView_telegramRecipients = cropView (_bakeViewSelector_telegramRecipients vs) (_bakeView_telegramRecipients v)
  , _bakeView_alertCount = cropView (_bakeViewSelector_alertCount vs) (_bakeView_alertCount v)
  , _bakeView_snapshotMeta = cropView (_bakeViewSelector_snapshotMeta vs) (_bakeView_snapshotMeta v)
  , _bakeView_connectedLedger = cropView (_bakeViewSelector_connectedLedger vs) (_bakeView_connectedLedger v)
  , _bakeView_showLedger = cropView (_bakeViewSelector_showLedger vs) (_bakeView_showLedger v)
  , _bakeView_prompting = cropView (_bakeViewSelector_prompting vs) (_bakeView_prompting v)
  , _bakeView_votePrompting = cropView (_bakeViewSelector_votePrompting vs) (_bakeView_votePrompting v)
  , _bakeView_rightNotificationSettings = cropView (_bakeViewSelector_rightNotificationSettings vs) (_bakeView_rightNotificationSettings v)
  , _bakeView_bakerRegistered = cropView (_bakeViewSelector_bakerRegistered vs) (_bakeView_bakerRegistered v)
  }

instance Filterable BakeViewSelector where
  mapMaybe f a = BakeViewSelector
    { _bakeViewSelector_config = mapMaybe f $ _bakeViewSelector_config a
    , _bakeViewSelector_parameters = mapMaybe f $ _bakeViewSelector_parameters a
    , _bakeViewSelector_nodeDetails = mapMaybe f $ _bakeViewSelector_nodeDetails a
    , _bakeViewSelector_latestTezosRelease = mapMaybe f $ _bakeViewSelector_latestTezosRelease a
    , _bakeViewSelector_bakerAddresses = mapMaybe f $ _bakeViewSelector_bakerAddresses a
    , _bakeViewSelector_bakerAlerts = mapMaybe f $ _bakeViewSelector_bakerAlerts a
    , _bakeViewSelector_bakerDetails = mapMaybe f $ _bakeViewSelector_bakerDetails a
    , _bakeViewSelector_mailServer = mapMaybe f $ _bakeViewSelector_mailServer a
    , _bakeViewSelector_nodeAddresses = mapMaybe f $ _bakeViewSelector_nodeAddresses a
    , _bakeViewSelector_nodeVersions = mapMaybe f $ _bakeViewSelector_nodeVersions a
    , _bakeViewSelector_errors = (fmap.mapMaybe) f $ _bakeViewSelector_errors a
    , _bakeViewSelector_latestHead = mapMaybe f $ _bakeViewSelector_latestHead a
    , _bakeViewSelector_amendment = mapMaybe f $ _bakeViewSelector_amendment a
    , _bakeViewSelector_proposals = mapMaybe f $ _bakeViewSelector_proposals a
    , _bakeViewSelector_bakerVote = mapMaybe f $ _bakeViewSelector_bakerVote a
    , _bakeViewSelector_periodTestingVote = mapMaybe f $ _bakeViewSelector_periodTestingVote a
    , _bakeViewSelector_periodTesting = mapMaybe f $ _bakeViewSelector_periodTesting a
    , _bakeViewSelector_periodPromotionVote = mapMaybe f $ _bakeViewSelector_periodPromotionVote a
    , _bakeViewSelector_periodAdoption = mapMaybe f $ _bakeViewSelector_periodAdoption a
    , _bakeViewSelector_upstreamVersion = mapMaybe f $ _bakeViewSelector_upstreamVersion a
    , _bakeViewSelector_telegramConfig = mapMaybe f (_bakeViewSelector_telegramConfig a)
    , _bakeViewSelector_telegramRecipients = mapMaybe f (_bakeViewSelector_telegramRecipients a)
    , _bakeViewSelector_alertCount = mapMaybe f (_bakeViewSelector_alertCount a)
    , _bakeViewSelector_snapshotMeta = mapMaybe f (_bakeViewSelector_snapshotMeta a)
    , _bakeViewSelector_connectedLedger = mapMaybe f (_bakeViewSelector_connectedLedger a)
    , _bakeViewSelector_showLedger = mapMaybe f (_bakeViewSelector_showLedger a)
    , _bakeViewSelector_prompting = mapMaybe f (_bakeViewSelector_prompting a)
    , _bakeViewSelector_votePrompting = mapMaybe f (_bakeViewSelector_votePrompting a)
    , _bakeViewSelector_rightNotificationSettings = mapMaybe f $ _bakeViewSelector_rightNotificationSettings a
    , _bakeViewSelector_bakerRegistered = mapMaybe f $ _bakeViewSelector_bakerRegistered a
    }

instance Filterable BakeView where
  mapMaybe f a = BakeView
    { _bakeView_config = mapMaybe f $ _bakeView_config a
    , _bakeView_parameters = mapMaybe f $ _bakeView_parameters a
    , _bakeView_nodeDetails = mapMaybe f $ _bakeView_nodeDetails a
    , _bakeView_latestTezosRelease = mapMaybe f $ _bakeView_latestTezosRelease a
    , _bakeView_bakerAddresses = mapMaybe f $ _bakeView_bakerAddresses a
    , _bakeView_bakerAlerts = mapMaybe f $ _bakeView_bakerAlerts a
    , _bakeView_bakerDetails = mapMaybe f $ _bakeView_bakerDetails a
    , _bakeView_mailServer = mapMaybe f $ _bakeView_mailServer a
    , _bakeView_nodeAddresses = mapMaybe f $ _bakeView_nodeAddresses a
    , _bakeView_nodeVersions = mapMaybe f $ _bakeView_nodeVersions a
    , _bakeView_errors = (fmap.mapMaybe) f $ _bakeView_errors a
    , _bakeView_latestHead = mapMaybe f $ _bakeView_latestHead a
    , _bakeView_amendment = mapMaybe f $ _bakeView_amendment a
    , _bakeView_proposals = mapMaybe f $ _bakeView_proposals a
    , _bakeView_bakerVote = mapMaybe f $ _bakeView_bakerVote a
    , _bakeView_periodTestingVote = mapMaybe f $ _bakeView_periodTestingVote a
    , _bakeView_periodTesting = mapMaybe f $ _bakeView_periodTesting a
    , _bakeView_periodPromotionVote = mapMaybe f $ _bakeView_periodPromotionVote a
    , _bakeView_periodAdoption = mapMaybe f $ _bakeView_periodAdoption a
    , _bakeView_upstreamVersion = mapMaybe f $ _bakeView_upstreamVersion a
    , _bakeView_telegramConfig = mapMaybe f $ _bakeView_telegramConfig a
    , _bakeView_telegramRecipients = mapMaybe f $ _bakeView_telegramRecipients a
    , _bakeView_alertCount = mapMaybe f $ _bakeView_alertCount a
    , _bakeView_snapshotMeta = mapMaybe f $ _bakeView_snapshotMeta a
    , _bakeView_connectedLedger = mapMaybe f $ _bakeView_connectedLedger a
    , _bakeView_showLedger = mapMaybe f $ _bakeView_showLedger a
    , _bakeView_prompting = mapMaybe f $ _bakeView_prompting a
    , _bakeView_votePrompting = mapMaybe f $ _bakeView_votePrompting a
    , _bakeView_rightNotificationSettings = mapMaybe f $ _bakeView_rightNotificationSettings a
    , _bakeView_bakerRegistered = mapMaybe f $ _bakeView_bakerRegistered a
    }

mapMaybeSnd :: Filterable f => (a -> Maybe b) -> f (e, a) -> f (e, b)
mapMaybeSnd f = mapMaybe $ \(e, a) -> case f a of
  Nothing -> Nothing
  Just b -> Just (e, b)

instance Semigroup a => Semigroup (BakeViewSelector a) where
  u <> v = BakeViewSelector
    { _bakeViewSelector_config = (<>) (_bakeViewSelector_config u) (_bakeViewSelector_config v)
    , _bakeViewSelector_parameters = (<>) (_bakeViewSelector_parameters u) (_bakeViewSelector_parameters v)
    , _bakeViewSelector_nodeDetails = (<>) (_bakeViewSelector_nodeDetails u) (_bakeViewSelector_nodeDetails v)
    , _bakeViewSelector_latestTezosRelease = (<>) (_bakeViewSelector_latestTezosRelease u) (_bakeViewSelector_latestTezosRelease v)
    , _bakeViewSelector_bakerAddresses = (<>) (_bakeViewSelector_bakerAddresses u) (_bakeViewSelector_bakerAddresses v)
    , _bakeViewSelector_bakerAlerts = (<>) (_bakeViewSelector_bakerAlerts u) (_bakeViewSelector_bakerAlerts v)
    , _bakeViewSelector_bakerDetails = (<>) (_bakeViewSelector_bakerDetails u) (_bakeViewSelector_bakerDetails v)
    , _bakeViewSelector_mailServer = (<>) (_bakeViewSelector_mailServer u) (_bakeViewSelector_mailServer v)
    , _bakeViewSelector_nodeAddresses = (<>) (_bakeViewSelector_nodeAddresses u) (_bakeViewSelector_nodeAddresses v)
    , _bakeViewSelector_nodeVersions = (<>) (_bakeViewSelector_nodeVersions u) (_bakeViewSelector_nodeVersions v)
    , _bakeViewSelector_errors = (<>) (_bakeViewSelector_errors u) (_bakeViewSelector_errors v)
    , _bakeViewSelector_latestHead = (<>) (_bakeViewSelector_latestHead u) (_bakeViewSelector_latestHead v)
    , _bakeViewSelector_amendment = (<>) (_bakeViewSelector_amendment u) (_bakeViewSelector_amendment v)
    , _bakeViewSelector_proposals = (<>) (_bakeViewSelector_proposals u) (_bakeViewSelector_proposals v)
    , _bakeViewSelector_bakerVote = (<>) (_bakeViewSelector_bakerVote u) (_bakeViewSelector_bakerVote v)
    , _bakeViewSelector_periodTestingVote = (<>) (_bakeViewSelector_periodTestingVote u) (_bakeViewSelector_periodTestingVote v)
    , _bakeViewSelector_periodTesting = (<>) (_bakeViewSelector_periodTesting u) (_bakeViewSelector_periodTesting v)
    , _bakeViewSelector_periodPromotionVote = (<>) (_bakeViewSelector_periodPromotionVote u) (_bakeViewSelector_periodPromotionVote v)
    , _bakeViewSelector_periodAdoption = (<>) (_bakeViewSelector_periodAdoption u) (_bakeViewSelector_periodAdoption v)
    , _bakeViewSelector_upstreamVersion = (<>) (_bakeViewSelector_upstreamVersion u) (_bakeViewSelector_upstreamVersion v)
    , _bakeViewSelector_telegramConfig = (<>) (_bakeViewSelector_telegramConfig u) (_bakeViewSelector_telegramConfig v)
    , _bakeViewSelector_telegramRecipients = (<>) (_bakeViewSelector_telegramRecipients u) (_bakeViewSelector_telegramRecipients v)
    , _bakeViewSelector_alertCount = (<>) (_bakeViewSelector_alertCount u) (_bakeViewSelector_alertCount v)
    , _bakeViewSelector_snapshotMeta = (<>) (_bakeViewSelector_snapshotMeta u) (_bakeViewSelector_snapshotMeta v)
    , _bakeViewSelector_connectedLedger = (<>) (_bakeViewSelector_connectedLedger u) (_bakeViewSelector_connectedLedger v)
    , _bakeViewSelector_showLedger = (<>) (_bakeViewSelector_showLedger u) (_bakeViewSelector_showLedger v)
    , _bakeViewSelector_prompting = (<>) (_bakeViewSelector_prompting u) (_bakeViewSelector_prompting v)
    , _bakeViewSelector_votePrompting = (<>) (_bakeViewSelector_votePrompting u) (_bakeViewSelector_votePrompting v)
    , _bakeViewSelector_rightNotificationSettings = (<>) (_bakeViewSelector_rightNotificationSettings u) (_bakeViewSelector_rightNotificationSettings v)
    , _bakeViewSelector_bakerRegistered = (<>) (_bakeViewSelector_bakerRegistered u) (_bakeViewSelector_bakerRegistered v)
    }

instance (Semigroup a, Monoid a) => Monoid (BakeViewSelector a) where
  mempty = BakeViewSelector
    { _bakeViewSelector_config = mempty
    , _bakeViewSelector_parameters = mempty
    , _bakeViewSelector_nodeDetails = mempty
    , _bakeViewSelector_latestTezosRelease = mempty
    , _bakeViewSelector_bakerAddresses = mempty
    , _bakeViewSelector_bakerAlerts = mempty
    , _bakeViewSelector_bakerDetails = mempty
    , _bakeViewSelector_mailServer = mempty
    , _bakeViewSelector_nodeAddresses = mempty
    , _bakeViewSelector_nodeVersions = mempty
    , _bakeViewSelector_errors = mempty
    , _bakeViewSelector_latestHead = mempty
    , _bakeViewSelector_amendment = mempty
    , _bakeViewSelector_proposals = mempty
    , _bakeViewSelector_bakerVote = mempty
    , _bakeViewSelector_periodTestingVote = mempty
    , _bakeViewSelector_periodTesting = mempty
    , _bakeViewSelector_periodPromotionVote = mempty
    , _bakeViewSelector_periodAdoption = mempty
    , _bakeViewSelector_upstreamVersion = mempty
    , _bakeViewSelector_telegramConfig = mempty
    , _bakeViewSelector_telegramRecipients = mempty
    , _bakeViewSelector_alertCount = mempty
    , _bakeViewSelector_snapshotMeta = mempty
    , _bakeViewSelector_connectedLedger = mempty
    , _bakeViewSelector_showLedger = mempty
    , _bakeViewSelector_prompting = mempty
    , _bakeViewSelector_votePrompting = mempty
    , _bakeViewSelector_rightNotificationSettings = mempty
    , _bakeViewSelector_bakerRegistered = mempty
    }


instance Group a => Group (BakeViewSelector a) where
  negateG = fmap negateG

instance (Semigroup a, Monoid a) => Monoid (BakeView a) where
  mempty = BakeView
    { _bakeView_config = mempty
    , _bakeView_parameters = mempty
    , _bakeView_nodeDetails = mempty
    , _bakeView_latestTezosRelease = mempty
    , _bakeView_bakerAddresses = mempty
    , _bakeView_bakerAlerts = mempty
    , _bakeView_bakerDetails = mempty
    , _bakeView_mailServer = mempty
    -- , _bakeView_graphs = mempty
    -- , _bakeView_summaryGraph = mempty
    , _bakeView_nodeAddresses = mempty
    , _bakeView_nodeVersions = mempty
    , _bakeView_errors = mempty
    , _bakeView_latestHead = mempty
    , _bakeView_amendment = mempty
    , _bakeView_proposals = mempty
    , _bakeView_bakerVote = mempty
    , _bakeView_periodTestingVote = mempty
    , _bakeView_periodTesting = mempty
    , _bakeView_periodPromotionVote = mempty
    , _bakeView_periodAdoption = mempty
    , _bakeView_upstreamVersion = mempty
    , _bakeView_telegramConfig = mempty
    , _bakeView_telegramRecipients = mempty
    , _bakeView_alertCount = mempty
    , _bakeView_snapshotMeta = mempty
    , _bakeView_connectedLedger = mempty
    , _bakeView_showLedger = mempty
    , _bakeView_prompting = mempty
    , _bakeView_votePrompting = mempty
    , _bakeView_rightNotificationSettings = mempty
    , _bakeView_bakerRegistered = mempty
    }

instance Semigroup a => Semigroup (BakeView a) where
  u <> v = BakeView
    { _bakeView_config = _bakeView_config u <> _bakeView_config v
    , _bakeView_parameters = _bakeView_parameters u <> _bakeView_parameters v
    , _bakeView_nodeDetails = _bakeView_nodeDetails u <> _bakeView_nodeDetails v
    , _bakeView_latestTezosRelease = _bakeView_latestTezosRelease u <> _bakeView_latestTezosRelease v
    , _bakeView_bakerAddresses = _bakeView_bakerAddresses u <> _bakeView_bakerAddresses v
    , _bakeView_bakerAlerts = _bakeView_bakerAlerts u <> _bakeView_bakerAlerts v
    , _bakeView_bakerDetails = _bakeView_bakerDetails u <> _bakeView_bakerDetails v
    , _bakeView_mailServer = _bakeView_mailServer u <> _bakeView_mailServer v
    -- , _bakeView_summaryGraph = _bakeView_summaryGraph u <> _bakeView_summaryGraph v
    -- , _bakeView_graphs = _bakeView_graphs u <> _bakeView_graphs v
    , _bakeView_nodeAddresses = _bakeView_nodeAddresses u <> _bakeView_nodeAddresses v
    , _bakeView_nodeVersions = _bakeView_nodeVersions u <> _bakeView_nodeVersions v
    , _bakeView_errors = _bakeView_errors u <> _bakeView_errors v
    , _bakeView_latestHead = _bakeView_latestHead u <> _bakeView_latestHead v
    , _bakeView_amendment = _bakeView_amendment u <> _bakeView_amendment v
    , _bakeView_proposals = _bakeView_proposals u <> _bakeView_proposals v
    , _bakeView_bakerVote = _bakeView_bakerVote u <> _bakeView_bakerVote v
    , _bakeView_periodTestingVote = _bakeView_periodTestingVote u <> _bakeView_periodTestingVote v
    , _bakeView_periodTesting = _bakeView_periodTesting u <> _bakeView_periodTesting v
    , _bakeView_periodPromotionVote = _bakeView_periodPromotionVote u <> _bakeView_periodPromotionVote v
    , _bakeView_periodAdoption = _bakeView_periodAdoption u <> _bakeView_periodAdoption v
    , _bakeView_upstreamVersion = _bakeView_upstreamVersion u <> _bakeView_upstreamVersion v
    , _bakeView_telegramConfig = _bakeView_telegramConfig u <> _bakeView_telegramConfig v
    , _bakeView_telegramRecipients = _bakeView_telegramRecipients u <> _bakeView_telegramRecipients v
    , _bakeView_alertCount = _bakeView_alertCount u <> _bakeView_alertCount v
    , _bakeView_snapshotMeta = _bakeView_snapshotMeta u <> _bakeView_snapshotMeta v
    , _bakeView_connectedLedger = _bakeView_connectedLedger u <> _bakeView_connectedLedger v
    , _bakeView_showLedger = _bakeView_showLedger u <> _bakeView_showLedger v
    , _bakeView_prompting = _bakeView_prompting u <> _bakeView_prompting v
    , _bakeView_votePrompting = _bakeView_votePrompting u <> _bakeView_votePrompting v
    , _bakeView_rightNotificationSettings = _bakeView_rightNotificationSettings u <> _bakeView_rightNotificationSettings v
    , _bakeView_bakerRegistered = _bakeView_bakerRegistered u <> _bakeView_bakerRegistered v
    }

instance (Monoid a, Semigroup a) => Query (BakeViewSelector a) where
  type QueryResult (BakeViewSelector a) = BakeView a
  crop = cropBakeView

instance (Semigroup a, FromJSON a) => FromJSON (BakeViewSelector a)
instance (Semigroup a, FromJSON a) => FromJSON (BakeView a)

instance ToJSON a => ToJSON (BakeViewSelector a)
instance (Semigroup a, ToJSON a) => ToJSON (BakeView a)

fmap concat $ sequence $ concat
  [ map makeLenses
    [ 'BakeView
    , 'BakeViewSelector
    , 'MailServerView
    , 'NodeSummary
    ]
  ]
