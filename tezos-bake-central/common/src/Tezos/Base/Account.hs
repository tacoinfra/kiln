{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tezos.Base.Account where

import Control.DeepSeq (NFData)
import Control.Applicative
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON(..), withObject, object, (.=), (.:), (.:?))
import Data.Bits (Bits)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Typeable
import Data.Word
import qualified Data.Sequence as Seq
import GHC.Generics

import qualified Tezos.Common.Binary as B
import Tezos.Common.Json
import Tezos.Common.Level
import Tezos.Common.PublicKey
import Tezos.Common.PublicKeyHash (PublicKeyHash)
import Tezos.Common.Tez (Tez)

data Account = Account
  { _account_delegate :: Maybe PublicKeyHash --  { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _account_balance :: Tez -- "2052452947621" "balance": { "$ref": "#/definitions/mutez" },
  , _account_counter :: Maybe TezosWord64 -- 1540 "counter": { "$ref": "#/definitions/positive_bignum" }
  } deriving (Show, Eq, Ord, Typeable)

type ManagerKey = Maybe PublicKey

concat <$> traverse deriveTezosFromJson
  [ ''Account
  ]

concat <$> traverse makeLenses
  [ 'Account
  ]
newtype Round = Round { unRound :: Word16 }
  deriving (Eq, Ord, Generic, Typeable, Show, FromJSON, ToJSON, NFData, Hashable, Enum, Num, Integral, Real, Bits, B.TezosBinary)

data EndorsingRights = EndorsingRights
  { _endorsingRights_level :: RawLevel
  , _endorsingRights_delegates :: Seq.Seq EndorsingRightsDelegateInfo
  , _endorsingRights_estimatedTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

data BakingRights = BakingRights
  { _bakingRights_level :: RawLevel
  , _bakingRights_delegate :: PublicKeyHash
  , _bakingRights_round :: Round
  , _bakingRights_estimatedTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

data EndorsingRightsDelegateInfo = EndorsingRightsDelegateInfo
  { _endorsingRightsDelegateInfo_delegate :: PublicKeyHash
  , _endorsingRightsDelegateInfo_firstSlot :: Word16
  , _endorsingRightsDelegateInfo_endorsingPower :: Word16
  } deriving (Eq, Ord, Show)

instance FromJSON EndorsingRightsDelegateInfo where
  parseJSON = withObject "EndorsingRightsDelegateInfo" $ \o -> do
    _endorsingRightsDelegateInfo_delegate        <- o .: "delegate"
    _endorsingRightsDelegateInfo_firstSlot       <- o .: "first_slot"
    _endorsingRightsDelegateInfo_endorsingPower  <- o .: "endorsing_power" <|> o .: "attestation_power"
    pure $ EndorsingRightsDelegateInfo {..}

instance ToJSON EndorsingRightsDelegateInfo where
  toJSON EndorsingRightsDelegateInfo {..} =
    object
      [ "delegate" .= _endorsingRightsDelegateInfo_delegate
      , "first_slot" .= _endorsingRightsDelegateInfo_firstSlot
      , "endorsing_power" .= _endorsingRightsDelegateInfo_endorsingPower
      , "attestation_power" .= _endorsingRightsDelegateInfo_endorsingPower
      ]

data PendingConsensusKey = PendingConsensusKey
  { _pendingConsensusKey_cycle :: Cycle
  , _pendingConsensusKey_pkh :: PublicKeyHash
  } deriving (Eq, Ord, Show)

data DelegateInfo = DelegateInfo
  { _delegateInfo_fullBalance           :: Tez
  , _delegateInfo_currentFrozenDeposits :: Tez
  , _delegateInfo_frozenDeposits        :: Tez
  , _delegateInfo_stakingBalance        :: Tez
  , _delegateInfo_delegatedBalance      :: Tez
  , _delegateInfo_deactivated           :: Bool
  , _delegateInfo_gracePeriod           :: Cycle
  , _delegateInfo_activeConsensusKey    :: PublicKeyHash
  , _delegateInfo_pendingConsensusKeys  :: [PendingConsensusKey]
  }

instance FromJSON DelegateInfo where
  parseJSON = withObject "DelegateInfo" $ \o -> do
    _delegateInfo_fullBalance           <- o .: "full_balance"
    _delegateInfo_currentFrozenDeposits <- o .: "current_frozen_deposits"
    _delegateInfo_frozenDeposits        <- o .: "frozen_deposits"
    _delegateInfo_stakingBalance        <- o .: "staking_balance"
    _delegateInfo_delegatedBalance      <- o .: "delegated_balance"
    _delegateInfo_deactivated           <- o .: "deactivated"
    _delegateInfo_gracePeriod           <- o .: "grace_period"
    _delegateInfo_activeConsensusKey    <- o .: "active_consensus_key"

    mbPendingConsensusKeys <- o .:? "pending_consensus_keys"
    let _delegateInfo_pendingConsensusKeys = fromMaybe [] mbPendingConsensusKeys

    pure $ DelegateInfo {..}


data ParticipationInfo = ParticipationInfo
  { _participationInfo_expectedCycleActivity       :: Word32
  , _participationInfo_minimalCycleActivity        :: Word32
  , _participationInfo_missedSlots                 :: Word32
  , _participationInfo_missedLevels                :: Word32
  , _participationInfo_remainingAllowedMissedSlots :: Word32
  , _participationInfo_expectedEndorsingRewards    :: Tez
  } deriving (Eq, Ord, Show)

instance FromJSON ParticipationInfo where
  parseJSON = withObject "ParticipationInfo" $ \o -> do
    _participationInfo_expectedCycleActivity <- o .: "expected_cycle_activity"
    _participationInfo_minimalCycleActivity <- o .: "minimal_cycle_activity"
    _participationInfo_missedSlots <- o .: "missed_slots"
    _participationInfo_missedLevels <- o .: "missed_levels"
    _participationInfo_remainingAllowedMissedSlots <- o .: "remaining_allowed_missed_slots"
    _participationInfo_expectedEndorsingRewards <- o .: "expected_endorsing_rewards" <|> o .: "expected_attesting_rewards"
    pure $ ParticipationInfo {..}

instance ToJSON ParticipationInfo where
  toJSON ParticipationInfo {..} =
    object
      [ "expected_cycle_activity" .= _participationInfo_expectedCycleActivity
      , "minimal_cycle_activity" .= _participationInfo_minimalCycleActivity
      , "missed_slots" .= _participationInfo_missedSlots
      , "missed_levels" .= _participationInfo_missedLevels
      , "remaining_allowed_missed_slots" .= _participationInfo_remainingAllowedMissedSlots
      , "expected_endorsing_rewards" .= _participationInfo_expectedEndorsingRewards
      , "expected_attesting_rewards" .= _participationInfo_expectedEndorsingRewards
      ]

data DelegateParameters = DelegateParameters
  { _delegateParameters_limitOfStakingOverBakingMillionth :: Int
  , _delegateParameters_edgeOfBakingOverStakingBillionth :: Int
  } deriving (Eq, Ord, Show)

concat <$> traverse deriveTezosFromJson
  [ ''BakingRights
  ]

concat <$> traverse deriveTezosJson
  [ ''EndorsingRights
  , ''DelegateParameters
  , ''PendingConsensusKey
  ]

concat <$> traverse makeLenses
  [ 'BakingRights
  , 'DelegateInfo
  , 'DelegateParameters
  , 'EndorsingRights
  , 'EndorsingRightsDelegateInfo
  , 'ParticipationInfo
  , 'PendingConsensusKey
  ]
