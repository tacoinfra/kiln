{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tezos.V014.Account where

import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bits (Bits)
import Data.Hashable (Hashable)
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

data DelegateInfo = DelegateInfo
  { _delegateInfo_fullBalance           :: Tez
  , _delegateInfo_currentFrozenDeposits :: Tez
  , _delegateInfo_frozenDeposits        :: Tez
  , _delegateInfo_stakingBalance        :: Tez
  , _delegateInfo_delegatedBalance      :: Tez
  , _delegateInfo_deactivated           :: Bool
  , _delegateInfo_gracePeriod           :: Cycle
  }

data ParticipationInfo = ParticipationInfo
  { _participationInfo_expectedCycleActivity       :: Word32
  , _participationInfo_minimalCycleActivity        :: Word32
  , _participationInfo_missedSlots                 :: Word32
  , _participationInfo_missedLevels                :: Word32
  , _participationInfo_remainingAllowedMissedSlots :: Word32
  , _participationInfo_expectedEndorsingRewards    :: Tez
  } deriving (Eq, Ord, Show)

concat <$> traverse deriveTezosFromJson
  [ ''BakingRights
  , ''DelegateInfo
  ]

concat <$> traverse deriveTezosJson
  [ ''EndorsingRights
  , ''EndorsingRightsDelegateInfo
  , ''ParticipationInfo
  ]

concat <$> traverse makeLenses
  [ 'BakingRights
  , 'DelegateInfo
  , 'EndorsingRights
  , 'EndorsingRightsDelegateInfo
  , 'ParticipationInfo
  ]
