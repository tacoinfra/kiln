{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.Level where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Typeable
import GHC.Generics (Generic)

import Tezos.Json

-- Units of blocks, not neccesarily absolute level above genesis block
newtype RawLevel = RawLevel {unRawLevel :: Int}
  deriving (Show, Eq, Ord, Typeable, Num, Real, Integral, Enum, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Generic, NFData, Hashable)

-- Units of blocksPerCycle, not neccesarily absolute level above genesis block
newtype Cycle = Cycle {unCycle :: Int}
  deriving (Show, Eq, Ord, Typeable, Num, Real, Integral, Enum, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Generic, NFData, Hashable)

-- | "level": {
data Level = Level
  { _level_cycle :: Cycle --  "cycle": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _level_cyclePosition :: RawLevel --  "cycle_position": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _level_expectedCommitment :: Bool --  "expected_commitment": { "type": "boolean" }
  , _level_level :: RawLevel --  "level": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _level_levelPosition :: RawLevel --  "level_position": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _level_votingPeriod :: RawLevel --  "voting_period": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _level_votingPeriodPosition :: RawLevel --  "voting_period_position": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  } deriving (Show, Eq, Ord, Typeable, Generic)
instance Hashable Level
instance NFData Level

deriveTezosJson ''Level
