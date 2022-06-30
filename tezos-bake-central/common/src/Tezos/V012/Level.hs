{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.V012.Level where

import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses)
import Data.Hashable (Hashable)
import Data.Typeable
import GHC.Generics (Generic)

import Tezos.Common.Json
import Tezos.Common.Level

-- | "level": {
data LevelInfo = LevelInfo
  { _levelInfo_cycle :: Cycle --  "cycle": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _levelInfo_cyclePosition :: RawLevel --  "cycle_position": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _levelInfo_expectedCommitment :: Bool --  "expected_commitment": { "type": "boolean" }
  , _levelInfo_level :: RawLevel --  "level": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _levelInfo_levelPosition :: RawLevel --  "level_position": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  } deriving (Show, Eq, Ord, Typeable, Generic)
instance Hashable LevelInfo
instance NFData LevelInfo

deriveTezosFromJson ''LevelInfo
makeLenses 'LevelInfo
