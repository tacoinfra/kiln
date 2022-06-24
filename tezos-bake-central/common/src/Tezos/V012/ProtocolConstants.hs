{-# LANGUAGE DeriveGeneric #-}
{-# Language LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wwarn #-}

module Tezos.V012.ProtocolConstants where

import Control.DeepSeq (NFData)
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Data.Hashable (Hashable)
import Data.Typeable (Typeable)
import Data.Time (NominalDiffTime)
import qualified Data.Time as Time
import GHC.Generics (Generic)

import Tezos.Common.Block
import Tezos.Common.Json
import Tezos.Common.Level
import Tezos.Common.Tez
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

-- | Datatype that represents all used protocol constants.
-- Some of the fields are @Maybe@s for cross-compatibility with old protocols.
data ProtoInfo = ProtoInfo
  { _protoInfo_preservedCycles :: Cycle -- "preserved_cycles": { "type": "integer", "minimum": 0, "maximum": 255 },
  , _protoInfo_blocksPerCycle :: RawLevel -- "blocks_per_cycle": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _protoInfo_blocksPerVotingPeriod :: RawLevel -- "blocks_per_voting_period": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },

  , _protoInfo_tokensPerRoll :: Tez -- "tokens_per_roll": { "$ref": "#/definitions/mutez" },

  , _protoInfo_minimalBlockDelay :: TezosWord64 -- "minimal_block_delay": { "$ref": "#/definitions/int64" }
  } deriving (Eq, Ord, Show, Typeable, Generic)
instance Hashable ProtoInfo
instance NFData ProtoInfo

deriveTezosJson ''ProtoInfo
makeLenses ''ProtoInfo

getTimeBetweenBlocks :: ProtoInfo -> NominalDiffTime
getTimeBetweenBlocks = fromIntegral . _protoInfo_minimalBlockDelay

-- | Predict timestamp of a block at the given level in the future. This estimate can be
-- imprecise due to network delays or missed baking opportunities. Also it may be incorrect
-- in case block period changes between the given block and the block in the future.
predictFutureTimestamp :: BlockLike blk => ProtoInfo -> RawLevel -> blk -> Time.UTCTime
predictFutureTimestamp protoInfo lvl blk =
  Time.addUTCTime (getTimeBetweenBlocks protoInfo * fromIntegral lvlDiff) (blk ^. timestamp)
  where
    lvlDiff :: RawLevel
    lvlDiff = lvl - (blk ^. level)

-- | Estimate timestamp of the block at the given level in the past
-- from 'BlockLike' data of a more recent block.
unsafeEstimatePastTimestamp :: BlockLike blk => ProtoInfo -> RawLevel -> blk -> Time.UTCTime
unsafeEstimatePastTimestamp protoInfo lvl blk = posixSecondsToUTCTime $
  utcTimeToPOSIXSeconds (blk ^. timestamp) - getTimeBetweenBlocks protoInfo * fromIntegral lvlDiff
  where
    lvlDiff :: RawLevel
    lvlDiff = (blk ^. level) - lvl
