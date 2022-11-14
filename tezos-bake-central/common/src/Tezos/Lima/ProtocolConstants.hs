{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tezos.Lima.ProtocolConstants where

import Control.DeepSeq (NFData)
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Data.Hashable (Hashable)
import Data.Typeable
import Data.Time (NominalDiffTime)
import qualified Data.Time as Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GHC.Generics

import Tezos.Common.Block
import Tezos.Common.Json
import Tezos.Common.Level
import Tezos.Common.Tez

-- | Datatype that represents all used protocol constants.
-- Some of the fields are @Maybe@s for cross-compatibility with old protocols.
data ProtoInfo = ProtoInfo
  { _protoInfo_preservedCycles :: Cycle -- "preserved_cycles": { "type": "integer", "minimum": 0, "maximum": 255 },
  , _protoInfo_blocksPerCycle :: RawLevel -- "blocks_per_cycle": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _protoInfo_blocksPerVotingPeriod :: Maybe RawLevel
  , _protoInfo_cyclesPerVotingPeriod :: Maybe RawLevel -- "blocks_per_voting_period": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },

  , _protoInfo_tokensPerRoll :: Tez -- "tokens_per_roll": { "$ref": "#/definitions/mutez" },

  , _protoInfo_minimalBlockDelay :: TezosWord64 -- "minimal_block_delay": { "$ref": "#/definitions/int64" }
  } deriving (Eq, Ord, Show, Typeable, Generic)
instance Hashable ProtoInfo
instance NFData ProtoInfo

getBlocksPerVotingPeriod :: ProtoInfo -> RawLevel
getBlocksPerVotingPeriod protoInfo = case (_protoInfo_blocksPerVotingPeriod protoInfo, _protoInfo_cyclesPerVotingPeriod protoInfo) of
  (Nothing, Just cycles) ->  cycles * _protoInfo_blocksPerCycle protoInfo
  (Just blocks, _) -> blocks
  _ -> error "Neither 'blocks_per_voting_period' nor 'cycles_per_voting_period' are present in protocol constants."

getCyclesPerVotingPeriod :: ProtoInfo -> RawLevel
getCyclesPerVotingPeriod protoInfo = case (_protoInfo_blocksPerVotingPeriod protoInfo, _protoInfo_cyclesPerVotingPeriod protoInfo) of
  (Nothing, Just cycles) -> cycles
  (Just blocks, _) -> blocks `div` _protoInfo_blocksPerCycle protoInfo
  _ -> error "Neither 'blocks_per_voting_period' nor 'cycles_per_voting_period' are present in protocol constants."

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
