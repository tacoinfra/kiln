{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.ProtocolConstants where

import Data.Typeable

import Tezos.Tez
import Tezos.Json
import Tezos.PeriodSequence

data ProtoInfo = ProtoInfo
  { _protoInfo_blockReward :: Tez
  , _protoInfo_blockSecurityDeposit :: Tez
  , _protoInfo_blocksPerCommitment :: Int
  , _protoInfo_blocksPerCycle :: Int
  , _protoInfo_blocksPerRollSnapshot :: Int
  , _protoInfo_blocksPerVotingPeriod :: Int
  , _protoInfo_endorsementReward :: Tez
  , _protoInfo_endorsementSecurityDeposit :: Tez
  , _protoInfo_endorsersPerBlock :: Int
  , _protoInfo_maxOperationDataLength :: Int
  , _protoInfo_michelsonMaximumTypeSize :: Int
  , _protoInfo_originationBurn :: Tez
  , _protoInfo_preservedCycles :: Int
  , _protoInfo_proofOfWorkThreshold :: TezosWord64
  , _protoInfo_seedNonceRevelationTip :: Tez
  , _protoInfo_timeBetweenBlocks :: PeriodSequence -- repeating sequence of seconds
  , _protoInfo_tokensPerRoll :: Tez
  -- TODO: these didn't show up in my quick greppings, so I don't know the types too exactly.
  -- , _protoInfo_instructionsPerTransaction :: 40000
  -- , _protoInfo_maxRevelationsPerBlock :: 32,
  -- , _protoInfo_nonceLength :: 32,
  -- , _protoInfo_proofOfWorkNonceSize :: 8
  } deriving (Eq, Ord, Show, Typeable)

deriveTezosJson ''ProtoInfo
