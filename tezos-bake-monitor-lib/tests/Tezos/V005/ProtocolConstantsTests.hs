{-# LANGUAGE OverloadedStrings #-}
module Tezos.V005.ProtocolConstantsTests where

import Data.List.NonEmpty (NonEmpty((:|)))
import Test.Tasty
import Test.Tasty.HUnit

import Tezos.TestUtils (aesonRoundTripTest)
import Tezos.V005.ProtocolConstants
import Tezos.V005.PeriodSequence (PeriodSequenceF(PeriodSequence))

tests :: TestTree
tests = aesonRoundTripTest
  "Tezos.V005.ProtocolConstants ProtoInfo RoundTrip"
  "tests/Tezos/V005/ProtocolConstantsTests/ProtoInfo.json"
  $ ProtoInfo
    { _protoInfo_proofOfWorkNonceSize = 8
    , _protoInfo_nonceLength = 32
    , _protoInfo_maxRevelationsPerBlock = 32
    , _protoInfo_maxOperationDataLength = 16384
    , _protoInfo_preservedCycles = 5
    , _protoInfo_blocksPerCycle = 128
    , _protoInfo_blocksPerCommitment = 32
    , _protoInfo_blocksPerRollSnapshot = 8
    , _protoInfo_blocksPerVotingPeriod = 2816
    , _protoInfo_timeBetweenBlocks = PeriodSequence (20 :| [])
    , _protoInfo_endorsersPerBlock = 32
    , _protoInfo_hardGasLimitPerOperation = 800000
    , _protoInfo_hardGasLimitPerBlock = 8000000
    , _protoInfo_proofOfWorkThreshold = 70368744177663
    , _protoInfo_tokensPerRoll = 8000
    , _protoInfo_michelsonMaximumTypeSize = 1000
    , _protoInfo_seedNonceRevelationTip = 0.125
    , _protoInfo_originationBurn = 0.257
    , _protoInfo_originationSize = 257
    , _protoInfo_blockSecurityDeposit = 512
    , _protoInfo_endorsementSecurityDeposit = 64
    , _protoInfo_blockReward = 16
    , _protoInfo_endorsementReward = 2
    , _protoInfo_costPerByte = 0.001
    , _protoInfo_hardStorageLimitPerOperation = 600000
    , _protoInfo_delayPerMissingEndorsement = 2
    , _protoInfo_initialEndorsers = Just 24
    , _protoInfo_quorumMin = Just 3000
    , _protoInfo_quorumMax = Just 7000
    , _protoInfo_minProposalQuorum = Just 500
  }
