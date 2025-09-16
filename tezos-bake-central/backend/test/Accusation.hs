{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Accusation where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Test.Tasty
import Data.Either
import Test.Tasty.HUnit

import Backend.Workers.Block (getAccusedBaker)
import Tezos.Base.Types
import qualified Tezos.Base.Types as Base


testAccusations :: TestTree
testAccusations = testGroup "Accusations"
  [ testGroup "Base"
    [ testDoubleBakingEvidence013
    , testDoublePreendorsementEvidence013
    ]
  ]

baseAccusationTest
  :: forall t. (FromJSON t)
  => String
  -> (t -> [BalanceUpdate])
  -> FilePath
  -> PublicKeyHash
  -> TestTree
baseAccusationTest testName getBalanceUpdates path expected = testCase testName $ do
  raw <- LBS.readFile path
  let
    op = either (error "Failed to decode operation contents") id $ eitherDecode @t raw
    balanceUpdates = getBalanceUpdates op
    accusedBaker = getAccusedBaker balanceUpdates
  accusedBaker @?= expected

extractBalanceUpdates :: AccusedInfo -> [BalanceUpdate]
extractBalanceUpdates (AccInfoBalanceUpdates b) = b
extractBalanceUpdates _ = error "Not balance updates"

testDoubleBakingEvidence013 :: TestTree
testDoubleBakingEvidence013 = baseAccusationTest
  "Double baking evidence"
  (toList . extractBalanceUpdates . Base._doubleBakingEvidenceMetadata_accusedInfo . Base._operationContentsDoubleBakingEvidence_metadata)
  -- https://ithacanet.tzkt.io/opX2JykJaQ96Mt8dK4sTcjVuRbNJTJrJVBy36Xj6cGFUBne4uBX
  "test/resources/double_baking_evidence.json"
  "tz3Q67aMz7gSMiQRcW729sXSfuMtkyAHYfqc"

testDoublePreendorsementEvidence013 :: TestTree
testDoublePreendorsementEvidence013 = baseAccusationTest
  "Double preendorsement evidence"
  (toList . Base._doublePreendorsementEvidenceMetadata_balanceUpdates . Base._operationContentsDoublePreendorsementEvidence_metadata)
  -- https://ithacanet.tzkt.io/ooUXVJPkfZpMy3LQshGGoJTAJCkzTEozWCk5rJK9MUBTWzaRnhw
  "test/resources/double_preendorsement_evidence.json"
  "tz3Q67aMz7gSMiQRcW729sXSfuMtkyAHYfqc"
