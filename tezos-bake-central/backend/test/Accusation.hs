{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Accusation where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Test.Tasty
import Test.Tasty.HUnit

import Backend.Workers.Block (getAccusedBaker)
import Tezos.Lima.Types
import qualified Tezos.Lima.Types as Lima


testAccusations :: TestTree
testAccusations = testGroup "Accusations"
  [ testGroup "013"
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

testDoubleBakingEvidence013 :: TestTree
testDoubleBakingEvidence013 = baseAccusationTest
  "Double baking evidence 013"
  (toList . Lima._doubleBakingEvidenceMetadata_balanceUpdates . Lima._operationContentsDoubleBakingEvidence_metadata)
  -- https://ithacanet.tzkt.io/opX2JykJaQ96Mt8dK4sTcjVuRbNJTJrJVBy36Xj6cGFUBne4uBX
  "test/Accusations/013/double_baking_evidence.json"
  "tz3Q67aMz7gSMiQRcW729sXSfuMtkyAHYfqc"

testDoublePreendorsementEvidence013 :: TestTree
testDoublePreendorsementEvidence013 = baseAccusationTest
  "Double preendorsement evidence 013"
  (toList . Lima._doublePreendorsementEvidenceMetadata_balanceUpdates . Lima._operationContentsDoublePreendorsementEvidence_metadata)
  -- https://ithacanet.tzkt.io/ooUXVJPkfZpMy3LQshGGoJTAJCkzTEozWCk5rJK9MUBTWzaRnhw
  "test/Accusations/013/double_preendorsement_evidence.json"
  "tz3Q67aMz7gSMiQRcW729sXSfuMtkyAHYfqc"
