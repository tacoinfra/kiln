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
import Tezos.V012.Types
import qualified Tezos.V012.Types as V012


testAccusations :: TestTree
testAccusations = testGroup "Accusations"
  [ testGroup "012"
    [ testDoubleBakingEvidence012
    , testDoublePreendorsementEvidence012
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

testDoubleBakingEvidence012 :: TestTree
testDoubleBakingEvidence012 = baseAccusationTest
  "Double baking evidence 012"
  (toList . V012._doubleBakingEvidenceMetadata_balanceUpdates . V012._operationContentsDoubleBakingEvidence_metadata)
  -- https://ithacanet.tzkt.io/opX2JykJaQ96Mt8dK4sTcjVuRbNJTJrJVBy36Xj6cGFUBne4uBX
  "test/Accusations/012/double_baking_evidence.json"
  "tz3Q67aMz7gSMiQRcW729sXSfuMtkyAHYfqc"

testDoublePreendorsementEvidence012 :: TestTree
testDoublePreendorsementEvidence012 = baseAccusationTest
  "Double preendorsement evidence 012"
  (toList . V012._doublePreendorsementEvidenceMetadata_balanceUpdates . V012._operationContentsDoublePreendorsementEvidence_metadata)
  -- https://ithacanet.tzkt.io/ooUXVJPkfZpMy3LQshGGoJTAJCkzTEozWCk5rJK9MUBTWzaRnhw
  "test/Accusations/012/double_preendorsement_evidence.json"
  "tz3Q67aMz7gSMiQRcW729sXSfuMtkyAHYfqc"
