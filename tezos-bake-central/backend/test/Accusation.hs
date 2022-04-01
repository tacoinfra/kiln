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
import Tezos.Common.BalanceUpdate
import qualified Tezos.V011.Types as V011
import Tezos.V012.Types
import qualified Tezos.V012.Types as V012


testAccusations :: TestTree
testAccusations = testGroup "Accusations"
  [ testGroup "011"
    [ testDoubleBakingEvidence011
    , testDoubleEndorsementEvidence011
    ]
  , testGroup "012"
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

testDoubleBakingEvidence011 :: TestTree
testDoubleBakingEvidence011 = baseAccusationTest
  "Double baking evidence 011"
  (toList . V011._doubleBakingEvidenceMetadata_balanceUpdates . V011._operationContentsDoubleBakingEvidence_metadata)
  -- https://hangzhounet.tzkt.io/opN53nP6bdWffc28GeeEnGxK5NfpWMx26w84EZAKf2Uy3vQkjCd
  "test/Accusations/011/double_baking_evidence.json"
  "tz1SMARcpWCydHsGgz4MRoK9NkbpBmmUAfNe"

testDoubleEndorsementEvidence011 :: TestTree
testDoubleEndorsementEvidence011 = baseAccusationTest
  "Double endorsement evidence 011"
  (toList . V011._doubleEndorsementEvidenceMetadata_balanceUpdates . V011._operationContentsDoubleEndorsementEvidence_metadata)
  -- https://hangzhounet.tzkt.io/oodotr3y1CH1GdkvxBTuiJqFoApM7tTZRmMCmQkMeFWdK4E94Yk
  "test/Accusations/011/double_endorsement_evidence.json"
  "tz1aWXP237BLwNHJcCD4b3DutCevhqq2T1Z9"

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
