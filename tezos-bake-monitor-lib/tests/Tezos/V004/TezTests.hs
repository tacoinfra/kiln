{-# LANGUAGE OverloadedStrings #-}
module Tezos.V004.TezTests where

import Test.Tasty
import Test.Tasty.HUnit

import Tezos.TestUtils (aesonRoundTripTest)
import Tezos.V004.Tez

tests :: TestTree
tests = testGroup "Tezos.V004.TezTests"
  [ testGroup "TezDelta"
    [ aesonRoundTripTest "Negative JSON String" "tests/Tezos/V004/Tez/Negative.json" (-0.001400 :: TezDelta)
    , aesonRoundTripTest "Positive JSON String" "tests/Tezos/V004/Tez/Positive.json" (0.001400 :: TezDelta)
    ]
  ]
