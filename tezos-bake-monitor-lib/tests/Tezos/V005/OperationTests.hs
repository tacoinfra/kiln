{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Tezos.V005.OperationTests where

import qualified Data.ByteString.Base16 as BS16
import Test.Tasty
import Test.Tasty.HUnit

import Tezos.TestUtils ((@?~))
import qualified Tezos.Common.Binary as B
import Tezos.V005.Operation
import Tezos.V005.Micheline
import Tezos.V005.Contract
import Tezos.V005.Types

binaryRoundTripTest :: (B.TezosBinary a, Eq a, Show a) => TestName -> a -> TestTree
binaryRoundTripTest tn a = testCase tn $
  B.decodeEither (B.encode a) @?~ Right a


tests :: TestTree
tests = testGroup "OperationTests"
  [ testGroup "Entrypoint"
    [ binaryRoundTripTest "Default" EntrypointDefault
    , binaryRoundTripTest "Root" EntrypointRoot
    , binaryRoundTripTest "Do" EntrypointDo
    , binaryRoundTripTest "SetDelegate" EntrypointSetDelegate
    , binaryRoundTripTest "RemoveDelegate" EntrypointRemoveDelegate
    , binaryRoundTripTest "Other" (EntrypointOther (EntrypointName "foobar"))
    ]
  , binaryRoundTripTest "OpParameters" $ OpParameters
    (EntrypointOther (EntrypointName "foo"))
    (Expression_Prim (MichelinePrimAp (MichelinePrimitive "UNIT") []))
  ]
