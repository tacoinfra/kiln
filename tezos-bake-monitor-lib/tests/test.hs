import Test.Tasty
import Test.Tasty.HUnit

import qualified Tezos.V005.NodeRPC.CrossCompatTests as CrossCompatTests
import qualified Tezos.V005.ProtocolConstantsTests as ProtocolConstantsTests
import qualified Tezos.V005.OperationTests as OperationTests
import qualified Tezos.V004.TezTests as TezTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tezos Bake Monitor Lib"
  [ testGroup "V004" 
    [ TezTests.tests
    ]
  , testGroup "V005" 
    [ CrossCompatTests.tests
    , ProtocolConstantsTests.tests
    , OperationTests.tests
    ]
  ]

