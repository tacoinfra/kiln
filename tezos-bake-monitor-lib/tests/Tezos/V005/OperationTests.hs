{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Tezos.V005.OperationTests where

import qualified Data.ByteString.Base16 as BS16
import Test.Tasty
import Test.Tasty.HUnit

import Tezos.TestUtils ((@?~), aesonRoundTripTest)
import qualified Tezos.Common.Binary as B
import Tezos.V005.Operation
import Tezos.V005.Micheline
import Tezos.V005.Contract
import Tezos.V005.Types

binaryRoundTripTest :: (B.TezosBinary a, Eq a, Show a) => TestName -> a -> TestTree
binaryRoundTripTest tn a = testCase tn $
  B.decodeEither (B.encode a) @?~ Right a

-- Block: BLET1iWsbKsBmca1iE214n7SBXuRkQfdXcr3ArYqUvGsSzShsaF on Zeronet 2019-10-01
testOperationOrigination:: Operation
testOperationOrigination= Operation
  { _operation_protocol = "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU"
  , _operation_chainId = "NetXKakFj1A7ouL"
  , _operation_hash = "oo2fU287a5Z7dQ9hVoxtnX6Zb8KdjxbJvcsxdf5S4UZcE7HnMPA"
  , _operation_branch = "BKw7TsvTfjMfy2sNTm3SiYCiQ7nE7PP4nz9J3YKKpURNoCs3N6c"
  , _operation_signature = Just "sigSLxaUDZB4sz1i3HFksQqRm1skLhZBDctLzsShgVMRKJUZGZ28UPMTGUsPs3yTnaQBsr4v48trX7JxMC7eg1Rf19G2oz6T"
  , _operation_contents =
    [ OperationContents_Reveal
      OperationContentsReveal
        { _operationContentsReveal_metadata = ManagerOperationMetadata
          { _managerOperationMetadata_balanceUpdates = 
            [ BalanceUpdate_Contract $
              ContractUpdate
                { _contractUpdate_contract = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
                , _contractUpdate_change = -0.001259
                }
            , BalanceUpdate_Freezer $
              FreezerUpdate
                { _freezerUpdate_category = FreezerCategory_Fees
                , _freezerUpdate_delegate = "tz1boot1pK9h2BVGXdyvfQSv8kd1LQM6H889"
                , _freezerUpdate_cycle = 1712
                , _freezerUpdate_change = 0.001259
                }
            ]
          , _managerOperationMetadata_operationResult = OperationResult
            { _operationResult_status = OperationResultStatus_Applied
            , _operationResult_errors = Nothing
            , _operationResult_content = Nothing
            }
          }
        , _operationContentsReveal_source = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
        , _operationContentsReveal_fee = 0.001259
        , _operationContentsReveal_counter = 437654
        , _operationContentsReveal_gasLimit = 10000
        , _operationContentsReveal_storageLimit = 0
        , _operationContentsReveal_publicKey = "edpktxQpBU6FcfwXzCaZHBmyk4vr91EVi7CghSw5SrE2tWoUoZZRUX"
        }
    , OperationContents_Origination
      OperationContentsOrigination
        { _operationContentsOrigination_metadata = ManagerOperationMetadata
          { _managerOperationMetadata_balanceUpdates =
            [ BalanceUpdate_Contract $
              ContractUpdate
                { _contractUpdate_contract = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
                , _contractUpdate_change = -0.004585
                }
            , BalanceUpdate_Freezer $
              FreezerUpdate
                { _freezerUpdate_category = FreezerCategory_Fees
                , _freezerUpdate_delegate = "tz1boot1pK9h2BVGXdyvfQSv8kd1LQM6H889"
                , _freezerUpdate_cycle = 1712
                , _freezerUpdate_change = 0.004585
                }
            ]
          , _managerOperationMetadata_operationResult = OperationResult
            { _operationResult_status = OperationResultStatus_Applied
            , _operationResult_errors = Nothing
            , _operationResult_content = Just $ OperationResultOrigination
              { _operationResultOrigination_balanceUpdates = 
                [ BalanceUpdate_Contract $ ContractUpdate
                    { _contractUpdate_contract = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
                    , _contractUpdate_change = -1.014000
                    }
                , BalanceUpdate_Contract $ ContractUpdate
                    { _contractUpdate_contract = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
                    , _contractUpdate_change = -0.257000
                    }
                , BalanceUpdate_Contract $ ContractUpdate
                    { _contractUpdate_contract = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
                    , _contractUpdate_change = -100.000000
                    }
                , BalanceUpdate_Contract $ ContractUpdate
                    { _contractUpdate_contract = "KT1Xrer5AzXyVLzS63K2Yovs4ZpRmWgGqAhn"
                    , _contractUpdate_change = 100.000000
                    }
                ]
              , _operationResultOrigination_originatedContracts = [ "KT1Xrer5AzXyVLzS63K2Yovs4ZpRmWgGqAhn" ]
              , _operationResultOrigination_consumedGas = 33606
              , _operationResultOrigination_storageSize = 1014
              , _operationResultOrigination_paidStorageSizeDiff = 1014
              }
            }
          }
        , _operationContentsOrigination_fee = 0.004585
        , _operationContentsOrigination_counter = 437655
        , _operationContentsOrigination_gasLimit = 33706
        , _operationContentsOrigination_storageLimit = 1291
        , _operationContentsOrigination_balance = 100.000000
        , _operationContentsOrigination_delegate = Nothing
        , _operationContentsOrigination_source = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
        , _operationContentsOrigination_script = ContractScript
          { _contractScript_code = Expression_Seq
            [ Expression_Prim $ MichelinePrimAp
              (MichelinePrimitive "parameter")
              [ Expression_Prim $ MichelinePrimAp
                (MichelinePrimitive "pair")
                [ Expression_Prim $ MichelinePrimAp (MichelinePrimitive "nat") []
                , Expression_Prim $ MichelinePrimAp
                  (MichelinePrimitive "option")
                  [ Expression_Prim $ MichelinePrimAp (MichelinePrimitive "key_hash") [] ]
                ]
              ]
            , Expression_Prim $ MichelinePrimAp
              (MichelinePrimitive "storage")
              [ Expression_Prim $ MichelinePrimAp
                (MichelinePrimitive "pair")
                [ Expression_Prim $ MichelinePrimAp (MichelinePrimitive "nat") []
                , Expression_Prim $ MichelinePrimAp
                  (MichelinePrimitive "pair")
                  [ Expression_Prim $ MichelinePrimAp (MichelinePrimitive "nat") []
                  , Expression_Prim $ MichelinePrimAp
                    (MichelinePrimitive "list")
                    [ Expression_Prim $ MichelinePrimAp (MichelinePrimitive "key") [] ]
                  ]
                ]
              ]
            , Expression_Prim $ MichelinePrimAp
               (MichelinePrimitive "code") [ Expression_Prim $ MichelinePrimAp (MichelinePrimitive "UNIT") [] ]
            ]
          , _contractScript_storage = Expression_Prim $ MichelinePrimAp
            (MichelinePrimitive "Pair")
            [ Expression_Int 0
            , Expression_Prim $ MichelinePrimAp
              (MichelinePrimitive "Pair")
              [ Expression_Int 2
              , Expression_Seq
                [ Expression_String "edpktjENiqmwLD1vQdLc2PwtTPWpAM9i3jQQenAEsSWLtsZMBbMjME"
                , Expression_String "edpkudQarx27avpEsMKGKXevZKb2Maa1voyfi7uJazcbxgCFd4Nufh"
                , Expression_String "edpkutw9BqRcChuVFLZ7rpAj59gQgjzm3ihdGj5iFmQwCcdwhRFCqm"
                ]
              ]
            ]
          }
        }
    ]
  }

testOperationTransaction :: Operation
testOperationTransaction = Operation
  { _operation_protocol = "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU"
  , _operation_chainId = "NetXKakFj1A7ouL"
  , _operation_hash = "oobbBmyF5TkL7hut4jKYNkqzzmW1h9FFQvFWiNq4FzVigRSXeQn"
  , _operation_branch = "BMWBDq7oEr5fdzeuxsuJ3nMYU4GWWquLRyvBZt812V3YucFe8BW"
  , _operation_signature = Just "sigsRgrQBpNYzpAJMJ2Ub1Pped1Hr5pwSRrYaFVcqsNrLRdveLrGie6zR8EneGhrPFaVT8fEiTofgJHq8MHX3KMfMwfKT9FW"
  , _operation_contents =
    [ OperationContents_Transaction
      OperationContentsTransaction
        { _operationContentsTransaction_metadata = ManagerOperationMetadata
          { _managerOperationMetadata_balanceUpdates =
            [ BalanceUpdate_Contract $
              ContractUpdate
                { _contractUpdate_contract = "tz1NF7b38uQ43N4nmTHvDKpr1Qo5LF9iYawk"
                , _contractUpdate_change = -0.002954
                }
            , BalanceUpdate_Freezer $
              FreezerUpdate
                { _freezerUpdate_category = FreezerCategory_Fees
                , _freezerUpdate_delegate = "tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB"
                , _freezerUpdate_cycle = 1417
                , _freezerUpdate_change = 0.002954
                }
            ]
          , _managerOperationMetadata_operationResult = OperationResult
            { _operationResult_status = OperationResultStatus_Applied
            , _operationResult_errors = Nothing
            , _operationResult_content = Just $ OperationResultTransaction
              { _operationResultTransaction_storage = Just $ Expression_Bytes $ Base16ByteString $ fst $ BS16.decode "001c92e58081a9d236c82e3e9d382c64e5642467c0"
              , _operationResultTransaction_balanceUpdates = [ ]
              , _operationResultTransaction_consumedGas = 15953
              , _operationResultTransaction_storageSize = 232
              , _operationResultTransaction_paidStorageSizeDiff = 0
              , _operationResultTransaction_originatedContracts = [ ]
              }
            }
          }
        , _operationContentsTransaction_source = "tz1NF7b38uQ43N4nmTHvDKpr1Qo5LF9iYawk"
        , _operationContentsTransaction_fee = 0.002954
        , _operationContentsTransaction_counter = 390931
        , _operationContentsTransaction_gasLimit = 26260
        , _operationContentsTransaction_storageLimit = 277
        , _operationContentsTransaction_amount = 0
        , _operationContentsTransaction_destination = "KT1DeKNWcB2hXv7M9ZfYmVSPECLJLWDfEJDh"
        , _operationContentsTransaction_parameters = Just $ OpParameters
          { _opParameters_entrypoint = EntrypointDo
          , _opParameters_value = Expression_Seq
            [ Expression_Prim $ MichelinePrimAp (MichelinePrimitive "DROP") []
            , Expression_Prim $ MichelinePrimAp (MichelinePrimitive "NIL")
              [ Expression_Prim $ MichelinePrimAp (MichelinePrimitive "operation") []
              ]
            , Expression_Prim $ MichelinePrimAp (MichelinePrimitive "PUSH")
              [ Expression_Prim $ MichelinePrimAp (MichelinePrimitive "key_hash") []
              , Expression_String "tz1M7RpncdPVx19rtZda42UNDWon4NE5kmGu"
              ]
            , Expression_Prim $ MichelinePrimAp (MichelinePrimitive "IMPLICIT_ACCOUNT") []
            , Expression_Prim $ MichelinePrimAp (MichelinePrimitive "PUSH")
              [ Expression_Prim $ MichelinePrimAp (MichelinePrimitive "mutez") []
              , Expression_Int 2000
              ]
            , Expression_Prim $ MichelinePrimAp (MichelinePrimitive "UNIT") []
            , Expression_Prim $ MichelinePrimAp (MichelinePrimitive "TRANSFER_TOKENS") []
            , Expression_Prim $ MichelinePrimAp (MichelinePrimitive "CONS") []
            ]
          }
        }
    ]
  }

tests :: TestTree
tests = testGroup "OperationTests"
  [ testGroup "Binary"
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
  , testGroup "JSON"
    [ aesonRoundTripTest "Origination" "tests/Tezos/V005/OperationTests/OperationOriginationV005.json" testOperationOrigination
    , aesonRoundTripTest "Transaction" "tests/Tezos/V005/OperationTests/OperationTransactionV005.json" testOperationTransaction
    ]
  ]
