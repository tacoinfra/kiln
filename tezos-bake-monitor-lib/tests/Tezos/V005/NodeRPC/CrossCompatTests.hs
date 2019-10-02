{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Tezos.V005.NodeRPC.CrossCompatTests where

import qualified Data.ByteString.Base16 as BS16
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Tezos.TestUtils ((@?~), aesonRoundTripTest)
import Tezos.V005.NodeRPC.CrossCompat (Account(..), Operation(..))
import qualified Tezos.V004.Operation as V004
import qualified Tezos.V004.Micheline as V004
import qualified Tezos.V004.Types as V004
import qualified Tezos.V005.Contract as V005
import qualified Tezos.V005.Operation as V005
import qualified Tezos.V005.Micheline as V005
import qualified Tezos.V005.Types as V005

yoloV004Pkh :: Text -> V004.PublicKeyHash
yoloV004Pkh = either (error . show) id .  V004.tryReadPublicKeyHashText

yoloV005Pkh :: Text -> V005.PublicKeyHash
yoloV005Pkh = either (error . show) id .  V005.tryReadPublicKeyHashText

testAccountV005 :: V005.Account
testAccountV005 = V005.Account
  (Just $ yoloV005Pkh "tz1LTKbz4KUTabtxmszTKNPGW89V4mxdrr3E")
  (V005.microTez 20015996000)
  Nothing
  (Just 422182)

testAccountV004 :: V004.Account
testAccountV004 = V004.Account
  (either (error . show) id $ V004.tryReadPublicKeyHashText "tz1SGiggd18isfyuXtjpGRJvetySfqEK5Ctg")
  (V005.microTez 497280)
  True
  (V004.AccountDelegate False Nothing)
  Nothing
  476417

testOperationTransactionV005 :: V005.Operation
testOperationTransactionV005 = V005.Operation
  { V005._operation_protocol = "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU"
  , V005._operation_chainId = "NetXKakFj1A7ouL"
  , V005._operation_hash = "oobbBmyF5TkL7hut4jKYNkqzzmW1h9FFQvFWiNq4FzVigRSXeQn"
  , V005._operation_branch = "BMWBDq7oEr5fdzeuxsuJ3nMYU4GWWquLRyvBZt812V3YucFe8BW"
  , V005._operation_signature = Just "sigsRgrQBpNYzpAJMJ2Ub1Pped1Hr5pwSRrYaFVcqsNrLRdveLrGie6zR8EneGhrPFaVT8fEiTofgJHq8MHX3KMfMwfKT9FW"
  , V005._operation_contents =
    [ V005.OperationContents_Transaction
      V005.OperationContentsTransaction
        { V005._operationContentsTransaction_metadata = V005.ManagerOperationMetadata
          { V005._managerOperationMetadata_balanceUpdates =
            [ V005.BalanceUpdate_Contract $
              V005.ContractUpdate
                { V005._contractUpdate_contract = "tz1NF7b38uQ43N4nmTHvDKpr1Qo5LF9iYawk"
                , V005._contractUpdate_change = -0.002954
                }
            , V005.BalanceUpdate_Freezer $
              V005.FreezerUpdate
                { V005._freezerUpdate_category = V005.FreezerCategory_Fees
                , V005._freezerUpdate_delegate = "tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB"
                , V005._freezerUpdate_cycle = 1417
                , V005._freezerUpdate_change = 0.002954
                }
            ]
          , V005._managerOperationMetadata_operationResult = V005.OperationResult
            { V005._operationResult_status = V005.OperationResultStatus_Applied
            , V005._operationResult_errors = Nothing
            , V005._operationResult_content = Just $ V005.OperationResultTransaction
              { V005._operationResultTransaction_storage = Just $ V005.Expression_Bytes $ V005.Base16ByteString $ fst $ BS16.decode "001c92e58081a9d236c82e3e9d382c64e5642467c0"
              , V005._operationResultTransaction_balanceUpdates = [ ]
              , V005._operationResultTransaction_consumedGas = 15953
              , V005._operationResultTransaction_storageSize = 232
              , V005._operationResultTransaction_paidStorageSizeDiff = 0
              , V005._operationResultTransaction_originatedContracts = [ ]
              }
            }
          }
        , V005._operationContentsTransaction_source = "tz1NF7b38uQ43N4nmTHvDKpr1Qo5LF9iYawk"
        , V005._operationContentsTransaction_fee = 0.002954
        , V005._operationContentsTransaction_counter = 390931
        , V005._operationContentsTransaction_gasLimit = 26260
        , V005._operationContentsTransaction_storageLimit = 277
        , V005._operationContentsTransaction_amount = 0
        , V005._operationContentsTransaction_destination = "KT1DeKNWcB2hXv7M9ZfYmVSPECLJLWDfEJDh"
        , V005._operationContentsTransaction_parameters = Just $ V005.OpParameters
          { V005._opParameters_entrypoint = V005.EntrypointDo
          , V005._opParameters_value = V005.Expression_Seq
            [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "DROP") []
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "NIL")
              [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "operation") []
              ]
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "PUSH")
              [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "key_hash") []
              , V005.Expression_String "tz1M7RpncdPVx19rtZda42UNDWon4NE5kmGu"
              ]
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "IMPLICIT_ACCOUNT") []
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "PUSH")
              [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "mutez") []
              , V005.Expression_Int 2000
              ]
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "UNIT") []
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "TRANSFER_TOKENS") []
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "CONS") []
            ]
          }
        }
    ]
  }

testOperationTransactionV004 :: V004.Operation
testOperationTransactionV004 = V004.Operation
  { V004._operation_protocol = "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"
  , V004._operation_chainId = "NetXdQprcVkpaWU"
  , V004._operation_hash = "opMxLTrA3XnubLCkiJtnQd8yX9DuKidwzR5DsHASFqwXXUBif1X"
  , V004._operation_branch = "BMB7GtVPtRzbnAQa8GRmq2kCwrZekdDN1WCyqT4AptoWuu5RSGw"
  , V004._operation_signature = Just "sigsMN6RBN7vpUjJD5WA11CjEnQzGu6zZU5ZX9K7ydHUHSf5WWqHhjCeGBoWqcieTwMkiWqRkcro2LT99BoTjHxMeH71yuDb"
  , V004._operation_contents =
    [ V004.OperationContents_Transaction
      V004.OperationContentsTransaction
        { V004._operationContentsTransaction_metadata = V004.ManagerOperationMetadata
          { V004._managerOperationMetadata_balanceUpdates =
            [ V004.BalanceUpdate_Contract $
              V004.ContractUpdate
                { V004._contractUpdate_contract = "tz1SiPXX4MYGNJNDsRc7n8hkvUqFzg8xqF9m"
                , V004._contractUpdate_change = -0.001420
                }
            , V004.BalanceUpdate_Freezer $
              V004.FreezerUpdate
                { V004._freezerUpdate_category = V004.FreezerCategory_Fees
                , V004._freezerUpdate_delegate = "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r"
                , V004._freezerUpdate_cycle = 153
                , V004._freezerUpdate_change = 0.001420
                }
            ]
          , V004._managerOperationMetadata_operationResult = V004.OperationResult
            { V004._operationResult_status = V004.OperationResultStatus_Applied
            , V004._operationResult_errors = Nothing
            , V004._operationResult_content = Nothing
            }
          }
        , V004._operationContentsTransaction_source = "tz1SiPXX4MYGNJNDsRc7n8hkvUqFzg8xqF9m"
        , V004._operationContentsTransaction_fee = 0.001420
        , V004._operationContentsTransaction_counter = 1991234
        , V004._operationContentsTransaction_gasLimit = 11000
        , V004._operationContentsTransaction_storageLimit = 300
        , V004._operationContentsTransaction_amount = 326.093080
        , V004._operationContentsTransaction_destination = "tz1ZADZUDP7brp17j6iRsujboGoUzsdqZzX2"
        , V004._operationContentsTransaction_parameters = Just $ V004.Expression_Prim $
          V004.MichelinePrimAp (V004.MichelinePrimitive "Unit") []
        }
    ]
  }

-- Block: BLFWHRz8FG8dsPPtafrYAuSCn6y767aCcCjCGhzWKMyKsfzRXeo
testOperationOriginationV004 :: V004.Operation
testOperationOriginationV004 = V004.Operation
  { V004._operation_protocol = "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"
  , V004._operation_chainId = "NetXdQprcVkpaWU"
  , V004._operation_hash = "op16gnSLMs6ZPuvHLqQyXm1SamwLKXTR6EEPr7J3ZCKZYYcJmgJ"
  , V004._operation_branch = "BLwuH5L83yzwHHxzhshEGaenagZxNQn6DYkFg8eRqVzohT2sf5h"
  , V004._operation_signature = Just "sigQ7W3XbvhMjMntkxPwWySbC3Pkwg24TXivici3LrEAmSz8v37DJwGGSQi21jsAZHKujXVZCStn6BNTWCDYMc7feGN9kqnz"
  , V004._operation_contents =
    [ V004.OperationContents_Reveal
      V004.OperationContentsReveal
        { V004._operationContentsReveal_metadata = V004.ManagerOperationMetadata
          { V004._managerOperationMetadata_balanceUpdates = 
            [ V004.BalanceUpdate_Contract $
              V004.ContractUpdate
                { V004._contractUpdate_contract = "tz1Qc1BNygfhs3LYi9PRFNKfBjiiBakcMwCu"
                , V004._contractUpdate_change = -0.001269
                }
            , V004.BalanceUpdate_Freezer $
              V004.FreezerUpdate
                { V004._freezerUpdate_category = V004.FreezerCategory_Fees
                , V004._freezerUpdate_delegate = "tz1RCFbB9GpALpsZtu6J58sb74dm8qe6XBzv"
                , V004._freezerUpdate_cycle = 153
                , V004._freezerUpdate_change = 0.001269
                }
            ]
          , V004._managerOperationMetadata_operationResult = V004.OperationResult
            { V004._operationResult_status = V004.OperationResultStatus_Applied
            , V004._operationResult_errors = Nothing
            , V004._operationResult_content = Nothing
            }
          }
        , V004._operationContentsReveal_source = "tz1Qc1BNygfhs3LYi9PRFNKfBjiiBakcMwCu"
        , V004._operationContentsReveal_fee = 0.001269
        , V004._operationContentsReveal_counter = 2058581
        , V004._operationContentsReveal_gasLimit = 10000
        , V004._operationContentsReveal_storageLimit = 0
        , V004._operationContentsReveal_publicKey = "edpkv7cG1rwW5K6nR5hs5MxuihtypqH7akrRMG1tP1Qsii7T1KvJCw"
        }
    , V004.OperationContents_Origination
      V004.OperationContentsOrigination
        { V004._operationContentsOrigination_metadata = V004.ManagerOperationMetadata
          { V004._managerOperationMetadata_balanceUpdates = 
            [ V004.BalanceUpdate_Contract $
              V004.ContractUpdate
                { V004._contractUpdate_contract = "tz1Qc1BNygfhs3LYi9PRFNKfBjiiBakcMwCu"
                , V004._contractUpdate_change = -0.001400
                }
            , V004.BalanceUpdate_Freezer $
              V004.FreezerUpdate
                { V004._freezerUpdate_category = V004.FreezerCategory_Fees
                , V004._freezerUpdate_delegate = "tz1RCFbB9GpALpsZtu6J58sb74dm8qe6XBzv"
                , V004._freezerUpdate_cycle = 153
                , V004._freezerUpdate_change = 0.001400
                }
            ]
          , V004._managerOperationMetadata_operationResult = V004.OperationResult
            { V004._operationResult_status = V004.OperationResultStatus_Applied
            , V004._operationResult_errors = Nothing
            , V004._operationResult_content = Just $ V004.OperationResultOrigination
              { V004._operationResultOrigination_balanceUpdates = 
                [ V004.BalanceUpdate_Contract $
                  V004.ContractUpdate
                    { V004._contractUpdate_contract = "tz1Qc1BNygfhs3LYi9PRFNKfBjiiBakcMwCu"
                    , V004._contractUpdate_change = -0.257000
                    }
                ]
              , V004._operationResultOrigination_originatedContracts = [ "KT1Ueq21uy4qgUEWcCUxDLbpsWX95QHcDUaS" ]
              , V004._operationResultOrigination_consumedGas = 10000
              , V004._operationResultOrigination_storageSize = 0
              , V004._operationResultOrigination_paidStorageSizeDiff = 0
              }
            }
          }
        , V004._operationContentsOrigination_fee = 0.001400
        , V004._operationContentsOrigination_counter = 2058582
        , V004._operationContentsOrigination_gasLimit = 10000
        , V004._operationContentsOrigination_storageLimit = 257
        , V004._operationContentsOrigination_managerPubkey = yoloV004Pkh "tz1Qc1BNygfhs3LYi9PRFNKfBjiiBakcMwCu"
        , V004._operationContentsOrigination_balance = 0
        , V004._operationContentsOrigination_spendable = True
        , V004._operationContentsOrigination_delegatable = True
        , V004._operationContentsOrigination_delegate = Just $ yoloV004Pkh "tz1VxbHcvqoiiZ3Fbc4oVeDhZQeoFRiWMaqN"
        , V004._operationContentsOrigination_source = "tz1Qc1BNygfhs3LYi9PRFNKfBjiiBakcMwCu"
        , V004._operationContentsOrigination_script = Nothing
        }
    ]
  }

-- Block: BLET1iWsbKsBmca1iE214n7SBXuRkQfdXcr3ArYqUvGsSzShsaF on Zeronet 2019-10-01
testOperationOriginationV005 :: V005.Operation
testOperationOriginationV005 = V005.Operation
  { V005._operation_protocol = "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU"
  , V005._operation_chainId = "NetXKakFj1A7ouL"
  , V005._operation_hash = "oo2fU287a5Z7dQ9hVoxtnX6Zb8KdjxbJvcsxdf5S4UZcE7HnMPA"
  , V005._operation_branch = "BKw7TsvTfjMfy2sNTm3SiYCiQ7nE7PP4nz9J3YKKpURNoCs3N6c"
  , V005._operation_signature = Just "sigSLxaUDZB4sz1i3HFksQqRm1skLhZBDctLzsShgVMRKJUZGZ28UPMTGUsPs3yTnaQBsr4v48trX7JxMC7eg1Rf19G2oz6T"
  , V005._operation_contents =
    [ V005.OperationContents_Reveal
      V005.OperationContentsReveal
        { V005._operationContentsReveal_metadata = V005.ManagerOperationMetadata
          { V005._managerOperationMetadata_balanceUpdates = 
            [ V005.BalanceUpdate_Contract $
              V005.ContractUpdate
                { V005._contractUpdate_contract = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
                , V005._contractUpdate_change = -0.001259
                }
            , V005.BalanceUpdate_Freezer $
              V005.FreezerUpdate
                { V005._freezerUpdate_category = V005.FreezerCategory_Fees
                , V005._freezerUpdate_delegate = "tz1boot1pK9h2BVGXdyvfQSv8kd1LQM6H889"
                , V005._freezerUpdate_cycle = 1712
                , V005._freezerUpdate_change = 0.001259
                }
            ]
          , V005._managerOperationMetadata_operationResult = V005.OperationResult
            { V005._operationResult_status = V005.OperationResultStatus_Applied
            , V005._operationResult_errors = Nothing
            , V005._operationResult_content = Nothing
            }
          }
        , V005._operationContentsReveal_source = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
        , V005._operationContentsReveal_fee = 0.001259
        , V005._operationContentsReveal_counter = 437654
        , V005._operationContentsReveal_gasLimit = 10000
        , V005._operationContentsReveal_storageLimit = 0
        , V005._operationContentsReveal_publicKey = "edpktxQpBU6FcfwXzCaZHBmyk4vr91EVi7CghSw5SrE2tWoUoZZRUX"
        }
    , V005.OperationContents_Origination
      V005.OperationContentsOrigination
        { V005._operationContentsOrigination_metadata = V005.ManagerOperationMetadata
          { V005._managerOperationMetadata_balanceUpdates =
            [ V005.BalanceUpdate_Contract $
              V005.ContractUpdate
                { V005._contractUpdate_contract = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
                , V005._contractUpdate_change = -0.004585
                }
            , V005.BalanceUpdate_Freezer $
              V005.FreezerUpdate
                { V005._freezerUpdate_category = V005.FreezerCategory_Fees
                , V005._freezerUpdate_delegate = "tz1boot1pK9h2BVGXdyvfQSv8kd1LQM6H889"
                , V005._freezerUpdate_cycle = 1712
                , V005._freezerUpdate_change = 0.004585
                }
            ]
          , V005._managerOperationMetadata_operationResult = V005.OperationResult
            { V005._operationResult_status = V005.OperationResultStatus_Applied
            , V005._operationResult_errors = Nothing
            , V005._operationResult_content = Just $ V005.OperationResultOrigination
              { V005._operationResultOrigination_balanceUpdates = 
                [ V005.BalanceUpdate_Contract $ V005.ContractUpdate
                    { V005._contractUpdate_contract = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
                    , V005._contractUpdate_change = -1.014000
                    }
                , V005.BalanceUpdate_Contract $ V005.ContractUpdate
                    { V005._contractUpdate_contract = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
                    , V005._contractUpdate_change = -0.257000
                    }
                , V005.BalanceUpdate_Contract $ V005.ContractUpdate
                    { V005._contractUpdate_contract = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
                    , V005._contractUpdate_change = -100.000000
                    }
                , V005.BalanceUpdate_Contract $ V005.ContractUpdate
                    { V005._contractUpdate_contract = "KT1Xrer5AzXyVLzS63K2Yovs4ZpRmWgGqAhn"
                    , V005._contractUpdate_change = 100.000000
                    }
                ]
              , V005._operationResultOrigination_originatedContracts = [ "KT1Xrer5AzXyVLzS63K2Yovs4ZpRmWgGqAhn" ]
              , V005._operationResultOrigination_consumedGas = 33606
              , V005._operationResultOrigination_storageSize = 1014
              , V005._operationResultOrigination_paidStorageSizeDiff = 1014
              }
            }
          }
        , V005._operationContentsOrigination_fee = 0.004585
        , V005._operationContentsOrigination_counter = 437655
        , V005._operationContentsOrigination_gasLimit = 33706
        , V005._operationContentsOrigination_storageLimit = 1291
        , V005._operationContentsOrigination_balance = 100.000000
        , V005._operationContentsOrigination_delegate = Nothing
        , V005._operationContentsOrigination_source = "tz1SoipFhLFjFhVBCEfNUWSRZ3EwMAYrhe9z"
        , V005._operationContentsOrigination_script = V005.ContractScript
          { V005._contractScript_code = V005.Expression_Seq
            [ V005.Expression_Prim $ V005.MichelinePrimAp
              (V005.MichelinePrimitive "parameter")
              [ V005.Expression_Prim $ V005.MichelinePrimAp
                (V005.MichelinePrimitive "pair")
                [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "nat") []
                , V005.Expression_Prim $ V005.MichelinePrimAp
                  (V005.MichelinePrimitive "option")
                  [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "key_hash") [] ]
                ]
              ]
            , V005.Expression_Prim $ V005.MichelinePrimAp
              (V005.MichelinePrimitive "storage")
              [ V005.Expression_Prim $ V005.MichelinePrimAp
                (V005.MichelinePrimitive "pair")
                [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "nat") []
                , V005.Expression_Prim $ V005.MichelinePrimAp
                  (V005.MichelinePrimitive "pair")
                  [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "nat") []
                  , V005.Expression_Prim $ V005.MichelinePrimAp
                    (V005.MichelinePrimitive "list")
                    [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "key") [] ]
                  ]
                ]
              ]
            , V005.Expression_Prim $ V005.MichelinePrimAp
               (V005.MichelinePrimitive "code") [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "UNIT") [] ]
            ]
          , V005._contractScript_storage = V005.Expression_Prim $ V005.MichelinePrimAp
            (V005.MichelinePrimitive "Pair")
            [ V005.Expression_Int 0
            , V005.Expression_Prim $ V005.MichelinePrimAp
              (V005.MichelinePrimitive "Pair")
              [ V005.Expression_Int 2
              , V005.Expression_Seq
                [ V005.Expression_String "edpktjENiqmwLD1vQdLc2PwtTPWpAM9i3jQQenAEsSWLtsZMBbMjME"
                , V005.Expression_String "edpkudQarx27avpEsMKGKXevZKb2Maa1voyfi7uJazcbxgCFd4Nufh"
                , V005.Expression_String "edpkutw9BqRcChuVFLZ7rpAj59gQgjzm3ihdGj5iFmQwCcdwhRFCqm"
                ]
              ]
            ]
          }
        }
    ]
  }

testFilePath :: FilePath -> FilePath
testFilePath = ("tests/Tezos/V005/NodeRPC/CrossCompatTests/" <>)
 
tests :: TestTree
tests = testGroup "Tezos.V005.NodeRPC.CrossCompat"
  [ testGroup "Account"
    [ aesonRoundTripTest "V005" (testFilePath "AccountV005.json") (AccountV005 testAccountV005)
    , aesonRoundTripTest "V004" (testFilePath "AccountV004.json") (AccountV004 testAccountV004)
    ]
  , testGroup "Operation"
    [ testGroup "Transaction"
      [ aesonRoundTripTest "V005" (testFilePath "OperationTransactionV005.json") (OperationV005 testOperationTransactionV005)
      , aesonRoundTripTest "V004" (testFilePath "OperationTransactionV004.json") (OperationV004 testOperationTransactionV004)
      ]
    , testGroup "Origination"
      [ aesonRoundTripTest "V005" (testFilePath "OperationOriginationV005.json") (OperationV005 testOperationOriginationV005)
      , aesonRoundTripTest "V004" (testFilePath "OperationOriginationV004.json") (OperationV004 testOperationOriginationV004)
      ]
    ]
  ]
