{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Tezos.V005.NodeRPC.CrossCompatTests where

import qualified Data.ByteString.Base16 as BS16
import Data.Text (Text)
import Data.Time (UTCTime(UTCTime), fromGregorian)
import Test.Tasty
import Test.Tasty.HUnit

import Tezos.TestUtils ((@?~), aesonRoundTripTest)
import Tezos.V005.NodeRPC.CrossCompat
import qualified Tezos.V004.Operation as V004
import qualified Tezos.V004.Micheline as V004
import qualified Tezos.V004.Types as V004
import qualified Tezos.V005.Contract as V005
import qualified Tezos.V005.Operation as V005
import qualified Tezos.V005.Micheline as V005
import qualified Tezos.V005.Types as V005

import qualified Tezos.Common.ShortByteString as Common

yoloV004Pkh :: Text -> V004.PublicKeyHash
yoloV004Pkh = either (error . show) id .  V004.tryReadPublicKeyHashText

yoloV005Pkh :: Text -> V005.PublicKeyHash
yoloV005Pkh = either (error . show) id .  V005.tryReadPublicKeyHashText

yoloB16ByteString = V005.Base16ByteString . fst . BS16.decode
yoloShortByteString = Common.toShort . fst . BS16.decode

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
          , V005._managerOperationMetadata_internalOperationResults = Nothing
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
            [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "DROP") [] []
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "NIL")
              [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "operation") [] []
              ] []
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "PUSH")
              [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "key_hash") [] []
              , V005.Expression_String "tz1M7RpncdPVx19rtZda42UNDWon4NE5kmGu"
              ] []
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "IMPLICIT_ACCOUNT") [] []
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "PUSH")
              [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "mutez") [] []
              , V005.Expression_Int 2000
              ] []
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "UNIT") [] []
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "TRANSFER_TOKENS") [] []
            , V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "CONS") [] []
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
          , V004._managerOperationMetadata_internalOperationResults = Nothing
          }
        , V004._operationContentsTransaction_source = "tz1SiPXX4MYGNJNDsRc7n8hkvUqFzg8xqF9m"
        , V004._operationContentsTransaction_fee = 0.001420
        , V004._operationContentsTransaction_counter = 1991234
        , V004._operationContentsTransaction_gasLimit = 11000
        , V004._operationContentsTransaction_storageLimit = 300
        , V004._operationContentsTransaction_amount = 326.093080
        , V004._operationContentsTransaction_destination = "tz1ZADZUDP7brp17j6iRsujboGoUzsdqZzX2"
        , V004._operationContentsTransaction_parameters = Just $ V004.Expression_Prim $
          V004.MichelinePrimAp (V004.MichelinePrimitive "Unit") [] []
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
          , V004._managerOperationMetadata_internalOperationResults = Nothing
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
          , V004._managerOperationMetadata_internalOperationResults = Nothing
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
          , V005._managerOperationMetadata_internalOperationResults = Nothing
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
          , V005._managerOperationMetadata_internalOperationResults = Nothing
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
                [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "nat") [] []
                , V005.Expression_Prim $ V005.MichelinePrimAp
                  (V005.MichelinePrimitive "option")
                  [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "key_hash") [] [] ] []
                ] []
              ] []
            , V005.Expression_Prim $ V005.MichelinePrimAp
              (V005.MichelinePrimitive "storage")
              [ V005.Expression_Prim $ V005.MichelinePrimAp
                (V005.MichelinePrimitive "pair")
                [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "nat") [] []
                , V005.Expression_Prim $ V005.MichelinePrimAp
                  (V005.MichelinePrimitive "pair")
                  [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "nat") [] []
                  , V005.Expression_Prim $ V005.MichelinePrimAp
                    (V005.MichelinePrimitive "list")
                    [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "key") [] [] ] []
                  ] []
                ] []
              ] []
            , V005.Expression_Prim $ V005.MichelinePrimAp
               (V005.MichelinePrimitive "code") [ V005.Expression_Prim $ V005.MichelinePrimAp (V005.MichelinePrimitive "UNIT") [] [] ] []
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
              ] []
            ] []
          }
        }
    ]
  }

testBlockV005 = V005.Block
  "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU"
  "NetXKakFj1A7ouL"
  "BLFoapow3NsnzgixARx5NGnqeukqyotyATbtJHy2qLq6mAdvFWE"
  (V005.BlockHeaderFull
    { V005._blockHeaderFull_level = 195065
    , V005._blockHeaderFull_proto = 1
    , V005._blockHeaderFull_predecessor = "BMN5KhRJqsqFkwiQVW1sz64iPrxYcw3xb6AXhvk9HFnKpf6Dj9L"
    , V005._blockHeaderFull_timestamp = UTCTime (fromGregorian 2019 9 23) (19*60*60 + 58*60 + 33)
    , V005._blockHeaderFull_validationPass = 4
    , V005._blockHeaderFull_operationsHash = "LLobDQLDmcttWHvKbJf97Q2RdabxE2FHKT5rzAj15HEkU9f2EVEFW"
    , V005._blockHeaderFull_fitness = V005.toFitness [yoloShortByteString "01", yoloShortByteString "000000000002f9f8"]
    , V005._blockHeaderFull_context = "CoVNLLcTdBpGWn2A5JL9SEnNRuXwaConz2vNVojMoD5yT7VEUjSe"
    , V005._blockHeaderFull_priority = 0
    , V005._blockHeaderFull_proofOfWorkNonce = yoloB16ByteString "00000003a170d53d"
    , V005._blockHeaderFull_seedNonceHash = Nothing
    , V005._blockHeaderFull_signature = Just "sigRnjCxTV8fGdJo5uy2Fqf4mJL1tR6cgLz9KPGG2bQ529zob32SPkMJiA8UFvBxMPzv6PaNvkyrhvuBfvW6ET3KQcTgKXWB"
    })

  (V005.BlockMetadata
    { V005._blockMetadata_protocol = "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU"
    , V005._blockMetadata_nextProtocol = "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU"
    , V005._blockMetadata_testChainStatus = V005.TestChainStatus_NotRunning
    , V005._blockMetadata_maxOperationsTtl = 60
    , V005._blockMetadata_maxOperationDataLength = 16384
    , V005._blockMetadata_maxBlockHeaderLength = 238
    , V005._blockMetadata_maxOperationListLength =
      [ V005.MaxOperationListLength 32768 (Just 32) 
      , V005.MaxOperationListLength 32768 Nothing
      , V005.MaxOperationListLength 135168 (Just 132)
      , V005.MaxOperationListLength 524288 Nothing
      ]
    , V005._blockMetadata_baker = "tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB"
    , V005._blockMetadata_level = V005.Level
      { V005._level_cycle = 1523
      , V005._level_cyclePosition = 120
      , V005._level_expectedCommitment = False
      , V005._level_level = 195065
      , V005._level_levelPosition = 195064
      , V005._level_votingPeriod = 69
      , V005._level_votingPeriodPosition = 760
      }
    , V005._blockMetadata_votingPeriodKind = V005.VotingPeriodKind_Proposal
    , V005._blockMetadata_nonceHash = Nothing
    , V005._blockMetadata_consumedGas = 0
    , V005._blockMetadata_deactivated = []
    , V005._blockMetadata_balanceUpdates =
      [ V005.BalanceUpdate_Contract $ V005.ContractUpdate
        { V005._contractUpdate_contract = "tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB"
        , V005._contractUpdate_change = -512
        }
      , V005.BalanceUpdate_Freezer $ V005.FreezerUpdate
        { V005._freezerUpdate_category = V005.FreezerCategory_Deposits
        , V005._freezerUpdate_delegate = "tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB"
        , V005._freezerUpdate_cycle = 1523 
        , V005._freezerUpdate_change = 512
        }
      , V005.BalanceUpdate_Freezer $ V005.FreezerUpdate
        { V005._freezerUpdate_category = V005.FreezerCategory_Rewards
        , V005._freezerUpdate_delegate = "tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB"
        , V005._freezerUpdate_cycle = 1523
        , V005._freezerUpdate_change = 16
        }
      ]
    }
  )
  [ [ V005.Operation
      { V005._operation_protocol = "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU"
      , V005._operation_chainId = "NetXKakFj1A7ouL"
      , V005._operation_hash = "ooduiNVDtPcaXPBuhtwpks5eXsTpyFkFRtTTXkiiFosYTt5Q25T"
      , V005._operation_branch = "BMN5KhRJqsqFkwiQVW1sz64iPrxYcw3xb6AXhvk9HFnKpf6Dj9L"
      , V005._operation_signature = Just "siguVYNSB4qudBTiQ6SHDpVWjeZT5uhM331EKbBisuG1dPWent6MyRBcozm5sMbNrLUA9vuXZeBvx5zrTYmYNXAs9SDJLLZ7"
      , V005._operation_contents = 
        [ V005.OperationContents_Endorsement $ V005.OperationContentsEndorsement
          { V005._operationContentsEndorsement_metadata = V005.EndorsementMetadata
            { V005._endorsementMetadata_balanceUpdates =
              [ V005.BalanceUpdate_Contract $ V005.ContractUpdate
                { V005._contractUpdate_contract = "tz1aWXP237BLwNHJcCD4b3DutCevhqq2T1Z9"
                , V005._contractUpdate_change = -256
                }
             , V005.BalanceUpdate_Freezer $ V005.FreezerUpdate
               { V005._freezerUpdate_category = V005.FreezerCategory_Deposits
               , V005._freezerUpdate_delegate = "tz1aWXP237BLwNHJcCD4b3DutCevhqq2T1Z9"
               , V005._freezerUpdate_cycle = 1523
               , V005._freezerUpdate_change = 256
               }
             , V005.BalanceUpdate_Freezer $ V005.FreezerUpdate
               { V005._freezerUpdate_category = V005.FreezerCategory_Rewards
               , V005._freezerUpdate_delegate = "tz1aWXP237BLwNHJcCD4b3DutCevhqq2T1Z9"
               , V005._freezerUpdate_cycle = 1523
               , V005._freezerUpdate_change = 8
               }
            ]
          , V005._endorsementMetadata_delegate = "tz1aWXP237BLwNHJcCD4b3DutCevhqq2T1Z9"
          , V005._endorsementMetadata_slots = [29, 19, 9, 3]
          }
        , V005._operationContentsEndorsement_level = 195064
        }
      ]
    }], [], [], []
  ]

testBlockV004 = V004.Block 
  "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"
  "NetXgtSLGNJvNye"
  "BLpSg6w9hrm3bmx7yrnNDT2fgfvtBAfxvrHZbPdRhoo9n11v16B"
  (V004.BlockHeaderFull
    { V004._blockHeaderFull_level = 681656
    , V004._blockHeaderFull_proto = 2
    , V004._blockHeaderFull_predecessor = "BLfMs3wwL7rAZN1v4YsX35F4wfzz3shYbjp3hWrP3fRkiPwBTnG"
    , V004._blockHeaderFull_timestamp = UTCTime (fromGregorian 2019 9 24) (3*60*60 + 45*60 + 36)
    , V004._blockHeaderFull_validationPass = 4
    , V004._blockHeaderFull_operationsHash = "LLoZNzRWtTVkzNMSe9YsUUNpqZi4T2KNcQk9Ahn47Jsra1yJpjQSZ"
    , V004._blockHeaderFull_fitness = V004.toFitness [yoloShortByteString "00", yoloShortByteString "00000000013aa275"]
    , V004._blockHeaderFull_context = "CoV3cidqukDYmpPDyGQEM9XhBq82gj14amZX3zbkzs76v8zgQQFN"
    , V004._blockHeaderFull_priority = 0
    , V004._blockHeaderFull_proofOfWorkNonce = yoloB16ByteString "000000036d00b628"
    , V004._blockHeaderFull_seedNonceHash = Nothing
    , V004._blockHeaderFull_signature = Just "siga6MwX3zFJ2ig4e726WWEDmiyQTwgkQSZnbndX7AzJQWnRXjWKEzrcF44r6GsnUwWe5sP36AV8sv4VsbqbziG41g2jcsbW"
    })

  (V004.BlockMetadata
    { V004._blockMetadata_protocol = "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"
    , V004._blockMetadata_nextProtocol = "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"
    , V004._blockMetadata_testChainStatus = V004.TestChainStatus_NotRunning
    , V004._blockMetadata_maxOperationsTtl = 60
    , V004._blockMetadata_maxOperationDataLength = 16384
    , V004._blockMetadata_maxBlockHeaderLength = 238
    , V004._blockMetadata_maxOperationListLength =
      [ V004.MaxOperationListLength 32768 (Just 32) 
      , V004.MaxOperationListLength 32768 Nothing
      , V004.MaxOperationListLength 135168 (Just 132)
      , V004.MaxOperationListLength 524288 Nothing
      ]
    , V004._blockMetadata_baker = "tz3gN8NTLNLJg5KRsUU47NHNVHbdhcFXjjaB"
    , V004._blockMetadata_level = V004.Level
      { V004._level_cycle = 332
      , V004._level_cyclePosition = 1719
      , V004._level_expectedCommitment = False
      , V004._level_level = 681656
      , V004._level_levelPosition = 681655
      , V004._level_votingPeriod = 83
      , V004._level_votingPeriodPosition = 1719
      }
    , V004._blockMetadata_votingPeriodKind = V004.VotingPeriodKind_Proposal
    , V004._blockMetadata_nonceHash = Nothing
    , V004._blockMetadata_consumedGas = 0
    , V004._blockMetadata_deactivated = []
    , V004._blockMetadata_balanceUpdates =
      [ V004.BalanceUpdate_Contract $ V004.ContractUpdate
        { V004._contractUpdate_contract = "tz3gN8NTLNLJg5KRsUU47NHNVHbdhcFXjjaB"
        , V004._contractUpdate_change = -512
        }
      , V004.BalanceUpdate_Freezer $ V004.FreezerUpdate
        { V004._freezerUpdate_category = V004.FreezerCategory_Deposits
        , V004._freezerUpdate_delegate = "tz3gN8NTLNLJg5KRsUU47NHNVHbdhcFXjjaB"
        , V004._freezerUpdate_cycle = 332
        , V004._freezerUpdate_change = 512
        }
      , V004.BalanceUpdate_Freezer $ V004.FreezerUpdate
        { V004._freezerUpdate_category = V004.FreezerCategory_Rewards
        , V004._freezerUpdate_delegate = "tz3gN8NTLNLJg5KRsUU47NHNVHbdhcFXjjaB"
        , V004._freezerUpdate_cycle = 332
        , V004._freezerUpdate_change = 16
        }
      ]
    }
  )
  [ [ V004.Operation
      { V004._operation_protocol = "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"
      , V004._operation_chainId = "NetXgtSLGNJvNye"
      , V004._operation_hash = "oodBLVMMphPw266cpSeUR48X25sp3wFzXKrXPa4VRQCBMMEKSRx"
      , V004._operation_branch = "BLfMs3wwL7rAZN1v4YsX35F4wfzz3shYbjp3hWrP3fRkiPwBTnG"
      , V004._operation_signature = Just "sigN1o64SRJ145uwkQgtFe5z2jDYE92ksEnVjdujuHooCMBBRUveNavjs2DMyFNYsMAEsfrrjLw3fQe63KyXDpEb2ugAWfAT"
      , V004._operation_contents = 
        [ V004.OperationContents_Endorsement $ V004.OperationContentsEndorsement
          { V004._operationContentsEndorsement_metadata = V004.EndorsementMetadata
            { V004._endorsementMetadata_balanceUpdates =
              [ V004.BalanceUpdate_Contract $ V004.ContractUpdate
                { V004._contractUpdate_contract = "tz3gN8NTLNLJg5KRsUU47NHNVHbdhcFXjjaB"
                , V004._contractUpdate_change = -768
                }
             , V004.BalanceUpdate_Freezer $ V004.FreezerUpdate
               { V004._freezerUpdate_category = V004.FreezerCategory_Deposits
               , V004._freezerUpdate_delegate = "tz3gN8NTLNLJg5KRsUU47NHNVHbdhcFXjjaB"
               , V004._freezerUpdate_cycle = 332
               , V004._freezerUpdate_change = 768
               }
             , V004.BalanceUpdate_Freezer $ V004.FreezerUpdate
               { V004._freezerUpdate_category = V004.FreezerCategory_Rewards
               , V004._freezerUpdate_delegate = "tz3gN8NTLNLJg5KRsUU47NHNVHbdhcFXjjaB"
               , V004._freezerUpdate_cycle = 332
               , V004._freezerUpdate_change = 4.8
               }
            ]
          , V004._endorsementMetadata_delegate = "tz3gN8NTLNLJg5KRsUU47NHNVHbdhcFXjjaB"
          , V004._endorsementMetadata_slots = [29, 26, 25, 24, 17, 15, 11, 8, 6, 3, 2, 1]
          }
        , V004._operationContentsEndorsement_level = 681655
        }
      ]
    }], [], [], []
  ]

testFilePath :: FilePath -> FilePath
testFilePath = ("tests/Tezos/V005/NodeRPC/CrossCompatTests/" <>)

tests :: TestTree
tests = testGroup "Tezos.V005.NodeRPC.CrossCompat"
  [ testGroup "Account"
    [ aesonRoundTripTest "V005" (testFilePath "AccountV005.json") (AccountV005 testAccountV005)
    , aesonRoundTripTest "V004" (testFilePath "AccountV004.json") (AccountV004 testAccountV004)
    ]
  , testGroup "Block"
    [ aesonRoundTripTest "V005" (testFilePath "BlockV005.json") (BlockV005 testBlockV005)
    , aesonRoundTripTest "V004" (testFilePath "BlockV004.json") (BlockV004 testBlockV004)

    ]
  ]
