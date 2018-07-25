{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Tezos.BalanceUpdate where

import Data.Aeson
import Data.Semigroup
import Data.Text (Text)
import Data.Typeable
import qualified Data.HashMap.Strict as HashMap

import Tezos.Contract
import Tezos.PublicKeyHash
import Tezos.Tez
import Tezos.Level
import Tezos.Json

data FreezerCategory
   = FreezerCategory_Rewards --  *category": { "type": "string", "enum": [ "rewards" ] },
   | FreezerCategory_Fees --  *category": { "type": "string", "enum": [ "fees" ] },
   | FreezerCategory_Deposits --  *category": { "type": "string", "enum": [ "deposits" ] },
  deriving (Eq, Ord, Show, Typeable)

data ContractUpdate = ContractUpdate
  { _contractUpdate_contract :: !ContractId --  *contract": { "$ref": "#/definitions/contract_id" },
  , _contractUpdate_change :: !Tez --  *change": { "$ref": "#/definitions/int64" } },
  }
  deriving (Eq, Ord, Show, Typeable)

data FreezerUpdate = FreezerUpdate
  { _freezerUpdate_category :: !FreezerCategory --  "category": { "type": "string", "enum": ... }
  , _freezerUpdate_delegate :: !PublicKeyHash --  *delegate": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _freezerUpdate_level :: !RawLevel --  *level": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _freezerUpdate_change :: !Tez --  *change": { "$ref": "#/definitions/int64" }
  }
  deriving (Eq, Ord, Show, Typeable)

data BalanceUpdate
   = BalanceUpdate_Contract ContractUpdate
   | BalanceUpdate_Freezer FreezerUpdate
  deriving (Eq, Ord, Show, Typeable)

instance FromJSON BalanceUpdate where
  parseJSON = withObject "BalanceUpdate" $ \v -> do
    kind :: Text <- v .: "kind"
    case kind of
      "contract" -> BalanceUpdate_Contract <$> parseJSON (Object v)
      "freezer" -> BalanceUpdate_Freezer <$> parseJSON (Object v)
      bad -> fail $ "wrong kind:" <> show bad

instance ToJSON BalanceUpdate where
  toJSON (BalanceUpdate_Contract x) = case toJSON x of
    Object xs -> Object $ xs <> HashMap.singleton "kind" "contract"
    _ -> error "ToJSON did not return an object"
  toJSON (BalanceUpdate_Freezer x) = case toJSON x of
    Object xs -> Object $ xs <> HashMap.singleton "kind" "freezer"
    _ -> error "ToJSON did not return an object"


concat <$> traverse deriveTezosJson
  [ ''ContractUpdate
  , ''FreezerUpdate
  , ''FreezerCategory
  ]
