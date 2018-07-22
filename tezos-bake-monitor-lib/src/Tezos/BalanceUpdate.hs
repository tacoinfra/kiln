{-# LANGUAGE TemplateHaskell #-}

module Tezos.BalanceUpdate where

import Data.Typeable

import Tezos.Contract
import Tezos.PublicKeyHash
import Tezos.Tez
import Tezos.Level
import Tezos.Json

data FreezerCategory
   = FreezerCategory_Rewards -- ^ *category": { "type": "string", "enum": [ "rewards" ] },
   | FreezerCategory_Fees -- ^ *category": { "type": "string", "enum": [ "fees" ] },
   | FreezerCategory_Deposits -- ^ *category": { "type": "string", "enum": [ "deposits" ] },
  deriving (Eq, Ord, Show, Typeable)

data ContractUpdate = ContractUpdate
  { _contractUpdate_contract :: !ContractId -- ^ *contract": { "$ref": "#/definitions/contract_id" },
  , _contractUpdate_change :: !Tez -- ^ *change": { "$ref": "#/definitions/int64" } },
  }
  deriving (Eq, Ord, Show, Typeable)

data FreezerUpdate = FreezerUpdate
  { _freezerUpdate_category :: !FreezerCategory -- ^ "category": { "type": "string", "enum": ... }
  , _freezerUpdate_delegate :: !PublicKeyHash -- ^ *delegate": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _freezerUpdate_level :: !RawLevel -- ^ *level": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 },
  , _freezerUpdate_change :: !Tez -- ^ *change": { "$ref": "#/definitions/int64" }
  }
  deriving (Eq, Ord, Show, Typeable)

data BalanceUpdate
   = BalanceUpdate_Contract ContractUpdate
   | BalanceUpdate_Freezer FreezerUpdate
  deriving (Eq, Ord, Show, Typeable)

concat <$> traverse deriveTezosJson
  [ ''BalanceUpdate
  , ''ContractUpdate
  , ''FreezerUpdate
  , ''FreezerCategory
  ]
