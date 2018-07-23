{-# LANGUAGE TemplateHaskell #-}
module Tezos.Account where

import Data.Time
import Data.Typeable
import Data.Word
import qualified Data.Sequence as Seq

import Tezos.BlockHeader (Priority(..))
import Tezos.Contract
import Tezos.Json
import Tezos.Level
import Tezos.PublicKeyHash
import Tezos.Tez

data AccountDelegate = AccountDelegate
  { _accountDelegate_setable :: !Bool -- "setable": { "type": "boolean" },
  , _accountDelegate_value :: !(Maybe PublicKeyHash) -- "value": { "$ref": "#/definitions/Signature.Public_key_hash" }
  } deriving (Show, Eq, Ord, Typeable)

data Account = Account
  { _account_manager :: !PublicKeyHash -- "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" "manager": { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _account_balance :: !Tez -- "2052452947621" "balance": { "$ref": "#/definitions/mutez" },
  , _account_spendable :: !Bool -- true "spendable": { "type": "boolean" },
  , _account_delegate :: !AccountDelegate -- "delegate": { "type": "object", "properties": {
  , _account_script :: !(Maybe ContractScript) -- "script": { "$ref": "#/definitions/scripted.contracts" },
  , _account_counter :: !TezosWord64 -- 1540 "counter": { "$ref": "#/definitions/positive_bignum" }
  } deriving (Show, Eq, Ord, Typeable)

data BakingRights = BakingRights
  { _bakingRights_level :: !RawLevel
  , _bakingRights_delegate :: !PublicKeyHash
  , _bakingRights_priority :: !Priority
  , _bakingRights_estimatedTime :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show)

data EndorsingRights = EndorsingRights
  { _endorsingRights_level :: !RawLevel
  , _endorsingRights_delegate :: !PublicKeyHash
  , _endorsingRights_slots :: !(Seq.Seq Word8)
  , _endorsingRights_estimatedTime :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show)


concat <$> traverse deriveTezosJson
  [ ''Account
  , ''AccountDelegate
  , ''BakingRights
  , ''EndorsingRights
  ]
