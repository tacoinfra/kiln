{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Tezos.V005.Account
  ( module Tezos.V005.Account
  , module Old
  )
  where
  
import Control.Lens.TH (makeLenses)
import Data.Typeable

import Tezos.V005.Contract
import Tezos.V005.Json
import Tezos.V005.PublicKeyHash
import Tezos.V005.Tez

import Tezos.V004.Account as Old hiding (Account(..), account_delegate, account_balance, account_counter, account_script)

data Account = Account
  { _account_delegate :: !(Maybe PublicKeyHash) --  { "$ref": "#/definitions/Signature.Public_key_hash" },
  , _account_balance :: !Tez -- "2052452947621" "balance": { "$ref": "#/definitions/mutez" },
  , _account_script :: !(Maybe ContractScript) -- "script": { "$ref": "#/definitions/scripted.contracts" },
  , _account_counter :: !(Maybe TezosWord64) -- 1540 "counter": { "$ref": "#/definitions/positive_bignum" }
  } deriving (Show, Eq, Ord, Typeable)

concat <$> traverse deriveTezosJson
  [ ''Account
  ]

concat <$> traverse makeLenses
  [ 'Account
  ]
