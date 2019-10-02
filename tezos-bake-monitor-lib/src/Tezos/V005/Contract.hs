{-# LANGUAGE TemplateHaskell #-}
module Tezos.V005.Contract (module Tezos.V005.Contract, module Old) where

import Data.Typeable (Typeable)

import Tezos.V005.Json (deriveTezosJson)
import Tezos.V005.Binary as B
import Tezos.V005.Micheline (Expression)

import Tezos.V004.Contract as Old hiding (ContractScript(..))

{-
Changes from V004 to V005:

- Updated to use the V005 version of Micheline that has the new APPLY primitive.
-}

-- | "scripted.contracts": {
data ContractScript = ContractScript
  { _contractScript_code :: Expression --  "code": { "$ref": "#/definitions/micheline.michelson_v1.expression" },
  , _contractScript_storage :: Expression --  "storage": { "$ref": "#/definitions/micheline.michelson_v1.expression" }
  }
  deriving (Eq, Ord, Show, Typeable)

instance B.TezosBinary ContractScript where
  put = B.puts _contractScript_code B.<** B.puts _contractScript_storage
  get = ContractScript <$> B.get <*> B.get

deriveTezosJson ''ContractScript
