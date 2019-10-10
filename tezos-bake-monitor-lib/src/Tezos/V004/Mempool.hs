{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wwarn=orphans #-}

module Tezos.V004.Mempool where

import Data.Aeson.TH
import Data.Dependent.Sum
import GHC.Generics

import Tezos.Common.Base58Check
import Tezos.Common.Json

import Tezos.V004.Operation

data Mempool = Mempool
  { _mempool_applied :: [DSum OpsKindTag PendingOp]
  , _mempool_refused :: [(OperationHash, DSum OpsKindTag ErroredOp)]
  , _mempool_branchRefused :: [(OperationHash, DSum OpsKindTag ErroredOp)]
  , _mempool_branchDelayed :: [(OperationHash, DSum OpsKindTag ErroredOp)]
  , _mempool_unprocessed :: [(OperationHash, DSum OpsKindTag ProtoOp)]
  } deriving Generic

deriveFromJSON tezosJsonOptions 'Mempool
