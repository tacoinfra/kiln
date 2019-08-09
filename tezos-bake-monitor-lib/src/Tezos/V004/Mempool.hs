{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wwarn=orphans #-}

module Tezos.V004.Mempool where

import Data.Aeson
import Data.Aeson.TH
import Data.Dependent.Sum
import GHC.Generics

import Tezos.Common.Base58Check
import Tezos.Common.Json

import Tezos.V004.Operation

data Mempool = Mempool
  { _mempool_applied :: [Result (DSum OpsKindTag PendingOp)]
  , _mempool_refused :: [Result (OperationHash, DSum OpsKindTag ErroredOp)]
  , _mempool_branchRefused :: [Result (OperationHash, DSum OpsKindTag ErroredOp)]
  , _mempool_branchDelayed :: [Result (OperationHash, DSum OpsKindTag ErroredOp)]
  , _mempool_unprocessed :: [Result (OperationHash, DSum OpsKindTag ProtoOp)]
  } deriving Generic

instance FromJSON (Result (DSum OpsKindTag PendingOp)) where
  parseJSON = pure . fromJSON

instance FromJSON (Result (OperationHash, DSum OpsKindTag ErroredOp)) where
  parseJSON = pure . fromJSON

instance FromJSON (Result (OperationHash, DSum OpsKindTag ProtoOp)) where
  parseJSON = pure . fromJSON

deriveFromJSON tezosJsonOptions 'Mempool
