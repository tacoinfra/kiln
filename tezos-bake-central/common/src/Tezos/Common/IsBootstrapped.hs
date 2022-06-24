{-# LANGUAGE TemplateHaskell #-}

module Tezos.Common.IsBootstrapped where

import Control.Lens.TH (makeLenses)

import Tezos.Common.Json (deriveTezosJson)

data IsBootstrapped = IsBootstrapped
  { _isBootstrapped_bootstrapped :: Bool
  , _isBootstrapped_syncState :: SyncState
  } deriving (Show)

data SyncState
  = SyncState_Stuck
  | SyncState_Synced
  | SyncState_Unsynced
  deriving (Show, Read, Enum, Ord, Eq)

concat <$> traverse deriveTezosJson
  [ ''SyncState
  , ''IsBootstrapped
  ]

concat <$> traverse makeLenses
 [ 'IsBootstrapped
 ]
