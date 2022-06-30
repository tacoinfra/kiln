{-# LANGUAGE FlexibleInstances #-}

module Tezos.Common.Accusation where

import Tezos.Common.BalanceUpdate
import Tezos.Common.Base58Check
import Tezos.Common.Level

data AccusationType
  = AccusationType_DoubleBake
  | AccusationType_DoubleEndorsement
  | AccusationType_DoublePreendorsement
  deriving (Eq, Ord, Enum, Read, Show)

data AccusationInfo = AccusationInfo
  { _accusationInfo_type :: AccusationType
  , _accusationInfo_accusedLevel :: RawLevel
  , _accusationInfo_opHash :: OperationHash
  , _accusationInfo_balanceUpdates :: [BalanceUpdate]
  } deriving (Eq, Ord, Show)

class MayHaveAccusations a where
  getAccusations :: a -> [AccusationInfo]

instance (Foldable t, MayHaveAccusations a) => MayHaveAccusations (t a) where
  getAccusations = concatMap getAccusations
