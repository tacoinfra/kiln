{-# LANGUAGE FlexibleInstances #-}

module Tezos.Common.Accusation where

import Tezos.Common.BalanceUpdate
import Tezos.Common.Base58Check
import Tezos.Common.PublicKeyHash (PublicKeyHash)
import Tezos.Common.Level

data AccusationType
  = AccusationType_DoubleBake
  | AccusationType_DoubleEndorsement
  | AccusationType_DoublePreendorsement
  deriving (Eq, Ord, Enum, Read, Show)

data AccusationInfo = AccusationInfo
  { _accusationInfo_type :: Maybe AccusationType
  , _accusationInfo_accusedLevel :: RawLevel
  , _accusationInfo_opHash :: OperationHash
  , _accusationInfo_accusedInfo :: Either [BalanceUpdate] PublicKeyHash
  -- In older protocol the accused information came in the form
  -- balance_updates, but starting from Oxford, it (the accused address) is
  -- explicitly included in the metadata. Hence the either type for this field.
  -- Ideally this should have been abstracted in the node interface layer, ie
  -- just pass the accused address instead of including balance updates, but
  -- just sticking to existing patterns that is in current code.
  } deriving (Eq, Ord, Show)

class MayHaveAccusations a where
  getAccusations :: a -> [AccusationInfo]

instance (Foldable t, MayHaveAccusations a) => MayHaveAccusations (t a) where
  getAccusations = concatMap getAccusations
