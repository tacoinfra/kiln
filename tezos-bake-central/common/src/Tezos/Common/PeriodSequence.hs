{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tezos.Common.PeriodSequence where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Function (fix)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Tezos.Common.Json

newtype PeriodSequenceF a = PeriodSequence { unPeriodSequence :: NonEmpty a }
  deriving (Eq, Ord, Show, Generic, Typeable, ToJSON, FromJSON, Functor, NFData, Hashable)

instance Foldable PeriodSequenceF where
  foldMap f (PeriodSequence xs) = go xs where
    go (x :| []) = fix (f x `mappend`)
    go (x :| (y:ys)) = f x `mappend` go (y :| ys)
  length _ = error "PeriodSequence is Too Long!"

type PeriodSequence = PeriodSequenceF TezosWord64
