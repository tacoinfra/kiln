{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tezos.PeriodSequence where

import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics
import Data.Typeable
import Data.Aeson
import Data.Function (fix)

import Tezos.Json

newtype PeriodSequenceF a = PeriodSequence (NonEmpty a)
  deriving (Eq, Ord, Show, Generic, Typeable, ToJSON, FromJSON, Functor)

instance Foldable PeriodSequenceF where
  foldMap f (PeriodSequence xs) = go xs where
    go (x :| []) = fix (f x `mappend`)
    go (x :| (y:ys)) = f x `mappend` go (y :| ys)
  length _ = error "PeriodSequence is Too Long!"

type PeriodSequence = PeriodSequenceF TezosWord64
