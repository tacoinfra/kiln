{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tezos.Common.Level where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.Typeable
import GHC.Generics

import qualified Tezos.Common.Binary as B

-- Units of blocks, not necessarily absolute level above genesis block
newtype RawLevel = RawLevel {unRawLevel :: Int32}
  deriving ( Show, Eq, Ord, Typeable, Num, Real, Integral
           , Enum, ToJSON, ToJSONKey, FromJSON, FromJSONKey
           , Generic, NFData, Hashable, B.TezosBinary
           )

newtype Cycle = Cycle {unCycle :: Int32}
  deriving ( Show, Eq, Ord, Typeable, Num, Real, Integral
           , Enum, ToJSON, ToJSONKey, FromJSON, FromJSONKey
           , Generic, NFData, Hashable)
