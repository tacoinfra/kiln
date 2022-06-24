{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.Common.Ballot where

import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Tezos.Common.Json (deriveTezosJson)

-- | "ballot": { "type": "string", "enum": [ "nay", "yay", "pass" ] },
data Ballot
   = Ballot_Nay
   | Ballot_Yay
   | Ballot_Pass
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Generic)

deriveTezosJson ''Ballot
