{-# LANGUAGE DeriveGeneric #-}

module Common.Vote where

import Data.Int
import Data.Typeable
import GHC.Generics

import Common.TezosBinary

type VotingPeriod = Int32 -- ^ period: Voting_period_repr.t ;

data Ballot
  = Ballot_Yay
  | Ballot_Nay
  | Ballot_Pass
  deriving (Eq, Ord, Show, Read, Typeable, Generic, Enum, Bounded)

instance TezosBinary Ballot where
  parseBinary = parseEnum
  encodeBinary = encodeEnum
