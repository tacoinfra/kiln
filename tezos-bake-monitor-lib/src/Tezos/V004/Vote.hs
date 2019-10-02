{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.V004.Vote where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Int
import Data.Typeable
import GHC.Generics
import Tezos.V004.Base58Check
import Tezos.V004.Json
import Tezos.V004.PublicKeyHash

type VotingPeriod = Int32 -- ^ period: Voting_period_repr.t ;

data Ballots = Ballots
  { _ballots_yay :: Int
  , _ballots_nay :: Int
  , _ballots_pass :: Int
  } deriving (Eq, Ord, Read, Show, Typeable, Generic)

deriveTezosJson ''Ballots
makeLenses 'Ballots

newtype ProposalVotes = ProposalVotes { unProposalVotes :: (ProtocolHash, Int) }
  deriving (Eq, Ord, Read, Show, Typeable, Generic, FromJSON, ToJSON)

data VoterDelegate = VoterDelegate
  { _voterDelegate_pkh :: PublicKeyHash
  , _voterDelegate_rolls :: Int
  } deriving (Eq, Ord, Read, Show, Typeable, Generic)

deriveTezosJson ''VoterDelegate
makeLenses 'VoterDelegate

