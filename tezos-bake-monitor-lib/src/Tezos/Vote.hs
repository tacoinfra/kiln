{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.Vote where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Int
import Data.Typeable
import GHC.Generics
import Tezos.Base58Check
import Tezos.Json
import Tezos.PublicKeyHash

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

data Delegate = Delegate
  { _delegate_pkh :: PublicKeyHash
  , _delegate_rolls :: Int
  } deriving (Eq, Ord, Read, Show, Typeable, Generic)

deriveTezosJson ''Delegate
makeLenses 'Delegate

