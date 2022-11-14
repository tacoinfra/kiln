{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.Lima.Vote where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Tezos.Common.Base58Check
import Tezos.Common.Json
import Tezos.Common.PublicKeyHash
import Tezos.Common.Tez

data Ballots = Ballots
  { _ballots_yay :: Tez
  , _ballots_nay :: Tez
  , _ballots_pass :: Tez
  } deriving (Eq, Ord, Read, Show)

deriveTezosJson ''Ballots
makeLenses 'Ballots

newtype ProposalVotes = ProposalVotes { unProposalVotes :: (ProtocolHash, Tez) }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON)

data VoterDelegate = VoterDelegate
  { _voterDelegate_pkh :: PublicKeyHash
  , _voterDelegate_votingPower :: Tez
  } deriving (Eq, Ord, Read, Show)

deriveTezosJson ''VoterDelegate
makeLenses 'VoterDelegate
