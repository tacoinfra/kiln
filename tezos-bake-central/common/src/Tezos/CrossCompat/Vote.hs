{-# LANGUAGE LambdaCase #-}
module Tezos.CrossCompat.Vote where

import Data.Aeson (FromJSON(..))
import Data.Sequence (Seq)

import Tezos.Common.Base58Check (ProtocolHash)
import qualified Tezos.Lima.Vote as Lima

data VoterListingsCrossCompat = VoterListingsLima (Seq Lima.VoterDelegate)
  deriving Show

instance FromJSON VoterListingsCrossCompat where
  parseJSON jv = VoterListingsLima <$> parseJSON jv

data ProposalVotesListCrossCompat = ProposalVotesListLima (Seq Lima.ProposalVotes)

getProposalVotesListCrossCompatProtocolHashes :: ProposalVotesListCrossCompat -> Seq ProtocolHash
getProposalVotesListCrossCompatProtocolHashes = \case
  ProposalVotesListLima l -> fmap (fst . Lima.unProposalVotes) l

instance FromJSON ProposalVotesListCrossCompat where
  parseJSON jv = ProposalVotesListLima <$> parseJSON jv

data BallotsCrossCompat = BallotsLima Lima.Ballots

instance FromJSON BallotsCrossCompat where
  parseJSON jv = BallotsLima <$> parseJSON jv
