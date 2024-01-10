{-# LANGUAGE LambdaCase #-}
module Tezos.CrossCompat.Vote where

import Data.Aeson (FromJSON(..))
import Data.Sequence (Seq)

import Tezos.Common.Base58Check (ProtocolHash)
import qualified Tezos.Oxford.Vote as Oxford

data VoterListingsCrossCompat = VoterListingsOxford (Seq Oxford.VoterDelegate)
  deriving Show

instance FromJSON VoterListingsCrossCompat where
  parseJSON jv = VoterListingsOxford <$> parseJSON jv

data ProposalVotesListCrossCompat = ProposalVotesListOxford (Seq Oxford.ProposalVotes)

getProposalVotesListCrossCompatProtocolHashes :: ProposalVotesListCrossCompat -> Seq ProtocolHash
getProposalVotesListCrossCompatProtocolHashes = \case
  ProposalVotesListOxford l -> fmap (fst . Oxford.unProposalVotes) l

instance FromJSON ProposalVotesListCrossCompat where
  parseJSON jv = ProposalVotesListOxford <$> parseJSON jv

data BallotsCrossCompat = BallotsOxford Oxford.Ballots

instance FromJSON BallotsCrossCompat where
  parseJSON jv = BallotsOxford <$> parseJSON jv
