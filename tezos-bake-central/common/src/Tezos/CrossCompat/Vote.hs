{-# LANGUAGE LambdaCase #-}
module Tezos.CrossCompat.Vote where

import Data.Aeson (FromJSON(..))
import Data.Sequence (Seq)

import Tezos.Common.Base58Check (ProtocolHash)
import qualified Tezos.Mumbai.Vote as Mumbai

data VoterListingsCrossCompat = VoterListingsMumbai (Seq Mumbai.VoterDelegate)
  deriving Show

instance FromJSON VoterListingsCrossCompat where
  parseJSON jv = VoterListingsMumbai <$> parseJSON jv

data ProposalVotesListCrossCompat = ProposalVotesListMumbai (Seq Mumbai.ProposalVotes)

getProposalVotesListCrossCompatProtocolHashes :: ProposalVotesListCrossCompat -> Seq ProtocolHash
getProposalVotesListCrossCompatProtocolHashes = \case
  ProposalVotesListMumbai l -> fmap (fst . Mumbai.unProposalVotes) l

instance FromJSON ProposalVotesListCrossCompat where
  parseJSON jv = ProposalVotesListMumbai <$> parseJSON jv

data BallotsCrossCompat = BallotsMumbai Mumbai.Ballots

instance FromJSON BallotsCrossCompat where
  parseJSON jv = BallotsMumbai <$> parseJSON jv
