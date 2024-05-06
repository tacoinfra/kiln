{-# LANGUAGE LambdaCase #-}
module Tezos.CrossCompat.Vote where

import Data.Aeson (FromJSON(..))
import Data.Sequence (Seq)

import Tezos.Common.Base58Check (ProtocolHash)
import qualified Tezos.Base.Vote as Base

data VoterListingsCrossCompat = VoterListingsBase (Seq Base.VoterDelegate)
  deriving Show

instance FromJSON VoterListingsCrossCompat where
  parseJSON jv = VoterListingsBase <$> parseJSON jv

data ProposalVotesListCrossCompat = ProposalVotesListBase (Seq Base.ProposalVotes)

getProposalVotesListCrossCompatProtocolHashes :: ProposalVotesListCrossCompat -> Seq ProtocolHash
getProposalVotesListCrossCompatProtocolHashes = \case
  ProposalVotesListBase l -> fmap (fst . Base.unProposalVotes) l

instance FromJSON ProposalVotesListCrossCompat where
  parseJSON jv = ProposalVotesListBase <$> parseJSON jv

data BallotsCrossCompat = BallotsBase Base.Ballots

instance FromJSON BallotsCrossCompat where
  parseJSON jv = BallotsBase <$> parseJSON jv
