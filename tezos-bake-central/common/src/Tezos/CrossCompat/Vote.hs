{-# LANGUAGE LambdaCase #-}
module Tezos.CrossCompat.Vote where

import Data.Aeson (FromJSON(..))
import Data.Sequence (Seq)

import Tezos.Common.Base58Check (ProtocolHash)
import qualified Tezos.Nairobi.Vote as Nairobi

data VoterListingsCrossCompat = VoterListingsNairobi (Seq Nairobi.VoterDelegate)
  deriving Show

instance FromJSON VoterListingsCrossCompat where
  parseJSON jv = VoterListingsNairobi <$> parseJSON jv

data ProposalVotesListCrossCompat = ProposalVotesListNairobi (Seq Nairobi.ProposalVotes)

getProposalVotesListCrossCompatProtocolHashes :: ProposalVotesListCrossCompat -> Seq ProtocolHash
getProposalVotesListCrossCompatProtocolHashes = \case
  ProposalVotesListNairobi l -> fmap (fst . Nairobi.unProposalVotes) l

instance FromJSON ProposalVotesListCrossCompat where
  parseJSON jv = ProposalVotesListNairobi <$> parseJSON jv

data BallotsCrossCompat = BallotsNairobi Nairobi.Ballots

instance FromJSON BallotsCrossCompat where
  parseJSON jv = BallotsNairobi <$> parseJSON jv
