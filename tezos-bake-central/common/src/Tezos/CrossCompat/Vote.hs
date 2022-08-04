{-# LANGUAGE LambdaCase #-}
module Tezos.CrossCompat.Vote where

import Data.Aeson (FromJSON(..))
import Data.Sequence (Seq)

import Tezos.Common.Base58Check (ProtocolHash)
import qualified Tezos.V014.Vote as V014

data VoterListingsCrossCompat = VoterListingsV014 (Seq V014.VoterDelegate)
  deriving Show

instance FromJSON VoterListingsCrossCompat where
  parseJSON jv = VoterListingsV014 <$> parseJSON jv

data ProposalVotesListCrossCompat = ProposalVotesListV014 (Seq V014.ProposalVotes)

getProposalVotesListCrossCompatProtocolHashes :: ProposalVotesListCrossCompat -> Seq ProtocolHash
getProposalVotesListCrossCompatProtocolHashes = \case
  ProposalVotesListV014 l -> fmap (fst . V014.unProposalVotes) l

instance FromJSON ProposalVotesListCrossCompat where
  parseJSON jv = ProposalVotesListV014 <$> parseJSON jv

data BallotsCrossCompat = BallotsV014 V014.Ballots

instance FromJSON BallotsCrossCompat where
  parseJSON jv = BallotsV014 <$> parseJSON jv
