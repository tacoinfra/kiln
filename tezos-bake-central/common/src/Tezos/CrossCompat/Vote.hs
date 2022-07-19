{-# LANGUAGE LambdaCase #-}
module Tezos.CrossCompat.Vote where

import Data.Aeson (FromJSON(..))
import Data.Sequence (Seq)

import Tezos.Common.Base58Check (ProtocolHash)
import qualified Tezos.V013.Vote as V013

data VoterListingsCrossCompat = VoterListingsV013 (Seq V013.VoterDelegate)
  deriving Show

instance FromJSON VoterListingsCrossCompat where
  parseJSON jv = VoterListingsV013 <$> parseJSON jv

data ProposalVotesListCrossCompat = ProposalVotesListV013 (Seq V013.ProposalVotes)

getProposalVotesListCrossCompatProtocolHashes :: ProposalVotesListCrossCompat -> Seq ProtocolHash
getProposalVotesListCrossCompatProtocolHashes = \case
  ProposalVotesListV013 l -> fmap (fst . V013.unProposalVotes) l

instance FromJSON ProposalVotesListCrossCompat where
  parseJSON jv = ProposalVotesListV013 <$> parseJSON jv

data BallotsCrossCompat = BallotsV013 V013.Ballots

instance FromJSON BallotsCrossCompat where
  parseJSON jv = BallotsV013 <$> parseJSON jv
