{-# LANGUAGE LambdaCase #-}
module Tezos.CrossCompat.Vote where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..))
import Data.Sequence (Seq)

import Tezos.Common.Base58Check (ProtocolHash)
import qualified Tezos.V012.Vote as V012
import qualified Tezos.V013.Vote as V013

data VoterListingsCrossCompat
  = VoterListingsV012 (Seq V012.VoterDelegate)
  | VoterListingsV013 (Seq V013.VoterDelegate)
  deriving Show

instance FromJSON VoterListingsCrossCompat where
  parseJSON jv = VoterListingsV012 <$> parseJSON jv <|>
    VoterListingsV013 <$> parseJSON jv

data ProposalVotesListCrossCompat
  = ProposalVotesListV012 (Seq V012.ProposalVotes)
  | ProposalVotesListV013 (Seq V013.ProposalVotes)

getProposalVotesListCrossCompatProtocolHashes :: ProposalVotesListCrossCompat -> Seq ProtocolHash
getProposalVotesListCrossCompatProtocolHashes = \case
  ProposalVotesListV012 l -> fmap (fst . V012.unProposalVotes) l
  ProposalVotesListV013 l -> fmap (fst . V013.unProposalVotes) l

instance FromJSON ProposalVotesListCrossCompat where
  parseJSON jv = ProposalVotesListV012 <$> parseJSON jv <|>
    ProposalVotesListV013 <$> parseJSON jv

data BallotsCrossCompat
  = BallotsV012 V012.Ballots
  | BallotsV013 V013.Ballots

instance FromJSON BallotsCrossCompat where
  parseJSON jv = BallotsV012 <$> parseJSON jv <|>
    BallotsV013 <$> parseJSON jv
