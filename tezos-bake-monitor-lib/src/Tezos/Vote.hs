{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Tezos.Vote where

import Data.Aeson
import Data.Int
import Data.Typeable
import GHC.Generics
import Tezos.Base58Check
import Tezos.Operation

type VotingPeriod = Int32 -- ^ period: Voting_period_repr.t ;

newtype Ballots = Ballots { unBallots :: Ballot -> Int } deriving (Typeable, Generic)

instance ToJSON Ballots where
  toJSON (Ballots f) = object
    [ "yay"  .= f Ballot_Yay
    , "nay"  .= f Ballot_Nay
    , "pass" .= f Ballot_Pass
    ]

instance FromJSON Ballots where
  parseJSON = withObject "ballot" $ \o -> do
    yay  <- o .: "yay"
    nay  <- o .: "nay"
    pass <- o .: "pass"
    pure $ Ballots $ \case
      Ballot_Yay -> yay
      Ballot_Nay -> nay
      Ballot_Pass -> pass

newtype ProposalVotes = ProposalVotes { unProposalVotes :: (ProtocolHash, Int) }
  deriving (Eq, Ord, Read, Show, Typeable, Generic, FromJSON, ToJSON)
