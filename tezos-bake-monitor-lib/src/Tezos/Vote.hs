{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Tezos.Vote where

import Data.Aeson
import Data.Int
import Data.Typeable
import GHC.Generics

type VotingPeriod = Int32 -- ^ period: Voting_period_repr.t ;

data Vote
  = Vote_Yay
  | Vote_Nay
  | Vote_Pass
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

newtype Ballot = Ballot (Vote -> Int)

instance ToJSON Ballot where
  toJSON (Ballot f) = object
    [ "yay"  .= f Vote_Yay
    , "nay"  .= f Vote_Nay
    , "pass" .= f Vote_Pass
    ]

instance FromJSON Ballot where
  parseJSON = withObject "ballot" $ \o -> do
    yay  <- o .: "yay"
    nay  <- o .: "nay"
    pass <- o .: "pass"
    pure $ Ballot $ \case
      Vote_Yay -> yay
      Vote_Nay -> nay
      Vote_Pass -> pass
