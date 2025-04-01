{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Snapshot where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Text.URI (Authority(..), URI(..), mkPathPiece)
import Data.Default
import Data.Typeable
import Data.List.NonEmpty
import Data.Maybe
import GHC.Generics
import qualified Text.URI.QQ as Uri
import Tezos.Common.Chain

data TzInitRegion
  = TzInitAsia
  | TzInitEurope
  | TzInitUs
  deriving (Show, Eq, Ord, Typeable, Generic)

instance FromJSON TzInitRegion
instance ToJSON TzInitRegion

instance Default TzInitRegion where
  def = TzInitEurope

tzInitUri :: NamedChain -> TzInitRegion -> URI
tzInitUri namedChain region  = let
  host = case region of
    TzInitAsia -> [Uri.host|snapshots.asia.tzinit.org|]
    TzInitEurope ->  [Uri.host|snapshots.eu.tzinit.org|]
    TzInitUs ->  [Uri.host|snapshots.us.tzinit.org|]
  chainNamePath = fromMaybe
    (error "Chain name returned by 'showNamedChain' cannot be parsed as a url segment.") $ mkPathPiece $ showNamedChain namedChain
  in URI
      { uriScheme = Just [Uri.scheme|https|]
      , uriAuthority = Right $ Authority Nothing host Nothing
      , uriPath = Just (False, chainNamePath :| [[Uri.pathPiece|rolling|]])
      , uriQuery = []
      , uriFragment = Nothing
      }
