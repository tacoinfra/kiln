{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Tezos.Common.AttestationInfo where

import Tezos.Common.PublicKeyHash (PublicKeyHash)
import Tezos.Common.Json (deriveTezosJson)
import Data.Typeable (Typeable)
import qualified Data.List as DL
import GHC.Word

searchAttestationInfoForSlot :: Word16 -> [AttestationInfo] -> Maybe PublicKeyHash
searchAttestationInfoForSlot _ [] = Nothing
searchAttestationInfoForSlot slot (ai:r) = case lookupAttestationInfo ai of
  Just x -> Just x
  Nothing -> searchAttestationInfoForSlot slot r
  where
    lookupAttestationInfo :: AttestationInfo -> Maybe PublicKeyHash
    lookupAttestationInfo (_attestationInfo_delegates -> delegates) =
      _attestationInfoDelegate_delegate <$> DL.find ((slot ==) . _attestationInfoDelegate_firstSlot) delegates

data AttestationInfo = AttestationInfo
  { _attestationInfo_delegates :: [AttestationInfoDelegate]
  } deriving (Eq, Ord, Show, Typeable)

data AttestationInfoDelegate = AttestationInfoDelegate
  { _attestationInfoDelegate_delegate :: PublicKeyHash
  , _attestationInfoDelegate_firstSlot :: Word16
  } deriving (Eq, Ord, Show, Typeable)

concat <$> traverse deriveTezosJson
  [ ''AttestationInfoDelegate
  , ''AttestationInfo
  ]
