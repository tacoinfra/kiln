{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Tezos.Ledger where

import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses)
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.Text as T

import Tezos.Json

newtype LedgerIdentifier = LedgerIdentifier
  { unLedgerIdentifier :: Text -- adjective-animal
  } deriving (Show, Read, Eq, Ord, Generic, Typeable)
instance ToJSON LedgerIdentifier
instance FromJSON LedgerIdentifier

data SecretKey = SecretKey
  { _secretKey_ledgerIdentifier :: !LedgerIdentifier
  , _secretKey_signingCurve :: !SigningCurve
  , _secretKey_derivationPath :: !DerivationPath
  } deriving (Show, Read, Eq, Ord, Generic, Typeable)
instance ToJSONKey SecretKey
instance FromJSONKey SecretKey

toSecretKeyText :: SecretKey -> Text
toSecretKeyText sk = "ledger://" <> T.intercalate "/"
  [ unLedgerIdentifier (_secretKey_ledgerIdentifier sk)
  , toSigningCurveText (_secretKey_signingCurve sk)
  , unDerivationPath (_secretKey_derivationPath sk)
  ]

data DerivationPath = DerivationPath
  { unDerivationPath :: Text
  } deriving (Show, Read, Eq, Ord, Generic, Typeable)
instance ToJSON DerivationPath
instance FromJSON DerivationPath

data SigningCurve
  = SigningCurve_Ed25519
  | SigningCurve_Secp256k1
  | SigningCurve_P256
  deriving (Show, Read, Eq, Ord, Generic, Typeable, Enum, Bounded)
instance NFData SigningCurve
instance ToJSON SigningCurve
instance FromJSON SigningCurve

toSigningCurveText :: SigningCurve -> Text
toSigningCurveText = \case
  SigningCurve_Ed25519 -> "ed25519"
  SigningCurve_Secp256k1 -> "secp256k1"
  SigningCurve_P256 -> "p256"

concat <$> traverse deriveTezosJson
  [ ''SecretKey
  ]

concat <$> traverse makeLenses
  [ 'LedgerIdentifier
  , 'SecretKey
  , 'DerivationPath
  ]
