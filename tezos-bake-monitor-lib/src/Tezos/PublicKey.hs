{-# LANGUAGE LambdaCase #-}

module Tezos.PublicKey where

import Data.Aeson
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Tezos.Base58Check

-- TODO: it'd be nice to unify all this into a tagged scheme.

data PublicKey
  = PublicKey_Ed25519 Ed25519PublicKey
  | PublicKey_Secp256k1 Secp256k1PublicKey
  deriving (Eq, Ord)

-- TODO: This could be done for any such sum of hashes with TH?
publicKeyConstructorDecoders :: [TryDecodeBase58 PublicKey]
publicKeyConstructorDecoders =
  [ TryDecodeBase58 PublicKey_Ed25519
  , TryDecodeBase58 PublicKey_Secp256k1
  ]

instance ToJSON PublicKey where
  toJSON (PublicKey_Ed25519 x) = toJSON x
  toJSON (PublicKey_Secp256k1 x) = toJSON x

  toEncoding (PublicKey_Ed25519 x) = toEncoding x
  toEncoding (PublicKey_Secp256k1 x) = toEncoding x

instance FromJSON PublicKey where
  parseJSON x = do
      x' <- T.encodeUtf8 <$> parseJSON x
      case tryFromBase58 publicKeyConstructorDecoders x' of
        Left bad -> fail $ show bad
        Right ok -> return ok


toPublicKeyText :: PublicKey -> Text
toPublicKeyText = \case
    PublicKey_Ed25519 x -> T.pack $ show x
    PublicKey_Secp256k1 x -> T.pack $ show x

instance Show PublicKey where
  show = ("fromString "  <>) . show . toPublicKeyText

instance IsString PublicKey where
  fromString x = either (error . show) id $ tryFromBase58 publicKeyConstructorDecoders $ fromString x
