{-# LANGUAGE LambdaCase #-}

module Tezos.Signature where

import Data.Typeable
import Data.Aeson
import Data.String
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import Data.Text (Text)

import Tezos.Base58Check

data Signature
  = Signature_Ed25519 Ed25519Signature -- see lib_crypto/ed25519.ml
  | Signature_Secp256k1 Secp256k1Signature
  | Signature_P256 P256Signature
  | Signature_Unknown GenericSignature
  deriving (Eq, Ord, Typeable)

signatureConstructorDecoders :: [TryDecodeBase58 Signature]
signatureConstructorDecoders =
  [ TryDecodeBase58 Signature_Ed25519
  , TryDecodeBase58 Signature_Secp256k1
  , TryDecodeBase58 Signature_P256
  , TryDecodeBase58 Signature_Unknown
  ]

tryReadSignature :: BS.ByteString -> Either HashBase58Error Signature
tryReadSignature = tryFromBase58 signatureConstructorDecoders

tryReadSignatureText :: Text -> Either HashBase58Error Signature
tryReadSignatureText = tryReadSignature . T.encodeUtf8

instance ToJSON Signature where
  toJSON (Signature_Ed25519 x) = toJSON x
  toJSON (Signature_Secp256k1 x) = toJSON x
  toJSON (Signature_P256 x) = toJSON x
  toJSON (Signature_Unknown x) = toJSON x

  toEncoding (Signature_Ed25519 x) = toEncoding x
  toEncoding (Signature_Secp256k1 x) = toEncoding x
  toEncoding (Signature_P256 x) = toEncoding x
  toEncoding (Signature_Unknown x) = toEncoding x

instance FromJSON Signature where
  parseJSON x = do
    x' <- T.encodeUtf8 <$> parseJSON x
    case tryFromBase58 signatureConstructorDecoders x' of
      Left bad -> fail $ show bad
      Right ok -> return ok

toSignatureText :: Signature -> Text
toSignatureText = \case
  Signature_Ed25519 x -> toBase58Text x
  Signature_Secp256k1 x -> toBase58Text x
  Signature_P256 x -> toBase58Text x
  Signature_Unknown x -> toBase58Text x

instance Show Signature where
  show = ("fromString " <>) . show . toSignatureText

instance IsString Signature where
  fromString x = either (error . show) id $ tryFromBase58 signatureConstructorDecoders $ fromString x
