{-# LANGUAGE LambdaCase #-}
module Common.PublicKeyHash where

import Data.Aeson
import Data.String
import Data.Text (Text)
import Data.Semigroup
import GHC.Word
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Common.TaggedHash
import Common.TezosBinary

-- TODO: it'd be nice to unify all this into a tagged scheme.

data PublicKeyHash
  = PublicKeyHash_Ed25519 Ed25519PublicKeyHash
  | PublicKeyHash_Secp256k1 Secp256k1PublicKeyHash
  deriving (Eq, Ord)

-- TODO: This could be done for any such sum of hashes with TH?
publicKeyHashConstructorDecoders :: [TryDecodeBase58 PublicKeyHash]
publicKeyHashConstructorDecoders = 
  [ TryDecodeBase58 PublicKeyHash_Ed25519
  , TryDecodeBase58 PublicKeyHash_Secp256k1
  ]

instance ToJSON PublicKeyHash where
  toJSON (PublicKeyHash_Ed25519 x) = toJSON x
  toJSON (PublicKeyHash_Secp256k1 x) = toJSON x

  toEncoding (PublicKeyHash_Ed25519 x) = toEncoding x
  toEncoding (PublicKeyHash_Secp256k1 x) = toEncoding x

instance FromJSON PublicKeyHash where
  parseJSON x = do
      x' <- T.encodeUtf8 <$> parseJSON x
      case tryFromBase58 publicKeyHashConstructorDecoders x' of
        Left bad -> fail $ show bad
        Right ok -> return ok


toPublicKeyHashText :: PublicKeyHash -> Text
toPublicKeyHashText = \case
    PublicKeyHash_Ed25519 x -> toBase58Text x
    PublicKeyHash_Secp256k1 x -> toBase58Text x

instance Show PublicKeyHash where
  show = ("fromString "  <>) . show . toPublicKeyHashText

instance IsString PublicKeyHash where
  fromString x = either (error . show) id $ tryFromBase58 publicKeyHashConstructorDecoders $ fromString x

instance TezosBinary PublicKeyHash where
  parseBinary = parseTagged 0 "ed25519" PublicKeyHash_Ed25519
        `mplus` parseTagged 1 "secp256k1" PublicKeyHash_Secp256k1

  encodeBinary (PublicKeyHash_Ed25519 x) = encodeBinary (0 :: Word8) <> encodeBinary x
  encodeBinary (PublicKeyHash_Secp256k1 x) = encodeBinary (1 :: Word8) <> encodeBinary x
