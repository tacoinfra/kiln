{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Tezos.Common.PublicKey where

import Control.DeepSeq (NFData)
import Data.Aeson
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Hashable (Hashable)
import Data.String
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)

import Tezos.Common.Base58Check
import qualified Tezos.Common.Binary as B

-- TODO: it'd be nice to unify all this into a tagged scheme.

data PublicKey
  = PublicKey_Ed25519 Ed25519PublicKey
  | PublicKey_Secp256k1 Secp256k1PublicKey
  | PublicKey_P256 P256PublicKey
  deriving (Eq, Ord, Generic, Typeable)
instance Hashable PublicKey
instance NFData PublicKey

-- TODO: This could be done for any such sum of hashes with TH?
publicKeyConstructorDecoders :: [TryDecodeBase58 PublicKey]
publicKeyConstructorDecoders =
  [ TryDecodeBase58 PublicKey_Ed25519
  , TryDecodeBase58 PublicKey_Secp256k1
  , TryDecodeBase58 PublicKey_P256
  ]

instance ToJSON PublicKey where
  toJSON (PublicKey_Ed25519 x) = toJSON x
  toJSON (PublicKey_Secp256k1 x) = toJSON x
  toJSON (PublicKey_P256 x) = toJSON x

  toEncoding (PublicKey_Ed25519 x) = toEncoding x
  toEncoding (PublicKey_Secp256k1 x) = toEncoding x
  toEncoding (PublicKey_P256 x) = toEncoding x

instance FromJSON PublicKey where
  parseJSON x = do
      x' <- T.encodeUtf8 <$> parseJSON x
      case tryFromBase58 publicKeyConstructorDecoders x' of
        Left bad -> fail $ show bad
        Right ok -> return ok

instance B.TezosBinary PublicKey where
  build = \case
    PublicKey_Ed25519 x -> B.build @Word8 0 <> B.build x
    PublicKey_Secp256k1 x -> B.build @Word8 1 <> B.build x
    PublicKey_P256 x -> B.build @Word8 2 <> B.build x
  get = B.get @Word8 >>= \case
    0 -> PublicKey_Ed25519 <$> B.get
    1 -> PublicKey_Secp256k1 <$> B.get
    2 -> PublicKey_P256 <$> B.get
    _ -> fail "invalid tag for PublicKey"

toPublicKeyText :: PublicKey -> Text
toPublicKeyText = \case
    PublicKey_Ed25519 x -> toBase58Text x
    PublicKey_Secp256k1 x -> toBase58Text x
    PublicKey_P256 x -> toBase58Text x

instance Show PublicKey where
  show = ("fromString "  <>) . show . toPublicKeyText

instance IsString PublicKey where
  fromString x = either (error . show) id $ tryFromBase58 publicKeyConstructorDecoders $ fromString x
