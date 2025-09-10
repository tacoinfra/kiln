{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Tezos.Common.PublicKeyHash where

import Control.DeepSeq (NFData)
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Hashable (Hashable)
import Data.String
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import qualified Text.ParserCombinators.ReadPrec as Read

import Tezos.Common.Base58Check
import qualified Tezos.Common.Binary as B


data PublicKeyHash
  = PublicKeyHash_Ed25519 Ed25519PublicKeyHash
  | PublicKeyHash_Secp256k1 Secp256k1PublicKeyHash
  | PublicKeyHash_P256 P256PublicKeyHash
  | PublicKeyHash_BLS12381 BLS12381PublicKeyHash
  deriving (Eq, Ord, Generic, Typeable)
instance NFData PublicKeyHash
instance Hashable PublicKeyHash

-- TODO: This could be done for any such sum of hashes with TH?
publicKeyHashConstructorDecoders :: [TryDecodeBase58 PublicKeyHash]
publicKeyHashConstructorDecoders =
  [ TryDecodeBase58 PublicKeyHash_Ed25519
  , TryDecodeBase58 PublicKeyHash_Secp256k1
  , TryDecodeBase58 PublicKeyHash_P256
  , TryDecodeBase58 PublicKeyHash_BLS12381
  ]

tryReadPublicKeyHash :: BS.ByteString -> Either HashBase58Error PublicKeyHash
tryReadPublicKeyHash = tryFromBase58 publicKeyHashConstructorDecoders

tryReadPublicKeyHashText :: Text -> Either HashBase58Error PublicKeyHash
tryReadPublicKeyHashText = tryReadPublicKeyHash . T.encodeUtf8

instance ToJSON PublicKeyHash where
  toJSON (PublicKeyHash_Ed25519 x) = toJSON x
  toJSON (PublicKeyHash_Secp256k1 x) = toJSON x
  toJSON (PublicKeyHash_P256 x) = toJSON x
  toJSON (PublicKeyHash_BLS12381 x) = toJSON x

  toEncoding (PublicKeyHash_Ed25519 x) = toEncoding x
  toEncoding (PublicKeyHash_Secp256k1 x) = toEncoding x
  toEncoding (PublicKeyHash_P256 x) = toEncoding x
  toEncoding (PublicKeyHash_BLS12381 x) = toEncoding x

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
  PublicKeyHash_P256 x -> toBase58Text x
  PublicKeyHash_BLS12381 x -> toBase58Text x

instance FromJSONKey PublicKeyHash
instance ToJSONKey PublicKeyHash

instance Show PublicKeyHash where
  show = ("fromString " <>) . show . toPublicKeyHashText

instance Read PublicKeyHash where
  readsPrec =
    Read.readPrec_to_S $ (PublicKeyHash_Ed25519 <$> Read.readS_to_Prec readsPrec)
                Read.<++ (PublicKeyHash_Secp256k1 <$> Read.readS_to_Prec readsPrec)
                Read.<++ (PublicKeyHash_P256 <$> Read.readS_to_Prec readsPrec)

instance IsString PublicKeyHash where
  fromString x = either (error . show) id $ tryFromBase58 publicKeyHashConstructorDecoders $ fromString x

instance B.TezosBinary PublicKeyHash where
  build = \case
    PublicKeyHash_Ed25519 h -> B.build @Word8 0 <> B.build h
    PublicKeyHash_Secp256k1 h -> B.build @Word8 1 <> B.build h
    PublicKeyHash_P256 h -> B.build @Word8 2 <> B.build h
    PublicKeyHash_BLS12381 h -> B.build @Word8 3 <> B.build h
  get = B.get @Word8 >>= \case
    0 -> PublicKeyHash_Ed25519 <$> B.get
    1 -> PublicKeyHash_Secp256k1 <$> B.get
    2 -> PublicKeyHash_P256 <$> B.get
    3 -> PublicKeyHash_BLS12381 <$> B.get
    _ -> fail "PublicKeyHash: unknown tag"
