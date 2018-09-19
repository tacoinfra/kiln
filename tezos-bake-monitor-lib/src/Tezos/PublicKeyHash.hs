{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Tezos.PublicKeyHash where

import Data.Aeson
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import qualified Data.ByteString as BS
import Tezos.ShortByteString (fromShort)
import qualified Data.ByteString.Base16 as BS16
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Tezos.Base58Check


data PublicKeyHash
  = PublicKeyHash_Ed25519 Ed25519PublicKeyHash
  | PublicKeyHash_Secp256k1 Secp256k1PublicKeyHash
  | PublicKeyHash_P256 P256PublicKeyHash
  deriving (Eq, Ord)

-- TODO: This could be done for any such sum of hashes with TH?
publicKeyHashConstructorDecoders :: [TryDecodeBase58 PublicKeyHash]
publicKeyHashConstructorDecoders =
  [ TryDecodeBase58 PublicKeyHash_Ed25519
  , TryDecodeBase58 PublicKeyHash_Secp256k1
  , TryDecodeBase58 PublicKeyHash_P256
  ]

tryReadPublicKeyHash :: BS.ByteString -> Either HashBase58Error PublicKeyHash
tryReadPublicKeyHash = tryFromBase58 publicKeyHashConstructorDecoders

tryReadPublicKeyHashText :: Text -> Either HashBase58Error PublicKeyHash
tryReadPublicKeyHashText = tryReadPublicKeyHash . T.encodeUtf8

instance ToJSON PublicKeyHash where
  toJSON (PublicKeyHash_Ed25519 x) = toJSON x
  toJSON (PublicKeyHash_Secp256k1 x) = toJSON x
  toJSON (PublicKeyHash_P256 x) = toJSON x

  toEncoding (PublicKeyHash_Ed25519 x) = toEncoding x
  toEncoding (PublicKeyHash_Secp256k1 x) = toEncoding x
  toEncoding (PublicKeyHash_P256 x) = toEncoding x

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

instance FromJSONKey PublicKeyHash
instance ToJSONKey PublicKeyHash

instance Show PublicKeyHash where
  show = ("fromString " <>) . show . toPublicKeyHashText

instance IsString PublicKeyHash where
  fromString x = either (error . show) id $ tryFromBase58 publicKeyHashConstructorDecoders $ fromString x

-- instance TezosBinary PublicKeyHash where
--   parseBinary = parseTagged 0 "ed25519" PublicKeyHash_Ed25519
--         `mplus` parseTagged 1 "secp256k1" PublicKeyHash_Secp256k1
--         `mplus` parseTagged 2 "p246" PublicKeyHash_P256
-- 
--   encodeBinary (PublicKeyHash_Ed25519 x) = encodeBinary (0 :: Word8) <> encodeBinary x
--   encodeBinary (PublicKeyHash_Secp256k1 x) = encodeBinary (1 :: Word8) <> encodeBinary x
--   encodeBinary (PublicKeyHash_P256 x) = encodeBinary (2 :: Word8) <> encodeBinary x


-- TODO: bitrotted since RPC proposal; can i still get this info?
rawContextLink :: PublicKeyHash -> Text
rawContextLink pkh = T.intercalate "/"
    [ "raw_context/contracts/index" , rawContextKeyPath pkh ]
  where
    b16 :: BS.ByteString -> Text
    b16 x = T.decodeUtf8 $ BS16.encode x

    rawContextKeyPath :: PublicKeyHash -> Text
    rawContextKeyPath (PublicKeyHash_Ed25519 (HashedValue x)) = "ed25519/" <> hashedValueKeyPath (b16 $ fromShort x)
    rawContextKeyPath (PublicKeyHash_Secp256k1 (HashedValue x)) = "secp256k1/" <> hashedValueKeyPath (b16 $ fromShort x)
    rawContextKeyPath (PublicKeyHash_P256 (HashedValue x)) = "p256/" <> hashedValueKeyPath (b16 $ fromShort x)

    hashedValueKeyPath :: Text -> Text
    hashedValueKeyPath x = T.toLower $ T.intercalate "/"
        [ T.drop 0 $ T.take 2 x
        , T.drop 2 $ T.take 4 x
        , T.drop 4 $ T.take 6 x
        , T.drop 6 $ T.take 8 x
        , T.drop 8 $ T.take 10 x
        , T.drop 10 x
        ]

