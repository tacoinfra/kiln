{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.PublicKeyHash where

import Data.Aeson
import Data.String
import Data.Text (Text)
import Data.Semigroup
import GHC.Word
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16

import Common.TaggedHash
import Common.TezosBinary
import Common.PublicKey


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


rawContextLink :: PublicKeyHash -> Text
rawContextLink pkh = T.intercalate "/"
    [ "raw_context/contracts/index" , rawContextKeyPath pkh ]
  where
    b16 :: BS.ByteString -> Text
    b16 x = T.decodeUtf8 $ BS16.encode x

    rawContextKeyPath :: PublicKeyHash -> Text
    rawContextKeyPath (PublicKeyHash_Ed25519 (HashedValue x)) = "ed25519/" <> hashedValueKeyPath (b16 x)
    rawContextKeyPath (PublicKeyHash_Secp256k1 (HashedValue x)) = "secp256k1/" <> hashedValueKeyPath (b16 x)

    hashedValueKeyPath :: Text -> Text
    hashedValueKeyPath x = T.toLower $ T.intercalate "/"
        [ T.drop 0 $ T.take 2 $ x
        , T.drop 2 $ T.take 4 $ x
        , T.drop 4 $ T.take 6 $ x
        , T.drop 6 $ T.take 8 $ x
        , T.drop 8 $ T.take 10 $ x
        , T.drop 10 $ x
        ]

