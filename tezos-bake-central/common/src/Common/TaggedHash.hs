{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Common.TaggedHash where

import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base58
import Data.Monoid
import Data.String
import Data.Text as T
import Data.Text.Encoding as T
import Data.Typeable
import GHC.Generics
#if defined(ghcjs_HOST_OS)
import qualified "hashing" Crypto.Hash as CryptoHash
import qualified Data.ByteString.Base16 as BS16
#else
import "cryptonite" Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteArray as BA
#endif


import Common.TezosBinary

-- see ~/tezos/src/lib_crypto/base58.ml
type BlockHash = HashedValue 'HashType_BlockHash ByteString
type OperationHash = HashedValue 'HashType_OperationHash ByteString
type OperationListHash = HashedValue 'HashType_OperationListHash ByteString
type OperationListListHash = HashedValue 'HashType_OperationListListHash ByteString
type ProtocolHash = HashedValue 'HashType_ProtocolHash ByteString
type ContextHash = HashedValue 'HashType_ContextHash ByteString
type Ed25519PublicKeyHash = HashedValue 'HashType_Ed25519PublicKeyHash ByteString
type Secp256k1PublicKeyHash = HashedValue 'HashType_Secp256k1PublicKeyHash ByteString
type CryptoboxPublicKeyHash = HashedValue 'HashType_CryptoboxPublicKeyHash ByteString
type Ed25519Seed = HashedValue 'HashType_Ed25519Seed ByteString
type Ed25519PublicKey = HashedValue 'HashType_Ed25519PublicKey ByteString
type Secp256k1SecretKey = HashedValue 'HashType_Secp256k1SecretKey ByteString
type Secp256k1PublicKey = HashedValue 'HashType_Secp256k1PublicKey ByteString
type Ed25519SecretKey = HashedValue 'HashType_Ed25519SecretKey ByteString
type Ed25519Signature = HashedValue 'HashType_Ed25519Signature ByteString
type Secp256k1Signature = HashedValue 'HashType_Secp256k1Signature ByteString
type GenericSignature = HashedValue 'HashType_GenericSignature ByteString
type ChainId = HashedValue 'HashType_ChainId ByteString

-- see ~/tezos/src/proto_alpha/lib_protocol/src/contract_hash.ml
type ContractHash = HashedValue 'HashType_ContractHash ByteString

-- see ~/tezos/src/proto_alpha/lib_protocol/src/nonce_hash.ml
type NonceHash = HashedValue 'HashType_NonceHash ByteString

-- see ~/tezos/src/proto_alpha/lib_protocol/src/blinded_public_key_hash.ml
type BlindedPublicKeyHash = HashedValue 'HashType_BlindedPublicKeyHash ByteString

data HashType
  = HashType_BlockHash
  | HashType_OperationHash
  | HashType_OperationListHash
  | HashType_OperationListListHash
  | HashType_ProtocolHash
  | HashType_ContextHash
  | HashType_Ed25519PublicKeyHash
  | HashType_Secp256k1PublicKeyHash
  | HashType_CryptoboxPublicKeyHash
  | HashType_Ed25519Seed
  | HashType_Ed25519PublicKey
  | HashType_Secp256k1SecretKey
  | HashType_Secp256k1PublicKey
  | HashType_Ed25519SecretKey
  | HashType_Ed25519Signature
  | HashType_Secp256k1Signature
  | HashType_GenericSignature
  | HashType_ChainId
  | HashType_ContractHash
  | HashType_NonceHash
  | HashType_BlindedPublicKeyHash
  deriving (Eq, Ord, Show, Typeable, Generic, Enum)

newtype HashedValue (tag :: HashType) (a :: *) = HashedValue { unHashedValue :: a }
  deriving (Eq, Ord)

instance IsBase58Hash tag => ToJSON (HashedValue tag ByteString) where
  toJSON = toJSON . T.decodeUtf8 . toBase58
  toEncoding = toEncoding . T.decodeUtf8 . toBase58

instance IsBase58Hash tag => FromJSON (HashedValue tag ByteString) where
  parseJSON x = do
    hexesText <- parseJSON x
    either (fail . show) pure $ fromBase58 $ T.encodeUtf8 hexesText

instance IsBase58Hash t => TezosBinary (HashedValue t ByteString) where
  parseBinary = fmap HashedValue <$> parseFixedByteString $ hashSize (Proxy :: Proxy t)
  encodeBinary (HashedValue x) | BS.length x == hashSize (Proxy :: Proxy t) = x
                               | otherwise = error "base58 tagged object wrong length"

class IsBase58Hash (t :: HashType) where
  hashSize :: f t -> Int
  prefix :: f t -> ByteString

checksum :: ByteString -> ByteString
checksum = BS.take 4 . sha256 . sha256

sha256 :: ByteString -> ByteString
#if defined ghcjs_HOST_OS
sha256 = fst . BS16.decode . T.encodeUtf8 . T.pack . show . CryptoHash.hash @ CryptoHash.SHA256
#else
sha256 = BS.pack . BA.unpack . (hash :: BS.ByteString -> Digest SHA256)
#endif


toBase58 :: forall t. IsBase58Hash t => HashedValue t ByteString -> ByteString
toBase58 (HashedValue x) = encodeBase58 bitcoinAlphabet $ x' <> checksum x'
  where
    x' = prefix (Proxy :: Proxy t) <> x

toBase58Text :: forall t. IsBase58Hash t => HashedValue t ByteString -> Text
toBase58Text = T.decodeUtf8 . toBase58


data HashBase58Error
  = HashBase58Error_DecodeError
  | HashBase58Error_InvalidPrefix BS.ByteString BS.ByteString
  | HashBase58Error_WrongLength Int Int
  | HashBase58Error_BadChecksum BS.ByteString BS.ByteString BS.ByteString
  deriving (Eq, Ord, Show, Generic, Typeable)


data TryDecodeBase58 a where
  TryDecodeBase58 :: IsBase58Hash t => (HashedValue t ByteString -> a) -> TryDecodeBase58 a

tryFromBase58 :: [TryDecodeBase58 a] -> ByteString -> Either HashBase58Error a
tryFromBase58 x y = go x
  where
    go [] = Left HashBase58Error_DecodeError
    go (TryDecodeBase58 f:fs) = case fromBase58 y of
      Right good -> Right $ f good
      Left (HashBase58Error_InvalidPrefix _ _) -> go fs
      Left bad -> Left bad

fromBase58 :: forall t. IsBase58Hash t => ByteString -> Either HashBase58Error (HashedValue t ByteString)
fromBase58 b58chk = return . HashedValue <=< verifyLength <=< verifyPrefix <=< verifyChecksum <=< swizzle58 $ b58chk
  where
    expectedPfx = prefix (Proxy :: (Proxy t))
    expectedSize = hashSize (Proxy :: (Proxy t))

    swizzle58 x = case decodeBase58 bitcoinAlphabet x of
      Just x' -> Right x'
      Nothing -> Left HashBase58Error_DecodeError

    verifyPrefix x =
      let
        n = BS.length expectedPfx
        pfx = BS.take n x

      in if expectedPfx == pfx
         then Right $ BS.drop n x
         else Left $ HashBase58Error_InvalidPrefix expectedPfx pfx

    verifyLength x | expectedSize == actualSize = Right x
                   | otherwise = Left $ HashBase58Error_WrongLength expectedSize actualSize
      where
        actualSize = BS.length x

    verifyChecksum x | actualSum == expectedSum = Right payload
                     | otherwise = Left $ HashBase58Error_BadChecksum b58chk expectedSum actualSum
      where
        checksummedSize = BS.length x - 4
        actualSum = BS.drop checksummedSize x
        payload = BS.take checksummedSize x
        expectedSum = checksum payload

instance IsBase58Hash t => IsString (HashedValue t ByteString) where
  fromString x = either (error . show) id $ fromBase58 $ fromString x

instance IsBase58Hash t => Show (HashedValue t ByteString) where
  show = ("fromString " <>) . show . toBase58

instance IsBase58Hash 'HashType_BlockHash where
  prefix _ = "\001\052"
  hashSize _ = 32

instance IsBase58Hash 'HashType_ChainId where
  prefix _ = "\087\082\000"
  hashSize _ = 4

instance IsBase58Hash 'HashType_ContextHash where
  prefix _ = "\079\199"
  hashSize _ = 32

instance IsBase58Hash 'HashType_CryptoboxPublicKeyHash where
  prefix _ = "\153\103"
  hashSize _ = 16

instance IsBase58Hash 'HashType_Ed25519PublicKey where
  prefix _ = "\013\015\037\217"
  hashSize _ = 32

instance IsBase58Hash 'HashType_Ed25519PublicKeyHash where
  prefix _ = "\006\161\159"
  hashSize _ = 20

instance IsBase58Hash 'HashType_Ed25519SecretKey where
  prefix _ = "\043\246\078\007"
  hashSize _ = 64

instance IsBase58Hash 'HashType_Ed25519Seed where
  prefix _ = "\013\015\058\007"
  hashSize _ = 32

instance IsBase58Hash 'HashType_Ed25519Signature where
  prefix _ = "\009\245\205\134\018"
  hashSize _ = 64

instance IsBase58Hash 'HashType_GenericSignature where
  prefix _ = "\004\130\043"
  hashSize _ = 64

instance IsBase58Hash 'HashType_OperationHash where
  prefix _ = "\005\116"
  hashSize _ = 32

instance IsBase58Hash 'HashType_OperationListHash where
  prefix _ = "\133\233"
  hashSize _ = 32

instance IsBase58Hash 'HashType_OperationListListHash where
  prefix _ = "\029\159\109"
  hashSize _ = 32

instance IsBase58Hash 'HashType_ProtocolHash where
  prefix _ = "\002\170"
  hashSize _ = 32

instance IsBase58Hash 'HashType_Secp256k1PublicKey where
  prefix _ = "\003\254\226\086"
  hashSize _ = 33

instance IsBase58Hash 'HashType_Secp256k1PublicKeyHash where
  prefix _ = "\006\161\161"
  hashSize _ = 20

instance IsBase58Hash 'HashType_Secp256k1SecretKey where
  prefix _ = "\017\162\224\201"
  hashSize _ = 32

instance IsBase58Hash 'HashType_Secp256k1Signature where
  prefix _ =  "\013\115\101\019\063"
  hashSize _ = 64

instance IsBase58Hash 'HashType_ContractHash where
  prefix _ =  "\003\099\029"
  hashSize _ = 20

instance IsBase58Hash 'HashType_NonceHash where
  prefix _ = "\069\220\169"
  hashSize _ = 32

instance IsBase58Hash 'HashType_BlindedPublicKeyHash where
  prefix _ = "\001\002\049\223"
  hashSize _ = 20

