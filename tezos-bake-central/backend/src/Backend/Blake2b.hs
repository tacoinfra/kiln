{-# LANGUAGE PackageImports #-}
module Backend.Blake2b where

import Common.PublicKey
import Common.PublicKeyHash

import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA

import "cryptonite" Crypto.Hash -- (Digest, Blake2b_160, hash)

blake2b :: BS.ByteString -> BS.ByteString
blake2b xs = BS.pack $ BA.unpack (hash xs :: Digest Blake2b_160)

hashPublicKey :: PublicKey -> PublicKeyHash
hashPublicKey = \case
  PublicKey_Ed25519   (HashedValue pk) -> PublicKeyHash_Ed25519   $ HashedValue $ blake2b pk
  PublicKey_Secp256k1 (HashedValue pk) -> PublicKeyHash_Secp256k1 $ HashedValue $ blake2b pk

-- TODO: it'd be nice to unify all this into a tagged scheme.



