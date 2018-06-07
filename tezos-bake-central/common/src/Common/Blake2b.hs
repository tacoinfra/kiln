{-# LANGUAGE PackageImports #-}
module Common.Blake2b where

import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA

import "cryptonite" Crypto.Hash (Digest, Blake2b_160, hash)

blake2b :: BS.ByteString -> BS.ByteString
blake2b xs = BS.pack $ BA.unpack $ (hash xs :: Digest Blake2b_160)

