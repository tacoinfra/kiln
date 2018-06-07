{-# LANGUAGE DeriveGeneric #-}
module Common.Signature where

import Control.Monad
import Data.Semigroup
import Data.Typeable
import GHC.Generics
import GHC.Word

import Common.TezosBinary
import Common.TaggedHash

data Signature
  = Signature_Ed25519 Ed25519Signature -- see lib_crypto/ed25519.ml
  | Signature_Secp256k1 Secp256k1Signature
  -- Signature_Unknown
  deriving (Eq, Ord, Show, Generic, Typeable)

instance TezosBinary Signature where
  parseBinary = parseTagged 0 "Signature_Ed25519" Signature_Ed25519
        `mplus` parseTagged 1 "Signature_Secp256k1" Signature_Secp256k1

  encodeBinary (Signature_Ed25519 x) = encodeBinary (0 :: Word8) <> encodeBinary x
  encodeBinary (Signature_Secp256k1 x) = encodeBinary (1 :: Word8) <> encodeBinary x
