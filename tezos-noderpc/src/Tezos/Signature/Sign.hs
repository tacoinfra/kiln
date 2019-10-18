module Tezos.Signature.Sign where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteArray as M
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)

import Crypto.Error (onCryptoFailure)
import qualified Crypto.MicroECC as U
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.Hash as H

import Tezos.Common.Base58Check
import Tezos.V005.PublicKey
import Tezos.Common.ShortByteString (ShortByteString, fromShort, toShort)
import Tezos.V005.Signature

data SecretKeyInMemory
  = SecretKey_Ed25519Seed Ed25519Seed
  | SecretKey_Ed25519 Ed25519SecretKey
  deriving (Show, Eq)

tryReadSecretKeyInMemory :: BS.ByteString -> Either HashBase58Error SecretKeyInMemory
tryReadSecretKeyInMemory = tryFromBase58 [ TryDecodeBase58 SecretKey_Ed25519Seed, TryDecodeBase58 SecretKey_Ed25519 ]

sign :: SecretKeyInMemory -> BS.ByteString -> Maybe Signature
sign key msg = case key of
  SecretKey_Ed25519Seed sk -> sign_Ed25519 (fromShort $ unHashedValue sk) digest
-- We could be faster and more error-checking here by not recalculating the public key, but this works.
  SecretKey_Ed25519 sk -> sign_Ed25519 (BS.take 32 . fromShort $ unHashedValue sk) digest
  where
    digest = M.convert $ H.hashWith H.Blake2b_256 msg

sign_Ed25519 :: BS.ByteString -> BS.ByteString -> Maybe Signature
sign_Ed25519 sk msg = onCryptoFailure (const Nothing) Just $ do
    edSk <- Ed25519.secretKey sk
    pure $ Signature_Ed25519 . HashedValue . toShort . M.convert $ Ed25519.sign edSk (Ed25519.toPublic edSk) msg
