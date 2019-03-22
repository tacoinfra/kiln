module Tezos.Signature.Verify where

import qualified Data.ByteArray as M
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)

import Crypto.Error (onCryptoFailure)
import qualified Crypto.MicroECC as U
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.Hash as H

import Tezos.Base58Check
import Tezos.PublicKey
import Tezos.ShortByteString (fromShort)
import Tezos.Signature

check :: PublicKey -> Signature -> BS.ByteString -> Bool
check pk sig msg = verify pk sig digest
  where
    digest = M.convert $ H.hashWith H.Blake2b_256 msg

verify :: PublicKey -> Signature -> BS.ByteString -> Bool
verify pk sig = case (pk, sig) of
  (PublicKey_Ed25519 pk', Signature_Ed25519 sig') -> verify_Ed25519 pk' sig'
  (PublicKey_Ed25519 pk', Signature_Unknown sig') -> verify_Ed25519 pk' sig'
  (PublicKey_Ed25519 _, _) -> const False
  (PublicKey_Secp256k1 pk', Signature_Secp256k1 sig') -> verify_Secp256k1 pk' sig'
  (PublicKey_Secp256k1 pk', Signature_Unknown sig') -> verify_Secp256k1 pk' sig'
  (PublicKey_Secp256k1 _, _) -> const False
  (PublicKey_P256 pk', Signature_P256 sig') -> verify_P256 pk' sig'
  (PublicKey_P256 pk', Signature_Unknown sig') -> verify_P256 pk' sig'
  (PublicKey_P256 _, _) -> const False

verify_Ed25519 :: Ed25519PublicKey -> HashedValue a -> BS.ByteString -> Bool
verify_Ed25519 (HashedValue pk) (HashedValue sig) msg = onCryptoFailure (const False) id $
  Ed25519.verify <$> Ed25519.publicKey (fromShort pk) <*> pure msg <*> Ed25519.signature (fromShort sig)
verify_Secp256k1 :: Secp256k1PublicKey -> HashedValue a -> BS.ByteString -> Bool
verify_Secp256k1 (HashedValue pk) = verify_UECC U.secp256k1 (fromShort pk)
verify_P256 :: P256PublicKey -> HashedValue a -> BS.ByteString -> Bool
verify_P256 (HashedValue pk) = verify_UECC U.secp256r1 (fromShort pk)

verify_UECC :: U.Curve -> BS.ByteString -> HashedValue a -> BS.ByteString -> Bool
verify_UECC c pk (HashedValue sig) msg = fromMaybe False $ do
  upk <- U.decompress c pk
  pure $ U.verify c upk msg (fromShort sig)
