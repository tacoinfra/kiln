{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Tezos.V005.Envelope where

-- import Control.DeepSeq (NFData)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)

import Tezos.Common.Base58Check
import qualified Tezos.Common.Binary as B
import Tezos.V005.BlockHeader
import Tezos.V005.Operation

{-
There are no changes to envelope, other than operation changed.
None of the Endorsements structure changed, but because there
duplicated types in V005 that are unchanged this needs to be
done. Operation reuses nothing from V004 when it probably could.

TODO: Maybe we should reimplement less in operation to make this
reimpl unnecessary.
-}

data Envelope
  = Envelope_BlockHeader !ChainId !BlockHeaderFull
  | Envelope_Endorsement !ChainId !(Op 'OpKind_Endorsement)
  deriving (Show, Eq, Ord, Generic, Typeable)
-- instance NFData Envelope

instance B.TezosBinary Envelope where
  put = \case
    Envelope_BlockHeader chainId blockHeader -> B.put @Word8 1 <* B.put chainId <* B.put (B.ToSign blockHeader)
    Envelope_Endorsement chainId endorsement -> B.put @Word8 2 <* B.put chainId <* B.put (B.ToSign endorsement)
  get = B.get @Word8 >>= \case
    1 -> Envelope_BlockHeader <$> B.get <*> (B.unToSign <$> B.get)
    2 -> Envelope_Endorsement <$> B.get <*> (B.unToSign <$> B.get)
    _ -> fail "unknown or unsupported magic byte in envelope"

