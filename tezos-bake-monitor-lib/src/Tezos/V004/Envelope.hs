{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Tezos.V004.Envelope where

-- import Control.DeepSeq (NFData)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)

import Tezos.V004.Base58Check
import qualified Tezos.V004.Binary as B
import Tezos.V004.BlockHeader
import Tezos.V004.Operation

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
