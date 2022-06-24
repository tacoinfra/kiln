{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Tezos.Common.Contract where

import Control.DeepSeq (NFData)
import Data.Aeson
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Hashable (Hashable)
import Data.String
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Typeable
import GHC.Generics (Generic)

import Tezos.Common.Base58Check
import Tezos.Common.PublicKeyHash

data ContractId
  = Implicit PublicKeyHash
  | Originated ContractHash
  deriving (Eq, Ord, Generic, Typeable)
instance Hashable ContractId
instance NFData ContractId

contractIdConstructorDecoders :: [TryDecodeBase58 ContractId]
contractIdConstructorDecoders =
  [ TryDecodeBase58 (Implicit . PublicKeyHash_Ed25519)
  , TryDecodeBase58 (Implicit . PublicKeyHash_Secp256k1)
  , TryDecodeBase58 (Implicit . PublicKeyHash_P256)
  , TryDecodeBase58 Originated
  ]

tryReadContractId :: BS.ByteString -> Either HashBase58Error ContractId
tryReadContractId = tryFromBase58 contractIdConstructorDecoders

tryReadContractIdText :: Text -> Either HashBase58Error ContractId
tryReadContractIdText = tryReadContractId . T.encodeUtf8

instance ToJSON ContractId where
  toJSON (Implicit x) = toJSON x
  toJSON (Originated x) = toJSON x

  toEncoding (Implicit x) = toEncoding x
  toEncoding (Originated x) = toEncoding x

instance FromJSON ContractId where
  parseJSON x = do
    x' <- T.encodeUtf8 <$> parseJSON x
    case tryFromBase58 contractIdConstructorDecoders x' of
      Left bad -> fail $ show bad
      Right ok -> return ok

instance FromJSONKey ContractId
instance ToJSONKey ContractId

toContractIdText :: ContractId -> Text
toContractIdText = \case
  Implicit x -> toPublicKeyHashText x
  Originated x -> toBase58Text x

instance Show ContractId where
  show = ("fromString " <>) . show . toContractIdText

instance IsString ContractId where
  fromString x = either (error . show) id $ tryFromBase58 contractIdConstructorDecoders $ fromString x
