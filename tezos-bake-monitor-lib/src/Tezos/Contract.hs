{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Tezos.Contract where

import Data.Aeson
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.String
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Typeable

import Tezos.Base58Check
import Tezos.PublicKeyHash
import Tezos.Micheline
import Tezos.Json

data ContractId
  = Implicit PublicKeyHash
  | Originated ContractHash
  deriving (Eq, Ord, Typeable)

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

-- | "scripted.contracts": {
data ContractScript = ContractScript
  { _contractScript_code :: Expression --  "code": { "$ref": "#/definitions/micheline.michelson_v1.expression" },
  , _contractScript_storage :: Expression --  "storage": { "$ref": "#/definitions/micheline.michelson_v1.expression" }
  }
  deriving (Eq, Ord, Show, Typeable)

deriveTezosJson ''ContractScript
