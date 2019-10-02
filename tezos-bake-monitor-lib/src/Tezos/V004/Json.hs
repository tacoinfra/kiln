{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tezos.V004.Json where

import Control.DeepSeq (NFData)
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON, Value, camelTo2, encode, parseJSON, toEncoding, toJSON)
import Data.Bits (Bits)
import Data.Hashable (Hashable)
import Data.List (uncons)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import qualified Data.Aeson.TH as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable, typeRep)
import Data.Vector (Vector)
import Data.Word (Word64)
import Language.Haskell.TH
import Text.Read (readMaybe)


parseAsString :: forall a. (Read a, Typeable a) => Aeson.Value -> Aeson.Parser a
parseAsString = Aeson.withText (show $ typeRep (Proxy :: Proxy a)) $ \txt ->
  maybe (fail "Failed to parse string") pure $ readMaybe (T.unpack txt)

parseIntegralAsString :: forall a. (Read a, Integral a, Typeable a) => Aeson.Value -> Aeson.Parser a
parseIntegralAsString x = ((floor :: Scientific -> a) <$> parseJSON x) <|> parseAsString x


deriveTezosJson :: Name -> Q [Dec]
deriveTezosJson = deriveTezosJsonKind "kind"

deriveTezosToJson :: Name -> Q [Dec]
deriveTezosToJson = Aeson.deriveToJSON (tezosJsonOptionsKind "kind")

deriveTezosJsonKind :: String -> Name -> Q [Dec]
deriveTezosJsonKind = Aeson.deriveJSON . tezosJsonOptionsKind

tezosJsonOptions :: Aeson.Options
tezosJsonOptions = tezosJsonOptionsKind "kind"

tezosJsonOptionsKind :: String -> Aeson.Options
tezosJsonOptionsKind tagFieldName = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = tail . camelTo2 '_' . dropWhile ('_' /=) . tail
  , Aeson.constructorTagModifier =
      \ctor -> camelTo2 '_' $ maybe ctor snd $ uncons $ dropWhile ('_' /=) ctor
  , Aeson.sumEncoding = Aeson.defaultTaggedObject
    { Aeson.tagFieldName = tagFieldName
    }
  }

data JsonValue
  = JsonObject !(Map Text JsonValue)
  | JsonArray !(Vector JsonValue)
  | JsonString !Text
  | JsonNumber !Scientific
  | JsonBool !Bool
  | JsonNull
  deriving (Eq, Ord, Typeable)

instance Show JsonValue where
  show x = "decode \"" <> T.unpack (T.decodeUtf8 $ LBS.toStrict $ encode $ fromJsonValue x) <> "\""

toJsonValue :: Aeson.Value -> JsonValue
toJsonValue (Aeson.Object x) = JsonObject $ Map.fromList $ HashMap.toList $ fmap toJsonValue x
toJsonValue (Aeson.Array x) = JsonArray $ fmap toJsonValue x
toJsonValue (Aeson.String x) = JsonString x
toJsonValue (Aeson.Number x) = JsonNumber x
toJsonValue (Aeson.Bool x) = JsonBool x
toJsonValue Aeson.Null = JsonNull

fromJsonValue :: JsonValue -> Aeson.Value
fromJsonValue (JsonObject x) = Aeson.Object $ HashMap.fromList $ Map.toList $ fmap fromJsonValue x
fromJsonValue (JsonArray x) = Aeson.Array $ fmap fromJsonValue x
fromJsonValue (JsonString x) = Aeson.String x
fromJsonValue (JsonNumber x) = Aeson.Number x
fromJsonValue (JsonBool x) = Aeson.Bool x
fromJsonValue JsonNull = Aeson.Null

instance ToJSON JsonValue where
  toJSON = fromJsonValue
  toEncoding = toEncoding . fromJsonValue

instance FromJSON JsonValue where
  parseJSON = pure . toJsonValue


-- | Tezos RPC JSON encodes 64-bit numbers as strings.
newtype TezosWord64 = TezosWord64 { unTezosWord64 :: Word64 }
  deriving (Eq, Ord, Show, Bounded, Enum, Typeable, Num, Integral, Bits, Real, NFData, Hashable)

instance FromJSON TezosWord64 where
  parseJSON x = TezosWord64 <$> parseIntegralAsString x

instance ToJSON TezosWord64 where
  toJSON (TezosWord64 x) = toJSON (show x)
  toEncoding (TezosWord64 x) = toEncoding (show x)

-- | "$ref": "#/definitions/error",
newtype JsonRpcError = JsonRpcError Value
  deriving (Eq, Show, Typeable, ToJSON, FromJSON)

instance Ord JsonRpcError where
  compare (JsonRpcError a) (JsonRpcError b) = encode a `compare` encode b
