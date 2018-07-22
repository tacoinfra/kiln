{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tezos.Json where

import Language.Haskell.TH
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON, parseJSON, toEncoding, toJSON, Value, encode, camelTo2)
import qualified Data.Aeson.TH as Aeson
import Data.Bits (Bits)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Typeable (Typeable, typeRep)
import Data.Word (Word64)
import Text.Read (readMaybe)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T


parseAsString :: forall a. (Read a, Typeable a) => Aeson.Value -> Aeson.Parser a
parseAsString = Aeson.withText (show $ typeRep (Proxy :: Proxy a)) $ \txt ->
  maybe (fail "Failed to parse string") pure $ readMaybe (T.unpack txt)

parseIntegralAsString :: forall a. (Read a, Integral a, Typeable a) => Aeson.Value -> Aeson.Parser a
parseIntegralAsString x = ((floor :: Scientific -> a) <$> parseJSON x) <|> parseAsString x


deriveTezosJson :: Name -> Q [Dec]
deriveTezosJson = deriveTezosJsonKind "kind"

deriveTezosJsonKind :: String -> Name -> Q [Dec]
deriveTezosJsonKind = Aeson.deriveJSON . tezosJsonOptionsKind

tezosJsonOptions :: Aeson.Options
tezosJsonOptions = tezosJsonOptionsKind "kind"

tezosJsonOptionsKind :: String -> Aeson.Options
tezosJsonOptionsKind tagFieldName = Aeson.defaultOptions
      { Aeson.fieldLabelModifier = camelTo2 '_' . dropWhile ('_' /=) . tail
      , Aeson.constructorTagModifier = camelTo2 '_' . dropWhile ('_' /=)
      , Aeson.sumEncoding = Aeson.defaultTaggedObject
        { Aeson.tagFieldName = tagFieldName
        }
      }

-- | Tezos RPC JSON encodes 64-bit numbers as strings.
newtype TezosWord64 = TezosWord64 { unTezosWord64 :: Word64 }
  deriving (Eq, Ord, Show, Bounded, Enum, Typeable, Num, Integral, Bits, Real)

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
