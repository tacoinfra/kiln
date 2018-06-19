{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Json where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON, parseJSON, toEncoding, toJSON)
import qualified Data.Aeson.Types as Aeson
import Data.Bits (Bits)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Data.Typeable (Typeable, typeRep)
import Data.Word (Word64)
import Text.Read (readMaybe)


parseAsString :: forall a. (Read a, Typeable a) => Aeson.Value -> Aeson.Parser a
parseAsString = Aeson.withText (show $ typeRep (Proxy :: Proxy a)) $ \txt ->
  maybe (fail "Failed to parse string") pure $ readMaybe (T.unpack txt)

parseIntegralAsString :: forall a. (Read a, Integral a, Typeable a) => Aeson.Value -> Aeson.Parser a
parseIntegralAsString x = ((floor :: Scientific -> a) <$> parseJSON x) <|> parseAsString x


-- | Tezos RPC JSON encodes 64-bit numbers as strings.
newtype TezosWord64 = TezosWord64 { unTezosWord64 :: Word64 }
  deriving (Eq, Ord, Show, Bounded, Enum, Typeable, Num, Integral, Bits, Real)

instance FromJSON TezosWord64 where
  parseJSON x = TezosWord64 <$> parseIntegralAsString x

instance ToJSON TezosWord64 where
  toJSON (TezosWord64 x) = toJSON (show x)
  toEncoding (TezosWord64 x) = toEncoding (show x)
