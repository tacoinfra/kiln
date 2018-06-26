{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Common.Tez where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toEncoding, toJSON)
import Data.Attoparsec.ByteString
import Data.Fixed (E6, Fixed, Micro, resolution)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Word (Word64)

import Common.Json (parseIntegralAsString)
import Common.TezosBinary

newtype Tez = Tez { getTez :: Micro }
  deriving (Eq, Ord, Show, Generic, Typeable, Enum, Fractional, Num, Real, RealFrac)

getMicroTez :: Tez -> Int64
getMicroTez
  = (floor :: Fixed E6 -> Int64)
  . ((fromInteger $ resolution (Proxy :: Proxy E6)) * )
  . getTez

microTez :: forall a. Integral a => a -> Tez
microTez
  = Tez
  . (/ (fromInteger $ resolution (Proxy :: Proxy E6)))
  . (fromIntegral :: a -> Fixed E6)

-- | the instance for Data.Fixed.Micro defined in Data.Aeson is perfectly
-- cromulent, its just not what we need.  tezos encodes these values as
-- integers.  Like the FromJSON instance below, it "may" be neccesary to encode
-- values larger than `2^31/resolution` as strings, but that's not handled
-- currently
instance ToJSON Tez where
  toJSON = toJSON . getMicroTez
  toEncoding = toEncoding . getMicroTez

instance FromJSON Tez where
  parseJSON x = microTez <$> parseIntegralAsString @Word64 x


instance TezosBinary Tez where
  parseBinary = microTez <$> (parseBinary :: Parser Int64)
  encodeBinary = encodeBinary . getMicroTez
