{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.Tez where

import Data.Proxy
import Data.Attoparsec.ByteString
import Control.Applicative
import Data.Aeson
import Data.Fixed
import GHC.Generics
import Data.Typeable
import Data.Int
import Data.Scientific

import Common.TezosBinary

newtype Tezzies = Tezzies { getTezzies :: Micro }
  deriving (Eq, Ord, Show, Generic, Typeable, Enum, Fractional, Num, Real, RealFrac)

getMicroTezzies :: Tezzies -> Int64
getMicroTezzies
  = (floor :: Fixed E6 -> Int64)
  . ((fromInteger $ resolution (Proxy :: Proxy E6)) * )
  . getTezzies

microTezzies :: forall a. Integral a => a -> Tezzies
microTezzies
  = Tezzies
  . (/ (fromInteger $ resolution (Proxy :: Proxy E6)))
  . (fromIntegral :: a -> Fixed E6)

-- | the instance for Data.Fixed.Micro defined in Data.Aeson is perfectly
-- cromulent, its just not what we need.  tezos encodes these values as
-- integers.  Like the FromJSON instance below, it "may" be neccesary to encode
-- values larger than `2^31/resolution` as strings, but that's not handled
-- currently
instance ToJSON Tezzies where
  toJSON = toJSON . getMicroTezzies
  toEncoding = toEncoding . getMicroTezzies

instance FromJSON Tezzies where
  parseJSON x = (microTezzies . (floor :: Scientific -> Int64)) <$> parseJSON x
            <|> (microTezzies . (read :: String -> Int64)) <$> parseJSON x

instance TezosBinary Tezzies where
  parseBinary = microTezzies <$> (parseBinary :: Parser Int64)
  encodeBinary = encodeBinary . getMicroTezzies
