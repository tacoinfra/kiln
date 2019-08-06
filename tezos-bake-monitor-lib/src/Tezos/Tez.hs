{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tezos.Tez where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toEncoding, toJSON)
import Data.Fixed (E6, Fixed, Micro, resolution)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.Word (Word64)
import Numeric.Natural (Natural)

import qualified Tezos.Binary as B
import Tezos.Json (parseIntegralAsString)

newtype Tez = Tez { getTez :: Micro }
  deriving (Eq, Ord, Show, Typeable, Enum, Fractional, Num, Real, RealFrac, NFData, Hashable)

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
-- strings.
instance ToJSON Tez where
  toJSON = toJSON . show . getMicroTez
  toEncoding = toEncoding . show . getMicroTez

instance FromJSON Tez where
  parseJSON x = microTez <$> parseIntegralAsString @Word64 x

instance B.TezosBinary Tez where
  put t = B.put @Natural $ fromIntegral $ getMicroTez t
  get = microTez <$> B.get @Natural
