{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tezos.V004.Tez
  ( Tez(..)
  , getMicroTez
  , microTez
  , TezDelta(..)
  , getMicroTezDelta
  , microTezDelta
  )
  where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toEncoding, toJSON)
import Data.Fixed (E6, Fixed, Micro, resolution)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Word (Word64)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Numeric.Natural (Natural)

import qualified Tezos.Common.Binary as B
import Tezos.Common.Json (parseIntegralAsString)

-- In the json schema this is called mutez and it must be positive (this is the value used as
-- an account balance).
newtype Tez = Tez { getTez :: Micro }
  deriving (Eq, Ord, Show, Typeable, Enum, Fractional, Num, Real, RealFrac, NFData, Hashable)

getMicroTez :: Tez -> Int64
getMicroTez = getMicros getTez

microTez :: forall a. Integral a => a -> Tez
microTez = fromMicros Tez

-- | the instance for Data.Fixed.Micro defined in Data.Aeson is perfectly
-- cromulent, its just not what we need.  tezos encodes these values as
-- strings.
instance ToJSON Tez where
  toJSON = toJSON . show . getMicroTez
  toEncoding = toEncoding . show . getMicroTez

instance FromJSON Tez where
  -- It's very important that you don't change this to Int64. It's supposed to be a natural.
  -- TODO: It's probably better that we just make this parser fail if the number isn't within
  -- the bounds we expect. This parseIntegralAsString is ripe for overflow type errors and
  -- it's a good idea to fix them all at some point.
  parseJSON x = microTez <$> parseIntegralAsString @Word64 x

instance B.TezosBinary Tez where
  put t = B.put @Natural $ fromIntegral $ getMicroTez t
  get = microTez <$> B.get @Natural

-- This is the Int64 that is used in balance updates in operations. It can be negative
newtype TezDelta = TezDelta { getTezDelta :: Micro }
  deriving (Eq, Ord, Show, Typeable, Enum, Fractional, Num, Real, RealFrac, NFData, Hashable)

getMicroTezDelta :: TezDelta -> Int64
getMicroTezDelta = getMicros getTezDelta

microTezDelta :: forall a. Integral a => a -> TezDelta
microTezDelta = fromMicros TezDelta

-- | the instance for Data.Fixed.Micro defined in Data.Aeson is perfectly
-- cromulent, its just not what we need.  tezos encodes these values as
-- strings.
instance ToJSON TezDelta where
  toJSON = toJSON . show . getMicroTezDelta
  toEncoding = toEncoding . show . getMicroTezDelta

instance FromJSON TezDelta where
  parseJSON x = microTezDelta <$> parseIntegralAsString @Int64 x

instance B.TezosBinary TezDelta where
  put t = B.put @Int64 $ fromIntegral $ getMicroTezDelta t
  get = microTezDelta <$> B.get @Int64

-- P.s: This will overflow if we ever have a Tez value that uses the last bit of the Word64.
-- (It was broken back before TezDelta got added, and it'll work for TezDelta)
getMicros :: (tz -> Micro) -> tz -> Int64
getMicros unWrap
  = (floor :: Fixed E6 -> Int64)
  . ((fromInteger $ resolution (Proxy :: Proxy E6)) * )
  . unWrap

fromMicros :: forall a tz. Integral a => (Micro -> tz) -> a -> tz
fromMicros wrap
  = wrap
  . (/ (fromInteger $ resolution (Proxy :: Proxy E6)))
  . (fromIntegral :: a -> Fixed E6)
