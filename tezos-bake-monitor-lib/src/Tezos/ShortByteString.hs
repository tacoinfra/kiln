{-# LANGUAGE CPP #-}

#if defined(ghcjs_HOST_OS)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#endif

module Tezos.ShortByteString (ShortByteString, toShort, fromShort) where

#if defined(ghcjs_HOST_OS)

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)

newtype ShortByteString = ShortByteString { fromShort :: ByteString }
  deriving (Eq, Ord, NFData, Hashable)

toShort :: ByteString -> ShortByteString
toShort = ShortByteString
{-# INLINE toShort #-}

#else

import Data.ByteString.Short (ShortByteString, fromShort, toShort)

#endif


