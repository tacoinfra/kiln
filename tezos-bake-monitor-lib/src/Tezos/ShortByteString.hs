{-# LANGUAGE CPP #-}

module Tezos.ShortByteString (ShortByteString, toShort, fromShort) where

#if defined(ghcjs_HOST_OS)
import Data.ByteString (ByteString)
#else
import Data.ByteString.Short (ShortByteString, toShort, fromShort)
#endif


#if defined(ghcjs_HOST_OS)
newtype ShortByteString = ShortByteString { fromShort :: ByteString } deriving (Eq, Ord)
toShort :: ByteString -> ShortByteString
toShort = ShortByteString
{-# INLINE toShort #-}
#endif


