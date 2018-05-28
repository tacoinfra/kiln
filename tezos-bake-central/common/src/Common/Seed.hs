{-# LANGUAGE DeriveGeneric #-}

module Common.Seed where

import Data.Typeable
import GHC.Generics
import qualified Data.ByteString as BS
import Common.TezosBinary

newtype Seed = Seed BS.ByteString
  deriving (Eq, Ord, Show, Generic, Typeable)


instance TezosBinary Seed where
  parseBinary = Seed <$> parseLengthPrefixedByteString
  encodeBinary (Seed x) = encodeLengthPrefixedByteString x
