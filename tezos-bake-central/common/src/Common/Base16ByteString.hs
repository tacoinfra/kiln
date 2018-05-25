{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
module Common.Base16ByteString where

import Data.Aeson
import GHC.Generics
import Data.Typeable
import Data.Semigroup

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS
import qualified Data.Text.Encoding as T

import Common.TezosBinary

newtype Base16ByteString a = Base16ByteString { unbase16ByteString :: a }
  deriving (Eq, Ord, Show, Generic, Typeable, Functor, Foldable, Traversable)

instance TezosBinary a => TezosBinary (Base16ByteString a) where
  parseBinary = Base16ByteString <$> parseBinary
  encodeBinary (Base16ByteString x) = encodeBinary x

instance TezosBinary a => FromJSON (Base16ByteString a) where
  parseJSON x = do
    hexesText <- parseJSON x
    -- TODO: this should probably be lazy...
    let (bytes, rest) = BS.decode $ T.encodeUtf8 hexesText
    if (BS.length rest > 0)
    then fail $ "unmatched characters" <> show rest
    else case eitherBinary bytes of
      Left bad -> fail bad
      Right value -> return $ Base16ByteString value

instance TezosBinary a => ToJSON (Base16ByteString a) where
  toJSON (Base16ByteString x) = toJSON $ T.decodeUtf8 $ BS.encode $ encodeBinary x
  toEncoding (Base16ByteString x) = toEncoding $ T.decodeUtf8 $ BS.encode $ encodeBinary x
