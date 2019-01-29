{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tezos.Base16ByteString where

import Control.DeepSeq (NFData)
import Data.Aeson
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS
import Data.Hashable (Hashable)
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import qualified Tezos.Binary as B
-- import Tezos.ShortByteString (ShortByteString, fromShort, toShort)


newtype Base16ByteString a = Base16ByteString { unbase16ByteString :: a }
  deriving (Eq, Ord, Show, Typeable, Functor, Foldable, Traversable, Generic, NFData, Hashable)

-- instance FromJSON (Base16ByteString ShortByteString) where
--   parseJSON x = fmap toShort <$> parseJSON x

-- instance ToJSON (Base16ByteString ShortByteString) where
--   toJSON = toJSON . fmap fromShort
--   toEncoding = toEncoding . fmap fromShort

-- instance FromJSON (Base16ByteString BS.ByteString) where
--   parseJSON x = do
--     hexesText <- modifyFailure (show x <>) $ parseJSON x
--     -- TODO: this should probably be lazy...
--     let (bytes, rest) = BS.decode $ T.encodeUtf8 hexesText
--     if BS.length rest > 0
--     then fail $ "unmatched characters" <> show rest
--     else return $ Base16ByteString bytes

-- instance ToJSON (Base16ByteString BS.ByteString) where
--   toJSON (Base16ByteString x) = toJSON $ T.decodeUtf8 $ BS.encode x
--   toEncoding (Base16ByteString x) = toEncoding $ T.decodeUtf8 $ BS.encode x

instance B.TezosBinary a => B.TezosBinary (Base16ByteString a) where
  build (Base16ByteString x) = B.build x
  put (Base16ByteString x) = B.put x
  get = Base16ByteString <$> B.get

instance B.TezosBinary a => FromJSON (Base16ByteString a) where
  parseJSON x = do
    hexesText <- modifyFailure (show x <>) $ parseJSON x
    -- TODO: this should probably be lazy...
    let (bytes, rest) = BS.decode $ T.encodeUtf8 hexesText
    if BS.length rest > 0
    then fail $ "unmatched characters" <> show rest
    else case B.decodeEither bytes of
      Left bad -> fail bad
      Right value -> return $ Base16ByteString value

instance B.TezosBinary a => ToJSON (Base16ByteString a) where
  toJSON (Base16ByteString x) = toJSON $ T.decodeUtf8 $ BS.encode $ B.encode x
  toEncoding (Base16ByteString x) = toEncoding $ T.decodeUtf8 $ BS.encode $ B.encode x
