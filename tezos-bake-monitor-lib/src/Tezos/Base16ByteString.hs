{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tezos.Base16ByteString where

import Data.Aeson
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Aeson.Types
import Tezos.ShortByteString (ShortByteString, fromShort, toShort)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS
import qualified Data.Text.Encoding as T
import Data.Typeable


newtype Base16ByteString a = Base16ByteString { unbase16ByteString :: a }
  deriving (Eq, Ord, Show, Typeable, Functor, Foldable, Traversable)

instance FromJSON (Base16ByteString ShortByteString) where
  parseJSON x = fmap toShort <$> parseJSON x

instance ToJSON (Base16ByteString ShortByteString) where
  toJSON = toJSON . fmap fromShort
  toEncoding = toEncoding . fmap fromShort

instance FromJSON (Base16ByteString BS.ByteString) where
  parseJSON x = do
    hexesText <- modifyFailure (show x <>) $ parseJSON x
    -- TODO: this should probably be lazy...
    let (bytes, rest) = BS.decode $ T.encodeUtf8 hexesText
    if BS.length rest > 0
    then fail $ "unmatched characters" <> show rest
    else return $ Base16ByteString bytes

instance ToJSON (Base16ByteString BS.ByteString) where
  toJSON (Base16ByteString x) = toJSON $ T.decodeUtf8 $ BS.encode x
  toEncoding (Base16ByteString x) = toEncoding $ T.decodeUtf8 $ BS.encode x

-- instance TezosBinary a => TezosBinary (Base16ByteString a) where
--   parseBinary = Base16ByteString <$> (parseBinary <?> "Base16ByteString")
--   encodeBinary (Base16ByteString x) = encodeBinary x

-- instance (Typeable a, TezosBinary a) => FromJSON (Base16ByteString a) where
--   parseJSON x = do
--     hexesText <- modifyFailure (show x <>) $ parseJSON x
--     -- TODO: this should probably be lazy...
--     let (bytes, rest) = BS.decode $ T.encodeUtf8 hexesText
--     if BS.length rest > 0
--     then fail $ "unmatched characters" <> show rest
--     else case eitherBinary (show $ typeRep (Proxy :: Proxy a)) bytes of
--       Left bad -> fail bad
--       Right value -> return $ Base16ByteString value
-- 
-- instance TezosBinary a => ToJSON (Base16ByteString a) where
--   toJSON (Base16ByteString x) = toJSON $ T.decodeUtf8 $ BS.encode $ encodeBinary x
--   toEncoding (Base16ByteString x) = toEncoding $ T.decodeUtf8 $ BS.encode $ encodeBinary x
