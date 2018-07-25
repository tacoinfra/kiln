{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tezos.Base16ByteString where

import Data.Aeson
import Data.Semigroup
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS
import qualified Data.Text.Encoding as T
import Data.Typeable


newtype Base16ByteString a = Base16ByteString { unbase16ByteString :: a }
  deriving (Eq, Ord, Show, Typeable, Functor, Foldable, Traversable)

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
