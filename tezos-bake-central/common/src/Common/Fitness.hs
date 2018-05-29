{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
module Common.Fitness where

import Data.Function
import Data.Semigroup
import Data.Aeson
import Data.Sequence (Seq)
import Data.Typeable
import GHC.Generics
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Sequence as Seq
import Data.Attoparsec.ByteString ((<?>))

import Focus.Schema (Json(..))
import Common.Base16ByteString
import Common.TezosBinary


data FitnessF a = FitnessF { unFitnessF :: Seq a }
  deriving (Eq, Show, Generic, Typeable, Functor, Foldable, Traversable)

-- | for these to be useful, you'd need `TezosBinary ByteString`, but that's
-- almost certainly the *wrong* one for this particular FromJSON, which needs
-- the "slurpy" variety, not the length prefixed
--
-- <strikeout>Not sure why GND doesn't work for this...</strikeout>
-- instance ToJSON a => ToJSON (FitnessF a) where
--   toJSON = toJSON . unFitnessF
--   toEncoding = toEncoding . unFitnessF
-- 
-- instance FromJSON a => FromJSON (FitnessF a) where
--   parseJSON = fmap FitnessF . parseJSON
instance FromJSON (FitnessF (Base16ByteString BS.ByteString)) where
  parseJSON x = FitnessF . fmap (Base16ByteString . fst . BS16.decode . T.encodeUtf8) <$> parseJSON x

instance ToJSON (FitnessF (Base16ByteString BS.ByteString)) where
  toJSON (FitnessF xs) = toJSON $ T.decodeUtf8 . BS16.encode . unbase16ByteString <$> xs
  toEncoding (FitnessF xs) = toEncoding $ T.decodeUtf8 . BS16.encode . unbase16ByteString <$> xs

-- | for these to be useful, you'd need `TezosBinary ByteString`, but that's
-- definately not the same one as needed for the above FromJSON instances;
-- since both would be needed, both are wrong.  we satisfy ourselves with the
-- fully monomorphic instances for both
--
-- instance TezosBinary a => TezosBinary (FitnessF a) where
--   parseBinary = FitnessF <$> parseBinary
--   encodeBinary = encodeBinary . unFitnessF

instance TezosBinary (FitnessF (Base16ByteString BS.ByteString)) where
  parseBinary = (<?> "Fitness") $ do
    xs <- parserRecursiveLengthPrefixed parseLengthPrefixedByteString
    return $ FitnessF $ Seq.fromList $ fmap Base16ByteString xs

  encodeBinary (FitnessF xs) = encodeLengthPrefixedByteString $ foldMap (encodeLengthPrefixedByteString . unbase16ByteString) xs

type Fitness' a = Json (FitnessF (Base16ByteString a))
type Fitness = Fitness' BS.ByteString

toFitness :: Seq a -> Fitness' a
toFitness xs = Json (FitnessF $ fmap Base16ByteString xs)

unFitness :: Json (FitnessF (Base16ByteString a)) -> Seq a
unFitness (Json (FitnessF xs)) = fmap unbase16ByteString xs

instance Ord a => Ord (FitnessF a) where
  compare = (compare `on` length) <> (compare `on` unFitnessF)

