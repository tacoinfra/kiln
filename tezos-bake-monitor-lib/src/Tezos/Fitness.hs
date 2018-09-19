{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module Tezos.Fitness where

import Data.Aeson
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
-- import Data.Attoparsec.ByteString ((<?>))
import Tezos.ShortByteString (ShortByteString, toShort, fromShort)
import qualified Data.ByteString.Base16 as BS16
import Data.Function
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import Text.Show (showListWith, showString)

import Tezos.Base16ByteString


newtype FitnessF a = FitnessF { unFitnessF :: Seq a }
  deriving (Eq, Typeable, Functor, Foldable, Traversable)

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
instance FromJSON (FitnessF (Base16ByteString ShortByteString)) where
  parseJSON x = FitnessF . fmap (Base16ByteString . toShort . fst . BS16.decode . T.encodeUtf8) <$> parseJSON x

instance ToJSON (FitnessF (Base16ByteString ShortByteString)) where
  toJSON (FitnessF xs) = toJSON $ T.decodeUtf8 . BS16.encode . fromShort . unbase16ByteString <$> xs
  toEncoding (FitnessF xs) = toEncoding $ T.decodeUtf8 . BS16.encode . fromShort . unbase16ByteString <$> xs

-- | for these to be useful, you'd need `TezosBinary ByteString`, but that's
-- definately not the same one as needed for the above FromJSON instances;
-- since both would be needed, both are wrong.  we satisfy ourselves with the
-- fully monomorphic instances for both
--
-- instance TezosBinary a => TezosBinary (FitnessF a) where
--   parseBinary = FitnessF <$> parseBinary
--   encodeBinary = encodeBinary . unFitnessF

-- instance TezosBinary (FitnessF (Base16ByteString BS.ByteString)) where
--   parseBinary = (<?> "Fitness") $ do
--     xs <- parserRecursiveLengthPrefixed parseLengthPrefixedByteString
--     return $ FitnessF $ Seq.fromList $ fmap Base16ByteString xs
-- 
--   encodeBinary (FitnessF xs) = encodeLengthPrefixedByteString $ foldMap (encodeLengthPrefixedByteString . unbase16ByteString) xs

type Fitness' a = FitnessF (Base16ByteString a)
type Fitness = Fitness' ShortByteString

toFitness :: Seq a -> Fitness' a
toFitness xs = (FitnessF $ fmap Base16ByteString xs)

unFitness :: (FitnessF (Base16ByteString a)) -> Seq a
unFitness ((FitnessF xs)) = fmap unbase16ByteString xs

instance Show Fitness where
  showsPrec _ = showListWith (showString . T.unpack . T.decodeUtf8 . BS16.encode . fromShort) . toList . unFitness

instance Ord a => Ord (FitnessF a) where
  compare = (compare `on` length) <> (compare `on` unFitnessF)

instance Ord a => Semigroup (FitnessF a) where
  (<>) = max

instance Ord a => Monoid (FitnessF a) where
  mempty = FitnessF mempty
  mappend = (<>)
