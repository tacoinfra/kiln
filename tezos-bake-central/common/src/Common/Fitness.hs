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
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import Data.Attoparsec.ByteString ((<?>))

import Focus.Schema (Json(..))
import Common.Base16ByteString
import Common.TezosBinary


data FitnessF a = FitnessF { unFitnessF :: Seq a }
  deriving (Eq, Show, Generic, Typeable, Functor, Foldable, Traversable)

-- | Not sure why GND doesn't work for this...
instance ToJSON a => ToJSON (FitnessF a) where
  toJSON = toJSON . unFitnessF
  toEncoding = toEncoding . unFitnessF

instance FromJSON a => FromJSON (FitnessF a) where
  parseJSON = fmap FitnessF . parseJSON

type Fitness' a = Json (FitnessF (Base16ByteString a))
type Fitness = Fitness' BS.ByteString

toFitness :: Seq a -> Fitness' a
toFitness xs = Json (FitnessF $ fmap Base16ByteString xs)

unFitness :: Json (FitnessF (Base16ByteString a)) -> Seq a
unFitness (Json (FitnessF xs)) = fmap unbase16ByteString xs

instance Ord a => Ord (FitnessF a) where
  compare = (compare `on` length) <> (compare `on` unFitnessF)

-- instance TezosBinary a => TezosBinary (FitnessF a) where
--   parseBinary = FitnessF <$> parseBinary
--   encodeBinary = encodeBinary . unFitnessF

instance TezosBinary (FitnessF (Base16ByteString BS.ByteString)) where
  parseBinary = (<?> "Fitness") $ do
    xs <- parserRecursiveLengthPrefixed parseLengthPrefixedByteString
    return $ FitnessF $ Seq.fromList $ fmap Base16ByteString xs

  encodeBinary (FitnessF xs) = encodeLengthPrefixedByteString $ foldMap (encodeLengthPrefixedByteString . unbase16ByteString) xs
