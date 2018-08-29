{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Common.AppendIntervalMap where

import Control.Lens.Indexed (FoldableWithIndex, FunctorWithIndex, TraversableWithIndex (itraverse))
import Data.Aeson (FromJSON, FromJSON1, FromJSONKey, ToJSONKey, parseJSON, liftParseJSON)
import Data.Aeson (ToJSON, ToJSON1, toEncoding, toJSON, liftToJSONList, liftToJSON, liftToEncoding, liftToEncodingList)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Align (Align (align, nil))
import Data.Functor.Classes
import Data.Semigroup (Semigroup ((<>)))
import Data.Set (Set)
import Data.These (These (That, These, This))
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)
import Reflex.FunctorMaybe (FunctorMaybe (fmapMaybe))
import qualified Data.IntervalMap.Generic.Interval as IntervalClass
import qualified Data.IntervalMap.Generic.Lazy as IMap
import qualified Data.Set as Set

{-# OPTIONS_GHC -Wall -Werror -Wno-orphans #-}

type IsInterval i e = IntervalClass.Interval i e

newtype AppendIntervalMap k v = AppendIntervalMap { unAppendIntervalMap :: IMap.IntervalMap k v }
  deriving 
    (Functor, Foldable, Traversable, Show, Eq, Ord, Generic, Typeable, Generic1)

instance Eq k => Eq1 (AppendIntervalMap k) where
  liftEq f (AppendIntervalMap xs) (AppendIntervalMap ys) = liftEq f' (IMap.toAscList xs) (IMap.toAscList ys)
    where
      f' (k1, x) (k2, y) =  k1 == k2 && f x y

instance Ord k => Ord1 (AppendIntervalMap k) where
  liftCompare f (AppendIntervalMap xs) (AppendIntervalMap ys) = liftCompare f' (IMap.toAscList xs) (IMap.toAscList ys)
    where
      f' (k1, x) (k2, y) =  compare k1 k2 <> f x y

deriving instance (IsInterval k e, Ord k, Read k, Read v) => Read (AppendIntervalMap k v)

instance (IsInterval k e, Ord k, Semigroup v) => Semigroup (AppendIntervalMap k v) where
  (<>) = unionWith (<>)

instance (IsInterval k e, Ord k, Semigroup v) => Monoid (AppendIntervalMap k v) where
  mempty = AppendIntervalMap mempty
  mappend = (<>)

instance FunctorWithIndex k (AppendIntervalMap k)
instance FoldableWithIndex k (AppendIntervalMap k)
instance TraversableWithIndex k (AppendIntervalMap k) where
  itraverse f = fmap AppendIntervalMap . sequenceA . IMap.mapWithKey f . unAppendIntervalMap

instance (IsInterval k e, Ord k) => Align (AppendIntervalMap k) where
  nil = AppendIntervalMap mempty
  align m n = unionWith merge (This <$> m) (That <$> n)
    where merge (This m') (That n') = These m' n'
          merge _ _ = error "Impossible: Align AppendIntervalMap merge"

instance (IsInterval k e) => FunctorMaybe (AppendIntervalMap k) where
  fmapMaybe f v = AppendIntervalMap $ IMap.mapMaybe f (unAppendIntervalMap v)

instance (IsInterval k e, Ord k, ToJSON k, ToJSON v) => ToJSON (AppendIntervalMap k v) where
  toJSON = toJSON . IMap.toAscList . unAppendIntervalMap
  toEncoding = toEncoding . IMap.toAscList . unAppendIntervalMap

instance (IsInterval k e, Ord k, FromJSON k, FromJSON v) => FromJSON (AppendIntervalMap k v) where
  parseJSON = fmap (AppendIntervalMap . IMap.fromList) . parseJSON

instance (FromJSON k, IsInterval k e, Ord k) => FromJSON1 (AppendIntervalMap k) where
  liftParseJSON :: forall a. (Aeson.Value -> Aeson.Parser a) -> (Aeson.Value -> Aeson.Parser [a])
                -> Aeson.Value -> Aeson.Parser (AppendIntervalMap k a)
  liftParseJSON parse _parseList val = AppendIntervalMap . IMap.fromList <$> liftParseJSON parse' parseList' val
    where
      parse' :: Aeson.Value -> Aeson.Parser (k, a)
      parse' val' = do
        (k, aVal) <- parseJSON val'
        a <- parse aVal
        return (k, a)

      parseList' :: Aeson.Value -> Aeson.Parser [(k, a)]
      parseList' val' = traverse parse' =<< (parseJSON val' :: Aeson.Parser [Aeson.Value])

instance ToJSON k => ToJSON1 (AppendIntervalMap k) where
  liftToJSON :: forall a. (a -> Aeson.Value) -> ([a] -> Aeson.Value) -> AppendIntervalMap k a -> Aeson.Value
  liftToJSON to tos (AppendIntervalMap xs) = (liftToJSONList to tos :: [(k, a)] -> Aeson.Value ) $ IMap.toList xs

  liftToEncoding :: forall a. (a -> Aeson.Encoding) -> ([a] -> Aeson.Encoding) -> AppendIntervalMap k a -> Aeson.Encoding
  liftToEncoding to tos (AppendIntervalMap xs) = (liftToEncodingList to tos :: [(k, a)] -> Aeson.Encoding ) $ IMap.toList xs


singleton :: forall k v e. (Ord k, IsInterval k e) => k -> v -> AppendIntervalMap k v
singleton k = AppendIntervalMap . IMap.singleton k

fromList :: forall k v e. (Ord k, IsInterval k e) => [(k, v)] -> AppendIntervalMap k v
fromList = AppendIntervalMap . IMap.fromList

fromAscList :: forall k v e. (Ord k, IsInterval k e) => [(k, v)] -> AppendIntervalMap k v
fromAscList = AppendIntervalMap . IMap.fromAscList

intersectionWithKey :: forall k a b c e. (Ord k, IsInterval k e) => (k -> a -> b -> c) -> AppendIntervalMap k a -> AppendIntervalMap k b -> AppendIntervalMap k c
intersectionWithKey f a b = AppendIntervalMap $ IMap.intersectionWithKey f (unAppendIntervalMap a) (unAppendIntervalMap b)

unionWithKey :: forall k v e. (Ord k, IsInterval k e) => (k -> v -> v -> v) -> AppendIntervalMap k v -> AppendIntervalMap k v -> AppendIntervalMap k v
unionWithKey f a b = AppendIntervalMap $ IMap.unionWithKey f (unAppendIntervalMap a) (unAppendIntervalMap b)

mapMaybeWithKey :: forall k a b e. (Ord k, IsInterval k e) => (k -> a -> Maybe b) -> AppendIntervalMap k a -> AppendIntervalMap k b
mapMaybeWithKey f = AppendIntervalMap . IMap.mapMaybeWithKey f . unAppendIntervalMap

toList :: forall k v e. (Ord k, IsInterval k e) => AppendIntervalMap k v -> [(k, v)]
toList = IMap.toList . unAppendIntervalMap

keys :: forall k v e. (Ord k, IsInterval k e) => AppendIntervalMap k v -> [k]
keys = IMap.keys . unAppendIntervalMap

keysSet :: forall k v e. (Ord k, IsInterval k e) => AppendIntervalMap k v -> Set k
keysSet = IMap.keysSet . unAppendIntervalMap

elems :: forall k v e. (Ord k, IsInterval k e) => AppendIntervalMap k v -> [v]
elems = IMap.elems . unAppendIntervalMap

mapWithKey :: forall k a b e. (Ord k, IsInterval k e) => (k -> a -> b) -> AppendIntervalMap k a -> AppendIntervalMap k b
mapWithKey f = AppendIntervalMap . IMap.mapWithKey f . unAppendIntervalMap

filterWithKey :: forall k v e. (Ord k, IsInterval k e) => (k -> v -> Bool) -> AppendIntervalMap k v -> AppendIntervalMap k v
filterWithKey f = AppendIntervalMap . IMap.filterWithKey f . unAppendIntervalMap

intersectionWith :: forall k a b c e. (Ord k, IsInterval k e) => (a -> b -> c) -> AppendIntervalMap k a -> AppendIntervalMap k b -> AppendIntervalMap k c
intersectionWith f a b = AppendIntervalMap $ IMap.intersectionWith f (unAppendIntervalMap a) (unAppendIntervalMap b)

unionWith :: forall k v e. (Ord k, IsInterval k e) => (v -> v -> v) -> AppendIntervalMap k v -> AppendIntervalMap k v -> AppendIntervalMap k v
unionWith f a b = AppendIntervalMap $ IMap.unionWith f (unAppendIntervalMap a) (unAppendIntervalMap b)

fromSet :: forall k v e. (Ord k, IsInterval k e) => (k -> v) -> Set k -> AppendIntervalMap k v
fromSet toV s = fromAscList [(k, toV k) | k <- Set.toAscList s]

containing :: forall k v e. (IsInterval k e) => AppendIntervalMap k v -> e -> AppendIntervalMap k v
containing m = AppendIntervalMap . IMap.containing (unAppendIntervalMap m)

intersecting :: forall k v e. (IsInterval k e) => AppendIntervalMap k v -> k -> AppendIntervalMap k v
intersecting m = AppendIntervalMap . IMap.intersecting (unAppendIntervalMap m)

within :: forall k v e. (IsInterval k e) => AppendIntervalMap k v -> k -> AppendIntervalMap k v
within m = AppendIntervalMap . IMap.within (unAppendIntervalMap m)


-- | Builds a new 'AppendIntervalMap' with a function that can combine adjacent intervals.
-- Returning 'Nothing' from the combining function means that the two elements should not be combined.
-- They are kept distinct in the result.
flattenWithKey
  :: forall k v e. (Ord k, IsInterval k e)
  => ((k, v) -> (k, v) -> Maybe (k, v)) -> AppendIntervalMap k v -> AppendIntervalMap k v
flattenWithKey f a = AppendIntervalMap $ IMap.flattenWith f (unAppendIntervalMap a)

-- | Like 'flattenWithKey' but assumes that the combining function produces new keys monotonically.
flattenWithKeyMonotonic
  :: forall k v e. (Ord k, IsInterval k e)
  => ((k, v) -> (k, v) -> Maybe (k, v)) -> AppendIntervalMap k v -> AppendIntervalMap k v
flattenWithKeyMonotonic f a = AppendIntervalMap $ IMap.flattenWithMonotonic f (unAppendIntervalMap a)

flattenWithClosedInterval
  :: forall i v. (Ord i)
  => (v -> v -> v) -> AppendIntervalMap (ClosedInterval i) v -> AppendIntervalMap (ClosedInterval i) v
flattenWithClosedInterval f = flattenWithKeyMonotonic $
  \(k1@(ClosedInterval x1 y1), v1) (k2@(ClosedInterval x2 y2), v2) ->
    if k1 `IMap.overlaps` k2 then Just (ClosedInterval (min x1 x2) (max y1 y2), f v1 v2) else Nothing

data ClosedInterval a = ClosedInterval a a
  deriving (Eq, Ord, Generic, Typeable, Show, Read, Functor, Foldable, Traversable)
instance FromJSON a => FromJSON (ClosedInterval a)
instance ToJSON a => ToJSON (ClosedInterval a)

instance FromJSON a => FromJSONKey (ClosedInterval a)
instance ToJSON a => ToJSONKey (ClosedInterval a)

data WithInfinity a = LowerInfinity | Bounded a | UpperInfinity
  deriving (Eq, Ord, Generic, Typeable, Show, Read, Functor, Foldable, Traversable)
instance FromJSON a => FromJSON (WithInfinity a)
instance ToJSON a => ToJSON (WithInfinity a)

instance Bounded (WithInfinity a) where
  minBound = LowerInfinity
  maxBound = UpperInfinity

getBounded :: WithInfinity a -> Maybe a
getBounded = \case
  Bounded a -> Just a
  _ -> Nothing

flattenIntervals
  :: (Ord e, Semigroup a)
  => (ClosedInterval e, a) -> (ClosedInterval e, a) -> Maybe (ClosedInterval e, a)
flattenIntervals (i0@(ClosedInterval lb0 ub0), x0) (i1@(ClosedInterval lb1 ub1), x1)
  | IMap.overlaps i0 i1 = Just (ClosedInterval (min lb0 lb1) (max ub0 ub1), (x0 <> x1))
  | otherwise = Nothing

instance Ord a => IMap.Interval (ClosedInterval a) a where
  lowerBound (ClosedInterval x _) = x
  upperBound (ClosedInterval _ y) = y
  leftClosed _ = True
  rightClosed _ = True
  before (ClosedInterval _ y1) (ClosedInterval x2 _) = y1 < x2
  subsumes (ClosedInterval x1 y1) (ClosedInterval x2 y2) = x1 <= x2 && y2 <= y1
  overlaps (ClosedInterval x1 y1) (ClosedInterval x2 y2) = x2 <= y1 || x1 <= y2
  below e (ClosedInterval x _) = e < x
  above e (ClosedInterval _ y) = e > y
  inside e (ClosedInterval x y) = x <= e && e <= y
  isEmpty (ClosedInterval x y) = y >= x
  compareUpperBounds (ClosedInterval _ y1) (ClosedInterval _ y2) = y1 `compare` y2
