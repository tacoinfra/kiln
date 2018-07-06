{-# LANGUAGE ConstraintKinds #-}
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

module Common.AppendIntervalMap where

import Control.Lens.Indexed (FoldableWithIndex, FunctorWithIndex, TraversableWithIndex (itraverse))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON))
import Data.Align (Align (align, nil))
import qualified Data.IntervalMap.Generic.Interval as IntervalClass
import qualified Data.IntervalMap.Generic.Lazy as IMap
import Data.Semigroup (Semigroup ((<>)))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.These (These (That, These, This))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Reflex.FunctorMaybe (FunctorMaybe (fmapMaybe))

import Common.IsMap

type IsInterval i e = IntervalClass.Interval i e

newtype AppendIntervalMap k v = AppendIntervalMap { unAppendIntervalMap :: IMap.IntervalMap k v }
  deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

deriving instance (IsInterval k e, Ord k, Read k, Read v) => Read (AppendIntervalMap k v)

-- TODO: Can we derive this?
instance (IsInterval k e, Ord k) => IsMap k AppendIntervalMap where
  intersectionWithKey f a b = AppendIntervalMap $ intersectionWithKey f (unAppendIntervalMap a) (unAppendIntervalMap b)
  unionWithKey f a b = AppendIntervalMap $ unionWithKey f (unAppendIntervalMap a) (unAppendIntervalMap b)
  mapMaybeWithKey f = AppendIntervalMap . mapMaybeWithKey f . unAppendIntervalMap
  toList = IMap.toList . unAppendIntervalMap

  keys = keys . unAppendIntervalMap
  keysSet = keysSet . unAppendIntervalMap
  elems = elems . unAppendIntervalMap
  mapWithKey f = AppendIntervalMap . mapWithKey f . unAppendIntervalMap
  filterWithKey f = AppendIntervalMap . filterWithKey f . unAppendIntervalMap
  intersectionWith f a b = AppendIntervalMap $ intersectionWith f (unAppendIntervalMap a) (unAppendIntervalMap b)
  unionWith f a b = AppendIntervalMap $ unionWith f (unAppendIntervalMap a) (unAppendIntervalMap b)

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
    where merge (This m) (That n) = These m n
          merge _ _ = error "Impossible: Align AppendIntervalMap merge"

instance (IsInterval k e, Ord k) => FunctorMaybe (AppendIntervalMap k) where
  fmapMaybe f v = AppendIntervalMap $ IMap.mapMaybe f (unAppendIntervalMap v)

instance (IsInterval k e, Ord k, ToJSON k, ToJSON v) => ToJSON (AppendIntervalMap k v) where
  toJSON = toJSON . IMap.toAscList . unAppendIntervalMap
  toEncoding = toEncoding . IMap.toAscList . unAppendIntervalMap

instance (IsInterval k e, Ord k, Semigroup v, FromJSON k, FromJSON v) => FromJSON (AppendIntervalMap k v) where
  parseJSON = fmap (AppendIntervalMap . IMap.fromListWith (<>)) . parseJSON

singleton :: forall k v e. (Ord k, IsInterval k e) => k -> v -> AppendIntervalMap k v
singleton k = AppendIntervalMap . IMap.singleton k

fromList :: forall k v e. (Ord k, IsInterval k e) => [(k, v)] -> AppendIntervalMap k v
fromList = AppendIntervalMap . IMap.fromList

fromAscList :: forall k v e. (Ord k, IsInterval k e) => [(k, v)] -> AppendIntervalMap k v
fromAscList = AppendIntervalMap . IMap.fromAscList

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

data WithInfinity a = LowerInfinity | Bounded a | UpperInfinity
  deriving (Eq, Ord, Generic, Typeable, Show, Read, Functor, Foldable, Traversable)
instance FromJSON a => FromJSON (WithInfinity a)
instance ToJSON a => ToJSON (WithInfinity a)

getBounded :: WithInfinity a -> Maybe a
getBounded = \case
  Bounded a -> Just a
  _ -> Nothing

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
