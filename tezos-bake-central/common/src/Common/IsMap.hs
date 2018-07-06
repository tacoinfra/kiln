{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.IsMap where

import qualified Data.AppendMap as AppendMap
import Data.IntervalMap.Generic.Interval (Interval)
import qualified Data.IntervalMap.Generic.Lazy as LazyIntervalMap
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

class IsMap k map where
  -- Required
  intersectionWithKey :: (k -> a -> b -> c) -> map k a -> map k b -> map k c
  unionWithKey :: (k -> a -> a -> a) -> map k a -> map k a -> map k a
  mapMaybeWithKey :: (k -> a -> Maybe b) -> map k a -> map k b
  toList :: map k a -> [(k, a)]

  -- Optional
  keys :: map k a -> [k]
  keys = map fst . toList
  keysSet :: Ord k => map k a -> Set k
  keysSet = Set.fromList . keys
  elems :: map k a -> [a]
  elems = map snd . toList
  mapWithKey :: (k -> a -> b) -> map k a -> map k b
  mapWithKey f = mapMaybeWithKey (\k v -> Just $ f k v)
  filterWithKey :: (k -> a -> Bool) -> map k a -> map k a
  filterWithKey f = mapMaybeWithKey (\k v -> if f k v then Just v else Nothing)
  intersectionWith :: (a -> b -> c) -> map k a -> map k b -> map k c
  intersectionWith f = intersectionWithKey (const f)
  unionWith :: (a -> a -> a) -> map k a -> map k a -> map k a
  unionWith f = unionWithKey (const f)
  restrictKeys :: Ord k => map k a -> Set k -> map k a
  restrictKeys m ks = filterWithKey (\k _ -> k `Set.member` ks) m

instance (Ord k) => IsMap k Map.Map where
  intersectionWithKey = Map.intersectionWithKey
  unionWithKey = Map.unionWithKey
  mapMaybeWithKey = Map.mapMaybeWithKey
  toList = Map.toList

  keys = Map.keys
  keysSet = Map.keysSet
  elems = Map.elems
  mapWithKey = Map.mapWithKey
  filterWithKey = Map.filterWithKey
  intersectionWith = Map.intersectionWith
  unionWith = Map.unionWith
  -- restrictKeys = Map.restrictKeys -- TODO: Upgrade containers

deriving instance (Ord k) => IsMap k AppendMap.AppendMap

instance (Interval k e, Ord k) => IsMap k LazyIntervalMap.IntervalMap where -- Lazy and Strict are same type
  intersectionWithKey = LazyIntervalMap.intersectionWithKey
  unionWithKey = LazyIntervalMap.unionWithKey
  toList = LazyIntervalMap.toList
  mapMaybeWithKey = LazyIntervalMap.mapMaybeWithKey

  keys = LazyIntervalMap.keys
  keysSet = LazyIntervalMap.keysSet
  elems = LazyIntervalMap.elems
  mapWithKey = LazyIntervalMap.mapWithKey
  filterWithKey = LazyIntervalMap.filterWithKey
  intersectionWith = LazyIntervalMap.intersectionWith
  unionWith = LazyIntervalMap.unionWith
