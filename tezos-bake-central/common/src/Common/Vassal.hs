{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC -Wall -Werror -Wno-unused-imports -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Common.Vassal where

import Prelude hiding (lookup, null, (.))

import Common.AppendIntervalMap (AppendIntervalMap, ClosedInterval (..), WithInfinity (..))
import qualified Common.AppendIntervalMap as IMap
import Control.Category ((.))
import Control.Lens.Indexed (FoldableWithIndex, FunctorWithIndex, TraversableWithIndex, imap, itraverse)
import Control.Monad.Writer.CPS (Writer, runWriter, tell)
import Data.Aeson (FromJSON, FromJSON1, FromJSONKey, ToJSON, ToJSON1, ToJSONKey, liftParseJSON,
                   liftToEncoding, liftToJSON, parseJSON, toEncoding, toJSON)
import Data.Aeson.TH (defaultOptions, mkLiftParseJSON, mkLiftToEncoding, mkLiftToJSON, mkParseJSON,
                      mkToEncoding, mkToJSON)
import Data.Align
import Data.AppendMap ()
import Data.Constraint
import Data.Foldable (null, toList, traverse_)
import Data.Functor.Classes
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import qualified Data.IntervalMap.Generic.Lazy as BaseIMap
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup (First (..), Option (..), Semigroup, (<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.These (These (..))
import Data.Witherable (Filterable(mapMaybe, catMaybes))

import Common.WrappedShow1

-- we have the general problem of needing to send "incremental" updates to a
-- (view of) a shared data set.  The general idea is to have an initial query
-- that captures the desired view in roughly the right format for that data
-- set, then have a "monoidal" operation that can update the initial data set
-- with the updated data,  this can usually include whiteouts, counterfactuals
-- indicating that a previously known fact is now false and should be erased
-- from the dataset.
--
-- Whenever a new value for `View f` is learned, it
-- can be (left) `mappend`ed to the old value (`mappend new old`)

-- for ergonomics reasons, you can use the Query type in your code and the
-- correct instances will compute a suitable

-- TODO:
-- i think i know how to fix the deletes issues for all of the selectors here,
-- as with other notes, all Views should be a pair of "response data" with no
-- 'a's in them, and the verbatim ViewSelector.
--
-- for MaybeView/MapView, use maybes at the individual keys (theres nothing
-- else to do anyway)
--
-- for RangeView, keep a separate IntervalMap e () of whiteouts which indicates
-- what the response contains.
--
-- for IntervalView, the results data is essentially a RangeView, combined with
-- a mapping from the "Id's" associated with the
--  - if the IntervalSelector allowed the user to also query for these ID's,
--    then whiteouts work exactly as RangeView
--  - if not, the only way to make deletes work is to have per-element maybes
--    (as MaybeView/MapView above) with the extra side condition that the
--    individual elements result interval cannot ever get smaller, even once
--    they're deleted.

-- | this is the parametric replacement for *crop*.
chop :: (Semigroup a, ViewSelector t) => (a -> b -> Maybe c) -> t a -> View t b -> View t c
chop f vs = iMapMaybe $ \i b -> flip f b =<< lookup i vs

cropView :: (Semigroup a, ViewSelector t) => t a -> View t b -> View t a
cropView vs = iMapMaybe $ \i _ -> lookup i vs

class ( TraversableWithIndex (ViewIndex f) (View f)
      , Filterable (View f)
      -- these two shouldn't really be needed, rhyolite doesn't actually use
      -- them.  for now, this makes it easier to "migrate", especially since
      -- the needed semigroup instances can be more easily made with these
      , Filterable f, Functor f, Align f
      ) => ViewSelector f where
  data View f :: * -> *
  type ViewIndex f

  -- we could do this with QuantifiedConstraints, in 8.6
  viewIsSemigroup         :: Semigroup a :- Semigroup (View f a)
  viewIsMonoid            :: Semigroup a :- Monoid (View f a)
  viewSelectorIsSemigroup :: Semigroup a :- Semigroup (f a)

  lookup :: Semigroup a => ViewIndex f -> f a -> Maybe a

viewSelects :: (Semigroup a, ViewSelector f) => ViewIndex f -> f a -> Bool
viewSelects i vs = not $ null $ lookup i vs

type ComposeSelector = Compose

viewCompose :: f (g a) -> ComposeSelector f g a
viewCompose = Compose

type ComposeView f g a = View (Compose f g) a


instance
  ( ViewSelector f
  , ViewSelector g
  , Ord (ViewIndex f)
  )
  => ViewSelector (Compose f g) where

  data View (Compose f g) a = ComposeView
    { _composeView_upper :: View f a
    , _composeView_lower :: Compose (MonoidalMap (ViewIndex f)) (View g) a
    }
  type ViewIndex (Compose f g) = (ViewIndex f, ViewIndex g)

  viewIsMonoid = Sub Dict
  viewIsSemigroup = Sub Dict
  viewSelectorIsSemigroup = Sub Dict

  lookup = composeLookup

getComposeView :: View (Compose f g) a -> (View f a, MonoidalMap (ViewIndex f) (View g a))
getComposeView (ComposeView upper (Compose lower)) = (upper, lower)

deriving instance
  ( ViewSelector f, Eq1 (View f)
  , ViewSelector g, Eq1 (View g)
  , Eq (View f a) , Eq1 (MonoidalMap (ViewIndex f))
  , Eq a) => Eq (View (Compose f g) a)

deriving instance
  ( ViewSelector f, Ord1 (View f)
  , ViewSelector g, Ord1 (View g)
  , Ord (View f a), Ord1 (MonoidalMap (ViewIndex f))
  , Ord a) => Ord (View (Compose f g) a)


deriving instance
  ( ViewSelector f, Show1 (View f)
  , ViewSelector g, Show1 (View g)
  , Show (View f a), Show1 (MonoidalMap (ViewIndex f))
  , Show a) => Show (View (Compose f g) a)

composeLookup :: forall f g a.
  ( ViewSelector g , ViewSelector f , Semigroup a )
  => (ViewIndex f, ViewIndex g) -> Compose f g a -> Maybe a
composeLookup (k0, k1) (Compose xs) = case viewSelectorIsSemigroup :: Semigroup a :- Semigroup (g a) of
  Sub Dict -> lookup k0 xs >>= lookup k1

-- Well, without QuantifiedConstraints, we can't actually have these instances
-- in the first place, (the :- methods in ViewSelector are a hack that gives us
-- an alternative).  As such, orphan instances are a lesser evil
instance (ViewSelector f, ViewSelector g, Semigroup a) => Semigroup (Compose f g a) where
  Compose xs <> Compose ys = Compose $ xs <> ys
    \\ (viewSelectorIsSemigroup . viewSelectorIsSemigroup
        :: (Semigroup a :- Semigroup (f (g a))))

instance (ViewSelector f, ViewSelector g, Semigroup a, Ord (ViewIndex f)) => Semigroup (View (Compose f g) a) where
  ComposeView uxs (Compose lxs) <> ComposeView uys (Compose lys) = ComposeView (uxs <> uys) (Compose (lxs <> lys))
    \\ (viewIsSemigroup :: Semigroup a :- Semigroup (View f a))
    \\ (viewIsSemigroup :: Semigroup a :- Semigroup (View g a))


instance (Ord (ViewIndex f), ViewSelector f, ViewSelector g, Semigroup a) => Monoid (View (Compose f g) a) where
  mempty = ComposeView mempty (Compose mempty)
    \\ (viewIsMonoid :: Semigroup a :- Monoid (View f a))
    \\ (viewIsSemigroup :: Semigroup a :- Semigroup (View g a))


deriving instance (Functor (View v), Functor (View w)) => Functor (View (Compose v w))
deriving instance (Foldable (View v), Foldable (View w)) => Foldable (View (Compose v w))
deriving instance (Traversable (View v), Traversable (View w)) => Traversable (View (Compose v w))

-- chop out whole subtrees of the outer view when the inner view is
-- `Foldable.null`  That way we can trim out query responses that are not
-- supposed to be visible to the caller.  For now though, the default instance
-- is nearly right.
instance (ViewSelector v, ViewSelector w, Ord (ViewIndex v))
    => Filterable (View (Compose v w)) where
  mapMaybe :: forall a b. (a -> Maybe b) -> View (Compose v w) a -> View (Compose v w) b
  mapMaybe f (ComposeView upper (Compose lower)) = ComposeView (catMaybes upper') (Compose $ MMap.MonoidalMap lower')
    where
      swizzle :: ViewIndex v -> a -> Writer (Map.Map (ViewIndex v) (View w b)) (Maybe b)
      swizzle i x = case f x of
        Nothing -> return Nothing
        Just y -> do
          traverse_  (tell . Map.singleton i . mapMaybe f) (MMap.lookup i lower)
          return $ Just y
      (upper', lower') = runWriter $ itraverse swizzle upper :: ( View v (Maybe b) , Map.Map (ViewIndex v) (View w b) )

instance
  ( ViewSelector v
  , ViewSelector w
  , i ~ ViewIndex (Compose v w)
  , Ord (ViewIndex v)
  )
  => FunctorWithIndex i (View (Compose v w))
instance
  ( ViewSelector v
  , ViewSelector w
  , i ~ ViewIndex (Compose v w)
  , Ord (ViewIndex v)
  )
  => FoldableWithIndex i (View (Compose v w))
-- i think this is where i need UndecidableInstances
instance
  ( ViewSelector v
  , ViewSelector w
  , i ~ (ViewIndex v, ViewIndex w)
  , Ord (ViewIndex v)
  )
  => TraversableWithIndex i (View (Compose v w)) where

  itraverse :: forall f a b. Applicative f => ((ViewIndex v, ViewIndex w) -> a -> f b) -> View (Compose v w) a -> f (View (Compose v w) b)
  itraverse f (ComposeView upper lower) = ComposeView <$> upper' <*> lower'
    where
      lower' :: f (Compose (MonoidalMap (ViewIndex v)) (View w) b)
      lower' = itraverse f lower

      upper' :: f (View v b)
      upper' = iWither witherUpper upper

      witherUpper :: ViewIndex v -> a -> f (Maybe b)
      witherUpper i x = maybe (pure Nothing) (traverse getFirst . getOption . getConst . itraverse (\j _ -> Const $ Option $ Just $ First $ f (i,j) x)) $ MMap.lookup i $ getCompose lower


iMapMaybe :: (FunctorWithIndex i t, Filterable t) => (i -> a -> Maybe b) -> t a -> t b
iMapMaybe f = catMaybes . imap f

iWither :: (TraversableWithIndex i t, Filterable t, Applicative f) => (i -> a -> f (Maybe b)) -> t a -> f (t b)
iWither f = fmap catMaybes . itraverse f


-- because of the combining aspect of how these things get used, there will
-- also be an extra parameter that must be carried around with both queries and
-- their responses to tie response back to their queries.  That may be
-- explained in detail later, but for now, there will need to be some extra,
-- functorial data, that can usually be counted on to be a Semigroup

-- The simplest is "Single" which is a global value that can be queried or not,
-- and be updated or not.

type MaybeView v a = View (MaybeSelector v) a

newtype MaybeSelector (v :: *) a = MaybeSelector { unMaybeSelector :: Option a }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable, Monoid, Semigroup, ToJSON, ToJSON1, FromJSON, FromJSON1, Filterable, Align)



viewJust :: a -> MaybeSelector v a
viewJust = MaybeSelector . Option . Just

instance ViewSelector (MaybeSelector (v :: *)) where
  newtype View (MaybeSelector v) a = MaybeView { unSingle :: Option (First v, a) }
    deriving (Eq, Show, Ord, Semigroup, Monoid, Functor, Foldable, Traversable, FromJSON, ToJSON)

  type ViewIndex (MaybeSelector v) = ()
  viewIsMonoid = Sub Dict
  viewIsSemigroup = Sub Dict
  viewSelectorIsSemigroup  = Sub Dict

  lookup _ = getOption . unMaybeSelector
  {-# INLINE lookup #-}

getMaybeView :: View (MaybeSelector v) a -> Maybe v
getMaybeView (MaybeView (Option x)) = getFirst . fst <$> x

instance Eq v => Eq1 (View (MaybeSelector v)) where
  liftEq f (MaybeView (Option xs)) (MaybeView (Option ys)) =
    liftEq (liftEq f) xs ys

instance Filterable (View (MaybeSelector v)) where
  mapMaybe f = MaybeView . mapMaybe (traverse f) . unSingle

instance FunctorWithIndex () (View (MaybeSelector v))
instance FoldableWithIndex () (View (MaybeSelector v))
instance TraversableWithIndex () (View (MaybeSelector v)) where
  itraverse f = traverse $ f ()


newtype MapSelector k (v :: *) a = MapSelector { unMapSelector :: MonoidalMap k a }
  deriving (Eq, Ord, Eq1, Ord1, Show, Functor, Foldable, Traversable, Monoid, Semigroup, FromJSON, FromJSON1, ToJSON, ToJSON1, Filterable, Align)

instance Ord k => ViewSelector (MapSelector k v) where
  newtype View (MapSelector k v) a = MapView { unMapView :: MonoidalMap k (First v, a) }
    deriving
      ( Show, Read, Functor, Eq, Ord
      , Foldable, Traversable
      , Semigroup, Monoid
      )
  type ViewIndex (MapSelector k v) = k

  viewIsMonoid = Sub Dict
  viewIsSemigroup = Sub Dict
  viewSelectorIsSemigroup  = Sub Dict

  lookup k = MMap.lookup k . unMapSelector

instance (Eq v, Ord k) => Eq1 (View (MapSelector k v)) where
  liftEq f (MapView (MMap.MonoidalMap xs)) (MapView (MMap.MonoidalMap ys)) =
    liftEq (liftEq f) xs ys

instance (Ord k, Ord v) => Ord1 (View (MapSelector k v)) where
  liftCompare f (MapView (MMap.MonoidalMap xs)) (MapView (MMap.MonoidalMap ys)) =
    liftCompare (liftCompare f) xs ys

instance Filterable (View (MapSelector k v)) where
  mapMaybe f = MapView . mapMaybe (traverse f) . unMapView

instance FunctorWithIndex k (View (MapSelector k v))
instance FoldableWithIndex k (View (MapSelector k v))
instance TraversableWithIndex k (View (MapSelector k v)) where

  itraverse :: forall f a b. Applicative f => (k -> a -> f b) -> View (MapSelector k v) a -> f (View (MapSelector k v) b)
  itraverse f = fmap MapView . itraverse f' . unMapView
    where
      f' :: k -> (First v, a) -> f (First v, b)
      f' k (x, y) = (x,) <$> f k y

newtype IntervalSelector e (i :: *) (v :: *) a = IntervalSelector
  { unIntervalSelector :: (AppendIntervalMap (ClosedInterval e)) a }
  deriving (Eq, Ord, Eq1, Ord1, Show, Functor, Foldable, Traversable, Monoid, Semigroup, FromJSON, FromJSON1, ToJSON, ToJSON1, Filterable, Align)

type IntervalSelector' e = IntervalSelector (WithInfinity e)

viewInterval :: (e, e) -> a -> IntervalSelector e i v a
viewInterval (lb, ub) = IntervalSelector . IMap.singleton (ClosedInterval lb ub)

viewIntervalSet :: Ord e => Set (ClosedInterval e) -> a -> IntervalSelector e i v a
viewIntervalSet xs a = IntervalSelector $ IMap.fromSet (const a) xs

type IntervalView e i v = View (IntervalSelector e i v)
type IntervalView' e i v = View (IntervalSelector (WithInfinity e) i v)

instance (Ord i, Ord e) => ViewSelector (IntervalSelector e i v) where
  data View (IntervalSelector e i v) a = IntervalView
    { _intervalView_intervals :: AppendIntervalMap (ClosedInterval e) a -- really just the view selector
    , _intervalView_elements :: MMap.MonoidalMap i (First (v, ClosedInterval e))
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  type ViewIndex (IntervalSelector e i v) = ClosedInterval e

  viewIsMonoid = Sub Dict
  viewIsSemigroup = Sub Dict
  viewSelectorIsSemigroup  = Sub Dict

  lookup k (IntervalSelector xs) = getOption $ foldMap (Option . Just) $ IMap.intersecting xs k

getIntervalViewI :: forall e i v a. Ord e => View (IntervalSelector e i v) a -> AppendIntervalMap (ClosedInterval e) (NonEmpty (i, v))
getIntervalViewI (IntervalView _ entries) = IMap.fromList $ (\(i, First (v, k)) -> (k, pure (i, v))) <$> MMap.toList entries

instance (Ord i, Ord e, Semigroup a) => Monoid (View (IntervalSelector e i v) a ) where
  mempty = IntervalView mempty mempty

instance (Semigroup a, Ord e, Ord i) => Semigroup (View (IntervalSelector e i v) a) where
  IntervalView s1 e1 <> IntervalView s2 e2 = IntervalView (s1 <> s2) (e1 <> e2)

instance (Eq i, Eq v, Eq e) => Eq1 (View (IntervalSelector e i v)) where
  liftEq f (IntervalView xs xxs) (IntervalView ys yys) = liftEq f xs ys && xxs == yys

instance (Ord i, Ord v, Ord e) => Ord1 (View (IntervalSelector e i v)) where
  liftCompare f (IntervalView xs xxs) (IntervalView ys yys) = liftCompare f xs ys `mappend` compare xxs yys

instance (Ord i, Ord e) => Filterable (View (IntervalSelector e i v)) where
  mapMaybe :: forall a b. (a -> Maybe b) -> View (IntervalSelector e i v) a -> View (IntervalSelector e i v) b
  mapMaybe f (IntervalView support entries) = IntervalView support' entries'
    where
      support' = mapMaybe f support
      entries' :: MonoidalMap i (First (v, ClosedInterval e))
      entries' = mapMaybe (\x@(First (_, k)) -> x <$ lookup k viewSelector) entries

      viewSelector :: IntervalSelector e i v ()
      viewSelector = IntervalSelector (() <$ support')

instance FunctorWithIndex (ClosedInterval e) (View (IntervalSelector e i v))
instance FoldableWithIndex (ClosedInterval e) (View (IntervalSelector e i v))

instance TraversableWithIndex (ClosedInterval e) (View (IntervalSelector e i v)) where
  itraverse :: forall f a b. Applicative f => (ClosedInterval e -> a -> f b) -> View (IntervalSelector e i v) a -> f (View (IntervalSelector e i v) b)
  itraverse f (IntervalView support entries) = flip IntervalView entries <$> itraverse f support


newtype RangeSelector e (v :: *) a = RangeSelector
  { unRangeSelector :: (AppendIntervalMap (ClosedInterval e)) a }
  deriving
    ( Eq, Eq1
    , Ord, Ord1
    , Show
    , Functor, Foldable, Traversable
    , Monoid, Semigroup
    , FromJSON, FromJSON1
    , ToJSON, ToJSON1
    , Filterable
    , Align)

type RangeSelector' e = RangeSelector (WithInfinity e)

viewRangeAll :: Bounded e => a -> RangeSelector e v a
viewRangeAll = RangeSelector . IMap.singleton (ClosedInterval minBound maxBound)

viewRangeBetween :: (e, e) -> a -> RangeSelector e v a
viewRangeBetween (k1, k2) = RangeSelector . IMap.singleton (ClosedInterval k1 k2)

viewRangeExactly :: e -> a -> RangeSelector e v a
viewRangeExactly k = RangeSelector . IMap.singleton (ClosedInterval k k)

viewRangeSet :: Ord e => Set e -> a -> RangeSelector e v a
viewRangeSet ks a = RangeSelector $ IMap.fromSet (const a) (Set.mapMonotonic eqK ks)
  where
    eqK k = ClosedInterval k k

type RangeView e v = View (RangeSelector e v)
type RangeView' e v = View (RangeSelector (WithInfinity e) v)

instance Ord e => ViewSelector (RangeSelector e v) where
  data View (RangeSelector e v) a = RangeView
    { _rangeView_support :: AppendIntervalMap (ClosedInterval e) a
    , _rangeView_points :: MonoidalMap e v
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  type ViewIndex (RangeSelector e v) = e

  lookup k (RangeSelector xs) = getOption $ foldMap (Option . Just) $ IMap.containing xs k

  viewIsMonoid = Sub Dict
  viewIsSemigroup = Sub Dict
  viewSelectorIsSemigroup = Sub Dict

getRangeView :: View (RangeSelector e v) a -> MonoidalMap e v
getRangeView = _rangeView_points

getRangeView' :: View (RangeSelector' e v) a -> MonoidalMap e v
getRangeView' = MMap.fromDistinctAscList . mapMaybe getBounded . MMap.toAscList . _rangeView_points
  where
    getBounded :: (WithInfinity e, v) -> Maybe (e, v)
    getBounded (Bounded k, v) = Just (k, v)
    getBounded _ = Nothing

instance (Eq e, Eq v) => Eq1 (View (RangeSelector e v)) where
  liftEq f (RangeView s1 p1) (RangeView s2 p2) = p1 == p2 && liftEq f s1 s2

instance (Ord e, Ord v) => Ord1 (View (RangeSelector e v)) where
  liftCompare f (RangeView s1 p1) (RangeView s2 p2) = compare p1 p2 <> liftCompare f s1 s2

instance (Semigroup a, Ord e) => Semigroup (View (RangeSelector e v) a) where
  RangeView i1 xs1 <> RangeView i2 xs2 = RangeView i $ MMap.unionWith const xs1 $ iMapMaybe inI xs2
    where
      i = i1 <> i2
      inI k v = if null $ IMap.containing i k then Nothing else Just v

instance (Semigroup a, Ord e) => Monoid (View (RangeSelector e v) a) where
  mempty = RangeView mempty MMap.empty

instance (Ord e) => Filterable (View (RangeSelector e v)) where
  mapMaybe f (RangeView i xs) = RangeView i' $ iMapMaybe inI xs
    where
      i' = mapMaybe f i
      inI k v = if null $ IMap.containing i' k then Nothing else Just v

instance Ord e => FunctorWithIndex e (View (RangeSelector e v))
instance Ord e => FoldableWithIndex e (View (RangeSelector e v))
-- This is not 100% cromulent, but i think it's "correct" for the way they get
-- used, which is poking around in Views to make Filterable trim out just enough data properly
instance Ord e => TraversableWithIndex e (View (RangeSelector e v)) where
  itraverse f (RangeView i xs) = RangeView <$> itraverse (\(ClosedInterval lb _) x -> f lb x) i <*> pure xs -- xs <$> iWither _f i

-- produce a view that covers a single point, useful for NotifyHandlers
toRangeView1 :: (Semigroup a, Ord e) => RangeSelector e v a -> e -> Maybe v -> View (RangeSelector e v) a
toRangeView1 vs e xs = RangeView (IMap.fromList $ toList $ (k,) <$> vs') (fromMaybe MMap.empty $ MMap.singleton <$> e' <*> xs)
  where
    k = ClosedInterval e e
    vs' = lookup e vs
    e' = e <$ vs'

toRangeView
  :: Ord e
  => RangeSelector e v a
  -> [(e, v)]
  -> View (RangeSelector e v) a
toRangeView sel@(RangeSelector vs) rows = toRangeViewUnsafe sel $
  filter (not . null . IMap.containing vs . fst) rows

toRangeViewUnsafe :: Ord e => RangeSelector e v a -> [(e, v)] -> View (RangeSelector e v) a
toRangeViewUnsafe (RangeSelector vs) v = RangeView vs $ MMap.fromDistinctList v

toMaybeView :: MaybeSelector v a -> Maybe v -> View (MaybeSelector v) a
toMaybeView (MaybeSelector vs) (Just v) = MaybeView $ fmap (First v,) vs
toMaybeView (MaybeSelector _) Nothing = MaybeView $ Option Nothing

-- these are a terrible hack to get through the release.  the real deal would be
-- to make the query just test each range
isCompleteSelector :: Ord k => RangeSelector' k v a -> Bool
isCompleteSelector (RangeSelector (IMap.AppendIntervalMap vs)) = isJust $ BaseIMap.lookup (ClosedInterval LowerInfinity UpperInfinity) vs

tightenView :: ViewSelector v => View v a -> View v a
tightenView = mapMaybe Just

iMapSelectorKeys :: RangeSelector' k v a -> [k]
iMapSelectorKeys (RangeSelector vs) = mapMaybe f $ IMap.keys vs
  where
    f (ClosedInterval l _) = case l of
      Bounded x -> Just x
      _ -> Nothing

-- more orphans!

-- TODO Upstream into witherable
deriving instance Filterable Option

instance (Align f, Align g) => Align (Compose f g) where
  nil = Compose nil
  alignWith f (Compose xs) (Compose ys) = Compose $ alignWith f' xs ys
    where
      f' = \case
        This ga -> fmap (f . This) ga
        That gb -> fmap (f . That) gb
        These ga gb -> alignWith f ga gb

deriving instance Align Option


deriveShow1Methods [d|instance (Show v, Show e) => Show1 (View (RangeSelector e v))|]
deriveShow1Methods [d|instance (        Show e) => Show1       (RangeSelector e v) |]
deriveShow1Methods [d|instance (Show i, Show v, Show e) => Show1 (View (IntervalSelector e i v))|]
deriveShow1Methods [d|instance (                Show e) => Show1       (IntervalSelector e i v) |]
deriveShow1Methods [d|instance (Show k, Show v) => Show1 (View (MapSelector k v))|]
deriveShow1Methods [d|instance (Show k, Show v) => Show1       (MapSelector k v) |]
deriveShow1Methods [d|instance (Show v) => Show1 (View (MaybeSelector v))|]
deriveShow1Methods [d|instance             Show1       (MaybeSelector v) |]

instance (FromJSON e, Ord i, Ord e, FromJSON i, FromJSON v, Ord v, FromJSONKey i) => FromJSON1 (View (IntervalSelector e i v)) where
  liftParseJSON = $(mkLiftParseJSON defaultOptions 'IntervalView)
instance (ToJSON i, ToJSON e, Ord e, ToJSON v, Ord v, ToJSONKey i) => ToJSON1 (View (IntervalSelector e i v)) where
  liftToJSON = $(mkLiftToJSON defaultOptions 'IntervalView)
  liftToEncoding = $(mkLiftToEncoding defaultOptions 'IntervalView)
instance (Ord e, Ord i, FromJSON e, FromJSON i, FromJSON a, FromJSON v, FromJSONKey i) => FromJSON (View (IntervalSelector e i v) a) where
  parseJSON = $(mkParseJSON defaultOptions 'IntervalView)
instance (Ord e, ToJSON e, ToJSON i, ToJSON v, ToJSON a, ToJSONKey i) => ToJSON (View (IntervalSelector e i v) a) where
  toJSON = $(mkToJSON defaultOptions 'IntervalView)
  toEncoding = $(mkToEncoding defaultOptions 'IntervalView)


instance (FromJSON k, Ord k, FromJSON v, FromJSONKey k) => FromJSON1 (View (RangeSelector k v)) where
  liftParseJSON = $(mkLiftParseJSON defaultOptions 'RangeView)
instance (Ord k, Semigroup a, FromJSON k, FromJSON a, FromJSON v, FromJSONKey k) => FromJSON (View (RangeSelector k v) a) where
  parseJSON = $(mkParseJSON defaultOptions 'RangeView)

instance (ToJSON k, Ord k, ToJSON v, ToJSONKey k) => ToJSON1 (View (RangeSelector k v)) where
  liftToEncoding = $(mkLiftToEncoding defaultOptions 'RangeView)
  liftToJSON = $(mkLiftToJSON defaultOptions 'RangeView)
instance (Ord k, Semigroup a, ToJSON k, ToJSON a, ToJSON v, ToJSONKey k) => ToJSON (View (RangeSelector k v) a) where
  toEncoding = $(mkToEncoding defaultOptions 'RangeView)
  toJSON = $(mkToJSON defaultOptions 'RangeView)

instance (FromJSON k, Ord k, FromJSON v, FromJSONKey k) => FromJSON1 (View (MapSelector k v)) where
  liftParseJSON = $(mkLiftParseJSON defaultOptions 'MapView)
instance (Ord k, Semigroup a, FromJSON k, FromJSON a, FromJSON v, FromJSONKey k) => FromJSON (View (MapSelector k v) a) where
  parseJSON = $(mkParseJSON defaultOptions 'MapView)

instance (ToJSON k, Ord k, ToJSON v, ToJSONKey k) => ToJSON1 (View (MapSelector k v)) where
  liftToEncoding = $(mkLiftToEncoding defaultOptions 'MapView)
  liftToJSON = $(mkLiftToJSON defaultOptions 'MapView)
instance (Ord k, Semigroup a, ToJSON k, ToJSON a, ToJSON v, ToJSONKey k) => ToJSON (View (MapSelector k v) a) where
  toEncoding = $(mkToEncoding defaultOptions 'MapView)
  toJSON = $(mkToJSON defaultOptions 'MapView)

instance (FromJSON (View v a), FromJSON a, FromJSON1 (View w), ViewSelector v, ViewSelector w, Ord (ViewIndex v), FromJSONKey (ViewIndex v)) => FromJSON (View (Compose v w) a) where
  parseJSON = $(mkParseJSON defaultOptions 'ComposeView)

instance (ToJSON (View v a), ToJSON a, ToJSON1 (View w), ToJSONKey (ViewIndex v), Ord(ViewIndex v), ViewSelector v, ViewSelector w) => ToJSON (View (Compose v w) a) where
  toEncoding = $(mkToEncoding defaultOptions 'ComposeView)
  toJSON = $(mkToJSON defaultOptions 'ComposeView)
