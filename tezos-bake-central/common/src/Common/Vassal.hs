{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC -Wall -Werror -Wno-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Common.Vassal where

import Prelude hiding (lookup, (.), null)

import Data.Aeson(FromJSON, ToJSON, FromJSON1, ToJSON1, liftParseJSON, parseJSON, toJSON, toEncoding, liftToEncoding, liftToJSON)
import Data.Aeson.TH (deriveJSON, mkLiftParseJSON, defaultOptions, mkParseJSON, mkToEncoding, mkToJSON, mkLiftToEncoding, mkLiftToJSON)
import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.Foldable (null, traverse_, foldr')
import Control.Category ((.))
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Functor.Classes

import Control.Monad.Writer.CPS (Writer, runWriter, tell) -- because strict writer isn't strict enough
import Control.Monad.Writer.CPS (WriterT, runWriterT) -- because strict writer isn't strict enough


-- import GHC.Generics
-- import Data.Typeable
import Data.Functor.Compose

import Data.Semigroup
  ( First(..)
  , Option(..)
  , Semigroup, (<>)
  )

import Reflex.FunctorMaybe -- how bout Data.Witherable?
import Data.AppendMap()
import Reflex.Aeson.Orphans () -- more orphans?
import Data.Monoid(All(..))
import Data.Functor.Const(Const(..))
import Data.Constraint
import Control.Lens.Indexed (FunctorWithIndex, imap)
import Control.Lens.Indexed (FoldableWithIndex)
import Control.Lens.Indexed (TraversableWithIndex, itraverse, ifor)
import qualified Common.AppendIntervalMap as IMap
-- import qualified Data.IntervalMap.Generic.Lazy as IntervalMap
import Common.AppendIntervalMap (AppendIntervalMap, ClosedInterval(..), WithInfinity(..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Common.WrappedShow1
import Data.Reflection (Reifies)
import Data.Proxy(Proxy)
import Unsafe.Coerce (unsafeCoerce)

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

mergeMapA :: forall f k a b c. (Applicative f, Ord k)
  => (k -> a -> f (Maybe c))
  -> (k -> b -> f (Maybe c))
  -> (k -> a -> b -> f (Maybe c))
  -> Map.Map k a -> Map.Map k b -> f (Map.Map k c)
mergeMapA fx fy fxy mxs mys = Map.fromAscList <$> go (Map.toAscList mxs) (Map.toAscList mys)
  where
    step :: k -> Maybe c -> [(k, c)] -> [(k, c)]
    step _ Nothing = id
    step k (Just x) = ((k, x):)
    go :: [(k, a)] -> [(k, b)] -> f [(k, c)]
    go ((k1, x):xs) ((k2, y):ys)
      | k1 < k2 = step k1 <$> (fx k1 x) <*> go xs ((k2, y): ys)
      | k1 == k2 = step k1 <$> (fxy k1 x y) <*> go xs ys
      | otherwise = step k2 <$> (fy k1 y) <*> go ((k1, x):xs) ys
    go [] ((k, y):ys) = step k <$> (fy k y) <*> go [] ys
    go ((k, x):xs) [] = step k <$> (fx k x) <*> go xs []
    go [] [] = pure []

instance Ord k => Eq1 (Map.Map k) where
  liftEq f xs ys = getAll $ getConst $ mergeMapA
    (\_ _ -> Const $ All False)
    (\_ _ -> Const $ All False)
    (\_ x y -> Const $ All $ f x y)
    xs ys

instance Ord k => Ord1 (Map.Map k) where
  liftCompare f xs ys = getConst $ mergeMapA
    (\_ _ -> Const $ LT)
    (\_ _ -> Const $ GT)
    (\_ x y -> Const $ f x y)
    xs ys

deriving instance (Ord k) => Eq1 (MMap.MonoidalMap k)
deriving instance (Ord k) => Ord1 (MMap.MonoidalMap k)
deriving instance (Ord k, FromJSONKey k) => FromJSON1 (MMap.MonoidalMap k)
deriving instance (Ord k, ToJSONKey k) => ToJSON1 (MMap.MonoidalMap k)

-- | this is the parametric replacement for *crop*.
chop :: (Semigroup a, ViewSelector t) => (a -> b -> (Maybe c)) -> t a -> View t b -> View t c
chop f vs = iMapMaybe $ \i b -> maybe Nothing (flip f b) $ lookup i vs

cropView :: (Semigroup a, ViewSelector t) => t a -> View t b -> View t a
cropView vs = iMapMaybe $ \i _ -> lookup i vs

-- horray, orphans!
deriving instance FunctorMaybe Option
-- instance FunctorMaybe (MonoidalMap k) where
--   fmapMaybe = MMap.mapMaybe

class ( TraversableWithIndex (ViewIndex f) (View f)
      , FunctorMaybe (View f)
      ) => ViewSelector f where
  data View f :: * -> *
  type ViewIndex f

  -- we could do this with QuantifiedConstraints, in 8.6
  viewIsSemigroup         :: Semigroup a :- Semigroup (View f a)
  viewIsMonoid            :: Semigroup a :- Monoid (View f a)
  viewSelectorIsSemigroup :: Semigroup a :- Semigroup (f a)

  lookup :: Semigroup a => ViewIndex f -> f a -> Maybe a

type ComposeSelector = Compose

viewCompose :: f (g a) -> ComposeSelector f g a
viewCompose = Compose

type ComposeView f g a = View (Compose f g) a


instance
  ( ViewSelector f, Eq1 (View f)
  , ViewSelector g, Eq1 (View g)
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

-- deriving instance
--   ( ViewSelector f, ToJSON1 (View f)
--   , ViewSelector g, ToJSON1 (View g)
--   , ToJSON a) => ToJSON (View (Compose f g) a)
-- 
-- deriving instance
--   ( ViewSelector f, FromJSON1 (View f)
--   , ViewSelector g, FromJSON1 (View g)
--   , FromJSON a) => FromJSON (View (Compose f g) a)

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
  ComposeView uxs (Compose lxs) <> ComposeView uys (Compose lys) = (ComposeView (uxs <> uys) (Compose (lxs <> lys))
    \\ (viewIsSemigroup :: Semigroup a :- Semigroup (View f a))
    \\ (viewIsSemigroup :: Semigroup a :- Semigroup (View g a))
    )

instance (Ord (ViewIndex f), ViewSelector f, ViewSelector g, Semigroup a) => Monoid (View (Compose f g) a) where
  mappend = (<>)
  mempty = (ComposeView mempty (Compose mempty)
     \\ (viewIsMonoid :: Semigroup a :- Monoid (View f a))
     \\ (viewIsSemigroup :: Semigroup a :- Semigroup (View g a))
     )


deriving instance (Functor (View v), Functor (View w)) => Functor (View (Compose v w))
deriving instance (Foldable (View v), Foldable (View w)) => Foldable (View (Compose v w))
deriving instance (Traversable (View v), Traversable (View w)) => Traversable (View (Compose v w))

-- chop out whole subtrees of the outer view when the inner view is
-- `Foldable.null`  That way we can trim out query responses that are not
-- supposed to be visible to the caller.  For now though, the default instance
-- is nearly right.
instance (ViewSelector v, ViewSelector w, Ord (ViewIndex v))
    => FunctorMaybe (View (Compose v w)) where
  fmapMaybe :: forall a b. (a -> Maybe b) -> View (Compose v w) a -> View (Compose v w) b
  fmapMaybe f (ComposeView upper (Compose lower)) = ComposeView (catMaybes upper') (Compose $ MMap.MonoidalMap $  lower')
    where
      swizzle :: ViewIndex v -> a -> Writer (Map.Map (ViewIndex v) (View w b)) (Maybe b)
      swizzle i x = case f x of
        Nothing -> return Nothing
        Just y -> do
          traverse_  (tell . Map.singleton i . fmapMaybe f) (MMap.lookup i lower)
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
  -- itraverse :: 
  itraverse :: forall f a b. Applicative f => ((ViewIndex v, ViewIndex w) -> a -> f b) -> View (Compose v w) a -> f (View (Compose v w) b)
  itraverse f (ComposeView upper lower) = ComposeView <$> upper' <*> lower'
    where
      lower' :: f (Compose (MonoidalMap (ViewIndex v)) (View w) b)
      lower' = itraverse f lower

      upper' :: f (View v b)
      upper' = iWither witherUpper upper

      witherUpper :: ViewIndex v -> a -> f (Maybe b)
      witherUpper i x = maybe (pure Nothing) (sequenceA . fmap getFirst . getOption . getConst . itraverse (\j _ -> Const $ Option $ Just $ First $ f (i,j) x)) $ MMap.lookup i $ getCompose lower

-- | add to reflex and/or use Data.Witherable.Filterable
catMaybes :: FunctorMaybe f => f (Maybe a) -> f a
catMaybes = fmapMaybe id
{-# INLINE catMaybes #-}


iMapMaybe :: (FunctorWithIndex i t, FunctorMaybe t) => (i -> a -> Maybe b) -> t a -> t b
iMapMaybe f = catMaybes . imap f

iWither :: (TraversableWithIndex i t, FunctorMaybe t, Applicative f) => (i -> a -> f (Maybe b)) -> t a -> f (t b)
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
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable, Monoid, Semigroup, ToJSON, ToJSON1, FromJSON, FromJSON1)

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

instance FunctorMaybe (View (MaybeSelector v)) where
  fmapMaybe f = MaybeView . fmapMaybe (traverse f) . unSingle

-- instance Witherable (View (MaybeSelector v))

instance FunctorWithIndex () (View (MaybeSelector v))
instance FoldableWithIndex () (View (MaybeSelector v))
instance TraversableWithIndex () (View (MaybeSelector v)) where
  itraverse f = traverse $ f ()


newtype MapSelector k (v :: *) a = MapSelector { unMapSelector :: MonoidalMap k a }
  deriving (Eq, Ord, Functor, Foldable, Traversable, Semigroup)

instance Ord k => ViewSelector (MapSelector k v) where
  newtype View (MapSelector k v) a = MapView { unMapView :: MonoidalMap k (First v, a) }
    deriving
      ( Show, Read, Functor, Eq, Ord -- , NFData
      , Foldable, Traversable
      -- , Data, Typeable
      -- , Ixed, At, Each, Newtype, IsList
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

instance FunctorMaybe (View (MapSelector k v)) where
  fmapMaybe f = MapView . fmapMaybe (traverse f) . unMapView

-- instance Witherable (View (MapSelector k v))

instance FunctorWithIndex k (View (MapSelector k v))
instance FoldableWithIndex k (View (MapSelector k v))
instance TraversableWithIndex k (View (MapSelector k v)) where

  itraverse :: forall f a b. Applicative f => (k -> a -> f b) -> View (MapSelector k v) a -> f (View (MapSelector k v) b)
  itraverse f = fmap MapView . itraverse f' . unMapView
    where
      f' :: k -> (First v, a) -> f (First v, b)
      f' k (x, y) = (x,) <$> f k y

newtype IntervalSelector e (v :: *) a = IntervalSelector
  { unIntervalSelector :: (AppendIntervalMap (ClosedInterval (WithInfinity e))) a }
  deriving (Eq, Ord, Eq1, Ord1, Show, Functor, Foldable, Traversable, Monoid, Semigroup, FromJSON, FromJSON1, ToJSON, ToJSON1)

viewInterval :: Ord e => (e, e) -> a -> IntervalSelector e v a
viewInterval (lb, ub) = IntervalSelector . IMap.singleton (ClosedInterval (Bounded lb) (Bounded ub))
type IntervalView e v = View (IntervalSelector e v)

instance (Ord v, Ord e) => ViewSelector (IntervalSelector e v) where
  newtype View (IntervalSelector e v) a = IntervalView
    { unIntervalView :: AppendIntervalMap (ClosedInterval e) (Set v, a)
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Monoid, Semigroup, ToJSON, FromJSON)
  type ViewIndex (IntervalSelector e v) = ClosedInterval e

  viewIsMonoid = Sub Dict
  viewIsSemigroup = Sub Dict
  viewSelectorIsSemigroup  = Sub Dict

  lookup k (IntervalSelector xs) = getOption $ foldMap (Option . Just) $ IMap.intersecting xs (Bounded <$> k)

getIntervalView :: View(IntervalSelector e v) a -> AppendIntervalMap (ClosedInterval e) (Set v)
getIntervalView = fmap fst . unIntervalView

instance (Eq v, Eq e) => Eq1 (View (IntervalSelector e v)) where
  liftEq f (IntervalView xs) (IntervalView ys) = liftEq f' xs ys
    where
      f' (v1, x) (v2, y) = v1 == v2 && f x y

instance (Ord v, Ord e) => Ord1 (View (IntervalSelector e v)) where
  liftCompare f (IntervalView xs) (IntervalView ys) = liftCompare f' xs ys
    where
      f' (v1, x) (v2, y) = compare v1 v2 <> f x y

instance (Ord e, Ord v) => FunctorMaybe (View (IntervalSelector e v)) where
  fmapMaybe f = IntervalView . fmapMaybe f' . unIntervalView
    where
      f' (vs, a) =
        if Set.null vs
          then Nothing
          else (vs,) <$> f a

instance FunctorWithIndex (ClosedInterval e) (View (IntervalSelector e v))
instance FoldableWithIndex (ClosedInterval e) (View (IntervalSelector e v))

instance TraversableWithIndex (ClosedInterval e) (View (IntervalSelector e v)) where
  itraverse :: forall f a b. Applicative f => ((ClosedInterval e) -> a -> f b) -> View (IntervalSelector e v) a -> f (View (IntervalSelector e v) b)
  itraverse f = fmap IntervalView . itraverse f' . unIntervalView
    where
      f' :: ClosedInterval e -> (Set v, a) -> f (Set v, b)
      f' k (x, y) = (x,) <$> f k y


newtype RangeSelector e (v :: *) a = RangeSelector
  { unRangeSelector :: (AppendIntervalMap (ClosedInterval (WithInfinity e))) a }
  deriving
    ( Eq, Eq1
    , Ord, Ord1
    , Show
    , Functor, Foldable, Traversable
    , Monoid, Semigroup
    , FromJSON, FromJSON1
    , ToJSON, ToJSON1)

viewRangeAll :: Ord e => a -> RangeSelector e v a
viewRangeAll = RangeSelector . IMap.singleton (ClosedInterval LowerInfinity UpperInfinity)

viewRangeExactly :: Ord e => e -> a -> RangeSelector e v a
viewRangeExactly k = RangeSelector . IMap.singleton (ClosedInterval (Bounded k) (Bounded k))

viewRangeSet :: Ord e => Set e -> a -> RangeSelector e v a
viewRangeSet ks a = RangeSelector $ IMap.fromSet (const a) (Set.mapMonotonic eqK ks)
  where
    eqK k = ClosedInterval (Bounded k) (Bounded k)

type RangeView e v = View (RangeSelector e v)

instance (Ord v, Ord e) => ViewSelector (RangeSelector e v) where
  data View (RangeSelector e v) a = RangeView
    { _rangeView_support :: AppendIntervalMap (ClosedInterval e) a
    , _rangeView_points :: MonoidalMap e v
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  type ViewIndex (RangeSelector e v) = e

  lookup k (RangeSelector xs) = getOption $ foldMap (Option . Just) $ IMap.containing xs (Bounded k)

  viewIsMonoid = Sub Dict
  viewIsSemigroup = Sub Dict
  viewSelectorIsSemigroup = Sub Dict

getRangeView :: View (RangeSelector e v) a -> MonoidalMap e v
getRangeView = _rangeView_points

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
  mappend = (<>)
  mempty = RangeView mempty MMap.empty

instance (Ord e) => FunctorMaybe (View (RangeSelector e v)) where
  fmapMaybe f (RangeView i xs) = RangeView i' $ iMapMaybe inI xs
    where
      i' = fmapMaybe f i
      inI k v = if null $ IMap.containing i' k then Nothing else Just v

instance Ord e => FunctorWithIndex e (View (RangeSelector e v))
instance Ord e => FoldableWithIndex e (View (RangeSelector e v))
instance Ord e => TraversableWithIndex e (View (RangeSelector e v)) where
  itraverse f (RangeView i xs) = RangeView <$> itraverse _f i <*> pure xs -- xs <$> iWither _f i

-- more orphans!

deriveShow1Methods [d|instance (Show v, Show e) => Show1 (View (RangeSelector e v))|]
deriveShow1Methods [d|instance (        Show e) => Show1       (RangeSelector e v) |]
deriveShow1Methods [d|instance (Show v, Show e) => Show1 (View (IntervalSelector e v))|]
deriveShow1Methods [d|instance (        Show e) => Show1       (IntervalSelector e v) |]
deriveShow1Methods [d|instance (Show v) => Show1 (View (MaybeSelector v))|]
deriveShow1Methods [d|instance             Show1       (MaybeSelector v) |]

deriveShow1Methods [d|instance (Show k) => Show1       (MMap.MonoidalMap k) |]

instance (FromJSON k, Ord k, FromJSON v, Ord v) => FromJSON1 (View (IntervalSelector k v)) where
  liftParseJSON = $(mkLiftParseJSON defaultOptions 'IntervalView)
instance (ToJSON k, Ord k, ToJSON v, Ord v) => ToJSON1 (View (IntervalSelector k v)) where
  liftToJSON = $(mkLiftToJSON defaultOptions 'IntervalView)
  liftToEncoding = $(mkLiftToEncoding defaultOptions 'IntervalView)


instance (FromJSON k, Ord k, FromJSON v) => FromJSON1 (View (RangeSelector k v)) where
  liftParseJSON = $(mkLiftParseJSON defaultOptions 'RangeView)
instance (Ord k, Semigroup a, FromJSON k, FromJSON a, FromJSON v) => FromJSON (View (RangeSelector k v) a) where
  parseJSON = $(mkParseJSON defaultOptions 'RangeView)

instance (ToJSON k, Ord k, ToJSON v) => ToJSON1 (View (RangeSelector k v)) where
  liftToEncoding = $(mkLiftToEncoding defaultOptions 'RangeView)
  liftToJSON = $(mkLiftToJSON defaultOptions 'RangeView)
instance (Ord k, Semigroup a, ToJSON k, ToJSON a, ToJSON v) => ToJSON (View (RangeSelector k v) a) where
  toEncoding = $(mkToEncoding defaultOptions 'RangeView)
  toJSON = $(mkToJSON defaultOptions 'RangeView)

instance (FromJSON (View v a), FromJSON a, FromJSON1 (View w), ViewSelector v, ViewSelector w, Ord (ViewIndex v), FromJSONKey (ViewIndex v)) => FromJSON (View (Compose v w) a) where
  parseJSON = $(mkParseJSON defaultOptions 'ComposeView)

instance (ToJSON (View v a), ToJSON a, ToJSON1 (View w), ToJSONKey (ViewIndex v), Ord(ViewIndex v), ViewSelector v, ViewSelector w) => ToJSON (View (Compose v w) a) where
  toEncoding = $(mkToEncoding defaultOptions 'ComposeView)
  toJSON = $(mkToJSON defaultOptions 'ComposeView)
