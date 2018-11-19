{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LCA.Online.Monoidal
-- Copyright   :  (C) 2012-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides online calculation of the the lowest common ancestor in /O(log h)/
-- by compressing the spine of the paths using a skew-binary random access
-- list.
--
-- This library implements the technique described in my talk
--
-- <http://www.slideshare.net/ekmett/skewbinary-online-lowest-common-ancestor-search>
--
-- to improve the known asymptotic bounds on both online lowest common ancestor search
--
-- <http://en.wikipedia.org/wiki/Lowest_common_ancestor>
--
-- and the online level ancestor problem:
--
-- <http://en.wikipedia.org/wiki/Level_ancestor_problem>
--
-- Algorithms used here assume that the key values chosen for @k@ are
-- globally unique.
--
-- This version provides access to a monoidal \"summary\" of the
-- elided path for many operations.
--
----------------------------------------------------------------------------
module Data.LCA.Online.Polymorphic
  ( Path
  , toList, fromList
  , map, mapHom, mapWithKey
  , traverseP, traverseWithKey
  , empty
  , cons
  , uncons, view
  , null
  , length
  , measure
  , isAncestorOf
  , keep, mkeep
  , drop, mdrop
  , (~=)
  , lca, mlca
  , View(..)
  , nearest
  , graft
  , concatPaths
  ) where

import Control.Applicative hiding (empty)
import Control.DeepSeq (NFData)
import Data.Foldable hiding (toList)
import GHC.Generics (Generic)

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (Monoid (..))
#endif

import Data.Function (on)
import Safe.Foldable (maximumByMay)

import Prelude hiding
  ( drop
  , map
#if __GLASGOW_HASKELL__ < 710
  , length
  , null
#endif
#if MIN_VERSION_base(4,11,0)
  , (<>)
#endif
  )

infixl 6 <>
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
{-# INLINE (<>) #-}

-- | Complete binary trees
--
-- NB: we could ensure the complete tree invariant
data Tree k a
  = Bin a !k a (Tree k a) (Tree k a)
  | Tip !k a
  deriving (Show, Read, Generic)
instance (NFData k, NFData a) => NFData (Tree k a)

instance Foldable (Tree k) where
  foldMap f (Tip _ a) = f a
  foldMap f (Bin _ _ a l r) = f a <> foldMap f l <> foldMap f r

measureT :: Tree k a -> a
measureT (Tip _ a)       = a
measureT (Bin a _ _ _ _) = a

bin :: Monoid a => k -> a -> Tree k a -> Tree k a -> Tree k a
bin n a l r = Bin (a <> measureT l <> measureT r) n a l r

sameT :: Eq k => Tree k a -> Tree k b -> Bool
sameT xs ys = root xs == root ys where
  root (Tip k _)     = k
  root (Bin _ k _ _ _) = k

-- | A compressed 'Path' as a skew binary random access list
data Path k a
  = Nil
  | Cons a
         {-# UNPACK #-} !Int -- the number of elements @n@ in this entire skew list
         {-# UNPACK #-} !Int -- the number of elements @w@ in this binary tree node
         (Tree k a)          -- a complete binary tree @t@ of with @w@ elements
         (Path k a)          -- @n - w@ elements in a linked list @ts@, of complete trees in ascending order by size
  deriving (Show, Read, Generic)
instance (NFData k, NFData a) => NFData (Path k a)

instance Foldable (Path k) where
  foldMap _ Nil = mempty
  foldMap f (Cons _ _ _ t ts) = foldMap f t <> foldMap f ts
#if __GLASGOW_HASKELL__ >= 710
  length Nil = 0
  length (Cons _ n _ _ _) = n
  {-# INLINE length #-}

  null Nil = True
  null _ = False
  {-# INLINE null #-}
#else

-- | /O(1)/ Determine the 'length' of a 'Path'.
length :: Path k a -> Int
length Nil = 0
length (Cons _ n _ _ _) = n
{-# INLINE length #-}

-- | /O(1)/ Returns 'True' iff the path is 'empty'.
null :: Path k a -> Bool
null Nil = True
null _ = False
{-# INLINE null #-}
#endif

-- | Extract a monoidal summary of a 'Path'.
measure :: Monoid a => Path k a -> a
measure Nil = mempty
measure (Cons a _ _ _ _) = a

consT :: Monoid a => Int -> Tree k a -> Path k a -> Path k a
consT w t ts = Cons (measureT t <> measure ts) (w + length ts) w t ts

consN :: Monoid a => Int -> Int -> Tree k a -> Path k a -> Path k a
consN n w t ts = Cons (measureT t <> measure ts) n w t ts

-- | /O(n)/ Re-annotate a 'Path' full of monoidal values using a different 'Monoid'.
map :: Monoid b => (a -> b) -> Path k a -> Path k b
map f = go where
  go Nil = Nil
  go (Cons _ n k t ts) = consN n k (goT t) (go ts)
  goT (Tip k a) = Tip k (f a)
  goT (Bin _ k a l r) = bin k (f a) (goT l) (goT r)
{-# INLINE map #-}

-- | /O(n)/ Re-annotate a 'Path' full of monoidal values with access to the key.
mapWithKey :: Monoid b => (k -> a -> b) -> Path k a -> Path k b
mapWithKey f = go where
  go Nil = Nil
  go (Cons _ n k t ts) = consN n k (goT t) (go ts)
  goT (Tip k a) = Tip k (f k a)
  goT (Bin _ k a l r) = bin k (f k a) (goT l) (goT r)
{-# INLINE mapWithKey #-}

-- | /O(n)/ Re-annotate a 'Path' full of monoidal values/
--
-- Unlike 'map', @'mapHom' f@ assumes that @f@ is a 'Monoid' homomorphism, that is to say you must ensure
--
-- @
-- f a `'mappend'` f b = f (a `'mappend'` b)
-- f 'mempty' = 'mempty'
-- @
mapHom :: (a -> b) -> Path k a -> Path k b
mapHom f = go where
  go Nil               = Nil
  go (Cons a n k t ts) = Cons (f a) n k (goT t) (go ts)
  goT (Tip k a)        = Tip k (f a)
  goT (Bin m k a l r)  = Bin (f m) k (f a) (goT l) (goT r)
{-# INLINE mapHom #-}

-- | Convert a 'Path' to a list of @(ID, value)@ pairs.
toList :: Path k a -> [(k,a)]
toList Nil = []
toList (Cons _ _ _ t ts)  = go t (toList ts) where
  go (Tip k a) xs       = (k,a) : xs
  go (Bin _ k a l r) xs = (k,a) : go l (go r xs)

-- | Build a 'Path' from a list of @(ID, value)@ pairs.
fromList :: Monoid a => [(k,a)] -> Path k a
fromList [] = Nil
fromList ((k,a):xs) = cons k a (fromList xs)

-- | Traverse a 'Path' with access to the node IDs.
traverseWithKey :: (Applicative f, Monoid b) => (k -> a -> f b) -> Path k a -> f (Path k b)
traverseWithKey f = go where
  go Nil = pure Nil
  go (Cons _ n k t ts) = consN n k <$> goT t <*> go ts
  goT (Tip k a)        = Tip k <$> f k a
  goT (Bin _ k a l r)  = bin k <$> f k a <*> goT l <*> goT r
{-# INLINE traverseWithKey #-}

-- | Traverse a 'Path' yielding a new monoidal annotation.
traverseP :: (Applicative f, Monoid b) => (a -> f b) -> Path k a -> f (Path k b)
traverseP f = go where
  go Nil = pure Nil
  go (Cons _ n k t ts) = consN n k <$> goT t <*> go ts
  goT (Tip k a)        = Tip k <$> f a
  goT (Bin _ k a l r)  = bin k <$> f a <*> goT l <*> goT r
{-# INLINE traverseP #-}

-- | The empty 'Path'
empty :: Path k a
empty = Nil
{-# INLINE empty #-}

-- | /O(1)/ Invariant: most operations assume that the keys @k@ are globally unique
--
-- Extend the 'Path' with a new node ID and value.
cons :: Monoid a => k -> a -> Path k a -> Path k a
cons k a (Cons m n w t (Cons _ _ w' t2 ts)) | w == w' = Cons (a <> m) (n + 1) (2 * w + 1) (bin k a t t2) ts
cons k a ts = Cons (a <> measure ts) (length ts + 1) 1 (Tip k a) ts
{-# INLINE cons #-}

-- | /O(1)/ Extract the node ID and value from the newest node on the 'Path'.
uncons :: Monoid a => Path k a -> Maybe (k, a, Path k a)
uncons Nil = Nothing
uncons (Cons _ _ _ (Tip k a) ts) = Just (k, a, ts)
uncons (Cons _ _ w (Bin _ k a l r) ts) = Just (k, a, consT w2 l (consT w2 r ts)) where w2 = div w 2
{-# INLINE uncons #-}

-- | /O(1)/ Extract the node ID and value from the newest node on the 'Path', slightly faster than 'uncons'.
view :: Monoid a => Path k a -> View k Path a
view Nil = Root
view (Cons _ _ _ (Tip k a) ts) = Node k a ts
view (Cons _ _ w (Bin _ k a l r) ts) = Node k a (consT w2 l (consT w2 r ts)) where w2 = div w 2
{-# INLINE view #-}

-- | /O(log (h - k))/ to keep @k@ elements of 'Path' of 'length' @h@, and provide a monoidal summary of the dropped elements
-- using a supplied monoid homomorphism.
--
mkeep :: (Monoid a, Monoid b) => (a -> b) -> Int -> Path k a -> (b, Path k a)
mkeep f = go mempty where
  go as _ Nil = (as, Nil)
  go as k xs@(Cons _ n w t ts)
    | k >= n    = (as, xs)
    | otherwise = case compare k (n - w) of
      GT -> goT as (k - n + w) w t ts
      EQ -> (as <> f (measureT t), ts)
      LT -> go (as <> f (measureT t)) k ts
  -- goT :: Monoid a => Int -> Int -> Tree k a -> Path k a -> Path k a
  goT as n w (Bin _ _ a l r) ts = case compare n w2 of
    LT              -> goT (as <> f a <> f (measureT l)) n w2 r ts
    EQ              -> (as <> f a <> f (measureT l), consT w2 r ts)
    GT | n == w - 1 -> (as <> f a, consT w2 l (consT w2 r ts))
       | otherwise  -> goT (as <> f a) (n - w2) w2 l (consT w2 r ts)
    where w2 = div w 2
  goT as _ _ _ ts = (as, ts)
{-# INLINE mkeep #-}

-- | /O(log (h - k))/ to @'keep' k@ elements of 'Path' of 'length' @h@
--
-- This solves the online version of the \"level ancestor problem\" with no preprocessing in /O(log h)/ time,
-- improving known complexity bounds.
--
-- <http://en.wikipedia.org/wiki/Level_ancestor_problem>
keep :: Monoid a => Int -> Path k a -> Path k a
keep k xs = snd (mkeep (\_ -> ()) k xs)
{-# INLINE keep #-}

-- | /O(log k)/ to @'drop' k@ elements from a 'Path'
drop :: Monoid a => Int -> Path k a -> Path k a
drop k xs = snd (mdrop (\_ -> ()) k xs)
{-# INLINE drop #-}

-- | /O(log k)/ to drop @k@ elements from a 'Path' and provide a monoidal summary of the dropped elements
-- using a suplied monoid homomorphism
mdrop :: (Monoid a, Monoid b) => (a -> b) -> Int -> Path k a -> (b, Path k a)
mdrop f k xs = mkeep f (length xs - k) xs
{-# INLINE mdrop #-}

-- | /O(log h)/ @xs `'isAncestorOf'` ys@ holds when @xs@ is a prefix starting at the root of path @ys@.
isAncestorOf :: (Monoid b, Eq k) => Path k a -> Path k b -> Bool
isAncestorOf xs ys = xs ~= keep (length xs) ys

infix 4 ~=
-- | /O(1)/ Compare to see if two trees have the same leaf key
(~=) :: Eq k => Path k a -> Path k b -> Bool
Nil            ~= Nil            = True
Cons _ _ _ s _ ~= Cons _ _ _ t _ = sameT s t
_              ~= _              = False

-- $doctest
-- >>> let fromList' = fromList . fmap (flip (,) ())
-- >>> length (lca (fromList' [1, 2, 3, 4, 5, 6]) (fromList' [7, 8, 3, 4, 5, 6]))
-- 4

-- | /O(log h)/ Compute the lowest common ancestor of two paths
lca :: (Monoid a, Monoid b, Eq k) => Path k a -> Path k b -> Path k a
lca xs ys = zs where (_, zs, _, _) = mlca (\_ -> ()) (\_ -> ()) xs ys

-- | /O(log h)/ Compute the lowest common ancestor of two paths along with a monoidal summary of their respective tails using
-- the supplied monoid homomorphisms.
mlca :: (Monoid a, Monoid b, Monoid c, Monoid d, Eq k) => (a -> c) -> (b -> d) -> Path k a -> Path k b -> (c, Path k a, d, Path k b)
mlca f g xs0 ys0 = case compare nxs nys of
  LT -> let (bs, ys) = mkeep g nxs ys0 in go mempty bs xs0 ys
  EQ -> go mempty mempty xs0 ys0
  GT -> let (as, xs) = mkeep f nys xs0 in go as mempty xs ys0
  where
    nxs = length xs0
    nys = length ys0

    go as bs pa@(Cons _ _ w x xs) pb@(Cons _ _ _ y ys)
      | sameT x y = (as, pa, bs, pb)
      | xs ~= ys  = goT as bs w x y xs ys
      | otherwise = go (as <> f (measureT x)) (bs <> g (measureT y)) xs ys
    go as bs _ _ = (as, Nil, bs, Nil)

    goT as bs w (Bin _ _ a la ra) (Bin _ _ b lb rb) pa pb
      | sameT la lb = (as <> f a, consT w2 la (consT w2 ra pa), bs <> g b, consT w2 lb (consT w2 rb pb))
      | sameT ra rb = goT (as <> f a) (bs <> g b) w2 la lb (consT w2 ra pa) (consT w2 rb pb)
      | otherwise   = goT (as <> f a <> f (measureT la)) (bs <> g b <> g (measureT lb)) w2 ra rb pa pb
      where w2 = div w 2
    goT as bs _ ta tb pa pb = (as <> f (measureT ta), pa, bs <> g (measureT tb), pb)
{-# INLINE mlca #-}

nearest
  :: (Foldable f, Monoid a, Monoid b, Eq k)
  => Path k b -> f (Path k a) -> Maybe (Path k a)
nearest x = maximumByMay (compare `on` lal x)
  where
    -- least ancestor level.  the inverse distance between leaves.
    lal :: (Monoid a, Monoid b, Eq k) => Path k b -> Path k a -> Int
    lal y z = length $ lca z y

-- restore sharing between paths by taking the part of branch not on trunk.
-- TODO: i'm sure that this can be made O(log h) by retaining subtrees of the path above the lca
graft :: (Monoid a, Eq k) => Path k a -> Path k a -> Path k a
graft trunk branch = concatPaths common leaves
  where
    common = lca trunk branch
    leaves = keep (length branch - length common) branch

concatPaths :: (Monoid a, Eq k) => Path k a -> Path k a -> Path k a
concatPaths trunk = foldr (uncurry cons) trunk . toList

-- | Provides a consistent 'View' for peeling off the bottom node of a path.
data View k f a
  = Root
  | Node  !k a (f k a)
  deriving (Eq,Ord,Read,Show)

instance Functor (f k) => Functor (View k f) where
  fmap _ Root = Root
  fmap f (Node k a as) = Node k (f a) (fmap f as)

instance Foldable (f k) => Foldable (View k f) where
  foldMap _ Root = mempty
  foldMap f (Node _ a as) = f a `mappend` foldMap f as

instance Traversable (f k) => Traversable (View k f) where
  traverse _ Root = pure Root
  traverse f (Node k a as) = Node k <$> f a <*> traverse f as
