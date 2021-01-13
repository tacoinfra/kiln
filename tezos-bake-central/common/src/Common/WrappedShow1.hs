{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PackageImports #-}

{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-orphans #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Common.WrappedShow1 where

import Data.Functor.Classes
import Data.Reflection
import Data.Coerce
import Data.Proxy
import Unsafe.Coerce

import qualified "template-haskell" Language.Haskell.TH as TH

-- | short example
-- data YourThing f b
--   = Bin (f b) b (f b)
--   | Tip
--   deriving Show
-- deriveShow1 =<< [t|YourThing []|]

newtype WrappedShow1 f a = WrappedShow1 {unwrappedShow1 :: f a}


instance Show1 f => Show1 (WrappedShow1 f) where
  liftShowsPrec showsPrecFn showListFn prec = liftShowsPrec showsPrecFn showListFn prec . unwrappedShow1
  liftShowList showsPrecFn showListFn = liftShowList showsPrecFn showListFn . fmap unwrappedShow1

instance (Show1 f, Show a) => Show (WrappedShow1 f a) where
  showsPrec x = liftShowsPrec showsPrec showList x
  showList x = liftShowList showsPrec showList x

type ShowsPrec a = Int -> a -> ShowS
type ShowList a = [a] -> ShowS

data ReifiedShow a = ReifiedShow
  { reifiedShowsPrec :: ShowsPrec a
  , reifiedShowList :: ShowList a
  }

instance Reifies s (ReifiedShow a) => Show (ReflectedShow a s) where
  showsPrec prec p@(ReflectedShow x) = reifiedShowsPrec (reflect p) prec x
  showList xs = reifiedShowList (reflect (head xs)) (coerce xs)

newtype ReflectedShow a s = ReflectedShow a

unreflectedShow :: ReflectedShow a s -> proxy s -> a
unreflectedShow (ReflectedShow a) _ = a

-- instance Reifies s (ReifiedShow a) => Show (ReflectedShow a s) where
--   showsPrec prec p@(ReflectedShow x) = reifiedShowsPrec (reflect p) prec x
--   showList xs = reifiedShowList (reflect (head xs)) (coerce xs)

-- data ReifiedFromJSON a = ReifiedFromJSON
--   { reifiedParseJSON :: ParseJSON a
--   , reifiedParseJSONLIst :: ParseJSONList a
--   }


-- reify :: forall a r. a -> (forall (s :: *). Reifies s a => Proxy s -> r) -> r
reifyShow
  :: forall a t. ShowsPrec a -> ShowList a
  -> (forall (s :: *). Reifies s (ReifiedShow a) => Proxy s -> t)
  -> t
reifyShow f z m = reify (ReifiedShow f z) m


-- reifyShowsPrec :: forall a. (ShowsPrec a) -> (ShowList a) -> Int -> YourThing [] a -> ShowS
-- liftShowsPrec x y = reifyShow x y f
--   where
--   f :: forall (s :: *). Reifies s (ReifiedShow a) => Proxy s -> Int -> YourThing [] a -> ShowS
--   f _ = (unsafeCoerce :: (Int -> YourThing [] (ReflectedShow a s) -> ShowS) -> (Int -> YourThing [] a -> ShowS)) showsPrec


type LiftShowsPrec a f = ShowsPrec a -> ShowList a -> ShowsPrec (f a)
type ReifyShowsPrec s a f = Reifies s (ReifiedShow a) => Proxy s -> ShowsPrec (f a)
type CoerceShowsPrec s a f = ShowsPrec (f (ReflectedShow a s)) -> ShowsPrec (f a)

type LiftShowList a f = ShowsPrec a -> ShowList a -> ShowList (f a)
type ReifyShowList s a f = Reifies s (ReifiedShow a) => Proxy s -> ShowList (f a)
type CoerceShowList s a f = ShowList (f (ReflectedShow a s)) -> ShowList (f a)


renameDecl :: TH.Name -> TH.Dec -> TH.Dec
renameDecl x (TH.SigD _ ty) = TH.SigD x ty
renameDecl x (TH.FunD _ clauses) = TH.FunD x clauses
renameDecl _ other = other

deriveShow1Methods :: TH.DecsQ -> TH.DecsQ
deriveShow1Methods tyQ = do
  [TH.InstanceD overlap cxt ty0@(TH.AppT _ ty) decs] <- tyQ
  a <- TH.newName "a"
  s <- TH.newName "s"
  derivedLiftShowsPrec <- [d|
      liftShowsPrec :: $(TH.forallT [TH.PlainTV a] (pure []) [t|LiftShowsPrec $(TH.varT a) $(pure ty)|])
      liftShowsPrec x y = reifyShow x y f
        where
        f :: $(TH.forallT [TH.KindedTV s TH.StarT]
                (fmap pure [t|Reifies $(TH.varT s) (ReifiedShow $(TH.varT a))|])
                [t|Proxy $(TH.varT s) -> ShowsPrec ($(pure ty) $(TH.varT a))|])
        f _ = (unsafeCoerce :: CoerceShowsPrec $(TH.varT s) $(TH.varT a) $(pure ty)) showsPrec
    |]
  derivedLiftShowList <- [d|
      liftShowList :: $(TH.forallT [TH.PlainTV a] (pure []) [t|LiftShowList $(TH.varT a) $(pure ty)|])
      liftShowList x y = reifyShow x y f
        where
        f :: $(TH.forallT [TH.KindedTV s TH.StarT]
                (fmap pure [t|Reifies $(TH.varT s) (ReifiedShow $(TH.varT a))|])
                [t|Proxy $(TH.varT s) -> ShowList ($(pure ty) $(TH.varT a))|])
        f _ = (unsafeCoerce :: CoerceShowList $(TH.varT s) $(TH.varT a) $(pure ty)) showList


        |]
  return [TH.InstanceD overlap cxt ty0
      (  (renameDecl 'liftShowsPrec <$> derivedLiftShowsPrec)
      ++ (renameDecl 'liftShowList <$> derivedLiftShowList) ++ decs)
      ]

-- | short example
-- data YourThing f x b
--   = Bin (f b) b (f b)
--   | Tip x
--   deriving Show
--
-- deriveShow1Methods [d|instance Show x => Show1 (YourThing [] x)|]
