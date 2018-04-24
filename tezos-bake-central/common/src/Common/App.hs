{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}

module Common.App where

import GHC.Generics
import Data.Aeson
import Data.Typeable
import Data.Align
import Data.Semigroup (Semigroup, (<>))
import Data.These
import Reflex (FunctorMaybe(..), Group(..), Additive)
import Reflex.Query.Class

import Data.AppendMap (AppendMap)
import Focus.App
import Focus.Schema

import Common.Schema

data Bake = Bake

data BakeViewSelector a = BakeViewSelector
  { _bakeViewSelector_clients :: Maybe a -- not bothering with partial information listing yet.
  }
  deriving (Show, Eq, Ord, Functor, Generic, Typeable, Traversable, Foldable)

data BakeView a = BakeView
  { _bakeView_clients :: AppendMap (Id Client) (AppendMap ClientInfo a)
  }
  deriving (Show, Eq, Ord, Functor, Generic, Typeable, Traversable, Foldable)

cropBakeView :: (Semigroup a) => BakeViewSelector a -> BakeView a -> BakeView a
cropBakeView vs v =
  let clients = case _bakeViewSelector_clients vs of
        Nothing -> mempty
        Just _ -> _bakeView_clients v
  in BakeView
      { _bakeView_clients = clients
      }

instance Align BakeViewSelector where
  nil = BakeViewSelector nil
  alignWith f u v = BakeViewSelector
    { _bakeViewSelector_clients = alignWith f (_bakeViewSelector_clients u) (_bakeViewSelector_clients v)
    }

instance FunctorMaybe BakeViewSelector where
  fmapMaybe f a = BakeViewSelector
    { _bakeViewSelector_clients = fmapMaybe f $ _bakeViewSelector_clients a
    }

instance Align BakeView where
  nil = BakeView nil
  alignWith f u v = BakeView
    { _bakeView_clients = alignWith (alignTheseWith f) (_bakeView_clients u) (_bakeView_clients v)
    }

instance FunctorMaybe BakeView where
  fmapMaybe f a = BakeView
    { _bakeView_clients = fmap (fmapMaybe f) $ _bakeView_clients a
    }

alignTheseWith :: Align f => (These a b -> c) -> These (f a) (f b) -> f c
alignTheseWith f = these (fmap (f . This)) (fmap (f . That)) (alignWith f)

instance Monoid a => Monoid (BakeViewSelector a) where
  mempty = nil
  mappend = alignWith (mergeThese mappend)

instance Monoid a => Semigroup (BakeViewSelector a) where
  (<>) = mappend

instance Group (BakeViewSelector SelectedCount) where
  negateG = fmap negateG

instance Additive (BakeViewSelector SelectedCount)

instance Monoid a => Monoid (BakeView a) where
  mempty = nil
  mappend = alignWith (mergeThese mappend)

instance Monoid a => Semigroup (BakeView a) where
  (<>) = mappend

instance (Monoid a, Semigroup a) => Query (BakeViewSelector a) where
  type QueryResult (BakeViewSelector a) = BakeView a
  crop = cropBakeView

instance FromJSON a => FromJSON (BakeViewSelector a)
instance FromJSON a => FromJSON (BakeView a)

instance ToJSON a => ToJSON (BakeViewSelector a)
instance ToJSON a => ToJSON (BakeView a)

instance HasView Bake where
  type View Bake = BakeView
  type ViewSelector Bake = BakeViewSelector