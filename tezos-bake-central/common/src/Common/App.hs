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
import Data.Semigroup (Semigroup, (<>), First(..))
import Data.These
import Reflex (FunctorMaybe(..), Group(..), Additive)
import Reflex.Query.Class

import Data.AppendMap (AppendMap)
import Focus.App
import Focus.Schema

import Common.Schema
import Tezos.BakeMonitor.Types

data Bake = Bake

data BakeViewSelector a = BakeViewSelector
  { _bakeViewSelector_clients :: Maybe a -- not bothering with partial information listing yet.
  , _bakeViewSelector_parameters :: Maybe a -- not bothering with partial information listing yet.
  }
  deriving (Show, Eq, Ord, Functor, Generic, Typeable, Traversable, Foldable)

data BakeView a = BakeView
  { _bakeView_clients :: AppendMap (Id Client) (First (Maybe (ClientAddress, Maybe ClientInfo)), a)
  , _bakeView_parameters :: AppendMap (Id Node) (First (Maybe (ProtoInfo)), a)
  }
  deriving (Show, Eq, Functor, Generic, Typeable, Traversable, Foldable)

cropBakeView :: (Semigroup a) => BakeViewSelector a -> BakeView a -> BakeView a
cropBakeView vs v =
  let clients = case _bakeViewSelector_clients vs of
        Nothing -> mempty
        Just _ -> _bakeView_clients v
      parameters = case _bakeViewSelector_parameters vs of
        Nothing -> mempty
        Just _ -> _bakeView_parameters v
  in BakeView
      { _bakeView_clients = clients
      , _bakeView_parameters = parameters
      }

instance Align BakeViewSelector where
  nil = BakeViewSelector nil nil
  alignWith f u v = BakeViewSelector
    { _bakeViewSelector_clients = alignWith f (_bakeViewSelector_clients u) (_bakeViewSelector_clients v)
    , _bakeViewSelector_parameters = alignWith f (_bakeViewSelector_parameters u) (_bakeViewSelector_parameters v)
    }

instance FunctorMaybe BakeViewSelector where
  fmapMaybe f a = BakeViewSelector
    { _bakeViewSelector_clients = fmapMaybe f $ _bakeViewSelector_clients a
    , _bakeViewSelector_parameters = fmapMaybe f $ _bakeViewSelector_parameters a
    }

{-
instance Align BakeView where
  nil = BakeView nil
  alignWith f u v = BakeView
    { _bakeView_clients = alignTheseWith f (_bakeView_clients u) (_bakeView_clients v)
    { _bakeView_parameters = alignTheseWith f (_bakeView_parameters u) (_bakeView_parameters v)
    }
-}

instance FunctorMaybe BakeView where
  fmapMaybe f a = BakeView
    { _bakeView_clients = fmapMaybeSnd f $ _bakeView_clients a
    , _bakeView_parameters = fmapMaybeSnd f $ _bakeView_parameters a
    }

fmapMaybeSnd :: FunctorMaybe f => (a -> Maybe b) -> f (e, a) -> f (e, b)
fmapMaybeSnd f = fmapMaybe $ \(e, a) -> case f a of
  Nothing -> Nothing
  Just b  -> Just (e, b)

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

instance (Semigroup a) => Monoid (BakeView a) where
  mempty = BakeView mempty mempty
  mappend u v = BakeView
    { _bakeView_clients = _bakeView_clients u <> _bakeView_clients v
    , _bakeView_parameters = _bakeView_parameters u <> _bakeView_parameters v
    }

instance Semigroup a => Semigroup (BakeView a) where
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
