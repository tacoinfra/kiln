{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}

module Common.App where

import GHC.Generics
import Data.Aeson
import Data.AppendMap (AppendMap)
import Data.Fixed
import Data.Typeable
import Data.Align
import Data.Semigroup (Semigroup, (<>), First(..))
import Data.These
import Data.Word
import Reflex (FunctorMaybe(..), Group(..), Additive)
import Reflex.Query.Class
import Reflex.Aeson.Orphans ()
import Rhyolite.App
import Rhyolite.Schema

import Common.Schema


data Bake = Bake

data BakeViewSelector a = BakeViewSelector
  { _bakeViewSelector_clients :: Maybe a -- not bothering with partial information listing yet.
  , _bakeViewSelector_parameters :: Maybe a
  , _bakeViewSelector_level :: Maybe a
  , _bakeViewSelector_notificatees :: Maybe a
  }
  deriving (Show, Eq, Ord, Functor, Generic, Typeable, Traversable, Foldable)

data BakeView a = BakeView
  { _bakeView_clients :: AppendMap (Id Client) (First (Maybe (ClientAddress, Maybe ClientInfo)), a)
  , _bakeView_parameters :: AppendMap (Id Node) (First (Maybe ProtoInfo), a)
  , _bakeView_level :: AppendMap (Id Node) (First (Maybe Word64), a)
  , _bakeView_rewards :: AppendMap (Id Client) (First (AppendMap Word64 Micro), a) -- For each client, a mapping from (future) levels to expected rewards
  , _bakeView_notificatees :: AppendMap (Id Notificatee) (First (Maybe Email), a)
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
      level = case _bakeViewSelector_level vs of
        Nothing -> mempty
        Just _ -> _bakeView_level v
      rewards = case _bakeViewSelector_clients vs of
        Nothing -> mempty
        Just _ -> _bakeView_rewards v
      notificatees = case _bakeViewSelector_notificatees vs of
        Nothing -> mempty
        Just _ -> _bakeView_notificatees v
  in BakeView
      { _bakeView_clients = clients
      , _bakeView_parameters = parameters
      , _bakeView_level = level
      , _bakeView_rewards = rewards
      , _bakeView_notificatees = notificatees
      }

instance Align BakeViewSelector where
  nil = BakeViewSelector nil nil nil nil
  alignWith f u v = BakeViewSelector
    { _bakeViewSelector_clients = alignWith f (_bakeViewSelector_clients u) (_bakeViewSelector_clients v)
    , _bakeViewSelector_parameters = alignWith f (_bakeViewSelector_parameters u) (_bakeViewSelector_parameters v)
    , _bakeViewSelector_level = alignWith f (_bakeViewSelector_level u) (_bakeViewSelector_level v)
    , _bakeViewSelector_notificatees = alignWith f (_bakeViewSelector_notificatees u) (_bakeViewSelector_notificatees v)
    }

instance FunctorMaybe BakeViewSelector where
  fmapMaybe f a = BakeViewSelector
    { _bakeViewSelector_clients = fmapMaybe f $ _bakeViewSelector_clients a
    , _bakeViewSelector_parameters = fmapMaybe f $ _bakeViewSelector_parameters a
    , _bakeViewSelector_level = fmapMaybe f $ _bakeViewSelector_level a
    , _bakeViewSelector_notificatees = fmapMaybe f $ _bakeViewSelector_notificatees a
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
    , _bakeView_level = fmapMaybeSnd f $ _bakeView_level a
    , _bakeView_rewards = fmapMaybeSnd f $ _bakeView_rewards a
    , _bakeView_notificatees = fmapMaybeSnd f $ _bakeView_notificatees a
    }

fmapMaybeSnd :: FunctorMaybe f => (a -> Maybe b) -> f (e, a) -> f (e, b)
fmapMaybeSnd f = fmapMaybe $ \(e, a) -> case f a of
  Nothing -> Nothing
  Just b -> Just (e, b)

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

instance (Semigroup a, Monoid a) => Monoid (BakeView a) where
  mempty = BakeView mempty mempty mempty mempty mempty
  mappend u v = u <> v

instance Semigroup a => Semigroup (BakeView a) where
  u <> v = BakeView
    { _bakeView_clients = _bakeView_clients u <> _bakeView_clients v
    , _bakeView_parameters = _bakeView_parameters u <> _bakeView_parameters v
    , _bakeView_level = _bakeView_level u <> _bakeView_level v
    , _bakeView_rewards = _bakeView_rewards u <> _bakeView_rewards v
    , _bakeView_notificatees = _bakeView_notificatees u <> _bakeView_notificatees v
    }

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
