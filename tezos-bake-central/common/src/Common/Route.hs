{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-orphans #-}

module Common.Route where
import Prelude hiding (id, (.))

import Control.Category
import Control.Monad.Except
import Control.Monad.Reader (MonadReader(..), ReaderT)
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Text (Text)
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Route.TH
import Rhyolite.Frontend.App (RhyoliteWidget)

-- TODO: Upstream
instance MonadReader r' m => MonadReader r' (RoutedT t r m) where
  ask = lift ask
  local = mapRoutedT . local


instance (Monad m, Routed t r m) => Routed t r (ReaderT r' m) where
  askRoute = lift askRoute

instance (Monad m, Routed t r m) => Routed t r (RhyoliteWidget app t m) where
  askRoute = lift askRoute

instance (Monad m, SetRoute t r m) => SetRoute t r (ReaderT r' m) where
  modifyRoute = lift . modifyRoute

instance (Monad m, SetRoute t r m) => SetRoute t r (RhyoliteWidget app t m) where
  modifyRoute = lift . modifyRoute

instance (Monad m, RouteToUrl r m) => RouteToUrl r (ReaderT r' m) where
  askRouteToUrl = lift askRouteToUrl

instance (Monad m, RouteToUrl r m) => RouteToUrl r (RhyoliteWidget app t m) where
  askRouteToUrl = lift askRouteToUrl


data AppRoute :: * -> * where
  AppRoute_Index :: AppRoute ()
  AppRoute_Nodes :: AppRoute ()
  AppRoute_Options :: AppRoute ()

appRouteSegment :: (Applicative check, MonadError Text parse) => AppRoute a -> SegmentResult check parse a
appRouteSegment = \case
  AppRoute_Index -> PathEnd $ unitEncoder mempty
  AppRoute_Nodes -> PathSegment "nodes" $ unitEncoder mempty
  AppRoute_Options -> PathSegment "options" $ unitEncoder mempty

data BackendRoute :: * -> * where
  BackendRoute_Listen :: BackendRoute ()
  BackendRoute_Missing :: BackendRoute () -- Used to handle unparseable routes.
  BackendRoute_PublicCacheApi :: BackendRoute PageName

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute AppRoute))) PageName
backendRouteEncoder = handleEncoder (const (InR (ObeliskRoute_App AppRoute_Index) :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Listen -> PathSegment "listen" $ unitEncoder mempty
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_PublicCacheApi -> PathSegment "api" id
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute appRouteSegment

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''AppRoute
  ]
