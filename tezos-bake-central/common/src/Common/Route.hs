{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Common.Route where
import Prelude hiding (id, (.))

import Control.Category
import Control.Monad.Except
import Data.Some (Some)
import Data.Text
import Data.Functor.Sum
import Obelisk.Route
import Obelisk.Route.TH

data AppRoute :: * -> * where
  AppRoute_Index :: AppRoute ()

deriveRouteComponent ''AppRoute

appRouteComponentEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some AppRoute) (Maybe Text)
appRouteComponentEncoder = enum1Encoder $ \case
  AppRoute_Index -> Nothing

appRouteRestEncoder :: (Applicative check, MonadError Text parse) => AppRoute a -> Encoder check parse a PageName
appRouteRestEncoder = \case
  AppRoute_Index -> Encoder $ pure $ endValidEncoder mempty

data BackendRoute :: * -> * where
  BackendRoute_Listen :: BackendRoute ()
  BackendRoute_PublicCacheApi :: BackendRoute PageName

deriveRouteComponent ''BackendRoute

backendRouteEncoder
  :: ( check ~ parse
     , MonadError Text parse
     )
  => Encoder check parse (R (Sum BackendRoute (ObeliskRoute AppRoute))) PageName
backendRouteEncoder = Encoder $ do
  let myComponentEncoder = (backendRouteComponentEncoder `shadowEncoder` obeliskRouteComponentEncoder appRouteComponentEncoder) . someSumEncoder
  myObeliskRestValidEncoder <- checkObeliskRouteRestEncoder appRouteRestEncoder
  checkEncoder $ pathComponentEncoder myComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Listen -> endValidEncoder mempty
      BackendRoute_PublicCacheApi -> id -- endValidEncoder mempty
    InR obeliskRoute -> runValidEncoderFunc myObeliskRestValidEncoder obeliskRoute

backendRouteComponentEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some BackendRoute) (Maybe Text)
backendRouteComponentEncoder = enum1Encoder $ \case
  BackendRoute_Listen -> Just "listen"
  BackendRoute_PublicCacheApi -> Just "api"

backendRouteRestEncoder :: (Applicative check, MonadError Text parse) => BackendRoute a -> Encoder check parse a PageName
backendRouteRestEncoder = Encoder . pure . \case
  BackendRoute_Listen -> endValidEncoder mempty
  BackendRoute_PublicCacheApi -> id
