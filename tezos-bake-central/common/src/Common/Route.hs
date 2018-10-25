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
{-# OPTIONS_GHC -Wall -Werror #-}

module Common.Route where
import Prelude hiding (id, (.))

import Control.Category
import Control.Monad.Except
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Text (Text)
import Obelisk.Route
import Obelisk.Route.TH

data AppRoute :: * -> * where
  AppRoute_Index :: AppRoute ()

appRouteSegment :: (Applicative check, MonadError Text parse)
  => AppRoute a -> SegmentResult check parse a
appRouteSegment = \case
  AppRoute_Index -> PathEnd $ unitEncoder mempty

data BackendRoute :: * -> * where
  BackendRoute_Listen :: BackendRoute ()
  BackendRoute_Missing :: BackendRoute () -- Used to handle unparseable routes.
  BackendRoute_PublicCacheApi :: BackendRoute PageName

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute AppRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
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
