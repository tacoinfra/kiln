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
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-orphans #-}

module Common.Route where
import Prelude hiding (id, (.))

import Control.Category
import Control.Monad.Except
import Data.Functor.Identity
import Data.Text (Text)
import Obelisk.Route
import Obelisk.Route.TH
import Rhyolite.Schema

import Common.Schema

data AppRoute :: * -> * where
  AppRoute_Index :: AppRoute ()
  AppRoute_Nodes :: AppRoute (Id Node)
  AppRoute_Options :: AppRoute ()

appRouteSegment :: (Applicative check, MonadError Text parse) => AppRoute a -> SegmentResult check parse a
appRouteSegment = \case
  AppRoute_Index -> PathEnd $ unitEncoder mempty
  AppRoute_Nodes -> PathSegment "nodes" idPathSegmentEncoder
  AppRoute_Options -> PathSegment "options" $ unitEncoder mempty

data BackendRoute :: * -> * where
  BackendRoute_ExportLogs :: BackendRoute (R ExportLog)
  BackendRoute_Listen :: BackendRoute ()
  BackendRoute_Missing :: BackendRoute () -- Used to handle unparseable routes.
  BackendRoute_PublicCacheApi :: BackendRoute PageName
  BackendRoute_SnapshotUpload :: BackendRoute ()

data ExportLog :: * -> * where
  ExportLog_Node :: ExportLog ()
  ExportLog_Baker :: ExportLog ()
  ExportLog_Endorser :: ExportLog ()

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute AppRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Frontend (ObeliskRoute_App AppRoute_Index) :/ ())
  (\case
      BackendRoute_ExportLogs -> PathSegment "export-logs" $ pathComponentEncoder $ \case
        ExportLog_Node -> PathSegment "node" $ unitEncoder mempty
        ExportLog_Baker -> PathSegment "baker" $ unitEncoder mempty
        ExportLog_Endorser -> PathSegment "endorser" $ unitEncoder mempty
      BackendRoute_Listen -> PathSegment "listen" $ unitEncoder mempty
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_PublicCacheApi -> PathSegment "api" id
      BackendRoute_SnapshotUpload -> PathSegment "snapshot-upload" $ unitEncoder mempty
  )
  appRouteSegment

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''AppRoute
  , ''ExportLog
  ]
