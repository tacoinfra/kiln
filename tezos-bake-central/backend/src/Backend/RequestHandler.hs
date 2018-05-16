{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Backend.RequestHandler where

import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Web.ClientSession as CS
import Data.Pool (Pool)
import Database.Groundhog.Postgresql
import Focus.Api
import Focus.Backend.App
import Focus.Backend.DB (runDb)
import Focus.Backend.DB.PsqlSimple
import Focus.Backend.Listen
import Focus.Schema
import Control.Monad.Logger (runNoLoggingT)

import Common.App
import Common.Api
import Common.Schema
import Backend.Schema ()

-- Temporary graph rendering
import Control.Lens
import Data.Colour
import Data.Colour.SRGB
import Data.Default
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams hiding (SVG)
import Diagrams.Core (renderDia)
import Diagrams.Backend.SVG (SVG(..), Options(..))
import Diagrams.TwoD.Size (mkWidth)
import qualified Graphics.Svg.Core as SVG (renderText)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

requestHandler
  :: (MonadBaseControl IO m, MonadIO m)
  => CS.Key
  -> Pool Postgresql
  -> RequestHandler Bake m
requestHandler csk db = RequestHandler $ \req -> runNoLoggingT . runDb (Identity db) $
  case req of
    ApiRequest_Public r ->
      case r of
        PublicRequest_AddClient addr -> do
          _ <- insertAndNotify $ Client { _client_address = addr, _client_updated = Nothing }
          return ()
        PublicRequest_RemoveClient addr -> do
          _ <- [executeQ| DELETE FROM "PendingReward" p USING "Client" c WHERE p.client = c.id AND c.address = ?addr |]
          cids <- [queryQ| DELETE FROM "Client" WHERE "address" = ?addr RETURNING id |]
          forM_ cids $ \(Only cid) -> notifyEntityId NotificationType_Delete (cid :: Id Client)
          return ()
        PublicRequest_RenderGraph t xs -> liftIO $ renderGraph t xs
    ApiRequest_Private key r ->
      case r of
        PrivateRequest_NoOp -> return ()

renderGraph :: (Integral a, Real b) => Text -> [(a,b)] -> IO Text
renderGraph t xs = do
  let chart = toRenderable layout
      plot1 = plot_lines_style . line_color .~ (opaque $ sRGB 0.1 0.5 0.1)
            $ plot_lines_values .~ [[(fromIntegral l :: Integer,realToFrac x :: Double) | (l,x) <- xs]]
            $ def
      layout = layout_title .~ T.unpack t
             $ layout_plots .~ [toPlot plot1]
             $ def
  env <- defaultEnv vectorAlignmentFns 300 300
  let (diagram, _) = runBackendR env chart
      svgOptions = SVGOptions
        { _size = mkWidth 250
        , _svgDefinitions = Nothing
        , _idPrefix = ""
        , _svgAttributes = []
        , _generateDoctype = False
        }
  return (TL.toStrict (SVG.renderText (renderDia SVG svgOptions diagram)))