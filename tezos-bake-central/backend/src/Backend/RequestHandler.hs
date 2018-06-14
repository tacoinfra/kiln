{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Backend.RequestHandler where

import Control.Monad
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Pool (Pool)
import Database.Groundhog.Postgresql
import Rhyolite.Api
import Rhyolite.Backend.App
import Rhyolite.Backend.DB (getTime, runDb, selectMap)
import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.Backend.Listen
import Rhyolite.Schema
import qualified Web.ClientSession as CS

import Backend.Schema
import Common.Api
import Common.App
import Common.Schema

-- Temporary graph rendering
import Control.Lens
import Data.Colour
import Data.Colour.SRGB
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Diagrams.Backend.SVG (Options (..), SVG (..))
import Diagrams.Core (renderDia)
import Diagrams.TwoD.Size (mkWidth)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams hiding (SVG)
import qualified Graphics.Svg.Core as SVG (renderText)

requestHandler
  :: (MonadBaseControl IO m, MonadIO m)
  => CS.Key
  -> Pool Postgresql
  -> RequestHandler Bake m
requestHandler csk db = RequestHandler $ \req -> runNoLoggingT . runDb (Identity db) $
  case req of
    ApiRequest_Public r ->
      case r of
        PublicRequest_AddNode addr ->
          void $ insertAndNotify $ Node { _node_address = addr, _node_headLevel = Nothing }
        PublicRequest_RemoveNode addr -> do
          nodeIds <- [queryQ| SELECT id FROM "Node" where address = ?addr |]
          let inNodeIds = In (fromOnly <$> nodeIds)
          -- delete parameters
          void $ [executeQ| DELETE FROM "Parameters" where node in ?inNodeIds |]
          -- delete node
          void $ [executeQ| DELETE FROM "Node" where id in ?inNodeIds |]
          -- notify
          void $ forM_ nodeIds $ \(Only nodeId) -> notifyEntityId NotificationType_Delete (nodeId :: Id Node)
        PublicRequest_AddClient addr -> do
          _ <- insertAndNotify $ Client { _client_address = addr, _client_updated = Nothing }
          return ()
        PublicRequest_RemoveClient addr -> do
          _ <- [executeQ| DELETE FROM "PendingReward" p USING "Client" c WHERE p.client = c.id AND c.address = ?addr |]
          cids <- [queryQ| SELECT id FROM "Client" WHERE "address" = ?addr |]
          let inCids = In (map fromOnly cids)
          _ <- [executeQ| DELETE FROM "Client" c WHERE c.id IN ?inCids |]
          forM_ cids $ \(Only cid) -> notifyEntityId NotificationType_Delete (cid :: Id Client)
          return ()
        PublicRequest_AddNotificatee email -> do
          _ <- insertAndNotify $ Notificatee { _notificatee_email = email }
          return ()
        PublicRequest_RemoveNotificatee email -> do
          nids <- [queryQ| SELECT n.id FROM "Notificatee" n WHERE n.email = ?email |]
          _ <- [executeQ| DELETE FROM "Notificatee" n WHERE n.email = ?email |]
          forM_ nids $ \(Only nid) -> notifyEntityId NotificationType_Delete (nid :: Id Notificatee)
          return ()
        PublicRequest_SetMailServerConfig mailServerView password -> do
          now <- getTime
          let updatedMailServer = MailServerConfig
                { _mailServerConfig_hostName = _mailServerView_hostName mailServerView
                , _mailServerConfig_portNumber  = _mailServerView_portNumber  mailServerView
                , _mailServerConfig_smtpProtocol = _mailServerView_smtpProtocol mailServerView
                , _mailServerConfig_userName = _mailServerView_userName mailServerView
                , _mailServerConfig_password = password
                , _mailServerConfig_madeDefaultAt = now
                }
          defaultMailServer <- getDefaultMailServer
          case defaultMailServer of
            Nothing -> void $ insertAndNotify updatedMailServer
            Just (id_, _) -> updateAndNotify id_
              [ MailServerConfig_hostNameField =. _mailServerConfig_hostName updatedMailServer
              , MailServerConfig_portNumberField =. _mailServerConfig_portNumber updatedMailServer
              , MailServerConfig_smtpProtocolField =. _mailServerConfig_smtpProtocol updatedMailServer
              , MailServerConfig_userNameField =. _mailServerConfig_userName updatedMailServer
              , MailServerConfig_passwordField =. _mailServerConfig_password updatedMailServer
              , MailServerConfig_madeDefaultAtField =. _mailServerConfig_madeDefaultAt updatedMailServer
              ]
        PublicRequest_RenderGraph t xs -> liftIO $ renderGraph t xs
    ApiRequest_Private key r ->
      case r of
        PrivateRequest_NoOp -> return ()

renderGraph :: (Integral a, Real b) => Text -> [(a,b)] -> IO Text
renderGraph t xs = do
  let chart = toRenderable layout
      plot1 = plot_lines_style . line_color .~ opaque (sRGB 0.1 0.5 0.1)
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


getDefaultMailServer :: PersistBackend m => m (Maybe (Id MailServerConfig, MailServerConfig))
getDefaultMailServer =
  fmap (listToMaybe . Map.toList) $
    selectMap MailServerConfigConstructor $ CondEmpty `orderBy` [Desc MailServerConfig_madeDefaultAtField] `limitTo` 1
