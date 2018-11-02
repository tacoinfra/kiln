{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend where

import Control.Lens ((<>~))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader (ReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Fixed (Micro)
import Data.Function (on)
import Data.List (intersperse, sortBy)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Version (showVersion)
import Data.Word (Word64)
import qualified Form.Checks as Check
import qualified GHCJS.DOM as DOM
import GHCJS.DOM.Element (setInnerHTML)
import qualified GHCJS.DOM.Location as Location
import GHCJS.DOM.Types (MonadJSM)
import qualified GHCJS.DOM.Window as Window
import qualified Obelisk.ExecutableConfig
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static (static)
import Obelisk.Route (R)
import Prelude hiding (log)
import Reflex.Dom.Core
import Reflex.Dom.Form.FieldWriter (tellFieldErr, withFormFieldsErr)
import qualified Reflex.Dom.Form.Validators as Validator
import Reflex.Dom.Form.Widgets (formItem, formItem', validatedInput)
import qualified Reflex.Dom.SemanticUI as SemUi
import qualified Reflex.Dom.TextField as Txt
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (AppWebSocket (..), MonadRhyoliteFrontendWidget, runRhyoliteWidget,
                              watchViewSelector)
import Rhyolite.Schema (Email, Json (..))
import Rhyolite.WebSocket (WebSocketUrl (..))
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.NodeRPC.Sources (PublicNode (..), tzScanUri)
import Tezos.NodeRPC.Types
import Tezos.Types

import Common (humanBytes, uriHostPortPath)
import Common.Alerts (badNodeHeadMessage)
import Common.Api
import Common.App
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import Common.Config (FrontendConfig, HasFrontendConfig (frontendConfig), frontendConfig_appVersion,
                      frontendConfig_chain, frontendConfig_upgradeBranch)
import qualified Common.Config as Config
import Common.HeadTag (headTag)
import Common.Route (AppRoute)
import Common.Schema hiding (Event)
import Common.Vassal
import ExtraPrelude
import Frontend.Common
import Frontend.Modal.Base (ModalBackdropConfig (..), runModalT, withModals)
import Frontend.Modal.Class (HasModal (ModalM, tellModal))
import qualified Frontend.Settings.Telegram as Telegram

frontend :: Frontend (R AppRoute)
frontend = Frontend
  { _frontend_head = headTag
  , _frontend_body = prerender (return ()) frontendBody
  }

frontendBody
  :: forall m t x.
    ( MonadWidget t m
    , HasJS x m
    , MonadFix (Performable m)
    , PrimMonad m
    )
  => m ()
frontendBody = void $ do
  let getExecutableConfig = Obelisk.ExecutableConfig.get . ("config/" <>)
  let decodeViaJson = Aeson.eitherDecode . LBS.fromStrict . T.encodeUtf8 . T.strip
  route :: URI <- liftIO (getExecutableConfig $ T.pack Config.route) >>= \case
    Just r -> return $ either (error . ("Unable to parse injected route: " <>) . show) id (decodeViaJson r)
    Nothing ->
      Config.parseURIUnsafe <$> (Location.getHref =<< Window.getLocation =<< DOM.currentWindowUnchecked)

  let
    routeScheme = T.toLower . Uri.unRText <$> Uri.uriScheme route
    renderPathPieces pieces = T.intercalate "/" (map Uri.unRText $ toList pieces)
    routeAuthority = Uri.uriAuthority route ^? _Right
    wsPort = (Uri.authPort =<< routeAuthority)
      <|> ffor routeScheme (\case
        "http" -> 80
        "https" -> 443
        _ -> 80)
    listenPath = fromMaybe (error "sulk") $ Uri.mkPathPiece "listen"

    wsUrl = WebSocketUrl
      <$> (T.replace "http" "ws" <$> routeScheme)
      <*> (Uri.unRText . Uri.authHost <$> routeAuthority)
      <*> pure (fromIntegral $ fromMaybe 80 wsPort)
      <*> pure (renderPathPieces $ maybe (pure listenPath) ((<> pure listenPath) . snd) (Uri.uriPath route))

  rec
    (socketState, _) <- runRhyoliteWidget (Left $ fromMaybe (error "Invalid WS URL") wsUrl) $ do
      withFrontendContext $
        withConnectivityModal socketState $
          runModalT (ModalBackdropConfig $ "class"=:"modal-backdrop")
            appMain
  pure ()

withConnectivityModal
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadJSM m, TriggerEvent t m, MonadFix m)
  => AppWebSocket t app -> m () -> m ()
withConnectivityModal socketState f = do
  connectionChanged <- updatedWithInit =<< holdUniqDyn (_appWebSocket_connected socketState)
  let
    wsConnected = ffilter id connectionChanged
    wsDisconnected = ffilter not connectionChanged
    mkDisconnectedModal _ = basicModal $ do
      el "h3" $ icon "red icon-warning" *> text " Disconnected."
      el "p" $ text "Kiln is not receiving data from the server but will continue attempting to reconnect. You will be able to proceed as soon as the connection is made."

      divClass "suggested-fix" $ do
        divClass "heading" $ text "Check your network"
        divClass "content" $ text "You may want to check that your infrastructure and network connections are working and that your server is running or auto re-starting in the case that it crashed."

      divClass "suggested-fix" $ do
        divClass "heading" $ text "Leave page open"
        divClass "content" $ text "If your server or network is down, refreshing this page will fail and will prevent Kiln from auto re-connecting if the issue is only temporary."

      divClass "ui active tiny inline loader" blank *> text " Waiting for response from server…"
      pure wsConnected

  void $ withModals
    (ModalBackdropConfig $ "class"=:"disconnected modal-backdrop")
    (mkDisconnectedModal <$ wsDisconnected)
    f

withFrontendContext :: (MonadRhyoliteFrontendWidget Bake t m) => ReaderT (FrontendContext t) m () -> m ()
withFrontendContext f = do
  cfg <- watchFrontendConfig
  dyn_ $ ffor cfg $ \case
    Nothing -> waitingForResponse
    Just c -> do
      tz <- liftIO Time.getCurrentTimeZone
      t0 <- liftIO Time.getCurrentTime
      everySecondTick <- fmap _tickInfo_lastUTC <$> tickLossyFromPostBuildTime 1
      currentTime <- holdDyn t0 everySecondTick
      runReaderT f $ FrontendContext c tz currentTime

validatingRange :: (View (RangeSelector e v) a -> b) -> (View (RangeSelector e v) a -> Maybe b)
validatingRange f v =
  if null $ _rangeView_support v
    then Nothing
    else Just $ f v

watchFrontendConfig :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe FrontendConfig))
watchFrontendConfig =
  (fmap . fmap) (getMaybeView . _bakeView_config) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_config = viewJust 1
    }

watchProtoInfo :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe ProtoInfo))
watchProtoInfo =
  (fmap . fmap) (getMaybeView . _bakeView_parameters) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_parameters = viewJust 1
    }

watchLatestHead :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe VeryBlockLike))
watchLatestHead =
  (fmap . fmap) (getMaybeView . _bakeView_latestHead) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_latestHead = viewJust 1
    }

watchNodes :: (MonadRhyoliteFrontendWidget Bake t m) => Dynamic t (RangeSelector' (Id Node) (Deletable Node) ()) -> m (Dynamic t (MonoidalMap (Id Node) Node))
watchNodes nidsDyn = do
  theView <- watchViewSelector $ ffor nidsDyn $ \nids -> mempty
    { _bakeViewSelector_nodes = 1 <$ nids
    }
  return $ ffor theView $ \v -> fmapMaybe getFirst $ getRangeView' (_bakeView_nodes v)

watchNodesValid :: (MonadRhyoliteFrontendWidget Bake t m) => Dynamic t (RangeSelector' (Id Node) (Deletable Node) ()) -> m (Dynamic t (Maybe (MonoidalMap (Id Node) Node)))
watchNodesValid nidsDyn = do
  theView <- watchViewSelector $ ffor nidsDyn $ \nids -> mempty
    { _bakeViewSelector_nodes = 1 <$ nids
    }
  return $ ffor theView $ \v -> validatingRange (fmapMaybe getFirst . getRangeView') (_bakeView_nodes v)

watchNodeAddresses :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap (Id Node) NodeSummary))
watchNodeAddresses = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodeAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> fmapMaybe getFirst $ getRangeView' (_bakeView_nodeAddresses v')

watchNodeAddressesValid :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (MonoidalMap (Id Node) NodeSummary)))
watchNodeAddressesValid = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodeAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> validatingRange (fmapMaybe getFirst . getRangeView') (_bakeView_nodeAddresses v')

watchClient :: (MonadRhyoliteFrontendWidget Bake t m) => Dynamic t (Id Client) -> m (Dynamic t (MonoidalMap (Id Client) ClientInfo))
watchClient cidDyn = do
  theView <- watchViewSelector . ffor cidDyn $ \cid -> mempty
    { _bakeViewSelector_clients = viewRangeExactly cid 1
    }
  return $ ffor theView $ \v -> fmapMaybe getFirst $ getRangeView (_bakeView_clients v)

watchDelegatePublicKeyHashes :: (MonadRhyoliteFrontendWidget Bake t m) => m (Dynamic t (Set PublicKeyHash))
watchDelegatePublicKeyHashes = do
  theView <- watchViewSelector . pure $ mempty {_bakeViewSelector_delegates = viewRangeAll 1}
  return $ ffor theView $ MMap.keysSet . getRangeView' . _bakeView_delegates

watchDelegateStats :: (MonadRhyoliteFrontendWidget Bake t m) => Dynamic t (Set PublicKeyHash) -> m (Dynamic t (MonoidalMap PublicKeyHash (BakeEfficiency, Account)))
watchDelegateStats delegates = do
  let levels :: (RawLevel, RawLevel) = (0, 30)
      --levels' :: ClosedInterval RawLevel = ClosedInterval 0 30
  _theView <- watchViewSelector $ ffor delegates $ \ds -> mempty
    { _bakeViewSelector_delegateStats = viewCompose $ viewRangeSet ds $ viewRangeBetween levels 1
    }
  holdDyn MMap.empty never
  -- return $ ffor theView $ uncurry (mergeMMap
  --     (\_ acc -> Just (mempty, acc))
  --     (\_ _ -> Nothing)
  --     (\pkh acc (AppendIMMap.AppendIntervalMap effs) -> Just (fold $ IMMap.findWithDefault mempty levels' effs, acc))
  --   ) . second (fmap getRangeView) . first getRangeView . getComposeView . _bakeView_delegateStats

watchClientAddresses :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap (Id Client) URI))
watchClientAddresses = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_clientAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> fmapMaybe getFirst $ getRangeView' $ _bakeView_clientAddresses v'

watchNotificatees :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap (Id Notificatee) Email))
watchNotificatees = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_notificatees = viewRangeAll 1
    }
  return $ ffor theView $ \v -> fmapMaybe getFirst $ getRangeView' (_bakeView_notificatees v)

watchSummary :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (Report, Int)))
watchSummary = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_summary = viewJust 1
    }
  improvingMaybe $ ffor theView $ \v -> getMaybeView $ _bakeView_summary v

watchSummaryGraph :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (Micro, Text)))
watchSummaryGraph = holdDyn Nothing never -- "big" "TODO"
-- watchSummaryGraph = do
--   theView <- watchViewSelector . pure $ mempty
--     { _bakeViewSelector_summary = Just 1
--     }
--   improvingMaybe $ ffor theView $ \v -> join $ getSingle $ _bakeView_summaryGraph v

watchMailServer :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe MailServerView))
watchMailServer =
  (fmap . fmap) (join . getMaybeView . _bakeView_mailServer) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_mailServer = viewJust 1 }

watchErrors
  :: MonadRhyoliteFrontendWidget Bake t m
  => Dynamic t (Set (ClosedInterval (WithInfinity UTCTime)))
  -> m (Dynamic t (MMap.MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView)))
watchErrors intervals =
  -- TOOD: maybe we should just fix up IntervalSelector to operate on some semigroup instead of Set
  (fmap . fmap) (fmap (fst . getFirst) . _intervalView_elements . _bakeView_errors) $ watchViewSelector $ ffor intervals $ \ivals -> mempty
    { _bakeViewSelector_errors = viewIntervalSet ivals 1
    }

watchPublicNodeConfig :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap PublicNode PublicNodeConfig))
watchPublicNodeConfig =
  (fmap . fmap) (getRangeView . _bakeView_publicNodeConfig) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_publicNodeConfig = viewRangeAll 1 }

watchPublicNodeConfigValid :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (MonoidalMap PublicNode PublicNodeConfig)))
watchPublicNodeConfigValid =
  (fmap . fmap) (validatingRange getRangeView . _bakeView_publicNodeConfig) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_publicNodeConfig = viewRangeAll 1 }

isPublicNodeEnabled :: PublicNode -> MonoidalMap PublicNode PublicNodeConfig -> Bool
isPublicNodeEnabled pn pnc = (_publicNodeConfig_enabled <$> MMap.lookup pn pnc) == Just True

watchPublicNodeHeads :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap (Id PublicNodeHead) PublicNodeHead))
watchPublicNodeHeads =
  (fmap . fmap) (getRangeView' . _bakeView_publicNodeHeads) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_publicNodeHeads = viewRangeAll 1 }

watchTelegramRecipients :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap (Id TelegramRecipient) (Maybe TelegramRecipient)))
watchTelegramRecipients =
  (fmap . fmap) (fmap getFirst . getRangeView' . _bakeView_telegramRecipients) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_telegramRecipients = viewRangeAll 1 }

watchUpstreamVersion :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe UpstreamVersion))
watchUpstreamVersion = holdUniqDyn <=<
  (fmap . fmap) (getMaybeView . _bakeView_upstreamVersion) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_upstreamVersion = viewJust 1 }

watchAlertCount :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe Int))
watchAlertCount =
  (fmap . fmap) (getMaybeView . _bakeView_alertCount) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_alertCount = viewJust 1
    }

-- NB: The order of these constructors determines the order of the tabs in the UI.
data UITab = UITab_Nodes
           -- | UITab_Delegate PublicKeyHash
           -- | UITab_Client (Id Client) URI
           | UITab_Options
  deriving (Eq, Ord, Show)

appMain
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadRhyoliteFrontendWidget Bake t (ModalM m), HasModal t m
    , MonadJSM (Performable m)
    , MonadJSM m
    , MonadReader r m, HasFrontendConfig r, HasTimer t r, HasTimeZone r
    )
  => m ()
appMain = do
  elClass "div" "app-frame" $ do
    rec
      selectTab <- appSidebar selectedTab
      selectedTab <- holdDyn initialTab selectTab

    rec
      let openness = leftmost [Just SemUi.Out <$ eHide, Just SemUi.In <$ eShow]
      (eHide, eShow) <- SemUi.sidebar (pure SemUi.Side_Right) SemUi.Out openness
        (def
          & SemUi.sidebarConfig_transition .~ pure SemUi.SidebarTransition_Overlay
          & SemUi.sidebarConfig_dimming .~ pure False
          & SemUi.sidebarConfig_closeOnClick .~ pure False
          & SemUi.sidebarConfig_width .~ pure SemUi.SidebarWidth_VeryWide
        )
        -- Container for the content the sidebar accompanies. "app-right" must
        -- be this and not a child div for flexbox's sake.
        (\f -> SemUi.ui "div" $ f $ def
          & SemUi.classes SemUi.|~ "app-right")
        -- Sidebar content
        (\f -> SemUi.menu
          (f $ def & SemUi.menuConfig_inverted SemUi.|~ False & SemUi.menuConfig_vertical SemUi.|~ True)
          $ do
            let alertWindow = ClosedInterval LowerInfinity UpperInfinity
            nodesDyn <- watchNodes $ pure $ viewRangeAll ()
            alertsDyn <- watchErrors (pure $ Set.singleton alertWindow)
            -- TODO style icon better
            e <- elClass "h3" "ui header" $ do
              text "Notifications"
              domEvent Click <$> SemUi.icon' "icon-arrow-right" def
            liveErrorsWidget alertsDyn nodesDyn
            pure e)
        -- Accompanying content
        $ do
          e <- appHeader
          appContentArea selectedTab
          pure e
    pure ()
  where
    initialTab = UITab_Nodes

appName :: Text
appName = "Kiln"

appSidebar
  :: ( MonadRhyoliteFrontendWidget Bake t m
     , MonadRhyoliteFrontendWidget Bake t (ModalM m)
     , HasModal t m
     )
  => Dynamic t UITab
  -> m (Event t UITab)
appSidebar selectedTab = fmap (fmap getFirst . snd) $ runEventWriterT $ do
  SemUi.segment
    (def
      & SemUi.classes SemUi.|~ "app-sidebar"
      & SemUi.segmentConfig_vertical SemUi.|~ True
      & SemUi.segmentConfig_basic SemUi.|~ True
      )
    $ flip runReaderT (demux selectedTab) $ do
        appSideHeader
        appGutter
        appSideFooter

routeSelector' :: (Reflex t, MonadReader (Demux t r) m, Eq r, SemUi.HasElConfig t e, EventWriter t (First r) m, HasDomEvent t a 'ClickTag) => r -> (e -> ch -> m (a,b)) -> e -> ch -> m (a,b)
routeSelector' dest con cfg child = do
  isAtDest <- asks (\selected -> demuxed selected dest)
  let activated = ffor isAtDest $ \isAt ->
        if isAt then "active" else ""
  (e, a) <- con (cfg & SemUi.classes <>~ SemUi.Dyn activated) child
  tellEvent $ First dest <$ domEvent Click e
  return (e,a)

routeSelector :: (Reflex t, MonadReader (Demux t r) m, Eq r, SemUi.HasElConfig t e, EventWriter t (First r) m, HasDomEvent t a 'ClickTag) => r -> (e -> ch -> m (a,b)) -> e -> ch -> m b
routeSelector dest con cfg child = snd <$> routeSelector' dest con cfg child

appSideHeader :: (MonadRhyoliteFrontendWidget Bake t m, EventWriter t (First UITab) m, MonadReader (Demux t UITab) m) => m ()
appSideHeader =
  SemUi.segment
    (def
      & SemUi.classes SemUi.|~ "app-side-header"
      & SemUi.segmentConfig_basic SemUi.|~ True
      )
    $ do
        SemUi.header def $ do
          elAttr "img" ("src" =: static @ "images/logo.svg" <> "class" =: "app-logo") $ return ()
          text appName
        SemUi.menu
          (def
            & SemUi.menuConfig_vertical SemUi.|~ True
            & SemUi.menuConfig_fluid SemUi.|~ True
            )
          $ do
              routeSelector UITab_Nodes SemUi.menuItem' def $ do
                icon "icon-tiles"
                text "Dashboard"
        SemUi.divider def

appGutter :: (MonadRhyoliteFrontendWidget Bake t m, MonadRhyoliteFrontendWidget Bake t (ModalM m), HasModal t m) => m ()
appGutter =
  SemUi.segment
    (def
      & SemUi.classes SemUi.|~ "app-gutter"
      & SemUi.segmentConfig_basic SemUi.|~ True
      )
    nodesOptions

appSideFooter :: (MonadRhyoliteFrontendWidget Bake t m, EventWriter t (First UITab) m, MonadReader (Demux t UITab) m) => m ()
appSideFooter =
  SemUi.segment
    (def
      & SemUi.classes SemUi.|~ "app-side-footer"
      & SemUi.segmentConfig_basic SemUi.|~ True
      )
    $ do
        SemUi.divider def
        SemUi.menu
          (def
            & SemUi.menuConfig_secondary SemUi.|~ True
            & SemUi.menuConfig_vertical SemUi.|~ True
          )
          $ do
              routeSelector UITab_Options SemUi.menuItem' def $ do
                icon "icon-gear"
                text "Settings"
              SemUi.menuItem def $ do
                icon "icon-question-mark"
                text "Help"
        elAttr "img" ("src" =: static @ "images/ObsidianSystemsLogo-ICFP2017.svg" <> "class" =: "credits-obsidian") $ return ()

appHeader
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasTimer t r, HasFrontendConfig r, HasTimeZone r
    )
  => m (Event t ())
appHeader = SemUi.segment (def & SemUi.segmentConfig_vertical SemUi.|~ True) $
  divClass "ui stackable grid" $ do
    divClass "six wide column topbar" $ do
      divClass "ui horizontal list" $ do
        latestHead <- watchLatestHead
        let info title body = divClass "item" $ divClass "content" $ do
              divClass "header" $ text title
              body

        info "Network" $ text . showChain =<< asks (^. frontendConfig . frontendConfig_chain)

        protoInfo <- watchProtoInfo
        cyc <- holdUniqDyn $ (liftA2.liftA2) levelToCycle protoInfo $ (fmap.fmap) (view level) latestHead
        whenJustDyn cyc $ \c -> info "Cycle" $
          text $ tshow $ unCycle c

        whenJustDyn latestHead $ \b -> info "Block" $ do
          text $ tshow (unRawLevel $ b ^. level) <> " "
          localHumanizedTimestamp $ pure $ b ^. timestamp

    divClass "ten wide column" $ do
      headerBell


headerBell :: MonadRhyoliteFrontendWidget Bake t m => m (Event t ())
headerBell = do
  SemUi.segment
    (def
      & SemUi.segmentConfig_basic SemUi.|~ True
      & SemUi.segmentConfig_floated SemUi.|?~ SemUi.RightFloated
      )
    $ do
        alertCount <- holdUniqDyn =<< fmap (fromMaybe 0) <$> watchAlertCount
        (e,_) <- SemUi.ui' "span"
          (def
            & SemUi.classes .~ (SemUi.Dyn $ ffor alertCount $ bool "ui segment basic big" "ui circular big red link label" . (>0))
            )
          $ do
              dynText $ ffor alertCount $ (fromMaybe <*> T.stripPrefix "0") . tshow
              text " "
              SemUi.icon "icon-bell"
                (def
                  & SemUi.iconConfig_size SemUi.|?~ SemUi.Large
                  & SemUi.iconConfig_color .~ (SemUi.Dyn $ ffor alertCount $ bool (Just SemUi.Grey) Nothing . (>0))
                  & SemUi.iconConfig_link SemUi.|~ True
                  & SemUi.iconConfig_fitted .~ (SemUi.Dyn $ ffor alertCount (>0))
                  )
        return $ domEvent Click e


appContentArea
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadJSM (Performable m)
    , MonadJSM m
    , MonadReader r m, HasFrontendConfig r, HasTimer t r, HasTimeZone r
    , HasModal t m
    , MonadRhyoliteFrontendWidget Bake t (ModalM m)
    )
  => Dynamic t UITab -> m ()
appContentArea selectedTab = divClass "app-content" $
  dyn_ $ ffor selectedTab $ \case
    -- UITab_Summary -> summaryTab
    UITab_Nodes -> nodesTabOrWelcome
    UITab_Options -> settingsTab
    -- UITab_Client cid addr -> clientTab cid addr
    -- UITab_Delegate pkh -> delegateTab pkh

nodesTabOrWelcome
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasFrontendConfig r, HasTimeZone r, HasTimer t r
    )
  => m ()
nodesTabOrWelcome = do
  _clientAddresses <- watchClientAddresses
  _delegates <- watchDelegatePublicKeyHashes
  publicNodesMaybe <- watchPublicNodeConfigValid
  nodesMaybe <- watchNodeAddressesValid
  -- doing some straightforward calculations, but inside a Dynamic and a Maybe
  let haveNodesMaybe =
        (liftA2 . liftA2) ((||) . any _publicNodeConfig_enabled . toList) publicNodesMaybe $
        (fmap . fmap) (not . null) nodesMaybe
  dyn_ $ ffor haveNodesMaybe $ \case
    Nothing -> waitingForResponse
    Just False -> divClass "app-welcome" welcomeScreen
    Just True -> nodesTab

welcomeScreen :: forall t m. MonadRhyoliteFrontendWidget Bake t m => m ()
welcomeScreen = do
  SemUi.header
    (def
      & SemUi.headerConfig_size SemUi.|?~ SemUi.H1
      )
    $ do
        text $ "Welcome to " <> appName <> "."
  divClass "" $ do
    text $ appName <> " helps you monitor Tezos nodes to keep your system"
    el "br" blank
    text "running smoothly, with many more features to come."
    el "br" blank
    text "\160"
    el "br" blank
    text "Click \"Add Node\" on the left to get started."

summaryTab
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadJSM m
    , MonadReader r m, HasFrontendConfig r
    )
  => m ()
summaryTab = divClass "ui grid" $ do
  dparameters <- watchProtoInfo
  summaryReport <- watchSummary

  divClass "six wide column" $ do
    divClass "ui medium header" $ text "Summary"
    text "These are the totals of various events across all monitored bakers."
    let -- bakedCount = fmap (length . _report_baked . fst) <$> summaryReport
        errorCount = fmap (length . _report_errors . fst) <$> summaryReport
        waitingCount = fmap snd <$> summaryReport
    divClass "counts" $ el "ul" $ do
      {-
      whenJustDyn bakedCount $ \n -> do
        tooltipPos "right center" "The number of blocks that have been baked." $ do
          text $ "Blocks baked: " <> T.pack (show n)
      -}
      whenJustDyn errorCount $ \n -> do
        tooltipPos "right center" "The number of errors that have occurred." $ do
          text $ "Errors: " <> T.pack (show n)
      whenJustDyn waitingCount $ \n ->
        tooltipPos "right center" "This is the number of bakers from which we're still awaiting any response." $ do
          text $ "Waiting: " <> tshow n

    mGraph <- watchSummaryGraph
    (graphEl, _) <- el' "div" blank
    dyn_ . ffor mGraph $ \case
      Nothing -> blank
      Just (total, graphText) -> do
        setInnerHTML (_element_raw graphEl) graphText
        text $ "Total rewards earned: " <> tez (Tez total)

  whenJustDyn (fmap fst <$> summaryReport) $ \report -> do
    let baked = sortBy (flip (comparing _event_time)) (_report_baked report)
    divClass "ten wide column" $ do
      divClass "ui medium header" $ text "Activity"
      elAttr "table" ("class" =: "ui celled striped table") $ do
        el "thead" . el "tr" $ do
          elClass "th" "four wide" $ text "Time"
          el "th" $ text "Level"
          el "th" $ text "Block Hash"
          el "th" $ text "Reward"
        for_ baked $ \b -> el "tr" $ do
          el "td" $ el "strong" $ text $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d at %H:%M" $ _event_time b
          el "td" . text . T.pack . show . blockLevel $ b
          el "td" . blockHashLink $ pure $ _bakedEvent_hash $ _event_detail b
          el "td" . dyn . ffor dparameters $ \case
            Nothing -> text "N/A"
            Just protoInfo -> text . tez $ blockRewards b protoInfo

  return ()

radioLabels :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, Eq k) => k -> [(k, m ())] -> m (Dynamic t k)
radioLabels k0 ks = divClass "ui buttons" $ mdo
  selectedDyn <- holdDyn k0 $ leftmost kClicks
  kClicks <- for ks $ \(k, label) -> do
    fmap (k <$) $ uiDynButton (ffor selectedDyn $ bool "" "primary" . (== k)) label

  pure selectedDyn

data AlertsFilter = AlertsFilter_All | AlertsFilter_UnresolvedOnly | AlertsFilter_ResolvedOnly
  deriving (Eq, Ord, Show, Enum, Bounded)


liveErrorsWidget
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasFrontendConfig r, HasTimeZone r
    )
  => Dynamic t (MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView))
  -> Dynamic t (MonoidalMap (Id Node) Node)
  -> m ()
liveErrorsWidget errorsDyn nodesDyn = void $ do
  filterDyn <- holdUniqDyn =<< radioLabels AlertsFilter_All
    [ (AlertsFilter_All, text "All")
    , (AlertsFilter_UnresolvedOnly, text "Unresolved")
    , (AlertsFilter_ResolvedOnly, text "Resolved")
    ]

  filteredErrors <- holdUniqDyn $ liftA2
    (\errors filterFn -> MMap.filter (filterFn . fst) errors)
    errorsDyn
    (passesFilter <$> filterDyn)

  let
    (otherErrors, nodeErrors) = splitDynPure $ partitionErrors <$> filteredErrors

    nodeErrorsWithNode :: Dynamic t (MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView, Node))
    nodeErrorsWithNode = liftA2 joinNodeErrors nodeErrors nodesDyn

    combinedErrors = liftA2 (MMap.unionWith (error "Overlapping keys after partition"))
      (fmap (\(a, b) -> (a, b, Nothing)) `fmap` otherErrors)
      (fmap (_3 %~ Just) `fmap` nodeErrorsWithNode)

  SemUi.segment
    (def
      & SemUi.classes SemUi.|~ "app-notifications-list"
      & SemUi.segmentConfig_vertical SemUi.|~ True
      & SemUi.segmentConfig_basic SemUi.|~ True
      ) $
    listWithKey (errorsByTime Down <$> combinedErrors) $ \_ vDyn ->
      dyn_ $ ffor vDyn $ \v@(log, _, _) -> do
        divClass ("app-notification ui message " <> if isJust $ _errorLog_stopped log then "success" else "error") $ do
          logEntry v
          row $ timestamped ("First seen", _errorLog_started log)
          row $ timestamped $ maybe ("Last seen", _errorLog_lastSeen log) ("Stopped",) $ _errorLog_stopped log
  where
    row = el "div"
    timestamped (lbl,ts) = do
      el "label" $ text lbl
      localTimestamp $ pure ts

    nodeIdentification :: Node -> (Text, Maybe Text)
    nodeIdentification ns =
      let addr = Uri.render $ _node_address ns
      in maybe (addr, Nothing) (, Just addr) $ _node_alias ns

    passesFilter filterSelection log =
      filterSelection == AlertsFilter_All
        || filterSelection == AlertsFilter_UnresolvedOnly && not isResolved
        || filterSelection == AlertsFilter_ResolvedOnly && isResolved
      where isResolved = isJust $ _errorLog_stopped log

    partitionErrors
      :: MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView)
      -> ( MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView)
         , MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView, Id Node) )
    partitionErrors = MMap.mapEither $ \row@(log, logView) ->
      case nodeIdForErrorLogView logView of
        Nothing -> Left row
        Just nodeId -> Right (log, logView, nodeId)

    joinNodeErrors
      :: MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView, Id Node)
      -> MonoidalMap (Id Node) Node
      -> MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView, Node)
    joinNodeErrors errors nodes = flip MMap.mapMaybe errors $ \(log, logView, nodeId) ->
      case MMap.lookup nodeId nodes of
        Nothing -> Nothing
        Just node -> Just (log, logView, node)

    logEntry :: (ErrorLog, ErrorLogView, Maybe Node) -> m ()
    logEntry (log, specificLog, node') =
      let header = divClass "header" . text
          nodeLabel n = row $ do
            let (primary, secondary) = nodeIdentification n
            el "label" $ text primary
            for_ secondary $ elClass "label" "node-secondary-label" . text
      in case specificLog of
          ErrorLogView_InaccessibleNode (ErrorLogInaccessibleNode _ _ address alias) -> for_ node' $ \n -> do
            header $ "Unable to connect to node" <> maybe "" (" " <>) alias <> " at " <> Uri.render address
            nodeLabel n

          ErrorLogView_NodeWrongChain (ErrorLogNodeWrongChain _ _ address alias expectedChainId actualChainId) ->
            for_ node' $ \n -> do
              header $ "Node on wrong network: " <> fromMaybe (Uri.render address) alias
              nodeLabel n
              el "div" $
                text $ "The node is running on network " <> toBase58Text actualChainId <> " but is expected to be on " <> toBase58Text expectedChainId <> "."

          ErrorLogView_BakerNoHeartbeat (ErrorLogBakerNoHeartbeat _ lastLevel lastBlockHash _) -> do
            header "Baker lagging behind" -- TODO Show client address
            el "div" $ do
              text "Last block level seen: "
              blockHashLinkAs (pure lastBlockHash) (text $ tshow lastLevel)

          ErrorLogView_BadNodeHead l ->
            for_ node' $ \n -> do
            let (heading, message) = badNodeHeadMessage text (blockHashLink . pure) l
            header $ heading <> ": " <> fromMaybe (Uri.render $ _node_address n) (_node_alias n)
            nodeLabel n
            el "div" message

          ErrorLogView_MultipleBakersForSameDelegate ErrorLogMultipleBakersForSameDelegate{} -> do
            header "Multiple bakers for same delegate" -- TODO Fill this out

    errorsByTime direction errors = Map.fromList
      [ (direction (_errorLog_started l, elId), row)
      | (elId, row@(l, _, _)) <- MMap.toList errors
      ]

nodeTitleSubtitle :: URI -> Maybe Text -> (Text, Maybe Text)
nodeTitleSubtitle uri alias = (fromMaybe addr alias, addr <$ alias)
  where addr = uriHostPortPath uri

nodesOptions ::
  ( MonadRhyoliteFrontendWidget Bake t m
  , MonadRhyoliteFrontendWidget Bake t (ModalM m)
  , HasModal t m
  )
  => m ()
nodesOptions = do
  divClass "ui header" $ text "Nodes"
  divClass "ui list" $ do
    nodes <- watchNodeAddresses
    _ <- listWithKey (coerce <$> nodes) $ \_ node -> divClass "item bullet-before" $ do
      let dHealth = (> 0) . _nodeSummary_alertCount <$> node
      _ <- SemUi.ui' "i" (def & SemUi.elConfigClasses .~ "icon circle tiny" <> (SemUi.Dyn $ bool "green" "red" <$> dHealth)) blank
      divClass "content" $ do
        let (title, subtitle) = splitDynPure $ ffor node $ \n ->
              nodeTitleSubtitle (_nodeSummary_address n) (_nodeSummary_alias n)
        divClass "header" $ dynText title
        divClass "description" $ dynText $ fromMaybe "" <$> subtitle

    openAddNodeOptions <- buttonIconWithInfoCls "icon-plus" "modalopener fluid" "Add Node" "Configure Monitored Nodes"
    tellModal $ (<$ openAddNodeOptions) $ cancelableModal $ \close -> do
      el "h3" $ text "Add Nodes"
      divClass "basic small segment" $ text $ T.unlines
        [ "Choose from public nodes on the left, connect to your own nodes on the right."
        , "We recommend adding at least one public node."
        ]
      divClass "ui grid" $ do
        divClass "ten wide column" $ divClass "blue shaded" $ do
          elClass "h5" "ui header" $ text "Connect to a Public Node"
          publicNodeOptions
        divClass "six wide column" $ divClass "blue shaded" $ do
          elClass "h5" "ui header" $ text "Connect via address"
          addE <- aliasedInputForm validateUri "Add Node" "Begin monitoring the node at the address entered." "http://[host][:port]"
          nodeAddedE <- requestingIdentity $ fmap (\(addr,alias) -> public (PublicRequest_AddNode addr alias)) addE
          pure $ leftmost [nodeAddedE, close]

aliasedInputForm
  :: (MonadRhyoliteFrontendWidget Bake t m, Eq a)
  => Validator.Validator t m a -> Text -> Text -> Text -> m (Event t (a,Maybe Text))
aliasedInputForm validator label info placeholder = divClass "ui form fields" $ do
  (namedAddress, submitEvt) <- formWithSubmit $ do
    address <- formItem' "required"
      $ validatedInput validator
      $ def & Txt.setPlaceholder placeholder
            & Txt.setFluid
            & Txt.addLabel (el "label" $ text "Address")
    alias <- formItem
      $ validatedInput (Validator.optional Validator.validateText)
      $ def & Txt.setPlaceholder "alias"
            & Txt.setFluid
            & Txt.addLabel (el "label" $ text "Alias")
    _ <- submitButtonWithInfoCls "fluid primary" label info
    let namedAddress = liftA2 (liftA2 (,)) address alias
    return namedAddress
  return $ filterRight $ tag (current namedAddress) submitEvt

settingsTab
  :: forall r t m.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadJSM (Performable m)
    , MonadJSM m
    , MonadReader r m, HasFrontendConfig r, HasTimer t r, HasTimeZone r
    , HasModal t m, MonadRhyoliteFrontendWidget Bake t (ModalM m)
    )
  => m ()
settingsTab = do
  divClass "version-section" $ do
    currentVersion <- asks (^. frontendConfig . frontendConfig_appVersion)
    divClass "soft-heading" $ text $ "Kiln Version " <> T.pack (showVersion currentVersion)

    enableUpgradeCheck <- isJust <$> asks (^. frontendConfig . frontendConfig_upgradeBranch)
    when enableUpgradeCheck upgradeOptions

  divClass "notifications-section" $ do
    SemUi.header
      (def
        & SemUi.headerConfig_size SemUi.|?~ SemUi.H3
        )
      $ text "Notifications"

    sequence_ $ intersperse (SemUi.divider def) $ map notificationSection $
      [ ("Email","letter",mailServerOptions)
      , ("Telegram","telegram",telegramOptions)
      ]

  where
    notificationSection :: (Text, Text, m ()) -> m ()
    notificationSection (name, iconName, content) =
      divClass "notifications-subsection" $ do
        toggleSwitch <- SemUi.header
          (def
            & SemUi.headerConfig_size SemUi.|?~ SemUi.H4
            )
          $ do
              flip SemUi.checkbox
                (def
                  & SemUi.checkboxConfig_type SemUi.|?~ SemUi.Toggle
                  & SemUi.checkboxConfig_setValue . SemUi.initial .~ True
                  )
                $ do
                    icon ("icon-" <> iconName)
                    text name
        dyn_ $ ffor (toggleSwitch ^. SemUi.checkbox_value) $ \case
          False -> divClass "purpose" $ text $ name <> " notifications are turned off"
          True -> content

    telegramOptions = do
      divClass "purpose" $ text "Use a Telegram Bot to send alerts."
      Telegram.inlineSettings

    notificationOptions = do
      divClass "ui medium header" $ text "Notification Recipients"
      notificatees <- watchNotificatees

      let
        emailWidget email = do
          dynText email
          text " "
          ev <- uiButton "mini compact orange" "Send test"
          void $ requestingIdentity $ public . PublicRequest_SendTestEmail <$> tag (current email) ev

      rec (addN, removeN) <- listInput "user@example.com" (isRight . Check.email) emailWidget notificatees (Right "" <$ addedN)
          addedN <- requestingIdentity . ffor addN $ \email -> public (PublicRequest_AddNotificatee email)
          _ <- requestingIdentity . ffor removeN $ \(_, email) -> public (PublicRequest_RemoveNotificatee email)

      pure ()

    mailServerOptions = do
      divClass "ui medium header" $ text "SMTP Mail Server"
      mailServer <- watchMailServer
      dyn_ $ ffor mailServer $ \cfg -> do
        let form0 = fromMaybe (MailServerView "" 587 SmtpProtocol_Ssl "") cfg
        updatedForm <- mailServerForm form0
        requestingIdentity $ public . uncurry PublicRequest_SetMailServerConfig <$> updatedForm
      notificationOptions

    _clientsOptions = void $ do
      divClass "ui medium header" $ text "Clients"
      elClass "table" "ui celled striped compact table" $ do
        clients <- watchClientAddresses -- TODO
        _ <- listWithKey (coerce <$> clients) $ \_ dName -> el "tr" $ do
          el "td" $ dynText $ Uri.render <$> dName
          el "td" $ do
            eRemove <- buttonWithInfo "Remove" "Stop monitoring this client. It will continue running."
            requestingIdentity $ public . PublicRequest_RemoveClient <$> tag (current dName) eRemove

        addE <- aliasedInputForm validateUri "Add Baker" "Begin monitoring the baker at the address entered." "http://[host][:port]"
        void $ requestingIdentity $ ffor addE $ \(addr,alias) -> public (PublicRequest_AddClient addr alias)

    _delegatesOptions = do
      divClass "ui medium header" $ text "Delegates"
      elClass "table" "ui celled striped compact table" $ do
        delegates <- watchDelegatePublicKeyHashes
        _ <- listWithKey (Map.fromSet (const ()) <$> delegates) $ \pkh _ -> el "tr" $ do
          el "td" $ publicKeyHashLink pkh
          el "td" $ do
            eRemove <- buttonWithInfo "Remove" "Stop monitoring this delegate."
            requestingIdentity $ public . PublicRequest_RemoveDelegate <$> tag (pure pkh) eRemove

        addE <- aliasedInputForm (Validator.Validator (first tshow . tryReadPublicKeyHashText) id) "Add Delegate" "Begin monitoring wallet address entered." "tz..."
        void $ requestingIdentity $ ffor addE $ \(pkh,alias) -> public (PublicRequest_AddDelegate pkh alias)

    upgradeOptions = do
      currentVersion <- asks (^. frontendConfig . frontendConfig_appVersion)
      upstreamVersion <- watchUpstreamVersion

      elClass "p" "check-for-updates" $ do
        (aEl, _) <- el' "a" $ text "Check for updates"
        rec
          let submit = gate (not <$> current isLoading) $ domEvent Click aEl
          (isLoading, _gotResponse) <- formIsLoading ((<) `on` (^? _Just . upstreamVersion_updated)) upstreamVersion submit
        _ <- requestingIdentity $ public PublicRequest_CheckForUpgrade <$ submit

        dyn_ $ ffor2 upstreamVersion isLoading $ \v' loading -> case loading of
          True -> divClass "ui tiny active inline loader" blank *> text " Checking for updates..."
          False -> case v' of
            Just UpstreamVersion { _upstreamVersion_error = Just _e } -> text "Unable to reach update server."
            Just UpstreamVersion { _upstreamVersion_version = Just v, _upstreamVersion_updated = updatedTime } ->
              if v > currentVersion
              then changelogLink "" v $
                text ("Version " <> T.pack (showVersion v) <> " Available ") *> icon "icon-pop-out"
              else
                text "Up to date as of " *> localHumanizedTimestamp (pure updatedTime)
            _ -> blank

publicNodeOptions :: MonadRhyoliteFrontendWidget Bake t m => m ()
publicNodeOptions = do
  let
    publicNodesInOrder =
      [ PublicNode_Obsidian
      , PublicNode_Blockscale
      , PublicNode_TzScan
      ]
    showPublicNode = \case
      PublicNode_Obsidian -> "Obsidian Systems"
      PublicNode_Blockscale -> "Foundation"
      PublicNode_TzScan -> "tzscan.io"

    describePublicNode = \case
      PublicNode_Obsidian -> "Public Node Caching Service provided by Obsidian Systems"
      PublicNode_Blockscale -> "Load-balanced collection of nodes provided by the Tezos Foundation"
      PublicNode_TzScan -> "API provided by tzscan.io, the block explorer by OCamlPro"

  pncDyn <- watchPublicNodeConfig
  divClass "ui publicnodes" $ for_ publicNodesInOrder $ \pn -> do
    let pnActiveDyn = isPublicNodeEnabled pn <$> pncDyn
    (element', ()) <- SemUi.ui' "div"
        (def & SemUi.elConfigClasses .~ "ui padded divided grid " <> (SemUi.Dyn $ bool "" "active" <$> pnActiveDyn)) $ divClass "row" $ do
      divClass "four wide column label" $ divClass "ui center aligned icon header" $ do
        SemUi.ui "i" (def & SemUi.elConfigClasses .~ (SemUi.Dyn $ bool "" "icon icon-check" <$> pnActiveDyn)) blank
        dynText $ bool "Add Node" "Added" <$> pnActiveDyn
      divClass "twelve wide column" $ do
        divClass "twelve wide column" $ do
          divClass "header" $ text $ showPublicNode pn
          divClass "description" $ text $ describePublicNode pn

    let toggled = tag (current $ not . isPublicNodeEnabled pn <$> pncDyn) (domEvent Click element')
    void $ requestingIdentity $ ffor toggled $ \enabled -> public (PublicRequest_SetPublicNodeConfig pn enabled)

mailServerForm
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     )
  => MailServerView -> m (Event t (MailServerView, Text))
mailServerForm frm0 = do
  (form, save) <- formWithSubmit $ do
    form <- fields
    elDynAttr "button"
      (ffor (isRight <$> form) $ \s -> "type"=:"submit"
        <> "class"=:("ui tiny primary submit button" <> if s then "" else " disabled")
      ) $ text "Save"
    return form

  pure $ filterRight $ tag (current form) save

  where
    fields = withFormFieldsErr (frm0, "") $ do
      divClass "three fields" $ do
        tellFieldErr (_1 . mailServerView_hostName) <=< formItem' "required eight wide"
          $ validatedInput Validator.validateText
          $ defTxt "Host" & Txt.setInitial (_mailServerView_hostName frm0)

        tellFieldErr (_1 . mailServerView_portNumber) <=< formItem' "required four wide"
          $ validatedInput (Validator.validateNumeric "port" (Just 0, Just 65535) (Just 1))
          $ defTxt "Port" & Txt.setInitial (tshow $ _mailServerView_portNumber frm0)

        tellFieldErr (_1 . mailServerView_smtpProtocol) <=< formItem' "required four wide"
          $ fmap (fmap (maybe (Left "Please select a protocol") Right) . SemUi._dropdown_value)
          $ do
            labeled "Protocol"
            SemUi.dropdown (def & SemUi.dropdownConfig_placeholder .~ "Protocol"
                                & SemUi.dropdownConfig_fluid SemUi.|~ True)
              (Just $ _mailServerView_smtpProtocol frm0)
              never
              $ SemUi.TaggedStatic
              $ SmtpProtocol_Plain=:text "Plain"
              <> SmtpProtocol_Ssl=:text "SSL"
              <> SmtpProtocol_Starttls=:text "STARTTLS"

      divClass "two fields" $ do
        tellFieldErr (_1 . mailServerView_userName) <=< formItem
          $ validatedInput (Validator.optionalWith "" id Validator.validateText)
          $ defTxt "User name" & Txt.setInitial (_mailServerView_userName frm0)

        tellFieldErr _2 <=< formItem
          $ validatedInput (Validator.optionalWith "" id validatePassword)
          $ defTxt "Password"

    validatePassword = Validator.Validator (\x -> if T.null x then Left "Please enter a password" else Right x) Txt.setPasswordType
    defTxt txt = def & Txt.addLabel (labeled txt) & Txt.setPlaceholder txt
    labeled = el "label" . text

nodesTab
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasFrontendConfig r, HasTimeZone r, HasTimer t r
    )
  => m ()
nodesTab =
  divClass "nodes-dashboard" $ do
    el "h4" $ text "Nodes"
    nodesDyn <- watchNodes $ pure $ viewRangeAll ()
    nodeTilesWidget nodesDyn
  where
    nodeTilesWidget :: Dynamic t (MonoidalMap (Id Node) Node) -> m ()
    nodeTilesWidget nodesDyn = do
      publicNodeConfigDyn <- watchPublicNodeConfig
      rawPublicNodesDyn <- watchPublicNodeHeads
      let
        publicNodesDyn = zipDynWith (\pnc ->
          MMap.filter (flip isPublicNodeEnabled pnc . _publicNodeHead_source)
          ) publicNodeConfigDyn rawPublicNodesDyn

      useBlocker <- holdUniqDyn $ ffor (zipDyn publicNodesDyn nodesDyn) $ \(pn,n) -> MMap.null pn && MMap.null n

      dyn_ $ ffor useBlocker $ \case
        True -> waitingForResponse
        False -> divClass "ui stackable cards" $ do
          let alertWindow = ClosedInterval LowerInfinity UpperInfinity
          alerts <- watchErrors (pure $ Set.singleton alertWindow)
          void $ listWithKey (MMap.getMonoidalMap <$> nodesDyn) $ \nodeId vDyn -> do
            unresolvedAlertsForThisNode <- holdUniqDyn $
              foldMap toList . MMap.lookup nodeId . errorsByNode <$> alerts

            let
              errorMessages = ffor unresolvedAlertsForThisNode $ fmap $ \case
                ErrorLogView_InaccessibleNode{} -> text "Unable to connect."
                ErrorLogView_NodeWrongChain{} -> text "On wrong network."
                ErrorLogView_BadNodeHead l -> text $
                  fst (badNodeHeadMessage Const (Const . const "") l) <> "."
                _ -> blank

            let (title, subtitle) = splitDynPure $ liftA2 nodeTitleSubtitle (_node_address <$> vDyn) (_node_alias <$> vDyn)
            titleUniq <- holdUniqDyn title
            subtitleUniq <- holdUniqDyn subtitle

            nodeTile
              (dynText titleUniq)
              subtitleUniq
              getNodeHeadBlock
              (Just errorMessages)
              (Just _node_peerCount)
              (Just _node_networkStat)
              vDyn

          void $ listWithKey (MMap.getMonoidalMap <$> publicNodesDyn) $ \_ vDyn -> do
            source <- holdUniqDyn (_publicNodeHead_source <$> vDyn)
            chain <- holdUniqDyn $ getNamedChainOrChainId . _publicNodeHead_chain <$> vDyn
            let
              title = dyn_ $ ffor2 source chain $ \s c -> case s of
                PublicNode_TzScan -> either (urlLink . tzScanUri) (flip const) c $ text "tzscan"
                PublicNode_Blockscale -> text "Foundation Nodes"
                PublicNode_Obsidian -> text "Obsidian Systems"

            nodeTile
              title
              (pure Nothing)
              (Just . mkVeryBlockLike)
              Nothing
              Nothing
              Nothing
              vDyn


          --el "div" $ do
          --  eRemove <- buttonWithInfo "Remove" "Stop monitoring this node. It will continue running."
          --  void $ requestingIdentity $ public . PublicRequest_RemoveNode . _node_address <$> (node <$ eRemove)

    nodeTile
      :: m () -- ^ Title
      -> Dynamic t (Maybe Text) -- ^ Subtitle
      -> (a -> Maybe VeryBlockLike) -- ^ Function to get block information from a node
      -> Maybe (Dynamic t [m ()]) -- ^ (Optional) Function to build list of error messages for this node
      -> Maybe (a -> Maybe Word64) -- ^ (Optional) Function to get the peer count of the node
      -> Maybe (a -> NetworkStat) -- ^ (Optional) Function to get the network stats of the node
      -> Dynamic t a -- ^ Node
      -> m ()
    nodeTile title subtitle getBlock errors' getPeerCount' getNetworkStats' node = do
      b <- maybeDyn $ getBlock <$> node
      divClass "ui card node-tile" $ divClass "content" $ do
        divClass "menu-section" $ do
          icon "icon-ellipsis"

        divClass "title" $ do
          for_ errors' $ \errors -> do
            errorsEmpty <- holdUniqDyn $ null <$> errors
            iconDyn $ ffor errorsEmpty $ \e -> "tiny circle " <> bool "red" "green" e
          title
          divClass "subtitle" $ dynText =<< holdUniqDyn (fromMaybe nbsp <$> subtitle)

        for_ errors' $ \errors ->
          dyn_ $ ffor errors $ traverse_ (divClass "ui error message")

        divClass "divider" blank

        divClass "soft-heading" $
          withPlaceholder' "Connecting..." $ withMaybeDyn b display (unRawLevel . view level)
        text "#"
        withPlaceholder $ withMaybeDyn b blockHashLink (view hash)

        el "dl" $ do
          el "dt" (text "Fitness")
          el "dd" $
            withPlaceholder $ withMaybeDyn b dynText (fitnessText . view fitness)

          el "br" blank

          el "dt" (text "Baked")
          el "dd" $ do
            withPlaceholder $ withMaybeDyn b localHumanizedTimestamp (view timestamp)

        when (isJust getPeerCount' || isJust getNetworkStats') $
          divClass "divider" blank

        for_ getPeerCount' $ \getPeerCount -> do
          peerCount <- maybeDyn <=< holdUniqDyn $ getPeerCount <$> node
          elClass "span" "peer-count" $ withPlaceholder $ (fmap.fmap) display peerCount
          text " connected peers"

        for_ getNetworkStats' $ \getNetworKStats -> do
          let
            stat = getNetworKStats <$> node
            showSpeed n = dynText <=< holdUniqDyn $ ffor n $ fromIntegral >>> humanBytes >>> (<> "/s")
            showTotal n = dynText <=< holdUniqDyn $ ffor n $ unTezosWord64 >>> fromIntegral >>> humanBytes

          divClass "stats" $ do
            divClass "column heading" $ do
              divClass "cell" $ text "Speed"
              divClass "cell" $ text "Total"

            divClass "column" $ do
              divClass "cell" $ icon "icon-arrow-up" *> showSpeed (_networkStat_currentOutflow <$> stat)
              divClass "cell" $ icon "icon-arrow-up" *> showTotal (_networkStat_totalSent <$> stat)

            divClass "column" $ do
              divClass "cell" $ icon "icon-arrow-down" *> showSpeed (_networkStat_currentInflow <$> stat)
              divClass "cell" $ icon "icon-arrow-down" *> showTotal (_networkStat_totalRecv <$> stat)
      where
        withPlaceholder = withPlaceholder' "-"

        withPlaceholder' :: Text -> Dynamic t (Maybe (m ())) -> m ()
        withPlaceholder' placeholder f' = dyn_ $ ffor f' $ \case
          Nothing -> text placeholder
          Just f -> f

        withMaybeDyn :: Eq b => Dynamic t (Maybe (Dynamic t a)) -> (Dynamic t b -> m ()) -> (a -> b) -> Dynamic t (Maybe (m ()))
        withMaybeDyn d mkWidget f = (fmap.fmap) (mkWidget <=< holdUniqDyn . fmap f) d

        nbsp = "\x00A0"

    errorsByNode
      :: MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView)
      -> MonoidalMap (Id Node) (NonEmpty ErrorLogView)
    errorsByNode xs = MMap.fromListWith (<>)
      [ (k, pure t)
      | (ErrorLog{_errorLog_stopped = Nothing}, t) <- MMap.elems xs
      , Just k <- [nodeIdForErrorLogView t]
      ]

delegateTab
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasFrontendConfig r
    )
  => PublicKeyHash
  -> m ()
delegateTab pkh = do
  delegates <- watchDelegateStats $ pure $ Set.singleton pkh
  dparameters <- watchProtoInfo
    -- TODO: this could be a maybeDyn of some sort so that we don't redraw the dom for each balance change/block baked.
  thisDelegate <- (maybeDyn <=< holdDyn Nothing <=< updatedWithInit)  $ MMap.lookup pkh <$> delegates
  dyn_ $ ffor thisDelegate $ \case
    Nothing -> waitingForResponse
    Just d -> dyn_ $ ffor d $ \(bakeEfficiency, account) -> divClass "ui grid" $ do
      divClass "eight wide column" $ do
        elClass "h3" "ui medium header" $ publicKeyHashLink pkh

        let tz = _account_balance account
        elAttr "div" ("class" =: "balance" <> "data-tooltip" =: "This is the current number of tez in the account that this baker is using.") $ do
          text "Current Balance: "
          text (tez tz)
        dyn_ $ ffor dparameters $ traverse $ \protoInfo -> do
          let bSD = _protoInfo_blockSecurityDeposit protoInfo
              eSD = _protoInfo_endorsementSecurityDeposit protoInfo
              failures = ["baking or endorsement" | tz < min bSD eSD] <> ["baking" | tz < bSD] <> ["endorsement" | tz < eSD]
          case failures of
            (t:_) -> do
              text $ "The identity in use by this baker has not enough tez to pay the security deposit for " <> t <> ". "
                <> "The security deposit for baking is currently " <> tez bSD <> " and for endorsement is currently " <> tez eSD <> ". "
                <> "You'll need to transfer sufficient tez into the account before it can continue."
            [] | tz < 4 * (bSD + eSD) -> do
              text $ "The identity in use by this baker is running somewhat low on tez. "
                <> "The security deposit for baking is currently " <> tez bSD <> " and for endorsement is currently " <> tez eSD <> ". "
                <> "Be sure to keep enough tez in the account to pay the security deposits on blocks you'll be baking or endorsing."
            _ -> blank

        elClass "p" "efficiency" $ do
          elClass "h4" "ui medium header" $ text "Efficiency"
          elClass "td" "right aligned" $ do
            let baked = _bakeEfficiency_bakedBlocks bakeEfficiency
            let rights = _bakeEfficiency_bakingRights bakeEfficiency
            elAttr "span" ("data-tooltip"=:"Number of blocks where this baker either baked or was beaten by higher proiry baker (over past preserved cycles)") $
              text $ tshow baked
            text " of "
            elAttr "span" ("data-tooltip"=:"Number of blocks where this baker had rights to bake at any priority (over past preserved cycles)") $
              text $ tshow rights
            when (rights /= 0) $ do
              text " ("
              text $ tshow (round (fromIntegral baked / fromIntegral rights * 100 :: Double) :: Int)
              text "%)"

clientTab
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasFrontendConfig r
    )
  => Id Client -> URI -> m ()
clientTab cid addr = do
  clients <- watchClient (pure cid)
  dyn_ $ ffor (MMap.lookup cid <$> clients) $ \case
    Nothing -> waitingForResponse
    Just clientInfo -> divClass "ui grid" $ do
      dparameters <- watchProtoInfo
      let report = unJson (_clientInfo_report clientInfo)
          baked = sortBy (flip (comparing _event_time)) (_report_baked report)
          errors = sortBy (flip (comparing _error_time)) (map mkErr (_report_errors report))
      divClass "eight wide column" $ do
        elClass "h3" "ui medium header" $ text $ Uri.render addr
        _ <- divClass "delegates" $ do
          text "ID: "
          sequenceA $ intersperse (text " ") (fmap publicKeyHashLink $ _clientConfig_delegates $ unJson $ _clientInfo_config clientInfo)

        elClass "p" "counts" $ do
          tooltip "This counts the number of errors that this baker has encountered since it began running." $
            text $ "Errors: " <> tshow (length errors)

        for_ (nonEmpty errors) $ \es -> elClass "p" "errors" $ do
          elClass "h4" "ui medium header" $ text "Errors"
          elClass "table" "ui celled striped table" $ do
            el "thead" . el "tr" $ do
              elClass "th" "four wide" $ text "Time"
              el "th" $ text "Message"
            for_ es $ \e -> do
              el "tr" $ do
                el "td" . el "strong" . text . T.pack . formatTime defaultTimeLocale "%Y-%m-%d at %H:%M" . _error_time $ e
                el "td" $ do
                  for_ (T.lines (_error_text e)) $ \t ->
                    divClass "errorLine" $ text t

      divClass "eight wide column" $ do
        divClass "ui medium header" $ text "Activity"
        elAttr "table" ("class" =: "ui celled striped table") $ do
          el "thead" . el "tr" $ do
            elClass "th" "four wide" $ text "Time"
            el "th" $ text "Level"
            el "th" $ text "Block Hash"
            el "th" $ text "Reward"
          for_ baked $ \b -> el "tr" $ do
            el "td" $ el "strong" $ text $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d at %H:%M" $ _event_time b
            el "td" $ text $ tshow $ blockLevel b
            el "td" $ blockHashLink $ pure $ _bakedEvent_hash $ _event_detail b
            el "td" $ dyn_ $ ffor dparameters $ traverse $ \protoInfo ->
              text $ tez $ blockRewards b protoInfo

waitingForResponse :: DomBuilder t m => m ()
waitingForResponse = divClass "ui basic segment" $ divClass "ui active centered inline text loader" $ text "Waiting for response"

semuiTab :: (DomBuilder t m, PostBuild t m, Eq k) => m () -> k -> Demux t k -> Dynamic t Enabled -> m (Event t k)
semuiTab label k currentTab enabled =
  fmap ((k <$) . gate (isEnabled <$> current enabled) . domEvent Click . fst) $
    elDynAttr' "a" `flip` label $ ffor (zipDyn enabled $ demuxed currentTab k) $ \(e,b) ->
      "class" =: T.unwords (["item"] ++ ["disabled" | isDisabled e] ++ ["active" | b])

-- | Control that allows the user to build a list of items.
listInput :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Ord k)
          => Text -- ^ Placeholder for input
          -> (Text -> Bool) -- ^ Input validation
          -> (Dynamic t Text -> m ()) -- ^ Widget builder for each item in the list
          -> Dynamic t (MonoidalMap k Text) -- ^ Items in list
          -> Event t (Either [Text] Text) -- ^ Event of error messages or successful submission
          -> m (Event t Text, Event t (k, Text)) -- ^ Add item event, remove item event
listInput ph validate itemWidget items rsp = divClass "list-input" $ do
  rec (i, addClick) <- divClass "item-input" $ do
        itemInput <- inputElement $ def
          & initialAttributes .~ ("placeholder" =: ph)
          & inputElementConfig_setValue .~ ("" <$ fmapMaybe (^? _Right) rsp)
          & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ validationAttrs
        addItemClick <- fmap (domEvent Click . fst) $ elClass' "span" "add-button" $ elClass "i" "fa fa-plus-circle fa-fw" blank
        return (itemInput, addItemClick)
      let v = value i
          validationResults = leftmost
            [ (\v' -> if T.null v' then Left () else Right (validate v')) <$> updated v
            , Right . isJust . preview _Right <$> rsp
            ]
          validationAttrs = ffor validationResults $ \r -> mapKeysToAttributeName $ case r of
            Left () -> "class" =: Nothing
            Right True -> "class" =: Nothing
            Right False -> "class" =: Just "invalid"
          submit = tag (current v) $ leftmost
            [ () <$ ffilter ((==Enter) . keyCodeLookup . fromIntegral) (domEvent Keypress i)
            , addClick
            ]
      widgetHold_ blank $ ffor rsp $ \case
        Left errs -> for_ errs $ elClass "div" "modal-content__text-input-error" . text
        Right success -> elClass "div" "modal-content__text-input-success" $ text success
      remove <- fmap (fmap (leftmost . Map.elems)) $ elClass "ul" "list-input-items" $
        listWithKey (coerce <$> items) $ \k t -> el "li" $ do
          el "span" $ itemWidget t
          fmap ((,) k) . tag (current t) . domEvent Click . fst <$> el' "span" (elClass "i" "fa fa-fw fa-times-circle" blank)
  return (ffilter validate submit, switch . current $ remove)
