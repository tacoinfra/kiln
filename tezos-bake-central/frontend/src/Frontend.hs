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
import Data.List (intersperse, sortBy)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as Time
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word64)
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
import qualified Reflex.Dom.SemanticUI as SemUi
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (AppWebSocket (..), MonadRhyoliteFrontendWidget, runRhyoliteWidget)
import Rhyolite.Schema (Json (..))
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
import Common.Config (HasFrontendConfig (frontendConfig), frontendConfig_chain)
import qualified Common.Config as Config
import Common.HeadTag (headTag)
import Common.Route (AppRoute)
import Common.Schema hiding (Event)
import Common.Vassal
import ExtraPrelude
import Frontend.Common
import Frontend.Modal.Base (ModalBackdropConfig (..), runModalT, withModals)
import Frontend.Modal.Class (HasModal (ModalM, tellModal))
import Frontend.Settings
import Frontend.Watch

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

isPublicNodeEnabled :: PublicNode -> MonoidalMap PublicNode PublicNodeConfig -> Bool
isPublicNodeEnabled pn pnc = (_publicNodeConfig_enabled <$> MMap.lookup pn pnc) == Just True

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
          elAttr "img" ("src" =: static @"images/logo.svg" <> "class" =: "app-logo") $ return ()
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
        hrefLink "https://gitlab.com/obsidian.systems/tezos-bake-monitor" $
          elAttr "img" ("src" =: static @"images/ObsidianSystemsLogo-ICFP2017.svg" <> "class" =: "credits-obsidian") blank

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

    divClass "ten wide column right aligned" $ do
      headerBell


headerBell :: MonadRhyoliteFrontendWidget Bake t m => m (Event t ())
headerBell = do
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
appContentArea selectedTab =
  dyn_ $ ffor selectedTab $ \case
    -- UITab_Summary -> summaryTab
    UITab_Nodes -> nodesTabOrWelcome
    UITab_Options -> divClass "app-content" $ settingsTab
    -- UITab_Client cid addr -> clientTab cid addr
    -- UITab_Delegate pkh -> delegateTab pkh

nodesTabOrWelcome
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasFrontendConfig r, HasTimeZone r, HasTimer t r
    , HasModal t m, MonadRhyoliteFrontendWidget Bake t (ModalM m)
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
    Nothing -> divClass "app-content app-welcome" waitingForResponse
    Just False -> divClass "app-content app-welcome" welcomeScreen
    Just True -> divClass "app-content" nodesTab

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
  filterDyn <- holdUniqDyn <=< el "div" $ radioLabels AlertsFilter_All
    [ (AlertsFilter_All, text "All")
    , (AlertsFilter_UnresolvedOnly, text "Unresolved")
    , (AlertsFilter_ResolvedOnly, text "Resolved")
    ]

  filteredErrors <- holdUniqDyn $ liftA2
    (\errors filterFn -> MMap.filter (filterFn . fst) errors)
    errorsDyn
    (passesFilter <$> filterDyn)

  SemUi.divider def

  let
    (otherErrors, nodeErrors) = splitDynPure $ partitionErrors <$> filteredErrors

    nodeErrorsWithNode :: Dynamic t (MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView, Node))
    nodeErrorsWithNode = liftA2 joinNodeErrors nodeErrors nodesDyn

    combinedErrors = liftA2 (MMap.unionWith (error "Overlapping keys after partition"))
      (fmap (\(a, b) -> (a, b, Nothing)) `fmap` otherErrors)
      (fmap (_3 %~ Just) `fmap` nodeErrorsWithNode)

    showWhenErrors p attrs = elDynAttr "div" (ffor combinedErrors $ \ce -> attrs <> bool ("style" =: "display: none") Map.empty (p ce))

  showWhenErrors null ("class" =: "no-notifications") $ text "No notifications"
  showWhenErrors (not . null) Map.empty $ void $ SemUi.segment
    (def
      & SemUi.classes SemUi.|~ "app-notifications-list"
      & SemUi.segmentConfig_vertical SemUi.|~ True
      & SemUi.segmentConfig_basic SemUi.|~ True
    ) $
    listWithKey (errorsByTime Down <$> combinedErrors) $ \_ vDyn ->
      dyn_ $ ffor vDyn $ \v@(log, _, _) -> do
        divClass ("app-notification ui message " <> if isJust $ _errorLog_stopped log then "success" else "error") $ do
          logEntry v
          timestamped ("First seen", _errorLog_started log)
          timestamped $ maybe ("Last seen", _errorLog_lastSeen log) ("Stopped",) $ _errorLog_stopped log
  where
    timestamped (lbl,ts) = el "div" $ do
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
    logEntry (_, specificLog, node') =
      let header = divClass "header" . text
          nodeLabel n = el "div" $ do
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
    _ <- listWithKey (coerceDynamic nodes) $ \_ node -> divClass "item bullet-before" $ do
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

nodesTab
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasFrontendConfig r, HasTimeZone r, HasTimer t r
    , HasModal t m, MonadRhyoliteFrontendWidget Bake t (ModalM m)
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
              (\ev -> PublicRequest_RemoveNode . _node_address <$> current vDyn <@ ev)
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
              (\ev -> flip PublicRequest_SetPublicNodeConfig False <$> current source <@ ev)
              (Just . mkVeryBlockLike)
              Nothing
              Nothing
              Nothing
              vDyn

    nodeTile
      :: m () -- ^ Title
      -> Dynamic t (Maybe Text) -- ^ Subtitle
      -> (Event t () -> Event t (PublicRequest Bake ())) -- ^ Construct an API request with an 'Event' to remove this node.
      -> (a -> Maybe VeryBlockLike) -- ^ Function to get block information from a node
      -> Maybe (Dynamic t [m ()]) -- ^ (Optional) Function to build list of error messages for this node
      -> Maybe (a -> Maybe Word64) -- ^ (Optional) Function to get the peer count of the node
      -> Maybe (a -> NetworkStat) -- ^ (Optional) Function to get the network stats of the node
      -> Dynamic t a -- ^ Node
      -> m ()
    nodeTile title subtitle mkRemoveReq getBlock errors' getPeerCount' getNetworkStats' node = do
      b <- maybeDyn $ getBlock <$> node
      divClass "ui card node-tile" $ divClass "content" $ do
        divClass "menu-section" $ divClass "span" $ mdo
          menuTransition <- manageMenu (domEvent Click iconEl) uiEl
          (iconEl, _) <- elClass' "i" "ui icon icon-ellipsis" blank
          (uiEl, _) <- SemUi.ui' "span" (def
            & SemUi.classes .~ "ui popup bottom center"
            & SemUi.action .~ Just def
              { SemUi._action_initialDirection = SemUi.Out
              , SemUi._action_transition = ffor menuTransition $ \transition -> SemUi.Transition SemUi.Drop (Just transition) (def { SemUi._transitionConfig_duration = 0.2 })
              , SemUi._action_transitionStateClasses = SemUi.forceVisible
              }) $ do
                SemUi.list (def & SemUi.listConfig_link SemUi.|~ True & SemUi.listConfig_divided SemUi.|~ True) $ do
                  remove <- fmap (domEvent Click . fst) $ SemUi.listItem' def $ text "Remove Node"
                  tellModal $ remove $> removeNodeModal mkRemoveReq
          pure ()

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

    removeNodeModal mkRemoveReq = cancelableModal $ \close -> do
      el "h3" $ text "Remove this node?"
      el "p" $ text "You can always add this node again from the \"Add Node\" button."
      sure <- divClass "buttons" $ uiButton "primary" "Remove Node"
      response <- requestingIdentity $ public <$> mkRemoveReq sure
      pure $ leftmost [response, close]

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
