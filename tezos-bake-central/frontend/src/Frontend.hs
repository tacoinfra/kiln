{-# LANGUAGE ConstraintKinds #-}
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

import Control.Lens ((<>~), imap)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader (ReaderT)
import Data.Dependent.Sum (DSum(..), EqTag)
import Data.Functor.Infix hiding ((<&>))
import Data.Functor.Compose (Compose(..))
import Data.List (intersperse, sortBy)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import Data.String (IsString)
import qualified Data.Text as T
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
import Reflex.Dom.Form.Widgets (formItem, formItem')
import qualified Reflex.Dom.SemanticUI as SemUi
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (AppWebSocket (..), MonadRhyoliteFrontendWidget, runRhyoliteWidget)
import Rhyolite.Schema (Json (..), Id(..))
import Rhyolite.WebSocket (WebSocketUrl (..))
import Safe (minimumByMay)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.NodeRPC.Sources (PublicNode (..), tzScanUri)
import Tezos.NodeRPC.Types
import Tezos.Types

import Common (humanBytes, unixEpoch)
import Common.Alerts (AlertsFilter(..), badNodeHeadMessage
                     , bakerMissedDescriptions, bakerDeactivatedDescriptions, bakerDeactivationRiskDescriptions)
import Common.Api
import Common.App
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import Common.Config (HasFrontendConfig (frontendConfig), frontendConfig_chain)
import qualified Common.Config as Config
import Common.HeadTag (headTag)
import Common.Route (AppRoute(..))
import Common.Schema hiding (Event)
import ExtraPrelude
import Frontend.Common
import Frontend.Modal.Base (ModalBackdropConfig (..), runModalT, withModals)
import Frontend.Modal.Class (HasModal (ModalM, tellModal))
import Frontend.Settings
import Frontend.Watch

import Obelisk.Route.Frontend

type RouteConstraints t r m =
  ( Routed t (R r) m
  , RouteToUrl (R r) m
  , SetRoute t (R r) m
  , EqTag r Identity
  )

frontend :: Frontend (R AppRoute)
frontend = Frontend
  { _frontend_head = headTag
  , _frontend_body = prerender blank frontendBody
  }

frontendBody
  :: forall m t x.
    ( MonadWidget t m
    , HasJS x m
    , MonadFix (Performable m)
    , PrimMonad m
    , RouteConstraints t AppRoute m
    )
  => m ()
frontendBody = void $ do
  let getExecutableConfig = Obelisk.ExecutableConfig.get . ("config/" <>)
  route :: URI <- liftIO (getExecutableConfig $ T.pack Config.route) >>= \case
    Just r -> return $ fromMaybe (error $ "Unable to parse injected route: " <> show r) $ Uri.mkURI $ T.strip r
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
    listenPath = fromMaybe (error "sulk") $ Uri.mkPathPiece "listen" -- TODO: try to use BackendRoute_Listen instead

    wsUrl = WebSocketUrl
      <$> (T.replace "http" "ws" <$> routeScheme)
      <*> (Uri.unRText . Uri.authHost <$> routeAuthority)
      <*> pure (fromIntegral $ fromMaybe 80 wsPort)
      <*> pure (renderPathPieces [listenPath])

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

appMain
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadRhyoliteFrontendWidget Bake t (ModalM m), HasModal t m
    , MonadJSM (Performable m)
    , MonadJSM m
    , MonadReader r m, HasFrontendConfig r, HasTimer t r, HasTimeZone r
    , RouteConstraints t AppRoute m
    )
  => m ()
appMain = do
  elClass "div" "app-frame" $ mdo
    appSidebar

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
          e <- divClass "sidebar-title" $ do
            divClass "ui left floated header" $ text "Notifications"
            divClass "ui right floated header" $ domEvent Click <$> SemUi.icon' "icon-arrow-right blue" def
          liveErrorsWidget
          pure e)
      -- Accompanying content
      $ do
        e <- appHeader
        appContentArea
        pure e
    pure ()


appName :: Text
appName = "Kiln"

appSidebar
  :: ( MonadRhyoliteFrontendWidget Bake t m
     , MonadRhyoliteFrontendWidget Bake t (ModalM m)
     , HasModal t m
     , RouteConstraints t AppRoute m
     )
  => m ()
appSidebar = do
  SemUi.segment
    (def
      & SemUi.classes SemUi.|~ "app-sidebar"
      & SemUi.segmentConfig_vertical SemUi.|~ True
      & SemUi.segmentConfig_basic SemUi.|~ True
      )
    $ do
        appSideHeader
        appGutter
        appSideFooter

routeSelector' :: (DomBuilder t m, SemUi.HasElConfig t e, RouteConstraints t r m)
               => R r -> (e -> ch -> m a) -> e -> ch -> m a
routeSelector' dest con cfg child = do
  r <- askRoute
  let activated = ffor r $ bool "" "active" . (== dest)
  routeLink dest $ con (cfg & SemUi.classes <>~ SemUi.Dyn activated) child

routeSelector :: (DomBuilder t m, SemUi.HasElConfig t e, RouteConstraints t r m)
              => R r -> (e -> ch -> m (a,b)) -> e -> ch -> m b
routeSelector dest con cfg child = snd <$> routeSelector' dest con cfg child

appSideHeader :: (MonadRhyoliteFrontendWidget Bake t m, RouteConstraints t AppRoute m) => m ()
appSideHeader =
  SemUi.segment
    (def
      & SemUi.classes SemUi.|~ "app-side-header"
      & SemUi.segmentConfig_basic SemUi.|~ True
      )
    $ do
        SemUi.header def $ do
          kilnLogo
          text appName
        SemUi.menu
          (def
            & SemUi.menuConfig_vertical SemUi.|~ True
            & SemUi.menuConfig_fluid SemUi.|~ True
            )
          $ do
              routeSelector (AppRoute_Nodes :/ ()) SemUi.menuItem' def $ do
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
    $ do
        bakersList
        nodesList

appSideFooter :: (MonadRhyoliteFrontendWidget Bake t m, RouteConstraints t AppRoute m) => m ()
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
              routeSelector (AppRoute_Options :/ ()) SemUi.menuItem' def $ do
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
appHeader = SemUi.segment (def & SemUi.segmentConfig_vertical SemUi.|~ True) $ do
  alertWindow <- fmap Set.singleton <$> thirtySixHoursToInfinity
  collectedNodesStatus <- watchCollectiveNodesStatus alertWindow
  let disconnected = isLeft <$> collectedNodesStatus
  divClass "ui stackable grid" $ do
    divClass "twelve wide column topbar" $ do
      divClass "ui horizontal list" $ do
        latestHead <- watchLatestHead
        let info faded title body = divClass "item" $
              elDynAttr "div" (bool Map.empty ("class" =: "faded") <$> faded) $ divClass "content" $ do
                divClass "header" $ text title
                body

        info (pure False) "Network" $ text . showChain =<< asks (^. frontendConfig . frontendConfig_chain)

        protoInfo <- watchProtoInfo
        cyc <- holdUniqDyn $ (liftA2.liftA2) levelToCycle protoInfo $ (fmap.fmap) (view level) latestHead
        whenJustDyn cyc $ \c -> info disconnected "Cycle" $
          text $ tshow $ unCycle c

        whenJustDyn latestHead $ \b -> info disconnected "Block" $ el "span" $ do
          text $ tshow (unRawLevel $ b ^. level)
          elClass "span" "metadescription" $ text " Baked "
          localHumanizedTimestamp (pure Nothing) $ pure $ b ^. timestamp

      dyn_ $ ffor disconnected $ flip when $ tooltipped TooltipPos_BottomCenter disconnectedTooltip $
        SemUi.icon "icon-disconnected"
        (def
          & SemUi.iconConfig_color SemUi.|?~ SemUi.Red
          & SemUi.iconConfig_size SemUi.|?~ SemUi.Big
          )

    divClass "four wide column right aligned" $ do
      headerBell

  where
    disconnectedTooltip = divClass "disconnected-tooltip" $ do
      el "p" $ divClass "tooltip-title" $ text "Disconnected from the block chain."
      divClass "tooltip-description" $ do
        el "p" $ text "Kiln cannot gather data if no nodes are synced with the blockchain. Data shown is stale."
        el "p" $ do
          text "Add a node from the left panel or make sure any nodes you’ve already added are"
          icon "circle small green"
          text "healthy."

headerBell :: MonadRhyoliteFrontendWidget Bake t m => m (Event t ())
headerBell = do
  alertCount <- holdUniqDyn =<< fmap (fromMaybe 0) <$> watchAlertCount
  (e,_) <- SemUi.ui' "span"
    (def
      & SemUi.classes .~ (SemUi.Dyn $ ffor alertCount $ ((<>) "ui circular label link ") . bool "basic" "red" . (>0))
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
    , Routed t (R AppRoute) m
    )
  => m ()
appContentArea = do
  r <- askRoute
  flip runRoutedT r $ subRoute_ $ \case
    AppRoute_Index -> nodesTabOrWelcome
    AppRoute_Nodes -> nodesTabOrWelcome
    AppRoute_Options -> divClass "app-content" settingsTab

nodesTabOrWelcome
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasFrontendConfig r, HasTimeZone r, HasTimer t r
    , HasModal t m, MonadRhyoliteFrontendWidget Bake t (ModalM m)
    )
  => m ()
nodesTabOrWelcome = do
  _clientAddresses <- watchClientAddresses
  bakersMaybe <- watchBakerAddressesValid
  publicNodesMaybe <- watchPublicNodeConfigValid
  nodesMaybe <- watchNodeAddressesValid
  -- doing some straightforward calculations, but inside a Dynamic and a Maybe
  let haveBakersMaybe =
        (fmap . fmap) (not . null) bakersMaybe
      haveNodesMaybe =
        (liftA2 . liftA2) ((||) . any _publicNodeConfig_enabled . toList) publicNodesMaybe $
        (fmap . fmap) (not . null) nodesMaybe
  haveBakersHaveNodesMaybe <- holdUniqDyn $
    (liftA2 . liftA2) (,) haveBakersMaybe haveNodesMaybe

  mchain <- asks $ preview (frontendConfig . frontendConfig_chain . _Left)
  whenJust mchain $ \chain -> do
    let everythingWindow = pure $ Set.singleton $ ClosedInterval LowerInfinity UpperInfinity
    dXs <- watchErrors (pure $ Just AlertsFilter_UnresolvedOnly) everythingWindow
    mUpgradeLog <- holdUniqDyn $ ffor dXs $ \xs -> listToMaybe $ toList $ flip MMap.mapMaybeWithKey xs $ \_ -> \case
      (ErrorLog { _errorLog_stopped = Nothing }, ErrorLogView_NetworkUpdate ua) -> do
        guard $ _errorLogNetworkUpdate_namedChain ua == chain
        return ua
      _ -> Nothing
    dyn_ $ ffor mUpgradeLog $ \case
      Just elua -> divClass "dashboard-section dashboard-section-global-alerts" $ do
        SemUi.segment (def & SemUi.classes SemUi.|~ "dashboard-section-overview") $
          networkUpdateAlert elua
      Nothing -> return ()

  dyn_ $ ffor haveBakersHaveNodesMaybe $ \case
    Nothing -> divClass "app-content app-welcome" waitingForResponse
    Just (False,False) -> divClass "app-content app-welcome" welcomeScreen
    Just (haveBakers, haveNodes) -> divClass "app-content" $ do
      when haveBakers bakersTab
      when haveNodes nodesTab

networkUpdateAlert :: (MonadRhyoliteFrontendWidget Bake t m) => ErrorLogNetworkUpdate -> m ()
networkUpdateAlert elua = do
  let namedChain = showNamedChain $ _errorLogNetworkUpdate_namedChain elua
  renderResolvableSplashAlert
    (icon "icon-alert-badge big blue")
    ("New Tezos '" <> namedChain <> "' software version.")
    (do el "p" $ text $ mconcat
          [ "There is a new version of the ", namedChain
          , " software available on GitLab. To find further information about this release check Obsidian's Baker Slack channel, the Tezos Riot chat, or other social channels."
          ]
        el "p" $ do
          text "Get the new software here  🡒  "
          let url = "https://gitlab.com/tezos/tezos/tree/" <> namedChain -- FIXME the url should be based on the project id
          elAttr "a" ("href" =: url <> "target" =: "_blank" <> "rel" =: "noopener") $ text url)
    (Just $ LogTag_NetworkUpdate :=> pure elua)

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

data ErrorLogView' = ErrorLogView' ErrorLogView (Maybe NodeSummary)

-- | Different constructor name because presumably more would be added
newtype SynthError
  = SynthError_BakersInformationDown (NonEmpty PublicKeyHash)
  deriving (Eq, Ord, Show)

liveErrorsWidget
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasFrontendConfig r, HasTimeZone r, HasTimer t r
    )
  => m ()
liveErrorsWidget = void $ do
  let everythingWindow = pure $ Set.singleton $ ClosedInterval LowerInfinity UpperInfinity
  nodesDyn <- watchNodeAddresses
  alertWindow <- fmap Set.singleton <$> thirtySixHoursToInfinity
  filterDyn <- holdUniqDyn <=< el "div" $ radioLabels AlertsFilter_All
    [ (AlertsFilter_All, text "All")
    , (AlertsFilter_UnresolvedOnly, text "Unresolved")
    , (AlertsFilter_ResolvedOnly, text "Resolved")
    ]
  let includesFilter f = \case
        AlertsFilter_All -> True
        f' -> f == f'
      soleFilter f filterSel = do
        guard $ includesFilter f filterSel
        return f
      unresolvedFilter = soleFilter AlertsFilter_UnresolvedOnly <$> filterDyn
      resolvedFilter = soleFilter AlertsFilter_UnresolvedOnly <$> filterDyn
  unresolvedErrorsDyn <- MMap.getMonoidalMap <$$> watchErrors unresolvedFilter everythingWindow
  resolvedErrorsDyn <- MMap.getMonoidalMap <$$> watchErrors resolvedFilter alertWindow
  let errorsDyn = zipDynWith (<>) unresolvedErrorsDyn resolvedErrorsDyn
  filteredErrors <- holdUniqDyn $ liftA2
    (\errors filterFn -> Map.filter (filterFn . fst) errors)
    errorsDyn
    (passesFilter <$> filterDyn)

  SemUi.divider def

  collectedNodesStatus <- watchCollectiveNodesStatus alertWindow
  let dAllNodesDownTime = ffor collectedNodesStatus $ \case
        Left (CollectiveNodesFailure_AllNodesDownSince t) -> Just t
        _ -> Nothing -- TODO think about alert for the no configured nodes case
  dTimer <- asks $ view timer
  -- TODO: PERF: only watch when we need to for `SyntheticError_allNodesDown`
  dBakerKeys <- MMap.keys <$$> watchBakerAddresses

  let
    combinedRealErrors
      :: Dynamic t (Map.Map (Id ErrorLog) (ErrorLog, ErrorLogView'))
    combinedRealErrors = ffor2 filteredErrors nodesDyn $ \errors nodes ->
      ffor errors $ \(errorLog, errorLogView) -> let
        mNode = do
          nodeId <- nodeIdForNodeErrorLogView <$> nodeErrorViewOnly errorLogView
          MMap.lookup nodeId nodes
      in (errorLog, ErrorLogView' errorLogView mNode)

    -- There is no `Id SynthError` so just use whole thing.
    synthErrors
      :: Dynamic t (Map.Map SynthError (ErrorLog, SynthError))
    synthErrors = ffor3 dBakerKeys dTimer dAllNodesDownTime $
      \bakerKeys now allNodesDownTime ->
        fromMaybe mempty $ do
          keys1 <- NEL.nonEmpty bakerKeys
          since <- allNodesDownTime
          let k = SynthError_BakersInformationDown keys1
          pure $ Map.singleton k $ (, k) $
            ErrorLog
              { _errorLog_started = since
              , _errorLog_stopped = Nothing
              , _errorLog_lastSeen = now
              , _errorLog_noticeSentAt = Nothing
              }

    combinedErrors
      :: Dynamic t (Map.Map (Down (Time.UTCTime, Either (Id ErrorLog) SynthError))
                            (ErrorLog, m ()))
    combinedErrors = fold
      [ fmap (errorsByTime Left . (fmap . fmap) logEntry) combinedRealErrors
      , fmap (errorsByTime Right . (fmap . fmap) synthEntry) synthErrors
      ]

    errorsByTime
      :: Ord k1
      => (k0 -> k1)
      -> Map.Map k0 (ErrorLog, v)
      -> Map.Map (Down (Time.UTCTime, k1)) (ErrorLog, v)
    errorsByTime inj errors = Map.fromList
      [ (Down (_errorLog_started l, inj elId), row)
      | (elId, row@(l, _)) <- Map.toList errors
      ]

    showWhenErrors p attrs = elDynAttr "div" (ffor combinedErrors $ \ce -> attrs <> bool ("style" =: "display: none") Map.empty (p ce))

  showWhenErrors null ("class" =: "no-notifications") $ text "No notifications"
  showWhenErrors (not . null) Map.empty $ void $ SemUi.segment
    (def
      & SemUi.classes SemUi.|~ "app-notifications-list"
      & SemUi.segmentConfig_vertical SemUi.|~ True
      & SemUi.segmentConfig_basic SemUi.|~ True
    ) $
    listWithKey combinedErrors $ \_ vDyn ->
      dyn_ $ ffor vDyn $ \(log, domBuilder) -> do
        divClass ("app-notification ui message " <> if isJust $ _errorLog_stopped log then "success" else "error") $ do
          domBuilder
          timestamped ("First seen", _errorLog_started log)
          timestamped $ maybe ("Last seen", _errorLog_lastSeen log) ("Stopped",) $ _errorLog_stopped log
  where
    timestamped (lbl,ts) = el "div" $ do
      el "label" $ text lbl
      localTimestamp $ pure ts

    passesFilter filterSelection log =
      filterSelection == AlertsFilter_All
        || filterSelection == AlertsFilter_UnresolvedOnly && not isResolved
        || filterSelection == AlertsFilter_ResolvedOnly && isResolved
      where isResolved = isJust $ _errorLog_stopped log

    header = divClass "header" . text

    synthEntry :: SynthError -> m ()
    synthEntry (SynthError_BakersInformationDown pkhs) = do
      header "Cannot gather baker data."
      errorLabel "My Bakers" $ toPublicKeyHashText <$> pkhs
      el "div" $
        text "Kiln cannot gather data about this baker if no nodes are synced with the blockchain."

    logEntry :: ErrorLogView' -> m ()
    logEntry (ErrorLogView' specificLog node') =
        case specificLog of
          ErrorLogView_NodeError ne -> case ne of
            NodeErrorLogView_InaccessibleNode (ErrorLogInaccessibleNode _ _ address alias) -> for_ node' $ \n -> do
              header $ "Unable to connect to node" <> maybe "" (" " <>) alias <> " at " <> Uri.render address
              nodeLabel n

            NodeErrorLogView_NodeWrongChain (ErrorLogNodeWrongChain _ _ address alias expectedChainId actualChainId) ->
              for_ node' $ \n -> do
                header $ "Node on wrong network: " <> fromMaybe (Uri.render address) alias
                nodeLabel n
                el "div" $
                  text $ "The node is running on network " <> toBase58Text actualChainId <> " but is expected to be on " <> toBase58Text expectedChainId <> "."

            NodeErrorLogView_BadNodeHead l -> do
              for_ node' $ \n -> do
                let (heading, message) = badNodeHeadMessage text (blockHashLink . pure) l
                let (primary, _) = nodeSummaryIdentification n
                header $ heading <> ": " <> primary
                nodeLabel n
                el "div" message

            NodeErrorLogView_NodeInvalidPeerCount (ErrorLogNodeInvalidPeerCount _ _ minPeerCount _) -> do
              for_ node' $ \n -> do
                let (main, _) = nodeSummaryIdentification n
                header $ "Node has too few peers: " <> main
                nodeLabel n
                el "div" $ text $
                  "This node has fewer peers than the configured minimum of " <> tshow minPeerCount <> "."

          ErrorLogView_BakerError ne -> case ne of
            BakerErrorLogView_BakerDeactivated log -> renderBakerError
              (bakerDeactivatedDescriptions log)
              (_errorLogBakerDeactivated_publicKeyHash log)
            BakerErrorLogView_BakerDeactivationRisk log -> renderBakerError
              (bakerDeactivationRiskDescriptions log)
              (_errorLogBakerDeactivationRisk_publicKeyHash log)

            BakerErrorLogView_MultipleBakersForSameBaker ErrorLogMultipleBakersForSameBaker{} -> do
              header "Multiple bakers for same baker" -- TODO Fill this out
            BakerErrorLogView_BakerMissed elbm -> do
              let
                rightTxt = case _errorLogBakerMissed_right elbm of
                  RightKind_Baking -> "a bake"
                  RightKind_Endorsing -> "an endorsement"
              header $ "Missed " <> rightTxt <> " opportunity"
              el "div" $ do
                text $ toPublicKeyHashText (unId $ _errorLogBakerMissed_baker elbm)

          ErrorLogView_BakerNoHeartbeat (ErrorLogBakerNoHeartbeat _ lastLevel lastBlockHash _) -> do
            header "Baker lagging behind" -- TODO Show client address
            el "div" $ do
              text "Last block level seen: "
              blockHashLinkAs (pure lastBlockHash) (text $ tshow lastLevel)

          ErrorLogView_NetworkUpdate (ErrorLogNetworkUpdate { _errorLogNetworkUpdate_namedChain = namedChain }) -> do
            let chainText = "'" <> showNamedChain namedChain <> "'"
            header $ T.unwords ["New", chainText, "version."]
            el "div" $ do
              text $ "There is a new version of the " <> chainText <> " software available on GitLab."
    renderBakerError dsc pkh = do
      bakersDyn <- watchBakerAddresses
      header $ _bakerErrorDescriptions_title dsc
      dyn_ $ ffor bakersDyn $ maybe blank (bakerSummaryLabel pkh) . MMap.lookup pkh
      el "div" $ text $ _bakerErrorDescriptions_notification dsc

pluralOf :: Text -> Text
pluralOf = (<> "s") -- good enough for all existing uses, lol

data MonitoredStatus
  = MonitoredStatus_Healthy
  | MonitoredStatus_Unhealthy
  | MonitoredStatus_Unknown
  deriving (Eq, Ord, Bounded, Enum, Show)

statusColor :: IsString a => MonitoredStatus -> a
statusColor = \case
  MonitoredStatus_Healthy -> "green"
  MonitoredStatus_Unhealthy -> "red"
  MonitoredStatus_Unknown -> "grey"

sidebarList :: forall t m k.
  ( MonadRhyoliteFrontendWidget Bake t m
  , MonadRhyoliteFrontendWidget Bake t (ModalM m)
  , HasModal t m
  , Ord k
  )
  => Text -> Dynamic t (MonoidalMap k ((Text, Maybe Text), MonitoredStatus, Bool)) -> (Event t () -> ModalM m (Event t ())) -> m ()
sidebarList name nodes' modal = do
  let nodes :: Dynamic t (Map.Map k ((Text, Maybe Text), MonitoredStatus, Bool)) = coerceDynamic nodes'
  divClass "ui sub header" $ text (pluralOf name)
  divClass "ui list" $ do
    _ <- listWithKey nodes $ \_ node -> divClass "item bullet-before" $ do
      -- let
      --   addressDyn = view _1 <$> node
      --   aliasDyn = view _2 <$> node
      --   monitoredStatus = view _3 <$> node

      let color = (\(_,s,_) -> statusColor s) <$> node
      _ <- SemUi.ui' "i" (def & SemUi.elConfigClasses .~ "icon circle tiny" <> SemUi.Dyn color) blank
      divClass "content" $ do
        let (title, subtitle) = splitDynPure $ ffor node $ \(tst, _, _) -> tst
        divClass "header" $ do
          dynText title
          useSymbol <- holdUniqDyn $ view _3 <$> node
          let tooltippedKilnLogo = tooltipped TooltipPos_BottomCenter (text $ "This " <> name <> " is run by Kiln.") kilnLogo
          divClass "ui image right floated" $ dyn_ $ bool blank tooltippedKilnLogo <$> useSymbol

        divClass "description" $ dynText $ fromMaybe "" <$> subtitle

    openAddItemOptions <- buttonIconWithInfoCls "icon-plus" "modalopener fluid" ("Add " <> name) ("Configure Monitored " <> pluralOf name)
    tellModal $ (openAddItemOptions $>) $ cancelableModalWithClasses ["add-" <> T.toLower name] modal

bakerStatus :: BakerSummary -> MonitoredStatus
bakerStatus bakerSummary
  | _bakerSummary_alertCount bakerSummary > 0 = MonitoredStatus_Unhealthy
  | _bakerSummary_nextRightFetchRemaining bakerSummary > 0 = MonitoredStatus_Unknown
  | otherwise = MonitoredStatus_Healthy

bakersList ::
  ( MonadRhyoliteFrontendWidget Bake t m
  , MonadRhyoliteFrontendWidget Bake t (ModalM m)
  , HasModal t m
  )
  => m ()
bakersList = do
  bakers <- imap (\pkh b ->
    ( bakerSummaryIdentification (pkh, b)
    , bakerStatus b
    , False)
    ) <$$> watchBakerAddresses
  sidebarList "Baker" bakers addBakerModal

addBakerModal :: MonadRhyoliteFrontendWidget Bake t m => Event t () -> m (Event t ())
addBakerModal close = mdo
  el "h3" $ text "Add Baker"
  divClass "basic small segment" $ text
    "Enter a Baker address to begin monitoring."
  addE <- formWithReset "Add Baker" "Begin monitoring the baker at the address entered." blank added $ do
    zipFields
      (pkhField "Baker Wallet Address" "tz1bvNMQ95vfAYtG8193ymshqjSvmxiCUuR5")
      (aliasField "My Baker")
  added <- requestingIdentity $ fmap (\(addr,alias) -> public (PublicRequest_AddBaker addr alias)) addE
  pure $ leftmost [added, close]

nodesList ::
  ( MonadRhyoliteFrontendWidget Bake t m
  , MonadRhyoliteFrontendWidget Bake t (ModalM m)
  , HasModal t m
  )
  => m ()
nodesList = do
  let nodeStatus = \case
        0 -> MonitoredStatus_Healthy
        _ -> MonitoredStatus_Unhealthy
  nodes <- (\ns -> ( nodeSummaryIdentification ns
                   , nodeStatus (_nodeSummary_alertCount ns)
                   , isRight $ _nodeSummary_node ns))
           <$$$> watchNodeAddresses
  sidebarList "Node" nodes addNodeModal

addNodeModal :: MonadRhyoliteFrontendWidget Bake t m => Event t () -> m (Event t ())
addNodeModal close = do
  divClass "ui header" $ text "Add Nodes"
  divClass "ui grid divided" $ do
    addInternal *> addExternal <* addPublic

  where
    section header explanation = do
      elClass "h5" "ui header" $ text header
      divClass "explanation" $ text explanation

    addPublic = do
      divClass "seven wide column" $ divClass "section" $ do
        section
          "Connect to a Public Node"
          "We recommend adding all public nodes to enhance monitoring accuracy."
        publicNodeOptions

    addInternal = do
      divClass "five wide column" $ divClass "section" $ do
        section
          "Launch a Kiln node"
          "Launch a node that is managed from within Kiln. Required if you intend to use Kiln to bake. Kiln only supports running a single node."
        node <- maybeDyn =<< watchInternalNode
        dyn_ $ ffor node $ \case
          Nothing -> do
            launch <- uiButton "primary" "Launch Node"
            void $ requestingIdentity $ launch $> public PublicRequest_AddInternalNode

          Just _ -> do
            kilnLogo
            text "A Kiln node is running."

    addExternal = do
     divClass "four wide column" $ divClass "section" $ mdo
       let feedback = elDynAttr "div" (ffor showSuccess $ ("class" =: "feedback" <>) . bool ("style" =: "display:none") mempty) $ do
             icon "check blue"
             text "Node added!"

       section
         "Connect via address"
         "Via RPC monitor nodes running locally or remotely."

       addE <- formWithReset "Add Node" "Begin monitoring the node at the address entered." feedback showMsg $ do
         zipFields
           (zipFields
             (formItem' "required" $ uriField "Node Address" "127.0.0.1:8732")
             (formItem $ aliasField "Public Facing Node 1"))
           (formItem minConnectionsField)

       showMsg <- requestingIdentity $ fmap (\((addr,alias),minPeerConn) -> public (PublicRequest_AddExternalNode addr alias minPeerConn)) addE
       hideMsg <- delay 3 showMsg
       showSuccess <- holdDyn False $ leftmost [True <$ showMsg, False <$ hideMsg]
       pure close


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
        (def & SemUi.elConfigClasses .~ "public-node ui padded divided grid " <> (SemUi.Dyn $ bool "" "active" <$> pnActiveDyn)) $ divClass "row" $ do
      divClass "four wide column label" $ divClass "ui center aligned icon header" $ do
        SemUi.ui "i" (def & SemUi.elConfigClasses .~ (SemUi.Dyn $ bool "" "icon icon-check" <$> pnActiveDyn)) blank
        dynText $ bool "Add Node" "Added" <$> pnActiveDyn
      divClass "twelve wide column" $ do
        divClass "twelve wide column" $ do
          divClass "header" $ text $ showPublicNode pn
          divClass "description" $ text $ describePublicNode pn

    let toggled = tag (current $ not . isPublicNodeEnabled pn <$> pncDyn) (domEvent Click element')
    void $ requestingIdentity $ ffor toggled $ \enabled -> public (PublicRequest_SetPublicNodeConfig pn enabled)

thirtySixHoursToInfinity
  ::
  ( MonadReader r m, HasTimer t r
  , MonadRhyoliteFrontendWidget Bake t m
  )
  => m (Dynamic t (ClosedInterval (WithInfinity Time.UTCTime)))
thirtySixHoursToInfinity = do
  let thirtySixHoursAgo = (-1.5) * Time.nominalDay
  let oneHour = 60 * 60
  let quantize = flip Time.addUTCTime unixEpoch . (* oneHour) . fromIntegral @Integer . floor . (/ oneHour) . flip Time.diffUTCTime unixEpoch
  time <- holdUniqDyn =<< fmap quantize <$> asks (view timer)

  return $ fmap (flip ClosedInterval UpperInfinity . Bounded . Time.addUTCTime thirtySixHoursAgo) time

tileMenuEntry :: (DomBuilder t m, MonadFix m, MonadIO (Performable m)
                 , PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadHold t m)
              => Text -> m (Event t ())
tileMenuEntry = fmap (domEvent Click . fst) . SemUi.listItem' def . text

tileMenuEntryModal :: (DomBuilder t m, MonadFix m, MonadIO (Performable m)
                      , PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadHold t m
                      , HasModal t m)
                   => Text -> (Event t () -> ModalM m (Event t ())) -> m ()
tileMenuEntryModal txt modal = do
  open <- tileMenuEntry txt
  tellModal $ open $> modal

nodesTab
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasFrontendConfig r, HasTimeZone r, HasTimer t r
    , HasModal t m, MonadRhyoliteFrontendWidget Bake t (ModalM m)
    )
  => m ()
nodesTab =
  divClass "dashboard-section dashboard-section-nodes" $ do
    elClass "h4" "dashboard-section-title" $ text "Nodes"
    nodesDyn <- watchNodeAddresses
    nodeTilesWidget nodesDyn
  where
    nodeTilesWidget :: Dynamic t (MonoidalMap (Id Node) NodeSummary) -> m ()
    nodeTilesWidget nodesDyn = do
      publicNodeConfigDyn <- watchPublicNodeConfig
      rawPublicNodesDyn <- watchPublicNodeHeads
      let
        publicNodesDyn = zipDynWith (\pnc ->
          MMap.filter (flip isPublicNodeEnabled pnc . _publicNodeHead_source)
          ) publicNodeConfigDyn rawPublicNodesDyn

      useBlocker <- holdUniqDyn $ ffor (zipDyn publicNodesDyn nodesDyn) $ \(pn,n) -> MMap.null pn && MMap.null n
      -- let alertWindow = ClosedInterval LowerInfinity UpperInfinity
      alertWindow <- fmap Set.singleton <$> thirtySixHoursToInfinity

      dyn_ $ ffor useBlocker $ \case
        True -> waitingForResponse
        False -> divClass "ui stackable cards" $ do
          ebn <- snd <$$$$> watchErrorsByNode alertWindow

          let
            errorMessages nodeId = do
              unresolvedAlertsForThisNode <- holdUniqDyn $ foldMap toList . MMap.lookup nodeId <$> ebn
              pure $ ffor unresolvedAlertsForThisNode $ fmap $ \case
                NodeErrorLogView_InaccessibleNode{} -> text "Unable to connect."
                NodeErrorLogView_NodeWrongChain{} -> text "On wrong network."
                NodeErrorLogView_NodeInvalidPeerCount{} -> text "Node has too few peers."
                NodeErrorLogView_BadNodeHead l -> text $
                  fst (badNodeHeadMessage Const (Const . const "") l) <> "."

          let partition = (fmapMaybe $ preview _Left) &&& (fmapMaybe $ preview _Right)
              (external, internal) = splitDynPure $ partition . fmap _nodeSummary_node . MMap.getMonoidalMap <$> nodesDyn

          void $ listWithKey external $ \nodeId vDyn -> do
            let
              (title, subtitle) = splitDynPure $ nodeDataIdentification . Left <$> vDyn

              mkRemoveReq ev = PublicRequest_RemoveNode . Left . _nodeExternalData_address <$> current vDyn <@ ev

              externalNodeMenu :: m ()
              externalNodeMenu = tileMenuEntryModal "Remove Node" $ removeItemModal "node" mkRemoveReq

            titleUniq <- holdUniqDyn title
            subtitleUniq <- holdUniqDyn subtitle

            errors <- errorMessages nodeId
            nodeDetails <- watchNodeDetails nodeId
            standardNodeTile
              (dynText titleUniq)
              (dynText $ fromMaybe nbsp <$> subtitleUniq)
              externalNodeMenu
              (>>= getNodeHeadBlock)
              (Just errors)
              (Just $ (=<<) _nodeDetailsData_peerCount)
              (Just $ fromMaybe (NetworkStat 0 0 0 0) . fmap _nodeDetailsData_networkStat)
              nodeDetails

          void $ listWithKey internal $ \nodeId nodeData -> do
            errors <- errorMessages nodeId
            state <- holdUniqDyn $ _nodeInternalData_state <$> nodeData
            let
                internalNodeMenu :: m ()
                internalNodeMenu = do
                  let
                    stopModal = confirmationModal
                      ("Stop this node?")
                      ("You can always restart this node from the tile menu.")
                      ("Stop node")

                  running :: Dynamic t Bool <- holdUniqDyn $ _nodeInternalData_running <$> nodeData
                  dyn_ $ ffor running $ \case
                    True -> tileMenuEntryModal "Stop Node" $ stopModal (PublicRequest_UpdateInternalNode False <$)
                    False -> do
                      start <- tileMenuEntry "Start Node"
                      void $ requestingIdentity $ public (PublicRequest_UpdateInternalNode True) <$ start

                  tileMenuEntryModal "Remove Node" $ removeItemModal "node" $ (PublicRequest_RemoveNode (Right ()) <$)

                badge :: m ()
                badge = tileBadgeImpliedByErrors (Just errors) (Just state)

                title :: m ()
                title = text "Kiln Node"

                subtitle :: m ()
                subtitle = do
                  kilnLogo
                  divClass "ui sub header" $ dynText $ ffor state $ \case
                    NodeInternalState_Stopped -> "Stopped"
                    NodeInternalState_Initializing -> "Initializing"
                    NodeInternalState_Starting -> "Starting"
                    NodeInternalState_Running -> "Running"
                    NodeInternalState_Failed -> "Failed"

                workingTile :: m ()
                workingTile = do
                  nodeDetails <- watchNodeDetails nodeId
                  standardNodeTile
                    title
                    subtitle
                    internalNodeMenu
                    (>>= getNodeHeadBlock)
                    (Just errors)
                    (Just $ (=<<) _nodeDetailsData_peerCount)
                    (Just $ fromMaybe (NetworkStat 0 0 0 0) . fmap _nodeDetailsData_networkStat)
                    nodeDetails

                generatingTile :: m ()
                generatingTile = nodeTileWithSections $
                  [ tileHeader title subtitle internalNodeMenu badge Nothing
                  , do
                      divClass "ui row" $ do
                        icon "id-badge"
                        divClass "ui active tiny inline loader" blank
                      divClass "ui row" $ divClass "ui sub header" $ text "Generating node identity"
                      divClass "ui row" $ divClass "explanation" $ text "Before the node can run it must generate a secure identity to use on the netowrk. This may take several minutes."
                  ]

            isInitializing <- holdUniqDyn $ (== NodeInternalState_Initializing) <$> state
            dyn_ $ bool workingTile generatingTile <$> isInitializing

          void $ listWithKey (MMap.getMonoidalMap <$> publicNodesDyn) $ \_ vDyn -> do
            source <- holdUniqDyn (_publicNodeHead_source <$> vDyn)
            chain <- holdUniqDyn $ getNamedChainOrChainId . _publicNodeHead_chain <$> vDyn
            let
              title = dyn_ $ ffor2 source chain $ \s c -> case s of
                PublicNode_TzScan -> either (urlLink . tzScanUri) (flip const) c $ text "tzscan"
                PublicNode_Blockscale -> text "Foundation Nodes"
                PublicNode_Obsidian -> text "Obsidian Systems"

              publicNodeMenu :: m ()
              publicNodeMenu = do
                let mkRemoveReq ev = flip PublicRequest_SetPublicNodeConfig False <$> current source <@ ev
                tileMenuEntryModal "Remove Node" $ removeItemModal "node" mkRemoveReq

            standardNodeTile
              title
              blank
              publicNodeMenu
              (Just . mkVeryBlockLike)
              Nothing
              Nothing
              Nothing
              vDyn

    tileHeader
      :: m () -- ^ Title
      -> m () -- ^ Subtitle
      -> m () -- ^ Tile menu contents
      -> m () -- ^ Status badge
      -> Maybe (Dynamic t [m ()]) -- ^ (Optional) Function to build list of error messages for this node
      -> m ()
    tileHeader title subtitle menuContents badge errors' = do
      tileMenu menuContents
      divClass "title" $ do
        badge
        title
        divClass "secondary-name" subtitle
      tileErrors errors'

    tileErrors = traverse_ $ \errors ->
      dyn_ $ ffor errors $ traverse_ (divClass "ui error message")

    tileBadgeImpliedByErrors :: Maybe (Dynamic t [a]) -> Maybe (Dynamic t NodeInternalState) -> m ()
    tileBadgeImpliedByErrors mErrors mState = for_ mErrors $ \errors -> do
      errorsEmpty <- holdUniqDyn $ null <$> errors
      let color = maybe (pure badgeColor) (fmap badgeColorInternal) mState
      iconDyn $ ("tiny circle " <>) <$> (color <*> errorsEmpty)

    badgeColor :: Bool -> Text
    badgeColor = bool "red" "green"

    badgeColorInternal :: NodeInternalState -> Bool -> Text
    badgeColorInternal = \case
      NodeInternalState_Stopped -> const "orange"
      NodeInternalState_Initializing -> const "grey"
      NodeInternalState_Starting -> const "grey"
      NodeInternalState_Running -> badgeColor
      NodeInternalState_Failed -> const "red"

    tileBlockStats getBlock node = do
      b <- maybeDyn $ getBlock <$> node
      divClass "soft-heading" $
        withPlaceholder' "Connecting..." $ withMaybeDyn b display (unRawLevel . view level)
      text "#"
      withPlaceholder $ withMaybeDyn b blockHashLink (view hash)

      el "dl" $ do
        el "div" $ do
          el "dt" (text "Fitness")
          el "dd" $
            withPlaceholder $ withMaybeDyn b dynText (fitnessText . view fitness)

        el "div" $ do
          el "dt" (text "Baked")
          el "dd" $ do
            withPlaceholder $ withMaybeDyn b (localHumanizedTimestamp $ pure $ pure "Block Header Timestamp") (view timestamp)

    tileConnectionStats getPeerCount' getNetworkStats' node =
      if isNothing getPeerCount' && isNothing getNetworkStats'
      then Nothing
      else Just $ do
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

    -- TODO: errors' is a Maybe because we statically state that public nodes
    -- don't display a status icon, but I don't think that's a good way to
    -- execute that design.  Pass in what you have, and decide to show the icon
    -- or not in CSS
    standardNodeTile
      :: m () -- ^ Title
      -> m () -- ^ Subtitle
      -> m () -- ^ Tile menu contents
      -> (a -> Maybe VeryBlockLike) -- ^ Function to get block information from a node
      -> Maybe (Dynamic t [m ()]) -- ^ (Optional) Function to build list of error messages for this node
      -> Maybe (a -> Maybe Word64) -- ^ (Optional) Function to get the peer count of the node
      -> Maybe (a -> NetworkStat) -- ^ (Optional) Function to get the network stats of the node
      -> Dynamic t a -- ^ Node
      -> m ()
    standardNodeTile title subtitle menuContents getBlock errors' getPeerCount' getNetworkStats' node =
      nodeTileWithSections $
        [ tileHeader title subtitle menuContents (tileBadgeImpliedByErrors errors' Nothing) errors'
        , tileBlockStats getBlock node
        ]
        <> toList (tileConnectionStats getPeerCount' getNetworkStats' node)

    nodeTileWithSections :: [m ()] -> m ()
    nodeTileWithSections = divClass "ui card dashboard-tile node-tile" . divClass "content" .
      sequence_ . intersperse (divClass "divider" blank)


data BakersBanner
  = BakersBanner_Gathering
  | BakersBanner_CannotGather
  deriving (Eq, Ord, Show)

bakersTab
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasTimer t r, HasTimeZone r
    , HasModal t m, MonadRhyoliteFrontendWidget Bake t (ModalM m)
    )
  => m ()
bakersTab =
  divClass "dashboard-section dashboard-section-bakers" $ do
    elClass "h4" "dashboard-section-title" $ text "Bakers"
    tilesWidget =<< watchBakerAddresses
  where
    tilesWidget :: Dynamic t (MonoidalMap PublicKeyHash BakerSummary) -> m ()
    tilesWidget tilesDyn = do
      useBlocker <- holdUniqDyn $ MMap.null <$> tilesDyn
      alertWindow <- fmap Set.singleton <$> thirtySixHoursToInfinity
      dEbb <- snd <$$$$> watchErrorsByBaker alertWindow
      dCollectiveNodesStatus <- watchCollectiveNodesStatus alertWindow
      dyn_ $ ffor useBlocker $ \case
        True -> waitingForResponse
        False -> mdo
         let wantBakerData = (||)
               <$> (any ((== MonitoredStatus_Unknown) . bakerStatus) <$> tilesDyn)
               <*> (any isNothing <$> joinDynThroughMap bakersDetails)
         (bakersBanner :: Dynamic t (Maybe BakersBanner)) <-
           holdUniqDyn $ ffor2 dCollectiveNodesStatus wantBakerData $ \case
             Left _ -> \_ -> Just BakersBanner_CannotGather
             Right () -> \cond -> BakersBanner_Gathering <$ guard cond
         dyn_ $ ffor bakersBanner $ mkBakersBanner

         dyn_ $ ffor dEbb $
           traverse (splashAlert tilesDyn) . foldMap toList . MMap.elems

         (bakersDetails :: Dynamic t (Map.Map PublicKeyHash
                                              (Dynamic t (Maybe BakerDetails)))) <- divClass "ui stackable cards" $ do
          listWithKey (coerceDynamic tilesDyn) $ \pkh vDyn -> do
            unresolvedAlerts <- holdUniqDyn $ foldMap toList . MMap.lookup pkh <$> dEbb

            let
              renderBakerError = text . _bakerErrorDescriptions_tile

              connectivityAndUnresolvedAlerts = (++)
                <$> (ffor dCollectiveNodesStatus $ \case
                        Left e -> [Left e]
                        Right _ -> [])
                <*> (Right <$$> unresolvedAlerts)

              errorMessages = ffor connectivityAndUnresolvedAlerts $ fmap $ \case
                Left (_ :: CollectiveNodesFailure) -> text "Cannot gather baker data."
                Right e -> case e of
                  BakerErrorLogView_MultipleBakersForSameBaker{} -> text "Multiple bakers for same baker."
                  BakerErrorLogView_BakerMissed elbm -> text $ "Missed " <> aRight
                    where
                      aRight = case _errorLogBakerMissed_right elbm of
                        RightKind_Baking -> "a bake"
                        RightKind_Endorsing -> "an endorse"
                  BakerErrorLogView_BakerDeactivated log -> renderBakerError $ bakerDeactivatedDescriptions log
                  BakerErrorLogView_BakerDeactivationRisk log -> renderBakerError $ bakerDeactivationRiskDescriptions log

            let (title, subtitle) = splitDynPure $ bakerSummaryIdentification . (pkh,) <$> vDyn
            titleUniq <- holdUniqDyn title
            subtitleUniq <- holdUniqDyn subtitle
            details <- watchBakerDetails pkh

            tile
              (dynText titleUniq)
              subtitleUniq
              (\ev -> PublicRequest_RemoveBaker pkh <$ ev)
              -- if you have both a bake and endorse for the same level, you
              -- must *first* bake the block at that level, then you may
              -- immediately endorse that block.  the times are the same,
              -- baking happens first.
              (minimumByMay (on compare snd <> on compare fst) . Map.toList . _bakerSummary_nextRight)
              (Just errorMessages)
              vDyn
              details
              (not . isLeft <$> dCollectiveNodesStatus)

            pure details
         blank

    mkBakersBanner :: Maybe BakersBanner -> m ()
    mkBakersBanner = \case
      Nothing -> blank
      Just sort -> SemUi.segment (def & SemUi.classes SemUi.|~ "dashboard-section-overview") $ case sort of
        BakersBanner_Gathering -> renderSplashAlert
          (icon "icon-download big grey")
          (do
             divClass "ui active inline loader small blue" blank
             text "Gathering baker data...")
          (text "Some information will be temporarily unavailable as Kiln gathers baker information from the blockchain. This only needs to be done once for each baker.")
        BakersBanner_CannotGather -> renderSplashAlert
          (icon "icon-disconnected big red")
          (text "Cannot gather baker data - no nodes online.")
          (do
             el "p" $ text "Kiln cannot gather baker data if no nodes are synced with the blockchain."
             el "p" $ do
               el "strong" $ text "Fix:"
               text " "
               text "Add a node from the left panel or make sure any nodes you’ve already added are healthy.")

    splashAlert :: Dynamic t (MonoidalMap PublicKeyHash BakerSummary) -> BakerErrorLogView -> m ()
    splashAlert tilesDyn = SemUi.segment (def & SemUi.classes SemUi.|~ "dashboard-section-overview") . \case
      -- TODO
      BakerErrorLogView_MultipleBakersForSameBaker{} -> text "Multiple bakers for same baker."
      BakerErrorLogView_BakerMissed log -> renderBakerError
        (bakerMissedDescriptions log)
        (unId $ _errorLogBakerMissed_baker log)
      BakerErrorLogView_BakerDeactivated log -> renderBakerError
        (bakerDeactivatedDescriptions log)
        (_errorLogBakerDeactivated_publicKeyHash log)
      BakerErrorLogView_BakerDeactivationRisk log -> renderBakerError
        (bakerDeactivationRiskDescriptions log)
        (_errorLogBakerDeactivationRisk_publicKeyHash log)

      where
        renderBakerError dsc pkh = do
          let warning = _bakerErrorDescriptions_warning dsc
          renderResolvableSplashAlert
            (icon $ "icon-warning big " <> bool "red" "orange" (isJust warning))
            (_bakerErrorDescriptions_title dsc)
            (do dyn_ $ ffor tilesDyn $ maybe blank (bakerSummaryLabel pkh) . MMap.lookup pkh
                el "div" $ text $ _bakerErrorDescriptions_problem dsc
                for_ warning $ el "div" . text
                el "div" $ do
                  el "strong" $ text "Fix:"
                  text " "
                  text $ _bakerErrorDescriptions_fix dsc)
            (_bakerErrorDescriptions_userResolvable dsc)

    tile
      :: m () -- ^ Title
      -> Dynamic t (Maybe Text) -- ^ Subtitle
      -> (Event t () -> Event t (PublicRequest Bake ())) -- ^ Construct an API request with an 'Event' to remove this baker.
      -> (BakerSummary -> Maybe (RightKind, RawLevel)) -- ^ (Optional) Function to get the next event of the baker
      -> Maybe (Dynamic t [m ()]) -- ^ (Optional) Function to build list of error messages for this baker
      -> Dynamic t BakerSummary -- ^ Baker
      -> Dynamic t (Maybe BakerDetails) -- ^ Details
      -> Dynamic t Bool -- ^ have network connectivity
      -> m ()
    tile title subtitle mkRemoveReq getNextEvent' errors' bakerDyn details' connected = do
      divClass "ui card dashboard-tile baker-tile" $ divClass "content" $ do
        tileMenu $ do
          remove <- fmap (domEvent Click . fst) $ SemUi.listItem' def $ text "Remove Baker"
          tellModal $ remove $> removeItemModal "baker" mkRemoveReq

        divClass "title" $ do
          for_ errors' $ \errors -> do
            _errorsEmpty <- holdUniqDyn $ null <$> errors
            iconDyn $ ("tiny circle " <>) . statusColor . bakerStatus <$> bakerDyn
          title
          divClass "subtitle" $ dynText =<< holdUniqDyn (fromMaybe nbsp <$> subtitle)

        for_ errors' $ \errors -> do
          dyn_ $ ffor errors $ traverse_ (divClass "ui error message")

        let dWantToGatherData = (/= 0) . _bakerSummary_nextRightFetchRemaining <$> bakerDyn
        isGathering <- holdUniqDyn $ (&&) <$> dWantToGatherData <*> connected
        dyn_ $ ffor isGathering $ \case
          True -> divClass "ui active inline loader mini blue" blank *> text "Gathering baker data."
          False -> blank

        (details'' :: Dynamic t (Maybe (Dynamic t BakerDetails))) <- maybeDyn details'
        dyn_ $ ffor details'' $ \case
          Nothing -> blank
          Just details -> el "dl" $ do
            let dmDelegateInfo = unJson <$$> (_bakerDetails_delegateInfo <$> details)
            el "div" $ do
              el "dt" (text "Available Balance")
              el "dd" $ withPlaceholder $ ffor dmDelegateInfo $ fmap $
                text . tez . _cacheDelegateInfo_balance

            el "div" $ do
              el "dt" (text "Staking Balance")
              el "dd" $ withPlaceholder $ ffor dmDelegateInfo $ fmap $
                text . tez . _cacheDelegateInfo_stakingBalance
            --el "div" $ do
            --  el "dt" (text "Bake Success:")
            --  el "dd" $
            --    withPlaceholder $ ffor details $ fmap (text . (<> "%") . T.pack . ($[]) . showFFloat (Just 0) . (100*)) . getBakeSuccess'

            --el "div" $ do
            --  el "dt" (text "Endorsement Success:")
            --  el "dd" $ do
            --    withPlaceholder $ ffor details $ fmap (text . (<> "%") . T.pack . ($[]) . showFFloat (Just 0) . (100*)) . getEndorseSuccess'

        nextEventDyn <- maybeDyn $ getNextEvent' <$> bakerDyn
        latestHead <- watchLatestHead
        dparameters <- watchProtoInfo
        dyn_ $ ffor nextEventDyn $ \case
          Nothing -> blank
          Just eventDyn -> el "dl" $ do
            el "div" $ do
              el "dt" (text "Next")
              el "dd" $ do
                (dynText $ eventDyn <&> \case {RightKind_Baking -> "Bake block "; RightKind_Endorsing -> "Endorse block "} . fst)
                (dynText $ tshow . unRawLevel . snd <$> eventDyn)
                etaDyn <- maybeDyn $ getCompose $ predictFutureTimestamp <$> Compose dparameters <*> (Compose $ fmap (Just . snd) eventDyn) <*> Compose latestHead
                text nbsp
                dyn_ $ ffor etaDyn $ maybe blank $ localHumanizedTimestamp (pure Nothing)

renderResolvableSplashAlert :: (MonadRhyoliteFrontendWidget Bake t m)
  => m () -- ^ Alert icon
  -> Text -- ^ Title
  -> m () -- ^ Description body
  -> Maybe (DSum LogTag Identity) -- ^ Optional resolvable request
  -> m ()
renderResolvableSplashAlert splashIcon title desc mReq = do
  renderSplashAlert splashIcon (text title) $ do
    desc
    for_ mReq $ \resolveReq -> do
      resolve <- divClass "buttons" $ uiButton "primary" "Resolve"
      requestingIdentity $ public (PublicRequest_ResolveAlert resolveReq) <$ resolve

renderSplashAlert :: (MonadRhyoliteFrontendWidget Bake t m)
  => m () -- ^ Alert icon
  -> m () -- ^ Title
  -> m () -- ^ Description body
  -> m ()
renderSplashAlert splashIcon title desc = do
  elClass "div" "dashboard-section-overview-icon" $ splashIcon
  elClass "div" "dashboard-section-overview-body" $ do
    divClass "ui header" $ title
    divClass "description" $ desc

withPlaceholder :: (DomBuilder t m, PostBuild t m) => Dynamic t (Maybe (m ())) -> m ()
withPlaceholder = withPlaceholder' "-"

withPlaceholder' :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t (Maybe (m ())) -> m ()
withPlaceholder' placeholder f' = dyn_ $ ffor f' $ \case
  Nothing -> text placeholder
  Just f -> f

withMaybeDyn :: (Eq b, MonadFix m, MonadHold t m, Reflex t) => Dynamic t (Maybe (Dynamic t a)) -> (Dynamic t b -> m ()) -> (a -> b) -> Dynamic t (Maybe (m ()))
withMaybeDyn d mkWidget f = (fmap.fmap) (mkWidget <=< holdUniqDyn . fmap f) d

removeItemModal :: MonadRhyoliteFrontendWidget app t m
                => Text
                -> (Event t () -> Event t (PublicRequest app ()))
                -> Event t ()
                -> m (Event t ())
removeItemModal name = confirmationModal
  ("Remove this " <> name <> "?")
  ("You can always add this " <> name <> " again from the \"Add " <> T.toTitle name <> "\" button.")
  ("Remove " <> T.toTitle name)

tileMenu :: (DomBuilder t m, TriggerEvent t m, MonadIO (Performable m), PerformEvent t m, PostBuild t m, MonadHold t m, MonadFix m) => m b -> m ()
tileMenu content =
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
          SemUi.list (def & SemUi.listConfig_link SemUi.|~ True & SemUi.listConfig_divided SemUi.|~ True) content
    pure ()

bakerTab
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasFrontendConfig r
    )
  => PublicKeyHash
  -> m ()
bakerTab pkh = do
  bakers <- watchBakerStats $ pure $ Set.singleton pkh
  dparameters <- watchProtoInfo
    -- TODO: this could be a maybeDyn of some sort so that we don't redraw the dom for each balance change/block baked.
  thisBaker <- (maybeDyn <=< holdDyn Nothing <=< updatedWithInit)  $ MMap.lookup pkh <$> bakers
  dyn_ $ ffor thisBaker $ \case
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
        _ <- divClass "bakers" $ do
          text "ID: "
          sequenceA $ intersperse (text " ") (fmap publicKeyHashLink $ _clientConfig_bakers $ unJson $ _clientInfo_config clientInfo)

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
