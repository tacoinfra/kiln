{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend where

import Control.Lens ((<>~), imap, to)
import Control.Monad (unless)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader (ReaderT)
import Data.Default
import Data.Dependent.Sum (DSum(..), EqTag)
import Data.Functor.Infix hiding ((<&>))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Sum
import Data.List (intersperse, minimumBy, maximumBy, foldl')
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time (UTCTime)
import Data.Word (Word64)
import Data.Version
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Location as Location
import qualified GHCJS.DOM.File as File
import GHCJS.DOM.Types (MonadJSM, liftJSM)
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
import Safe (headMay)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.NodeRPC.Sources (PublicNode (..), tzScanUri)
import Tezos.NodeRPC.Types
import Tezos.Types

import Common (humanBytes)
import Common (unixEpoch, uriHostPortPath)
import Common.Alerts (
    AlertsFilter(..),
    BakerErrorDescriptions(..),
    badNodeHeadMessage,
    bakerAccusedDescriptions,
    bakerDeactivatedDescriptions,
    bakerDeactivationRiskDescriptions,
    bakerGroupedMissedDescriptions,
    bakerInsufficientFundsDescriptions,
    bakerMissedDescriptions,
    bakerVotingReminderDescriptions,
    isUserResolvable,
    networkUpdateDescription,
    standardTimeFormat,
  )
import Common.Api
import Common.App
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import Common.Config (HasFrontendConfig (frontendConfig), frontendConfig_chain, frontendConfig_appVersion, FrontendConfig(..))
import qualified Common.Config as Config
import Common.HeadTag (headTag)
import Common.Route
import Common.Schema hiding (Event)
import ExtraPrelude
import Frontend.Amendment
import Frontend.Common
import Frontend.Ledger
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
  , _frontend_body = prerender_ blank frontendBody
  }

frontendBody
  :: forall m t x.
    ( MonadWidget t m
    , HasJS x m
    , PrimMonad m
    , RouteConstraints t AppRoute m
    )
  => m ()
frontendBody = void $ do
  mWsUri <- getBackendPath (InL BackendRoute_Listen :/ ()) True
  rec
    (socketState, _) <- runRhyoliteWidget (maybe (error "Invalid WS URL") Uri.render mWsUri) $ do
      withFrontendContext $
        withConnectivityModal socketState $
          runModalT (ModalBackdropConfig $ "class"=:"modal-backdrop")
            appMain
  pure ()

getBackendPath :: (MonadJSM m) => R (Sum BackendRoute (ObeliskRoute AppRoute)) -> Bool -> m (Maybe URI)
getBackendPath backendRoute isWebsocket = do
  let getExecutableConfig = Obelisk.ExecutableConfig.get . ("config/" <>)
  route :: URI <- liftIO (getExecutableConfig $ T.pack Config.route) >>= \case
    Just r -> return $ fromMaybe (error $ "Unable to parse injected route: " <> show r) $ Uri.mkURI $ T.strip r
    Nothing ->
      Config.parseRootURIUnsafe <$> (Location.getHref =<< Window.getLocation =<< DOM.currentWindowUnchecked)
  let
    url = do
      encoder <- either (const Nothing) Just $ checkEncoder backendRouteEncoder
      let path = fst $ encode encoder $ backendRoute
      pathPiece <- NEL.nonEmpty =<< mapM Uri.mkPathPiece path
      scheme <- if isWebsocket
        then case Uri.uriScheme route of
          rtextScheme | rtextScheme == Uri.mkScheme "https" -> Uri.mkScheme "wss"
          rtextScheme | rtextScheme == Uri.mkScheme "http" -> Uri.mkScheme "ws"
          _ -> Nothing
        else Uri.uriScheme route
      return $ route
        { Uri.uriPath = Just (False, pathPiece)
        , Uri.uriScheme = Just scheme
        }
  pure url

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
    , MonadJSM (Performable (ModalM m))
    , MonadJSM (ModalM m)
    , MonadJSM (Performable m)
    , HasJSContext (Performable (ModalM m))
    , MonadJSM m
    , MonadReader r m, HasFrontendConfig r, HasTimer t r, HasTimeZone r, MonadReader r (ModalM m)
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
     , MonadJSM (ModalM m)
     , MonadJSM (Performable (ModalM m))
     , HasJSContext (Performable (ModalM m))
     , HasFrontendConfig r, HasModal t m, HasTimer t r, MonadReader r m, MonadReader r (ModalM m)
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
              routeSelector (AppRoute_Index :/ ()) SemUi.menuItem' def $ do
                icon "icon-tiles"
                text "Dashboard"
        SemUi.divider def

appGutter
  :: ( MonadRhyoliteFrontendWidget Bake t m
     , MonadRhyoliteFrontendWidget Bake t (ModalM m)
     , MonadJSM (ModalM m)
     , MonadJSM (Performable (ModalM m))
     , HasJSContext (Performable (ModalM m))
     , HasModal t m, HasTimer t r, MonadReader r m, MonadReader r (ModalM m)
     )
  => m ()
appGutter =
  SemUi.segment
    (def
      & SemUi.classes SemUi.|~ "app-gutter"
      & SemUi.segmentConfig_basic SemUi.|~ True
      )
    $ do
        bakersList
        nodesList

appSideFooter :: (MonadRhyoliteFrontendWidget Bake t m, RouteConstraints t AppRoute m, MonadReader r m, HasFrontendConfig r) => m ()
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
                currentVersion <- asks (^. frontendConfig . frontendConfig_appVersion)
                upstreamVersion <- watchUpstreamVersion
                dyn_ $ ffor upstreamVersion $ \case
                  Just uv | Just v <- _upstreamVersion_version uv , v > currentVersion ->
                    elAttr "i" ("class" =: iconClass "upgrade-icon icon-arrow-up" <> "style" =: "float: right; margin: -2px 0 0 0") blank
                  _ -> pure ()

        hrefLink "https://gitlab.com/obsidian.systems/tezos-bake-monitor" $
          elAttr "img" ("src" =: static @"images/ObsidianSystemsLogo-ICFP2017.svg" <> "class" =: "credits-obsidian") blank

appHeader
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m)
    , MonadReader r m, HasTimer t r, HasFrontendConfig r, HasTimeZone r
    )
  => m (Event t ())
appHeader = SemUi.segment (def & SemUi.segmentConfig_vertical SemUi.|~ True) $ do
  alertWindow <- fmap Set.singleton <$> thirtySixHoursToInfinity
  collectedNodesStatus <- watchCollectiveNodesStatus alertWindow
  let disconnected = ffor collectedNodesStatus $ \case
        Left CollectiveNodesFailure_NoNodes -> True
        Left (CollectiveNodesFailure_AllNodesDownSince _) -> True
        Right () -> False

  divClass "topbar" $ do
    divClass "ui horizontal list" $ do
      latestHead <- watchLatestHead
      let infoItem faded title body = divClass "item" $
            elDynAttr "div" (bool Map.empty ("class" =: "faded") <$> faded) $ divClass "content" $ do
              divClass "header" $ text title
              divClass "description" body

      infoItem (pure False) "Network" $ text . showChain =<< asks (^. frontendConfig . frontendConfig_chain)

      protoInfo' <- watchProtoInfo
      cyc <- holdUniqDyn $ (liftA2.liftA2) levelToCycle protoInfo' $ (fmap.fmap) (view level) latestHead
      whenJustDyn cyc $ \c -> infoItem disconnected "Cycle" $
        text $ tshow $ unCycle c

      whenJustDyn latestHead $ \b -> infoItem disconnected "Block" $ el "span" $ do
        text $ tshow (unRawLevel $ b ^. level)
        elClass "span" "metadescription" $ text " Baked "
        localHumanizedTimestampBasic $ pure $ b ^. timestamp

      amendments <- watchAmendment
      mProtoInfo <- maybeDyn protoInfo'
      mAmendment <- maybeDyn $ fmap snd . Map.lookupMax <$> amendments
      whenJustDyn (liftA2 . (,,) <$> disconnected <*> mProtoInfo <*> mAmendment) $ \(dc, protoInfo, amendment) -> unless dc $ do
        let amendmentWrapper = elAttr' "div" ("class" =: "item" <> "style" =: "position: relative")
        tooltippedWrapper amendmentWrapper TooltipPos_BottomCenter (amendmentPopup amendment amendments protoInfo) $ divClass "content" $ do
          kind <- holdUniqDyn $ _amendment_period <$> amendment
          infoItem (pure False) "Amendment Period" $ do
            dynText $ textPeriod <$> kind
            text " "
            display $ (\a -> unCycle . currentCyclePosition a) <$> amendment <*> protoInfo
            text "/"
            display $ unCycle . cyclesPerPeriod <$> protoInfo
            dyn_ $ ffor (isVotingPeriod <$> kind) $ flip when $ elClass "i" "blue icon-vote-badge icon" blank

    dyn_ $ ffor disconnected $ flip when $ tooltipped TooltipPos_BottomCenter disconnectedTooltip $
      SemUi.icon "icon-disconnected"
      (def
        & SemUi.iconConfig_color SemUi.|?~ SemUi.Red
        & SemUi.iconConfig_size SemUi.|?~ SemUi.Big
        )
  headerBell

  where
    disconnectedTooltip = divClass "disconnected-tooltip" $ do
      el "p" $ divClass "tooltip-title" $ text "Disconnected from the blockchain."
      divClass "tooltip-description" $ do
        el "p" $ text "Kiln cannot gather data if no monitored nodes are synced with the blockchain (public nodes do not provide baker data). Data shown is stale."
        el "p" $ ensureHealthyNodes

headerBell :: MonadRhyoliteFrontendWidget Bake t m => m (Event t ())
headerBell = do
  alertCount <- holdUniqDyn =<< fmap (fromMaybe 0) <$> watchAlertCount
  let hasAlerts = fmap (> 0) alertCount
  (e,_) <- SemUi.ui' "span"
    (def
      & SemUi.classes .~ (SemUi.Dyn $ ffor hasAlerts $ ((<>) "ui circular label link ") . bool "basic" "red")
      )
    $ do
        dynText $ ffor alertCount $ (fromMaybe <*> T.stripPrefix "0") . tshow
        text " "
        SemUi.icon "icon-bell"
          (def
            & SemUi.iconConfig_size SemUi.|?~ SemUi.Large
            & SemUi.iconConfig_color .~ (SemUi.Dyn $ ffor hasAlerts $ bool (Just SemUi.Grey) Nothing)
            & SemUi.iconConfig_link SemUi.|~ True
            & SemUi.iconConfig_fitted .~ (SemUi.Dyn hasAlerts)
            )
  return $ domEvent Click e

appContentArea
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadJSM (ModalM m), MonadJSM (Performable (ModalM m))
    , MonadJSM (Performable m)
    , MonadJSM m
    , MonadReader r m, HasFrontendConfig r, HasTimer t r, HasTimeZone r
    , MonadReader r (ModalM m)
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
    , MonadReader r (ModalM m), MonadJSM (Performable (ModalM m))
    , HasModal t m, MonadRhyoliteFrontendWidget Bake t (ModalM m)
    )
  => m ()
nodesTabOrWelcome = do
  -- _clientAddresses <- watchClientAddresses
  bakersMaybe <- watchBakerAddressesValid
  publicNodesMaybe <- watchPublicNodeConfigValid
  nodesMaybe <- watchNodeAddressesValid
  mUsingOsPubNode <- (fmap . fmap) _frontendConfig_usingOsPublicNode <$> watchFrontendConfig

  -- doing some straightforward calculations, but inside a Dynamic and a Maybe
  let haveBakersMaybe =
        (fmap . fmap) (not . null) bakersMaybe
      haveNodesMaybe =
        (liftA2 . liftA2) ((||) . any _publicNodeConfig_enabled . toList) publicNodesMaybe $
        (fmap . fmap) (not . null) nodesMaybe
      onlyOsPubNode = ffor2 publicNodesMaybe mUsingOsPubNode $ liftA2 $ \pNodes usingOs -> usingOs &&
        (length (filter _publicNodeConfig_enabled $ MMap.elems pNodes) == 1)
          && maybe False (_publicNodeConfig_enabled . snd)
            (headMay (filter ((== PublicNode_Obsidian) . fst) $ MMap.assocs pNodes))
  haveBakersHaveNodesMaybe <- holdUniqDyn $
    (liftA3 . liftA3) (,,) haveBakersMaybe haveNodesMaybe onlyOsPubNode

  globalAlerts

  dyn_ $ ffor haveBakersHaveNodesMaybe $ \case
    Nothing -> divClass "app-content app-welcome" waitingForResponse
    Just (False, False, False) -> divClass "app-content app-welcome" $ welcomeScreen False
    Just (haveBakers, haveNodes, onlyOsNode) -> divClass "app-content" $ do
      when (onlyOsNode && (not haveBakers)) $ welcomeScreen True
      when haveBakers bakersTab
      when haveNodes nodesTab

globalAlerts
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasFrontendConfig r
    )
  => m ()
globalAlerts = do
  mchain <- asks $ preview (frontendConfig . frontendConfig_chain . _Left)
  mNetworkAlert <- for mchain $ \chain -> do
    let everythingWindow = pure $ Set.singleton $ ClosedInterval LowerInfinity UpperInfinity
    dXs <- watchErrors (pure $ Just AlertsFilter_UnresolvedOnly) everythingWindow
    mUpgradeLog <- holdUniqDyn $ ffor dXs $ \xs -> listToMaybe $ toList $ flip MMap.mapMaybeWithKey xs $ \_ -> \case
      (ErrorLog { _errorLog_stopped = Nothing }, LogTag_NetworkUpdate :=> Identity ua) -> do
        guard $ _errorLogNetworkUpdate_namedChain ua == chain
        return ua
      _ -> Nothing
    pure $ fmap networkUpdateAlert <$> mUpgradeLog

  currentVersion <- asks (^. frontendConfig . frontendConfig_appVersion)
  upstreamVersion <- watchUpstreamVersion
  let
    mUpdateAlert :: Dynamic t (Maybe (m ()))
    mUpdateAlert = ffor upstreamVersion $ \case
      Just uv
        | Just v <- _upstreamVersion_version uv
        , v > currentVersion
        , not (_upstreamVersion_dismissed uv) -> Just $ kilnUpdateAlert v
      _ -> Nothing

    allAlerts :: Dynamic t [m ()]
    allAlerts = catMaybes <$> sequence [ (fmap join . sequence) mNetworkAlert, mUpdateAlert ]
  dyn_ $ ffor allAlerts $ traverse_ $ divClass "dashboard-section dashboard-section-global-alerts" . \m -> do
    SemUi.segment (def & SemUi.classes SemUi.|~ "dashboard-section-overview") $ m

networkUpdateAlert :: (MonadRhyoliteFrontendWidget Bake t m) => ErrorLogNetworkUpdate -> m ()
networkUpdateAlert elua = do
  let namedChain = _errorLogNetworkUpdate_namedChain elua
  let (header, bodyFirstPara) = networkUpdateDescription namedChain
  renderResolvableSplashAlert
    (pure (LogTag_NetworkUpdate :=> pure elua))
    (icon "icon-alert-badge big blue")
    (text header)
    Nothing
    (do el "p" $ text bodyFirstPara
        el "p" $ do
          text "Get the new software here "
          elClass "i" "ui icon small icon-arrow-right" blank
          let url = "https://gitlab.com/tezos/tezos/tree/" <> showNamedChain namedChain -- FIXME the url should be based on the project id
          elAttr "a" ("href" =: url <> "target" =: "_blank" <> "rel" =: "noopener") $ text url)

kilnUpdateAlert :: (MonadRhyoliteFrontendWidget Bake t m) => Version -> m ()
kilnUpdateAlert v = do
  let
    header = "Kiln " <> T.pack (showVersion v) <> " is available!"
    body = el "div" $ do
      el "p" $ do
        text "This may be a crucial update that provides functionality to support upcoming Tezos protocol changes. Please check the release notes for details on the importance of this update: "
        let url = "https://gitlab.com/obsidian.systems/tezos-bake-monitor/-/releases"
        elAttr "a" ("href" =: url <> "target" =: "_blank" <> "rel" =: "noopener") $ text url
      el "p" $ do
        resolve <- divClass "buttons" $ uiButtonM "primary" $ do
          icon "icon-check"
          text "Dismiss"
        void $ requestingIdentity $ public PublicRequest_DismissUpgradeAlert <$ resolve
  renderSplashAlert
    (icon "icon-update-circle big blue")
    (text header)
    Nothing
    body

welcomeScreen :: forall t m. MonadRhyoliteFrontendWidget Bake t m => Bool -> m ()
welcomeScreen hasOsPubNode = mdo
  closeEv <- switch . current <$> widgetHold banner (pure never <$ closeEv)
  pure ()
  where
    banner = divClass "app-content app-welcome" $ divClass "dashboard-section dashboard-section-global-alerts" $ SemUi.segment def $ do
      (closeEl, _) <- elAttr' "div" ("class"=:"modal-close") $ elClass "i" "icon-x fitted icon" blank
      SemUi.header
        (def
          & SemUi.headerConfig_size SemUi.|?~ SemUi.H1
          )
        $ do
            text $ "Welcome to " <> appName <> "."
      divClass "welcome-description" $ do
        el "p" $ text $ appName <> " is a baking and monitoring tool for the Tezos blockchain network."
        el "p" $ text $ "Click \"Add Nodes\" to start or monitor a node. Adding public nodes is recommended to provide network context."
          <> (if hasOsPubNode then " The Obsidian public node has been added to provide a baseline source of network data." else "")
        el "p" $ text "Click \"Add Bakers\" to start or monitor an existing baker."
      pure $ domEvent Click closeEl

radioLabels :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, Eq k) => k -> [(k, m ())] -> m (Dynamic t k)
radioLabels k0 ks = divClass "ui buttons" $ mdo
  selectedDyn <- holdDyn k0 $ leftmost kClicks
  kClicks <- for ks $ \(k, label) -> do
    fmap (k <$) $ uiDynButton (ffor selectedDyn $ bool "" "primary" . (== k)) label

  pure selectedDyn

data ErrorLogView' = ErrorLogView' ErrorLogView (Maybe NodeSummary) deriving Eq

-- | Different constructor name because presumably more would be added
newtype SynthError
  = SynthError_BakersInformationDown (NonEmpty (PublicKeyHash, BakerData))
  deriving (Eq, Ord, Show)

-- | Meta info for alerts to customize their appearance/behaviour
data AlertMetaData = AlertMetaData
  { _alertMetaData_isEventBased :: !Bool
  }

class HasAlertMetaData a where
  getAlertMetaData :: a -> AlertMetaData

instance Default AlertMetaData where
  def = AlertMetaData
    { _alertMetaData_isEventBased = False
    }

instance HasAlertMetaData ErrorLogView' where
  getAlertMetaData (ErrorLogView' (logTag :=> _) _) = case logTag of
    LogTag_Node nlt -> case nlt of
      NodeLogTag_InaccessibleNode -> def
      NodeLogTag_NodeWrongChain -> def
      NodeLogTag_NodeInvalidPeerCount -> def
      NodeLogTag_BadNodeHead -> def
    LogTag_Baker blt -> case blt of
      BakerLogTag_BakerMissed -> def { _alertMetaData_isEventBased = True }
      BakerLogTag_BakerDeactivated -> def
      BakerLogTag_BakerDeactivationRisk -> def
      BakerLogTag_BakerAccused -> def { _alertMetaData_isEventBased = True }
      BakerLogTag_InsufficientFunds -> def
      BakerLogTag_VotingReminder -> def { _alertMetaData_isEventBased = True }
    LogTag_BakerNoHeartbeat -> def
    LogTag_NetworkUpdate -> def { _alertMetaData_isEventBased = True }

instance HasAlertMetaData SynthError where
  getAlertMetaData (SynthError_BakersInformationDown _) = def

instance (HasAlertMetaData a, HasAlertMetaData b) => HasAlertMetaData (Either a b) where
  getAlertMetaData (Left v) = getAlertMetaData v
  getAlertMetaData (Right v) = getAlertMetaData v

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
      resolvedFilter = soleFilter AlertsFilter_ResolvedOnly <$> filterDyn
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
        Right () -> Nothing
        -- TODO think about alert for the no configured nodes case
        Left CollectiveNodesFailure_NoNodes               -> Nothing
  dTimer <- asks $ view timer
  -- TODO: PERF: only watch when we need to for `SyntheticError_allNodesDown`
  dBakers <- watchBakerAddresses

  let
    combinedRealErrors
      :: Dynamic t (Map.Map (Id ErrorLog) (ErrorLog, ErrorLogView'))
    combinedRealErrors = ffor2 filteredErrors nodesDyn $ \errors nodes ->
      fforMaybe errors $ \(errorLog, errorLogView) ->
        let nodeSummary = do
              nodeId <- nodeIdForNodeErrorLogView <$> nodeErrorViewOnly errorLogView
              MMap.lookup nodeId nodes
        in case errorLogView of
          LogTag_Node _ :=> _ -> nodeSummary $> (errorLog, ErrorLogView' errorLogView nodeSummary)
          _ -> Just (errorLog, ErrorLogView' errorLogView Nothing)

    -- There is no `Id SynthError` so just use whole thing.
    synthErrors
      :: Dynamic t (Map.Map SynthError (ErrorLog, SynthError))
    synthErrors = ffor3 dBakers dTimer dAllNodesDownTime $
      \bakers now allNodesDownTime ->
        fromMaybe mempty $ do
          let getBaker (k, e) = case e of
                Left v -> Just (k, v)
                Right _ -> Nothing
          keys1 <- NEL.nonEmpty $ catMaybes $ map getBaker $ MMap.toList $ MMap.map _bakerSummary_baker $ bakers
          since <- allNodesDownTime
          let k = SynthError_BakersInformationDown keys1
          pure $ Map.singleton k $ (, k) $
            ErrorLog
              { _errorLog_started = since
              , _errorLog_stopped = Nothing
              , _errorLog_lastSeen = now
              , _errorLog_noticeSentAt = Nothing
              }
    filteredSynthErrors = ffor2 filterDyn synthErrors $ \f errs -> ffilter (passesFilter f . fst) errs

    combinedErrors
      :: Dynamic t (Map.Map (Down (Time.UTCTime, Either (Id ErrorLog) SynthError))
                            (ErrorLog, Either ErrorLogView' SynthError))
    combinedErrors = Map.take 20 <$> fold
      [ fmap (errorsByTime Left . (fmap . fmap) Left) combinedRealErrors
      , fmap (errorsByTime Right . (fmap . fmap) Right) filteredSynthErrors
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
    listWithKey combinedErrors $ \_ vDyn -> do
      (logDyn, widgetDyn) <- splitDynPure <$> holdUniqDyn vDyn
      let resolvedDyn = isJust . _errorLog_stopped <$> logDyn
      elDynAttr "div" (ffor resolvedDyn $ \resolved -> "class" =: ("app-notification ui message " <> if resolved then "success" else "error")) $ do
        wDyn <- holdUniqDyn widgetDyn
        dyn_ $ ffor wDyn $ either logEntry synthEntry
        let isEv = _alertMetaData_isEventBased . getAlertMetaData <$> wDyn
        el "div" $ do
          el "label" $ dynText $ ffor isEv $ bool "First Detected" "Detected"
          localTimestamp' $ _errorLog_started <$> logDyn
        dyn_ $ ffor isEv $ \b -> unless b $ el "div" $ do
          el "label" $ dynText $ ffor logDyn $ \log -> case _errorLog_stopped log of
            Nothing -> "Last Detected"
            Just _ -> "Stopped"
          localTimestamp' $ ffor logDyn $ \log -> case _errorLog_stopped log of
            Nothing -> _errorLog_lastSeen log
            Just x -> x
  where
    localTimestamp' dt = do
      tz <- asks (^. timeZone)
      dynText $ T.pack . Time.formatTime Time.defaultTimeLocale standardTimeFormat . Time.utcToZonedTime tz <$> dt

    passesFilter filterSelection log =
      filterSelection == AlertsFilter_All
        || filterSelection == AlertsFilter_UnresolvedOnly && not isResolved
        || filterSelection == AlertsFilter_ResolvedOnly && isResolved
      where isResolved = isJust $ _errorLog_stopped log

    header = divClass "header" . text

    synthEntry :: SynthError -> m ()
    synthEntry (SynthError_BakersInformationDown pkhs) = do
      header "Cannot gather baker data."
      let (pkh, bakerData) = NEL.head pkhs
      divClass "alert-entity" $ errorLabel (fromMaybe "Baker" $ _bakerData_alias bakerData) $ Identity $ toPublicKeyHashText pkh
      el "div" $
        text $ "Kiln cannot gather data about " <> (case NEL.tail pkhs of [] -> "this baker"; _ -> "these bakers") <> " if no nodes are synced with the blockchain."

    logEntry :: ErrorLogView' -> m ()
    logEntry (ErrorLogView' (logTag :=> Identity log) n') =
      case logTag of
        LogTag_Node nlt -> case n' of
          Nothing -> blank
          Just n -> case nlt of
            NodeLogTag_InaccessibleNode ->
              case _nodeSummary_node n of
                Right _ -> blank
                Left (NodeExternalData address alias _) -> do
                  header $ "Unable to connect to node" <> maybe "" (" " <>) alias <> " at " <> uriHostPortPath address <> "."
                  nodeLabel n

            NodeLogTag_NodeWrongChain -> do
              let ErrorLogNodeWrongChain _ _ expectedChainId actualChainId = log
                  (primary, _) = nodeSummaryIdentification n
              header $ "Wrong network: " <> primary <> "."
              nodeLabel n
              el "div" $
                text $ "The node is running on network " <> toBase58Text actualChainId <> " but is expected to be on " <> toBase58Text expectedChainId <> "."

            NodeLogTag_BadNodeHead -> do
              let (heading, message) = badNodeHeadMessage text (blockHashLink . pure) log
                  (primary, _) = nodeSummaryIdentification n
              header $ heading <> ": " <> primary <> "."
              nodeLabel n
              el "div" message

            NodeLogTag_NodeInvalidPeerCount -> do
              let ErrorLogNodeInvalidPeerCount _ _ minPeerCount _ = log
              header "Too few peers."
              nodeLabel n
              el "div" $ text $
                "This node has fewer peers than the configured minimum of " <> tshow minPeerCount <> "."

        LogTag_Baker blt -> case blt of
          BakerLogTag_BakerDeactivated -> renderBakerError
            (bakerDeactivatedDescriptions log)
            pkh
          BakerLogTag_BakerDeactivationRisk -> renderBakerError
            (bakerDeactivationRiskDescriptions log)
            pkh
          BakerLogTag_BakerAccused -> renderBakerError
            (bakerAccusedDescriptions log)
            pkh
          BakerLogTag_BakerMissed -> renderBakerError
            (bakerMissedDescriptions log)
            pkh
          BakerLogTag_InsufficientFunds -> renderBakerError
            (bakerInsufficientFundsDescriptions log)
            pkh
          BakerLogTag_VotingReminder ->
            withAmendmentPeriodProgress (_errorLogVotingReminder_votingPeriod log) $ \remaining -> do
              let dsc = bakerVotingReminderDescriptions log <$> remaining
              bakersDyn <- watchBakerAddresses
              divClass "header" $ do
                dynText $ _bakerErrorDescriptions_title <$> dsc
                text "."
              divClass "alert-entity" $ dyn_ $ ffor bakersDyn $ maybe blank (bakerSummaryLabel pkh) . MMap.lookup pkh
              el "div" $ dynText $ _bakerErrorDescriptions_notification <$> dsc
          where
            pkh = bakerIdForBakerErrorLogView (blt :=> Identity log)

        LogTag_BakerNoHeartbeat -> do
          let ErrorLogBakerNoHeartbeat _ lastLevel lastBlockHash _ = log
          header "Baker lagging behind." -- TODO Show client address
          el "div" $ do
            text "Last block level seen: "
            blockHashLinkAs (pure lastBlockHash) (text $ tshow lastLevel)

        LogTag_NetworkUpdate -> do
          let
            ErrorLogNetworkUpdate { _errorLogNetworkUpdate_namedChain = namedChain } = log
            chainText = "'" <> showNamedChain namedChain <> "'"
          header $ T.unwords ["New", chainText, "version."]
          el "div" $ do
            text $ "There is a new version of the " <> chainText <> " software available on GitLab."

    renderBakerError dsc pkh = do
      bakersDyn <- watchBakerAddresses
      header $ _bakerErrorDescriptions_title dsc <> "."
      divClass "alert-entity" $ dyn_ $ ffor bakersDyn $ maybe blank (bakerSummaryLabel pkh) . MMap.lookup pkh
      el "div" $ text $ _bakerErrorDescriptions_notification dsc

pluralOf :: Text -> Text
pluralOf = (<> "s") -- good enough for all existing uses, lol

-- The current order worse...better. This is so we take the minimum of various
-- sources of "badness"; a "weakest link" discipline. Be careful to check
-- existing uses of the Ord instance if the order is changed.
data MonitoredStatus
  = MonitoredStatus_Unhealthy
  | MonitoredStatus_Stopped
  | MonitoredStatus_Unknown
  | MonitoredStatus_Healthy
  deriving (Eq, Ord, Bounded, Enum, Show)

statusColor :: IsString a => MonitoredStatus -> a
statusColor = \case
  MonitoredStatus_Healthy -> "green"
  MonitoredStatus_Stopped -> "orange"
  MonitoredStatus_Unhealthy -> "red"
  MonitoredStatus_Unknown -> "grey"

sidebarList :: forall t m k.
  ( MonadRhyoliteFrontendWidget Bake t m
  , MonadRhyoliteFrontendWidget Bake t (ModalM m)
  , HasModal t m
  , Ord k
  )
  => Text -> Dynamic t (MonoidalMap k ((Text, Maybe Text), MonitoredStatus, Bool)) -> (Event t () -> ModalM m (Dynamic t [Text], Event t ())) -> m ()
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
          divClass "title" $ dynText title
          useSymbol <- holdUniqDyn $ view _3 <$> node
          let tooltippedKilnLogo = tooltipped
                TooltipPos_BottomRight
                (text $ "This " <> name <> " is run by Kiln.")
                (divClass "ui image right floated" kilnLogo)
          dyn_ $ bool blank tooltippedKilnLogo <$> useSymbol

        divClass "description" $ dynText $ fromMaybe "" <$> subtitle

    openAddItemOptions <- buttonIconWithInfoCls "icon-plus" "modalopener fluid" ("Add " <> pluralOf name)
    tellModal $ (openAddItemOptions $>) $ cancelableModalWithClasses modal

bakerStatus :: Either CollectiveNodesFailure BakerSummary -> MonitoredStatus
bakerStatus = \case
  Left e -> case e of
    CollectiveNodesFailure_NoNodes -> MonitoredStatus_Unhealthy
    CollectiveNodesFailure_AllNodesDownSince _ -> MonitoredStatus_Unhealthy
  Right bakerSummary
    | _bakerSummary_alertCount bakerSummary > 0 -> MonitoredStatus_Unhealthy
    | Right bid <- _bakerSummary_baker bakerSummary, not (_bakerInternalData_running bid) -> MonitoredStatus_Stopped
    | _bakerSummary_nextRight bakerSummary == BakerNextRight_GatheringData -> MonitoredStatus_Unknown
    | otherwise -> MonitoredStatus_Healthy

bakersList ::
  ( MonadReader r m, HasTimer t r
  , MonadReader r (ModalM m)
  , MonadRhyoliteFrontendWidget Bake t m
  , MonadRhyoliteFrontendWidget Bake t (ModalM m)
  , MonadJSM (ModalM m)
  , MonadJSM (Performable (ModalM m))
  , HasJSContext (Performable (ModalM m))
  , HasModal t m
  )
  => m ()
bakersList = do
  alertWindow <- fmap Set.singleton <$> thirtySixHoursToInfinity
  dCollectedNodesStatus <- watchCollectiveNodesStatus alertWindow
  bakerAddrs <- watchBakerAddresses
  let bakers = ffor2 dCollectedNodesStatus bakerAddrs
        $ \collectiveNodeStatus -> imap $ \pkh b ->
          ( bakerSummaryIdentification (pkh, b)
          , bakerStatus $ b <$ collectiveNodeStatus
          , isRight $ _bakerSummary_baker b -- Is this an internal baker?
          )
  sidebarList "Baker" bakers addBakerModal

addBakerModal :: forall t m r.
  ( MonadRhyoliteFrontendWidget Bake t m, MonadJSM m, MonadJSM (Performable m)
  , MonadReader r m, HasTimer t r
  , HasJSContext (Performable m)
  )
  => Event t () -> m (Dynamic t [Text], Event t ())
addBakerModal close = ffor (workflow splash) $ \d -> let (c, e) = splitDynPure d in (("add-baker":) <$> c, close <> switch (current e))
  where
    splash = Workflow $ do
      divClass "ui header" $ text "Add Bakers"
      divClass "ui grid stackable divided" $ do
        start <- startBaking
        close' <- connectBaker
        ebn :: Dynamic t (MonoidalMap (Id Node) (NonEmpty (ErrorLog, NodeErrorLogView)))
          <- watchErrorsByNode . fmap Set.singleton =<< thirtySixHoursToInfinity
        node <- watchInternalNode
        nodeDetails <- fmap join $ holdDyn (constDyn Nothing) <=< dyn $ ffor node $ \case
          Nothing -> pure $ constDyn Nothing
          Just (nid, pd) -> (fmap . fmap . fmap) (nid, pd,) $ watchNodeDetails nid
        let isBootstrapLevel = (< 2)
            nodeNotReady = handleClientErrorWorkflow splash ClientError_NodeNotReady
            f Nothing _ = launchNode -- With no internal node, we prompt the user to launch a kiln node
            f (Just (nid, pd, nd)) es
              -- If we have errors associated with the internal node, or the process isn't running, we redirect to node-not-ready modal
              | MMap.member nid es = nodeNotReady
              | ProcessControl_Stop == _processData_control pd = nodeNotReady
              | isProcessStateNode (_processData_state pd) = nodeNotReady
              | maybe True isBootstrapLevel (_nodeDetailsData_headLevel nd) = nodeNotReady
              | otherwise = Workflow $ do
                result <- ledgerSetupSteps
                let (err, done) = fanEither result
                pure ((["ledger-setup-steps"], close <> done), handleClientErrorWorkflow splash <$> err)
            afterDisclaimer = tag $ current (liftA2 f nodeDetails ebn)
        pure (([], close'), disclaimer afterDisclaimer <$ start)

    startBaking = divClass "start-baking column" $ do
      elClass "h5" "ui header" $ text "Start Baking"
      divClass "explanation" $ do
        text "Bake and endorse on the Tezos blockchain using a baker that is managed from within Kiln. Requires using a "
        hrefLink "https://www.ledger.com/products/ledger-nano-s" $ text "Ledger Device" -- TODO is the link correct?
        text ". Kiln only supports running a single baker."
      baker <- maybeDyn =<< watchInternalBaker
      switchHold never <=< dyn $ ffor baker $ \case
        Nothing -> uiButton "primary fluid" "Start Baking"
        Just bid -> do
          kilnLogo
          dynText $ ffor (_bakerInternalData_running . snd <$> bid) $ \case
            True -> "A Kiln baker is running."
            False -> "A Kiln baker is configured, but is stopped."
          pure never

    connectBaker = divClass "connect-baker column" $ mdo
      elClass "h5" "ui header" $ text "Monitor via Address"
      divClass "explanation" $ text "Monitor a local or remote baker via public key hash (PKH)."
      addE <- formWithReset "Add Baker" "Monitor a local or remote baker via public key hash (PKH)." blank added $ do
        zipFields
          (formItem' "required" $ pkhField "Baker Wallet Address" "tz1bvNMQ95vfAYtG8193ymshqjSvmxiCUuR5")
          (formItem $ aliasField "My Baker")
      added <- requestingIdentity $ fmap (\(addr,alias) -> public (PublicRequest_AddBaker addr alias)) addE
      pure added

    launchNode = Workflow $ do
      elClass "h5" "ui header" $ text "Kiln needs to launch a Tezos node which must be fully synced with the blockchain before baking."
      divClass "explanation" $ text "To bake with Kiln you will also need a Ledger hardware wallet device."
      start <- uiButton "primary" "Start Node"
      pure ((["launch-node"], never), startNodeWorkflow launchNode <$ start)

    disclaimer next = Workflow $ do
      elClass "h5" "ui header" $ text "Kiln Baking Disclaimer"
      divClass "explanation" $ do
        el "p" $ text "Obsidian Systems LLC has taken great care to create a baking product which is robust and effective. However, Obsidian Systems cannot make any guarantees in regards to baking success."

        el "p" $ text "To the maximum extent permitted by applicable law, we are not liable to any extent for any loss, damage, liability, expense or claim you suffer as a result of, but not limited to:"
        el "ul" $ traverse_ (el "li" . text)
          [ "missed rewards due to missed baking or endorsement opportunities"
          , "missed rewards due to failure to reveal a nonce"
          , "loss of funds due to double baking or double endorsing"
          , "blockchain reorganizations"
          , "general Tezos network issues"
          , "any other use of this software"
          ]
      consent <- fmap SemUi._checkbox_value $ SemUi.checkbox (text "I understand and agree.") def
      rec
        _ <- runWithReplace blank $ ffor hasError $ \() -> divClass "ui error message" $ text "You must accept to continue."
        attempt <- uiButton "primary" "Continue"
        let (hasError, continue) = fanEither $ attachWith (\c () -> if c then Right () else Left ()) (current consent) attempt
      pure ((["disclaimer"], never), next continue)

respondToPrompt :: DomBuilder t m => m () -> m ()
respondToPrompt prompt = do
  elClass "h5" "ui header" $ do
    divClass "ui active tiny inline blue loader" blank
    text "Respond to the prompt on your Ledger Device..."
  divClass "centered explanation" $ text "Your Ledger Device should show the following prompt:"
  elClass "h6" "ui header prompt-text" prompt

authorizeLedgerToBakeModal
  :: MonadRhyoliteFrontendWidget Bake t m
  => SecretKey -> PublicKeyHash -> Event t () -> m (Dynamic t [Text], Event t ())
authorizeLedgerToBakeModal sk pkh close = ffor (workflow auth) $ \d -> let (c, e) = splitDynPure d in (("add-baker":) <$> c, close <> switch (current e))
  where
    ledger = _secretKey_ledgerIdentifier sk
    auth = Workflow $ do
      ledgerCheckImg ledger
      elClass "h5" "ui header" $ text "Authorize this Ledger Device to bake for the following account?"
      elClass "h6" "ui header" $ text $ toPublicKeyHashText pkh
      authorize <- uiButton "primary" "Authorize"
      pure ((["ledger-prompt"], never), waiting <$ authorize)
    waiting = Workflow $ do
      ledgerCheckImg ledger
      respondToPrompt $ text $ "Authorize Baking With Public Key? Public Key Hash " <> toPublicKeyHashText pkh
      pb <- getPostBuild
      _ <- requestingIdentity $ public (PublicRequest_SetupLedgerToBake sk) <$ pb
      promptStep <- watchPrompting sk
      let changed = leftmost [updated promptStep, tag (current promptStep) pb]
          next = fforMaybe changed $ \case
            Just ss | Just (First setupStep) <- _setupState_setup ss -> case setupStep of
              SetupLedgerToBakeStep_Done -> Just authorized
              SetupLedgerToBakeStep_Disconnected -> Just $ handleClientErrorWorkflow waiting ClientError_LedgerDisconnected
              SetupLedgerToBakeStep_Declined -> Just $ handleClientErrorWorkflow waiting ClientError_RequestDeclinedByLedger
              SetupLedgerToBakeStep_Failed -> Just $ handleClientErrorWorkflow waiting $ ClientError_Other "unknown"
              SetupLedgerToBakeStep_Prompting -> Nothing
              SetupLedgerToBakeStep_OutdatedVersion _v -> Just $ handleClientErrorWorkflow waiting ClientError_LedgerDisconnected
            _ -> Nothing
      pure ((["ledger-prompt"], never), next)
    authorized = Workflow $ do
      ledgerCheckImg ledger
      elClass "h5" "ui header" $ do
        icon "blue icon-check"
        text "Ledger Device authorized."
      elClass "h6" "ui header" $ text $ "This Ledger Device is now authorized to bake for the address: " <> toPublicKeyHashText pkh
      continue <- uiButton "primary" "Continue"
      pure ((["ledger-prompt"], continue), never)

setHighWaterMark
  :: MonadRhyoliteFrontendWidget Bake t m
  => Dynamic t RawLevel -> SecretKey -> PublicKeyHash -> Event t () -> m (Dynamic t [Text], Event t ())
setHighWaterMark latestBlockLevelDyn secretKey pkh close = ffor (workflow set) $ \d -> let (c, e) = splitDynPure d in (("add-baker":) <$> c, close <> switch (current e))
  where
    ledger = _secretKey_ledgerIdentifier secretKey
    set = Workflow $ do
      ledgerCheckImg ledger
      elClass "h5" "ui header" $ text "Set the high-water mark for this account on the connected Ledger Device?"
      elClass "h6" "ui header" $ text $ toPublicKeyHashText pkh
      el "p" $ do
        text "The high-water mark (latest recorded block level) will"
        el "br" blank
        text "be set to "
        display $ unRawLevel <$> latestBlockLevelDyn
        text " for this account."
      confirm <- uiButton "primary" "Confirm"
      pure ((["set-high-water-mark"], never), attachWith (\bl () -> waiting bl) (current latestBlockLevelDyn) confirm)
    waiting latestBlockLevel = Workflow $ do
      ledgerCheckImg ledger
      respondToPrompt $ do
        text "Reset HWM"
        el "br" blank
        text $ tshow $ unRawLevel latestBlockLevel
      pb <- getPostBuild
      _ <- requestingIdentity $ public (PublicRequest_SetHWM secretKey latestBlockLevel) <$ pb
      promptStep <- watchPrompting secretKey
      let changed = leftmost [updated promptStep, tag (current promptStep) pb]
          next = fforMaybe changed $ \case
            Just ss | Just (First hwmStep) <- _setupState_setHWM ss -> case hwmStep of
              SetHWMStep_Done -> Just $ done latestBlockLevel
              SetHWMStep_Disconnected -> Just $ handleClientErrorWorkflow (waiting latestBlockLevel) ClientError_LedgerDisconnected
              SetHWMStep_Declined -> Just $ handleClientErrorWorkflow (waiting latestBlockLevel) ClientError_RequestDeclinedByLedger
              SetHWMStep_Failed e -> Just $ handleClientErrorWorkflow (waiting latestBlockLevel) $ ClientError_Other e
              SetHWMStep_Prompting -> Nothing
            _ -> Nothing
      pure ((["set-high-water-mark-prompt"], never), next)
    done latestBlockLevel = Workflow $ do
      ledgerCheckImg ledger
      elClass "h5" "ui header" $ do
        icon "blue icon-check"
        text "High-water mark has been set."
      elClass "h6" "ui header prompt-text" $ do
        text "The high-water mark was set to: "
        el "br" blank
        text $ tshow $ unRawLevel latestBlockLevel
      continue <- uiButton "primary" "Continue"
      pure ((["set-high-water-mark-set"], continue), never)

handleClientErrorWorkflow
  :: DomBuilder t m
  => Workflow t m ([Text], Event t a) -> ClientError -> Workflow t m ([Text], Event t a)
handleClientErrorWorkflow recover = \case
  ClientError_RequestDeclinedByLedger -> requestDeclinedByLedger recover
  ClientError_LedgerDisconnected -> ledgerDisconnected recover
  ClientError_NodeNotReady -> nodeNotReady recover
  _ -> Workflow $ do
    elClass "h5" "ui center aligned header" $ text "Something went wrong"
    retry <- uiButton "primary" "Retry"
    pure ((["ledger-disconnected"], never), recover <$ retry)
  where
    requestDeclinedByLedger tryAgain = Workflow $ do
      elAttr "img" ("src" =: static @"images/ledger.svg" <> "class" =: "ledger") blank
      elClass "h5" "ui header" $ do
        icon "red icon-x"
        text "The request was declined by the Ledger Device."
      divClass "explanation" $ text $ "If you did not intend to reject the prompt on the Ledger Device you may click retry."
      retry <- uiButton "primary" "Retry"
      pure ((["ledger-declined"], never), tryAgain <$ retry)

    ledgerDisconnected tryAgain = Workflow $ do
      elAttr "img" ("src" =: static @"images/ledger.svg" <> "class" =: "ledger") blank
      elClass "h5" "ui header" $ text "Ledger Device was disconnected."
      restart <- uiButton "primary" "Restart"
      pure ((["ledger-disconnected"], never), tryAgain <$ restart) -- TODO restart should go back to start?

    nodeNotReady tryAgain = Workflow $ do
      elClass "h5" "ui header" $ text "Kiln needs to fully sync the node it is running with the blockchain before baking."
      divClass "explanation" $ text "Try again after the node has fully synced."
      retry <- uiButton "primary" "Dismiss"
      pure ((["launch-node"], never), tryAgain <$ retry)

ledgerCheckImg :: DomBuilder t m => LedgerIdentifier -> m ()
ledgerCheckImg ledger = do
  divClass "ledger" $ do
    elAttr "img" ("src" =: static @"images/ledger.svg") blank
    icon "blue icon-check"
  divClass "ledger-name" $ text $ unLedgerIdentifier ledger

nodeStatus :: Maybe ProcessState -> Int -> MonitoredStatus
nodeStatus mInternalState alertCount = min fromStatus fromAlert
  where
    fromStatus = case mInternalState of
      Just internalState -> case internalState of
        ProcessState_Stopped -> MonitoredStatus_Stopped
        ProcessState_Initializing -> MonitoredStatus_Unknown
        ProcessState_Node _ -> MonitoredStatus_Unknown
        ProcessState_Starting -> MonitoredStatus_Unknown
        ProcessState_Running -> MonitoredStatus_Healthy
        ProcessState_Failed -> MonitoredStatus_Unhealthy
      Nothing -> MonitoredStatus_Healthy
    fromAlert = case alertCount of
      0 -> MonitoredStatus_Healthy
      _ -> MonitoredStatus_Unhealthy

nodesList ::
  ( MonadRhyoliteFrontendWidget Bake t m
  , MonadRhyoliteFrontendWidget Bake t (ModalM m)
  , HasModal t m
  , MonadJSM (ModalM m)
  , MonadJSM (Performable (ModalM m))
  , HasJSContext (Performable (ModalM m))
  )
  => m ()
nodesList = do
  nodes <- ffor
    watchNodeAddresses
    $ fmap $ fmap $ \ns ->
      ( nodeSummaryIdentification ns
      , nodeStatus
        (nodeSummaryStateIfInternal ns)
        (_nodeSummary_alertCount ns)
      , isRight $ _nodeSummary_node ns
      )
  sidebarList "Node" nodes addNodeModal

addNodeModal ::
  ( MonadRhyoliteFrontendWidget Bake t m
  , MonadJSM m
  , MonadJSM (Performable m)
  , HasJSContext (Performable m)
  )
  => Event t () -> m (Dynamic t [Text], Event t ())
addNodeModal close = ffor (workflow splash) $ \d -> let (c, e) = splitDynPure d in (c, close <> switch (current e))

  where
    splash = Workflow $ do
      divClass "ui header" $ text "Add Nodes"
      divClass "ui grid stackable divided" $ do
        startNodeEv <- addInternal
        e <- addExternal
        addPublic
        pure ((pure "add-node", e), startNodeWorkflow splash <$ startNodeEv)

      where
        section header explanation = do
          elClass "h5" "ui header" $ text header
          divClass "explanation" $ text explanation

        addPublic = do
          divClass "add-public column" $ do
            section
              "Connect to a Public Node"
              "We recommend adding all public nodes to enhance monitoring accuracy."
            publicNodeOptions

        addInternal = do
          divClass "add-internal column" $ do
            section
              "Start a Kiln Node"
              "Start a node that is managed from within Kiln. Required if you intend to use Kiln to bake. Kiln only supports running a single node."
            node <- maybeDyn =<< watchInternalNode
            dEv <- dyn $ ffor node $ \case
              Nothing -> do
                uiButton "primary" "Start Node"

              Just _ -> do
                kilnLogo
                text "A Kiln node is running."
                pure never
            switchHold never dEv

        addExternal = do
         divClass "add-external column" $ mdo
           let feedback = elDynAttr "div" (ffor showSuccess $ ("class" =: "feedback" <>) . bool ("style" =: "display:none") mempty) $ do
                 icon "check blue"
                 text "Node added!"

           section
             "Monitor via Address"
             "Monitor nodes running locally or remotely via RPC."

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

startNodeWorkflow ::
  ( MonadRhyoliteFrontendWidget Bake t m
  , MonadJSM m
  , MonadJSM (Performable m)
  , HasJSContext (Performable m)
  )
  => Workflow t m ([Text], Event t ()) -> Workflow t m ([Text], Event t ())
startNodeWorkflow backWF = Workflow $ do
  backEv <- backButton
  divClass "ui header" $ text "Start a Kiln Node"
  elClass "h5" "ui header" $ text "Initialize Chain Data From:"
  rec
    useSnapshot <- holdDyn True (leftmost [True <$ e1, False <$ e2])
    (e1, mSelectedSnapshot) <- fakeRadioItem useSnapshot $ el "div" $ do
      el "div" $ text "Snapshot (Recommended)"
      divClass "explanation" $ do
        el "p" $ text "Snapshots are compressed versions of the blockchain, taken at a specific block level. Use a snapshot to considerably reduce initial node syncing time."
      divClass "" $ do
        rec
          let fileName = headMay <$> value fi
          dyn_ $ ffor fileName $ mapM $ \file -> do
            name <- liftJSM $ File.getName file
            divClass "file-name" $ text name
          fi <- fileInput def
        pure fileName
    (e2, _) <- fakeRadioItem (not <$> useSnapshot) $ divClass "" $ do
      divClass "" $ text "Peer to Peer Download"
      divClass "explanation" $ do
        el "p" $ text "Download the chain history from Genesis to the current head via peer to peer download (as nodes normally communicate on the blockchain)."

  contEv <- uiButton "primary" "Add Node"
  let
    ev = tag (current $ (,) <$> useSnapshot <*> mSelectedSnapshot) contEv
    next = ffor ev $ \(b, s) -> if b
      then Left s
      else Right ()
    launch = filterRight next
    uploadSnapshotEv = fmapMaybe id $ filterLeft next
  formEv <- performEvent $ ffor uploadSnapshotEv $ \f -> do
    liftIO $ putStrLn "starting file upload"
    fileToFormValue f

  mUri <- getBackendPath (InL BackendRoute_SnapshotUpload :/ ()) False
  let
    formUploadEv = (: []) . Map.singleton "snapshot-file" <$> formEv
  for_ mUri $ \uri -> postForms (Uri.render uri) formUploadEv

  launchedEv2 <- requestingIdentity $ formUploadEv $> public (PublicRequest_AddInternalNode (Just NodeProcessState_ImportingSnapshot))
  launchedEv <- requestingIdentity $ launch $> public (PublicRequest_AddInternalNode Nothing)

  pure ((pure "start-node", leftmost [launchedEv, launchedEv2]), leftmost
       [ backWF <$ backEv
       ])

verifySnapshotModal ::
  ( MonadReader r m
  , HasTimer t r
  , HasTimeZone r
  , MonadJSM (Performable m)
  , MonadRhyoliteFrontendWidget Bake t m
  )
  => SnapshotMeta -> Event t () -> m (Event t ())
verifySnapshotModal smd = cancelableModalWithClasses $ \close -> do
  divClass "ui header" $ text "Verify Snapshot"
  divClass "verify-top-message" $ do
    icon "large orange icon-warning"
    divClass "header" $ text "Verifying the Block Hash"
    divClass "explanation" $ do
      el "p" $ text "It is highly recommended to verify the hash of the highest block level of the snapshot. Use a third-party source that you trust to verify the data below."
      el "p" $ text "Copy the block hash and search for it on a block explorer. Make sure the block is valid and that the block date corresponds to the date the snapshot was taken."
      el "p" $ text "If you are in doubt that the snapshot is valid, close this window, remove the node and restart using a snapshot you trust."

  divClass "field" $ do
    divClass "detail" $ text "Snapshot's Highest Block Hash:"
    let
      hashText = case smd ^. snapshotMeta_headBlock of
        Just blk -> toBase58Text blk
        Nothing -> fromMaybe "<not-available>" $ smd ^. snapshotMeta_headBlockPrefix
    divClass "proposal-hash" $ do
      copyButton $ pure hashText
      text hashText
  divClass "field" $ do
    divClass "detail" $ text "Highest Block Level:"
    divClass "" $ text $ maybe "" (tshow . unRawLevel) (smd ^. snapshotMeta_headBlockLevel)
  divClass "field" $ do
    divClass "detail" $ text "Date Baked:"
    divClass "" $ maybe (text "") (localHumanizedTimestampBasic . constDyn) (smd ^. snapshotMeta_headBlockBakeTime)
  start <- divClass "buttons" $ uiButton "primary" "Start Node"
  response <- requestingIdentity $ public (PublicRequest_UpdateInternalWorker WorkerType_Node True) <$ start
  pure (pure ["confirmation"], leftmost [() <$ response, close])

showImportLogModal ::
  ( MonadReader r m
  , MonadRhyoliteFrontendWidget Bake t m
  )
  => Text -> Event t () -> m (Event t ())
showImportLogModal errorLog = cancelableModalWithClasses $ \close -> do
  divClass "ui header" $ text "Snapshot import log"
  divClass "log-message" $ el "pre" $ text errorLog
  close1 <- uiButton "primary" "Close"
  pure (pure ["show-error-log"], leftmost [close1, close])

osPublicNodeRemoveMessage :: DomBuilder t m => m ()
osPublicNodeRemoveMessage = do
  text $ "This Node can only be turned off via "
  let url = "https://gitlab.com/obsidian.systems/tezos-bake-monitor/blob/develop/docs/config.md#enable-obsidian-node-bool"
  elAttr "a" ("href" =: url <> "target" =: "_blank" <> "rel" =: "noopener") $ text "command line or config file."

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
      PublicNode_Blockscale -> "Tezos Foundation"
      PublicNode_TzScan -> "tzscan.io"

    describePublicNode = \case
      PublicNode_Obsidian -> text "Public Node Caching Service provided by Obsidian Systems. " *> osPublicNodeRemoveMessage
      PublicNode_Blockscale -> text "Load-balanced collection of nodes provided by the Tezos Foundation."
      PublicNode_TzScan -> text "API provided by tzscan.io, the block explorer by OCamlPro."

  pncDyn <- watchPublicNodeConfig
  divClass "ui publicnodes" $ for_ publicNodesInOrder $ \pn -> do
    let pnActiveDyn = isPublicNodeEnabled pn <$> pncDyn
        activeClass = if pn == PublicNode_Obsidian
          then constDyn "active"
          else bool "" "active" <$> pnActiveDyn
    (element', ()) <- SemUi.ui' "div"
        (def & SemUi.elConfigClasses .~ "public-node ui padded divided grid " <> (SemUi.Dyn activeClass)) $ divClass "row" $ do
      divClass "four wide column label" $ divClass "ui center aligned icon header" $ do
        SemUi.ui "i" (def & SemUi.elConfigClasses .~ (SemUi.Dyn $ bool "" "icon icon-check" <$> pnActiveDyn)) blank
        dynText $ bool (if pn == PublicNode_Obsidian then "Disabled" else "Add Node") "Added" <$> pnActiveDyn
      divClass "twelve wide column" $ do
        divClass "header" $ text $ showPublicNode pn
        divClass "description" $ describePublicNode pn

    let toggled = if pn == PublicNode_Obsidian
          then never
          else not . isPublicNodeEnabled pn <$> current pncDyn  <@ domEvent Click element'
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
    , MonadReader r m
    , MonadReader r (ModalM m)
    , MonadJSM (Performable (ModalM m))
    , HasFrontendConfig r, HasTimeZone r, HasTimer t r
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

        partition = (fmapMaybe $ preview _Left) &&& (fmapMaybe $ preview _Right)
        (external, internal) = splitDynPure $ partition . fmap _nodeSummary_node . MMap.getMonoidalMap <$> nodesDyn
        kilnNodeState = ((fmap _processData_state) . headMay . Map.elems) <$> internal

      useBlocker <- holdUniqDyn $ ffor (zipDyn publicNodesDyn nodesDyn) $ \(pn,n) -> MMap.null pn && MMap.null n
      -- let alertWindow = ClosedInterval LowerInfinity UpperInfinity
      alertWindow <- fmap Set.singleton <$> thirtySixHoursToInfinity

      kilnNodeStateD <- holdUniqDyn kilnNodeState
      -- Node alerts
      let
        verifySnapshotAlert = SemUi.segment (def & SemUi.classes SemUi.|~ "dashboard-section-overview") $ do
          dSm <- watchSnapshotMeta
          tz <- asks (^. timeZone)
          let
            i = icon "icon-check big blue"
            title = text "Snapshot import complete, verify to start node."
            timeFormat = "%-l:%M%P"
            time t = T.pack $ Time.formatTime Time.defaultTimeLocale timeFormat $ Time.utcToZonedTime tz t

            desc = dynText $ ffor dSm $ \sm -> "Your snapshot was successfully imported "
              <> maybe "" (\t -> "at " <> time t <> ", ") (_snapshotMeta_headBlockBakeTime =<< sm)
              <> "and a Kiln Node has been created. Before starting the node you must verify the snapshot."
            btn = do
              ev <- divClass "buttons" $ uiButtonM "" $ do
                icon "icon-angle-right"
                text "Start Verification"
              dyn_ $ ffor dSm $ traverse $ \sm -> tellModal $ verifySnapshotModal sm <$ ev
          renderSplashAlert i title Nothing (desc *> btn)

        snapshotImportFailedAlert = SemUi.segment (def & SemUi.classes SemUi.|~ "dashboard-section-overview") $ do
          let
            i = icon "icon-warning big red"
            title = text "Snapshot import failed."
            desc = do
              el "p" $ text "An unknown error has occured and the Kiln Node cannot be started."
              el "p" $ text "Fix: Logs may provide insight as to why this happened. Click the menu on the Kiln Node tile and select “Show import log”. Alternatively, removing and starting the Kiln Node again may fix the issue, but is not guaranteed. You may want to verify the snapshot you are using is valid."
          renderSplashAlert i title Nothing desc

      dyn_ $ ffor kilnNodeStateD $ traverse_ $ \case
        ProcessState_Node NodeProcessState_ImportComplete -> verifySnapshotAlert
        ProcessState_Node NodeProcessState_ImportFailed -> snapshotImportFailedAlert
        ProcessState_Node NodeProcessState_ImportTimeout -> snapshotImportFailedAlert
        ProcessState_Node NodeProcessState_ImportingSnapshot -> pure ()
        ProcessState_Node NodeProcessState_GeneratingIdentity -> pure ()
        ProcessState_Initializing -> pure ()
        ProcessState_Failed -> pure ()
        ProcessState_Starting -> pure ()
        ProcessState_Stopped -> pure ()
        ProcessState_Running -> pure ()

      -- Node tiles
      dyn_ $ ffor useBlocker $ \case
        True -> waitingForResponse
        False -> divClass "ui stackable cards" $ do
          ebn <- snd <$$$$> watchErrorsByNode alertWindow

          let
            errorMessages nodeId = do
              unresolvedAlertsForThisNode <- holdUniqDyn $ foldMap toList . MMap.lookup nodeId <$> ebn
              pure $ ffor unresolvedAlertsForThisNode $ fmap $ \(lTag :=> Identity log) -> case lTag of
                NodeLogTag_InaccessibleNode -> text "Unable to connect."
                NodeLogTag_NodeWrongChain -> text "On wrong network."
                NodeLogTag_NodeInvalidPeerCount -> text "Node has too few peers."
                NodeLogTag_BadNodeHead -> text $
                  fst (badNodeHeadMessage Const (Const . const "") log) <> "."

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
              (Just $ maybe True (all (\(t :=> _) -> case t of NodeLogTag_InaccessibleNode -> False; _ -> True)) . MMap.lookup nodeId <$> ebn)
              Nothing
              (Just $ (=<<) _nodeDetailsData_peerCount)
              (Just $ fromMaybe (NetworkStat 0 0 0 0) . fmap _nodeDetailsData_networkStat)
              nodeDetails

          void $ listWithKey internal $ \nodeId nodeData -> do
            errors <- errorMessages nodeId
            state <- holdUniqDyn $ _processData_state <$> nodeData

            bakerRunning <- fmap ((== Just True) . (fmap $ _bakerInternalData_running . snd))
              <$> watchInternalBaker
            let
              preface = "This node is run by Kiln. "
              notBakingBody action = action <> " it may affect any bakers you are running which depend on it."
              bakingBody action = "Kiln is also running a Baker that relies on this node to bake. "
                <> action <> " this node will stop Kiln’s Baker and may affect any other bakers you are running which depend on this node."
              body = bool notBakingBody bakingBody

              startStopNodeMenu :: m ()
              startStopNodeMenu = do
                let
                  stopModal running = warningModal "Stop Node?"
                    [preface, body running "Stopping"]
                    "Stop Node"

                runningDyn :: Dynamic t Bool <- (fmap . fmap) (== ProcessControl_Run) $ holdUniqDyn $ _processData_control <$> nodeData
                dyn_ $ ffor (zipDyn runningDyn bakerRunning) $ \case
                  (True, bRunning) ->
                    tileMenuEntryModal "Stop Node" $ stopModal bRunning $ (PublicRequest_UpdateInternalWorker WorkerType_Node False <$)
                  _ -> do
                    start <- tileMenuEntry "Start Node"
                    void $ requestingIdentity $ public (PublicRequest_UpdateInternalWorker WorkerType_Node True) <$ start

              verifyAndStartMenu sm = do
                tileMenuEntryModal "Verify and start node" $ (verifySnapshotModal sm)

              showLogMenu errorLog = do
                tileMenuEntryModal "Show Error Log" $ showImportLogModal errorLog

              removeNodeMenu = do
                let
                  epilogue = "All data for this node will be deleted from Kiln."
                  removeInternalNodeModal running = warningModal "Remove Node?"
                    [preface, body running "Removing", epilogue]
                    "Stop and Remove Node"
                dyn_ $ ffor bakerRunning $ \running ->
                  tileMenuEntryModal "Remove Node" $ removeInternalNodeModal running $
                    (PublicRequest_RemoveNode (Right ()) <$)

              title :: m ()
              title = text "Kiln Node"

              subtitle :: m ()
              subtitle =
                divClass "internal-subtitle" $ do
                  kilnLogo
                  divClass "ui sub header" $ dynText $ ffor state $ \case
                    ProcessState_Stopped -> "Stopped"
                    ProcessState_Initializing -> "Initializing"
                    ProcessState_Node s -> case s of
                      NodeProcessState_ImportingSnapshot -> "SETUP"
                      NodeProcessState_ImportComplete -> "SETUP"
                      NodeProcessState_ImportFailed -> "FAILED"
                      NodeProcessState_ImportTimeout -> "FAILED"
                      NodeProcessState_GeneratingIdentity -> "STARTING"
                    ProcessState_Starting -> "Starting"
                    ProcessState_Running -> "Running"
                    ProcessState_Failed -> "Failed"

              workingTile :: m ()
              workingTile = do
                nodeDetails <- watchNodeDetails nodeId
                standardNodeTile @(ProcessData, Maybe NodeDetailsData)
                  title
                  subtitle
                  (startStopNodeMenu *> removeNodeMenu)
                  ((=<<) getNodeHeadBlock . snd)
                  (Just errors)
                  (Just $ ffor2 ebn nodeData $ \es nd -> and
                    [ maybe True (all (\(t :=> _) -> case t of NodeLogTag_InaccessibleNode -> False; _ -> True)) (MMap.lookup nodeId es)
                    , _processData_state nd == ProcessState_Running
                    ])
                  (Just $ _processData_state . fst)
                  (Just $ (=<<) _nodeDetailsData_peerCount . snd)
                  (Just $ fromMaybe (NetworkStat 0 0 0 0) . fmap _nodeDetailsData_networkStat . snd)
                  ((,) <$> nodeData <*> nodeDetails)

              nodeStartTile :: NodeProcessState -> Maybe SnapshotMeta -> m ()
              nodeStartTile nodeState mSnapshotMeta = nodeTileWithSections $
                [ tileHeader title subtitle menu badge Nothing
                , divClass "internal-node-tile-body" $ do
                    -- when (nodeState == NodeProcessState_ImportingSnapshot || nodeState == NodeProcessState_GeneratingIdentity) $
                    case nodeState of
                      NodeProcessState_ImportingSnapshot -> divClass "generating-icons" $ do
                        icon "icon-install big"
                        divClass "ui active tiny inline loader blue small" blank
                      NodeProcessState_GeneratingIdentity -> divClass "generating-icons" $ do
                        icon "icon-id-badge big"
                        divClass "ui active tiny inline loader blue small" blank
                      _ -> blank
                    let
                      subHeader t = divClass "ui sub header" $ text t
                      errorMessage t = divClass "ui error message" $ text t
                    divClass "ui row" $ case nodeState of
                      NodeProcessState_ImportingSnapshot -> subHeader "Importing snapshot"
                      NodeProcessState_ImportComplete -> subHeader "Verify snapshot"
                      NodeProcessState_ImportFailed -> errorMessage "Snapshot import failed"
                      NodeProcessState_ImportTimeout -> errorMessage "Snapshot import failed"
                      NodeProcessState_GeneratingIdentity -> subHeader "Generating identity"
                    divClass "ui row" $ divClass "explanation" $ text $ case nodeState of
                      NodeProcessState_ImportingSnapshot -> "Depending on your hardware, importing a snapshot may take up to a few hours."
                      NodeProcessState_ImportComplete -> "You must verify this snapshot before starting the node."
                      NodeProcessState_ImportFailed -> ""
                      NodeProcessState_ImportTimeout -> ""
                      NodeProcessState_GeneratingIdentity -> "Before the node can run it must generate a secure identity to use on the network. This may take several minutes."
                    when (nodeState == NodeProcessState_ImportComplete) $ for_ mSnapshotMeta $ \sm -> for (_snapshotMeta_headBlock sm) $ \_ -> do
                      ev <- divClass "buttons" $ uiButtonM "" $ do
                        icon "icon-angle-right"
                        text "Start Verification"
                      tellModal $ ev $> verifySnapshotModal sm
                ]
                where
                  menu = case nodeState of
                    NodeProcessState_ImportingSnapshot -> Nothing -- no way to cancel this
                    NodeProcessState_ImportComplete -> Just $ do
                      mapM_ verifyAndStartMenu mSnapshotMeta
                      removeNodeMenu
                    NodeProcessState_ImportFailed -> Just $ do
                      mapM_ showLogMenu (_snapshotMeta_importError =<< mSnapshotMeta)
                      removeNodeMenu
                    NodeProcessState_ImportTimeout -> Just removeNodeMenu
                    NodeProcessState_GeneratingIdentity -> Just $ startStopNodeMenu *> removeNodeMenu
                  badge :: m ()
                  badge = tileBadgeImpliedByErrors (Just errors) (Just state)
            dSnapshotMeta <- watchSnapshotMeta
            dyn_ $ ffor2 state dSnapshotMeta $ \case
              (ProcessState_Node s) -> nodeStartTile s
              _ -> const workingTile

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
                dyn_ $ ffor source $ \s -> if s == PublicNode_Obsidian
                  then osPublicNodeRemoveMessage
                  else tileMenuEntryModal "Remove Node" $ removeItemModal "node" mkRemoveReq

            standardNodeTile
              title
              blank
              publicNodeMenu
              (Just . mkVeryBlockLike)
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              vDyn

    tileHeader
      :: m () -- ^ Title
      -> m () -- ^ Subtitle
      -> Maybe (m ()) -- ^ Tile menu contents
      -> m () -- ^ Status badge
      -> Maybe (Dynamic t [m ()]) -- ^ (Optional) Function to build list of error messages for this node
      -> m ()
    tileHeader title subtitle menuContents badge errors' = do
      case menuContents of
        Nothing -> divClass "tile-header-spacing" blank
        Just c -> tileMenu c
      divClass "title" $ do
        badge
        title
        divClass "secondary-name" subtitle
      tileErrors errors'

    tileErrors = traverse_ $ \errors ->
      dyn_ $ ffor errors $ traverse_ (divClass "ui error message")

    tileBadgeImpliedByErrors
      :: Maybe (Dynamic t [a])
      -> Maybe (Dynamic t ProcessState)
      -> m ()
    tileBadgeImpliedByErrors mErrors mInternalState = do
      let color = fmap statusColor $ nodeStatus
            <$> sequence mInternalState
            <*> (maybe (pure 0) (fmap length) mErrors)
      iconDyn $ ("tiny circle " <>) <$> color

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

    tileConnectionStats connected' getPeerCount' getNetworkStats' node =
      if isNothing connected' && isNothing getPeerCount' && isNothing getNetworkStats'
      then Nothing
      else Just $ do
        for_ (liftA2 (,) connected' getPeerCount') $ \(connected, getPeerCount) -> do
          peerCount <- holdUniqDyn $ getPeerCount <$> node
          elClass "span" "peer-count" $ dynText $ ffor2 peerCount connected $ \p c -> if c then maybe "-" tshow p else "-"
          text " connected peers"

        for_ (liftA2 (,) connected' getNetworkStats') $ \(connected, getNetworkStats) -> do
          let
            stat = getNetworkStats <$> node
            showSpeed c n = dynText <=< holdUniqDyn $ ffor2 c n $ \c' -> if c' then fromIntegral >>> humanBytes >>> (<> "/s") else const "-"
            showTotal c n = dynText <=< holdUniqDyn $ ffor2 c n $ \c' -> if c' then unTezosWord64 >>> fromIntegral >>> humanBytes else const "-"

          divClass "stats" $ do
            divClass "column heading" $ do
              divClass "cell" $ text "Speed"
              divClass "cell" $ text "Total"

            divClass "column" $ do
              divClass "cell" $ icon "icon-arrow-up" *> showSpeed connected (_networkStat_currentOutflow <$> stat)
              divClass "cell" $ icon "icon-arrow-up" *> showTotal connected (_networkStat_totalSent <$> stat)

            divClass "column" $ do
              divClass "cell" $ icon "icon-arrow-down" *> showSpeed connected (_networkStat_currentInflow <$> stat)
              divClass "cell" $ icon "icon-arrow-down" *> showTotal connected (_networkStat_totalRecv <$> stat)

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
      -> Maybe (Dynamic t Bool) -- ^ (Optional) Are we connected to the node?
      -> Maybe (a -> ProcessState) -- ^ (Optional) Function to build list of error messages for this node
      -> Maybe (a -> Maybe Word64) -- ^ (Optional) Function to get the peer count of the node
      -> Maybe (a -> NetworkStat) -- ^ (Optional) Function to get the network stats of the node
      -> Dynamic t a -- ^ Node
      -> m ()
    standardNodeTile title subtitle menuContents getBlock errors' connected internalState getPeerCount' getNetworkStats' node = do
      let badge = tileBadgeImpliedByErrors errors' $ fmap (<$> node) internalState
      nodeTileWithSections $
        [ tileHeader title subtitle (Just menuContents) badge errors'
        , tileBlockStats getBlock node
        ]
        <> toList (tileConnectionStats connected getPeerCount' getNetworkStats' node)

    nodeTileWithSections :: [m ()] -> m ()
    nodeTileWithSections = divClass "ui card dashboard-tile node-tile" . divClass "content" .
      sequence_ . intersperse (divClass "divider" blank)


data BakersBanner
  = BakersBanner_Gathering
  | BakersBanner_CannotGather
  deriving (Eq, Ord, Show)

data BakerAlert
  = BakerAlert_Alert (DSum BakerLogTag Identity)
  | BakerAlert_GroupedAlert (RawLevel, UTCTime) (RawLevel, UTCTime) (NonEmpty ErrorLogBakerMissed)
  deriving (Eq, Ord, Show)

groupBakerAlerts :: [(ErrorLog, DSum BakerLogTag Identity)] -> [BakerAlert]
groupBakerAlerts bs = (map BakerAlert_Alert others) ++ (group bakerMiss) ++ (group endorseMiss)
  where
    (others, bakerMiss, endorseMiss) = foldl' partitionF ([], [], []) bs
    partitionF
      :: (a ~ (DSum BakerLogTag Identity), c ~ ErrorLogBakerMissed)
      => ([a], [(b, c)], [(b, c)])
      -> (b, a)
      -> ([a], [(b, c)], [(b, c)])
    partitionF (os, bms, ems) (elog, v@(lTag :=> Identity log)) = case lTag of
      BakerLogTag_BakerMissed -> case _errorLogBakerMissed_right log of
        RightKind_Baking -> (os, (elog, log) : bms, ems)
        RightKind_Endorsing -> (os, bms, (elog, log) : ems)
      _ -> (v : os, bms, ems)

    group ls' = case NEL.nonEmpty ls' of
      Nothing -> []
      Just ((_,l) :| []) -> [BakerAlert_Alert (BakerLogTag_BakerMissed :=> Identity l)]
      Just ls -> [BakerAlert_GroupedAlert (applyF minimumBy) (applyF maximumBy) $ fmap snd ls]
        where
          applyF f = (\(e, log) -> (_errorLogBakerMissed_level log, _errorLog_started e)) $ f (comparing fst) ls

bakersTab
  :: forall r m t.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadReader r m, HasTimer t r, HasTimeZone r
    , MonadReader r (ModalM m), HasFrontendConfig r, MonadJSM (Performable (ModalM m))
    , HasModal t m, MonadRhyoliteFrontendWidget Bake t (ModalM m)
    )
  => m ()
bakersTab =
  divClass "dashboard-section dashboard-section-bakers" $ do
    tilesWidget =<< watchBakerAddresses
  where
    tilesWidget :: Dynamic t (MonoidalMap PublicKeyHash BakerSummary) -> m ()
    tilesWidget tilesDyn = do
      useBlocker <- holdUniqDyn $ MMap.null <$> tilesDyn
      alertWindow <- fmap Set.singleton <$> thirtySixHoursToInfinity
      dEbb :: Dynamic t (MonoidalMap PublicKeyHash (NonEmpty (ErrorLog, BakerErrorLogView))) <- watchErrorsByBaker alertWindow
      dCollectiveNodesStatus <- watchCollectiveNodesStatus alertWindow
      dyn_ $ ffor useBlocker $ \case
        True -> waitingForResponse
        False -> mdo
          let
            anyErrors = (any (\(f :=> _) -> isUserResolvable $ LogTag_Baker f)) . map snd . concatMap NEL.toList <$> dEbb
          resolveAll <- uiDynButton ((<>) "primary right floated " . bool "transition hidden" "" <$> anyErrors) $ do
            icon "icon-check"
            text "Resolve All"
          let
            toLogTag (f :=> k) = let g = LogTag_Baker f in if isUserResolvable g
              then Just $ g :=> Const (errorLogIdForErrorLogView $ g :=> k)
              else Nothing
            alerts = concatMap (catMaybes . fmap toLogTag . map snd . NEL.toList) . MMap.elems <$> current dEbb
          _ <- requestingIdentity $ attachWith (\as () -> public $ PublicRequest_ResolveAlerts as) alerts resolveAll
          elClass "h4" "dashboard-section-title" $ text "Bakers"

          let
            bakerStatus' = ffor2 dCollectiveNodesStatus tilesDyn $ \cns ->
              fmap $ \bakerSummary ->
                bakerStatus $ bakerSummary <$ cns
            wantBakerData = (||)
              <$> (elem MonitoredStatus_Unknown <$> bakerStatus')
              <*> (any isNothing <$> joinDynThroughMap bakersDetails)
          (bakersBanner :: Dynamic t (Maybe BakersBanner)) <-
            holdUniqDyn $ ffor2 dCollectiveNodesStatus wantBakerData $ \case
              Left _ -> \_ -> Just BakersBanner_CannotGather
              Right () -> \cond -> BakersBanner_Gathering <$ guard cond
          dyn_ $ ffor bakersBanner $ mkBakersBanner

          let notifications :: Dynamic t (Map.Map (Down BakerAlert) ())
              notifications = Map.fromList . fmap (\k -> (Down k, ())) . foldMap toList . MMap.elems . (fmap (groupBakerAlerts . NEL.toList)) <$> dEbb
          _ <- listWithKey notifications $ \(Down k) _ -> splashAlert tilesDyn k

          (bakersDetails :: Dynamic t (Map.Map PublicKeyHash
                                               (Dynamic t (Maybe BakerDetails)))) <- divClass "ui stackable cards" $ do
            listWithKey (coerceDynamic tilesDyn) $ \pkh vDyn -> do
              unresolvedAlerts <- holdUniqDyn $ foldMap toList . MMap.lookup pkh <$> dEbb

              let
                renderBakerError = text . _bakerErrorDescriptions_tile

                bakerAlerts = (++)
                  <$> (ffor dCollectiveNodesStatus $ \case
                          Left e -> [Left e]
                          Right _ -> [])
                  <*> (map Right . groupBakerAlerts <$> unresolvedAlerts)

                errorMessages = ffor bakerAlerts $ mapMaybe $ \case
                  Left (_ :: CollectiveNodesFailure) -> Just $ text "Cannot gather baker data."
                  Right (BakerAlert_Alert (lTag :=> Identity log)) -> case lTag of
                    BakerLogTag_BakerMissed -> Just $ text $ "Missed " <> aRight <> "."
                      where
                        aRight = case _errorLogBakerMissed_right log of
                          RightKind_Baking -> "a bake"
                          RightKind_Endorsing -> "an endorsement"
                    BakerLogTag_BakerDeactivated -> Just $ renderBakerError $ bakerDeactivatedDescriptions log
                    BakerLogTag_BakerDeactivationRisk -> Just $ renderBakerError $ bakerDeactivationRiskDescriptions log
                    BakerLogTag_BakerAccused -> Just $ renderBakerError $ bakerAccusedDescriptions log
                    BakerLogTag_InsufficientFunds -> Just $ renderBakerError $ bakerInsufficientFundsDescriptions log
                    BakerLogTag_VotingReminder -> Nothing
                  Right (BakerAlert_GroupedAlert _ _ ls@(log:|_)) -> Just $ el "span" $ do
                    elClass "span" "ui label circular" $ text $ tshow (length ls)
                    text nbsp
                    text $ "Missed " <> aRight <> "."
                    where
                      aRight = case _errorLogBakerMissed_right log of
                        RightKind_Baking -> "a bake"
                        RightKind_Endorsing -> "an endorsement"

              let (title, subtitle) = splitDynPure $ bakerSummaryIdentification . (pkh,) <$> vDyn
              titleUniq <- holdUniqDyn title
              subtitleUniq <- holdUniqDyn subtitle
              details <- watchBakerDetails pkh

              tile
                (dynText titleUniq)
                pkh
                subtitleUniq
                (\ev -> PublicRequest_RemoveBaker pkh <$ ev)
                -- if you have both a bake and endorse for the same level, you
                -- must *first* bake the block at that level, then you may
                -- immediately endorse that block.  the times are the same,
                -- baking happens first.
                (Just errorMessages)
                vDyn
                details
                dCollectiveNodesStatus

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
          Nothing
          (text "Some information will be temporarily unavailable as Kiln gathers information from the blockchain. This can take up to a few hours. Kiln will gather new data about this baker in the background at the beginning of each cycle when new rights are available.")
        BakersBanner_CannotGather -> renderSplashAlert
          (icon "icon-disconnected big red")
          (text "Cannot gather baker data - no nodes online.")
          Nothing
          (do
             el "p" $ text "Kiln cannot gather baker data if no nodes are synced with the blockchain."
             el "p" $ do
               el "strong" $ text "Fix:"
               text " "
               ensureHealthyNodes)

    splashAlert :: Dynamic t (MonoidalMap PublicKeyHash BakerSummary) -> BakerAlert -> m ()
    splashAlert tilesDyn = SemUi.segment (def & SemUi.classes SemUi.|~ "dashboard-section-overview") . \case
      BakerAlert_Alert errorView@(bTag :=> Identity log) ->
        let
          pkh = bakerIdForBakerErrorLogView errorView
          ev = (LogTag_Baker bTag :=> Identity log) :| []
        in case bTag of
          BakerLogTag_BakerMissed -> renderBakerError ev (pure $ bakerMissedDescriptions log) pkh
          BakerLogTag_BakerDeactivated -> renderBakerError ev (pure $ bakerDeactivatedDescriptions log) pkh
          BakerLogTag_BakerDeactivationRisk -> renderBakerError ev (pure $ bakerDeactivationRiskDescriptions log) pkh
          BakerLogTag_BakerAccused -> renderBakerError ev (pure $ bakerAccusedDescriptions log) pkh
          BakerLogTag_InsufficientFunds -> renderBakerError ev (pure $ bakerInsufficientFundsDescriptions log) pkh
          BakerLogTag_VotingReminder ->
            withAmendmentPeriodProgress (_errorLogVotingReminder_votingPeriod log) $ \remaining ->
              renderBakerError ev (bakerVotingReminderDescriptions log <$> remaining) pkh

      BakerAlert_GroupedAlert first' latest' ls@(log:|_) -> do
        tz <- asks (^. timeZone)
        let
          pkh = unId $ _errorLogBakerMissed_baker log
          ev = (\l -> LogTag_Baker BakerLogTag_BakerMissed :=> Identity l) <$> ls
        renderBakerError ev (pure $ bakerGroupedMissedDescriptions tz (length ls) first' latest' log) pkh

      where
        renderBakerError :: NonEmpty ErrorLogView -> Dynamic t BakerErrorDescriptions -> PublicKeyHash -> m ()
        renderBakerError ev dsc pkh = do
          let warning = _bakerErrorDescriptions_warning <$> dsc
          renderResolvableSplashAlert ev
            (iconDyn $ ffor warning $ \w -> "icon-warning big " <> bool "red" "orange" (isJust w))
            (dynText (_bakerErrorDescriptions_title <$> dsc) *> text ".")
            (Just $ dyn_ $ ffor tilesDyn $ maybe blank (bakerSummaryLabel pkh) . MMap.lookup pkh)
            (do
              dyn_ $ ffor (_bakerErrorDescriptions_problem <$> dsc) $ \problems ->
                for_ problems $ \par ->
                  htmlErrorDescription par *> el "br" blank
              dyn_ $ ffor warning $ \w -> for_ w $ el "p" . text
              elClass "p" "fix" $ do
                el "strong" $ text "Fix:"
                text " "
                dynText $ _bakerErrorDescriptions_fix <$> dsc
            )

    tile
      :: m () -- ^ Title
      -> PublicKeyHash
      -> Dynamic t (Maybe Text) -- ^ Subtitle
      -> (Event t () -> Event t (PublicRequest Bake ())) -- ^ Construct an API request with an 'Event' to remove this baker.
      -> Maybe (Dynamic t [m ()]) -- ^ (Optional) Function to build list of error messages for this baker
      -> Dynamic t BakerSummary -- ^ Baker
      -> Dynamic t (Maybe BakerDetails) -- ^ Details
      -> Dynamic t (Either CollectiveNodesFailure ())
      -> m ()
    tile title pkh subtitle mkRemoveReq errors' bakerDyn details' dCollectiveNodesStatus = do
      let connected = isRight <$> dCollectiveNodesStatus
      divClass "ui card dashboard-tile baker-tile" $ divClass "content" $ do

        tooltipAndBadge :: Dynamic t (Maybe (Dynamic t (m (), Text))) <- do
          damendment <- watchAmendment
          dmBakerVote <- watchBakerVote
          dproposals <- watchProposals
          let voteBadge = "large blue icon-vote-badge icon"
              dotsBadge = "large grey icon-dots-badge icon"
              checkBadge = "large grey icon-check-badge icon"

              accessVoting = divClass "detail" $ do
                text "Access Voting from the extras menu "
                icon "icon-ellipsis grey"
                text "on this baker tile."

              noProposals = (, dotsBadge) $ divClass "detail" $ text "Waiting for proposals to be submitted"

              hasUpvoted n =
                let outOfUpvotes = n >= maxProposalUpvotes
                in (, bool voteBadge checkBadge outOfUpvotes) $ do
                  divClass "detail" $ text $ T.intercalate " "
                    [ "You have upvoted"
                    , tshow n
                    , "proposals of"
                    , tshow maxProposalUpvotes
                    , "allowed."
                    ]
                  unless outOfUpvotes
                    accessVoting

              notVoted = (, voteBadge) $ do
                divClass "detail" $ text "This baker has not voted in the current period."
                accessVoting

              hasVoted bv =
                let included = isJust $ _bakerVote_included bv
                in (, bool dotsBadge checkBadge included) $ do
                  divClass "detail" $ text $
                    "You voted '" <> textBallot (_bakerVote_ballot bv) <> "' on the current proposal."
                  unless included $
                    divClass "detail" $ text "Waiting for your vote to be included in the block chain."

          maybeDyn $ ffor3 damendment dmBakerVote dproposals $ \am mBakerVote proposals -> case Map.lookupMax am of
            Nothing -> Nothing
            Just (k, _) -> case k of
              VotingPeriodKind_Proposal -> Just $
                if null proposals
                then noProposals
                else case Map.size $ Map.filter (isJust . snd) proposals of
                  0 -> notVoted
                  n -> hasUpvoted n
              VotingPeriodKind_Testing -> Nothing
              _ -> Just $ maybe notVoted hasVoted mBakerVote

        tileMenu $ do
          let
            removeEntry modal = tileMenuEntryModal "Remove Baker" $ modal mkRemoveReq
          dyn $ ffor bakerDyn $ \bs -> case _bakerSummary_baker bs of
            Left _ -> do -- not a kiln baker
              removeEntry $ removeItemModal "baker"
            Right bid -> do
              mPeriodKind_amendment <- maybeDyn . fmap Map.lookupMax =<< watchAmendment
              whenJustDyn mPeriodKind_amendment $ \periodKind_amendment -> do
                isTestingPeriod <- holdUniqDyn $ (VotingPeriodKind_Testing ==) . fst <$> periodKind_amendment
                dyn_ $ ffor isTestingPeriod $ \case
                  True -> pure ()
                  False -> do
                    open <- tileMenuEntry "Vote"
                    let amendment = snd <$> periodKind_amendment
                    mProtoInfo <- maybeDyn =<< watchProtoInfo
                    let baker = ffor (current bakerDyn) $ \summary -> case _bakerSummary_baker summary of
                          Left _ -> Nothing
                          Right b -> Just (pkh, _bakerInternalData_secretKey b)
                        xs = (liftA3 . liftA3) (,,) (current mProtoInfo) (pure . pure <$> current amendment) baker
                    tellModal $ attachWithMaybe (\ma () -> ffor ma $ \(p,a,b) -> cancelableModalWithClasses $ fmap (pure ["vote-modal"],) . voteModal b p a) xs open

              let sk = _bakerInternalData_secretKey bid
              tileMenuEntryModal "Authorize Ledger Device" $ cancelableModalWithClasses $ authorizeLedgerToBakeModal sk pkh
              latestHead <- maybeDyn =<< watchLatestHead
              dyn_ $ ffor latestHead $ \case
                Nothing -> pure () -- no head to set high water mark
                Just bl -> tileMenuEntryModal "Set High-Water Mark" $ cancelableModalWithClasses $ setHighWaterMark (_veryBlockLike_level <$> bl) sk pkh
              if _bakerInternalData_running bid
              then do
                let stopModal = warningModal "Stop Baker?"
                      ["This baker will not be able to sign blocks or endorsements once stopped. You can restart this baker at any time."]
                      "Stop Baker"
                tileMenuEntryModal "Stop Baker" $ stopModal (PublicRequest_UpdateInternalWorker WorkerType_Baker False <$)
              else do
                start <- tileMenuEntry "Start Baker"
                void $ requestingIdentity $ public (PublicRequest_UpdateInternalWorker WorkerType_Baker True) <$ start
              let
                removeInternalBakerModal = warningModal "Remove Baker?"
                  ["This baker will not be able to sign blocks or endorsements once removed and all related baker data will be deleted."]
                  "Remove Baker"
              removeEntry removeInternalBakerModal

        divClass "title" $ do
          let bakerStatusDyn = (\b n -> bakerStatus $ b <$ n) <$> bakerDyn <*> dCollectiveNodesStatus
          for_ errors' $ \errors -> do
            _errorsEmpty <- holdUniqDyn $ null <$> errors
            iconDyn $ fmap (("tiny circle " <>) . statusColor) bakerStatusDyn
          title
          isInternal <- holdUniqDyn $ isRight . _bakerSummary_baker <$> bakerDyn
          dyn_ $ ffor isInternal $ \i -> when i $ do
            divClass "baker-vote-popup" $ do
              whenJustDyn tooltipAndBadge $ \tb -> do
                let (t,b) = splitDynPure tb
                tooltipped TooltipPos_TopLeft (dyn_ t) $
                  elDynAttr "i" (("class" =:) <$> b) blank
            divClass "internal-subtitle" $ do
              kilnLogo
              divClass "ui sub header" $ dynText $ ffor bakerStatusDyn $ \case
                MonitoredStatus_Stopped -> "Stopped"
                MonitoredStatus_Healthy -> "Running"
                MonitoredStatus_Unhealthy -> "Unhealthy"
                MonitoredStatus_Unknown -> "Unknown"
          divClass "secondary-name" $ dynText =<< holdUniqDyn (fromMaybe nbsp <$> subtitle)

        for_ errors' $ \errors -> do
          dyn_ $ ffor errors $ traverse_ (divClass "ui error message")

        let
          nextRightsTxt = ffor (_bakerSummary_nextRight <$> bakerDyn) $ \case
            BakerNextRight_GatheringData -> Left $ text "-"
            BakerNextRight_WaitingForRights -> Left $ text "Waiting to receive rights"
            BakerNextRight_KnownNoRights -> Left $ text "None"
            BakerNextRight_KnownRights (r,l) -> Right (r,l)
          wantToGatherData = (== BakerNextRight_GatheringData) . _bakerSummary_nextRight <$> bakerDyn
        isGatheringData <- holdUniqDyn $ (&&) <$> wantToGatherData <*> connected

        divClass "divider" blank

        el "dl" $ do
          latestHead <- watchLatestHead
          dparameters <- watchProtoInfo
          el "div" $ do
            el "dt" (text "Next")
            el "dd" $ dyn_ $ ffor nextRightsTxt $ \case
              Left t -> t
              Right (r,l) -> do
                text $ case r of
                  RightKind_Baking -> "Bake block "
                  RightKind_Endorsing -> "Endorse block "
                text $ tshow $ unRawLevel l
                let eventDyn = constDyn (r, l)
                etaDyn <- maybeDyn $ getCompose $ predictFutureTimestamp <$> Compose dparameters <*> (Compose $ fmap (Just . snd) eventDyn) <*> Compose latestHead
                text nbsp
                dyn_ $ ffor etaDyn $ maybe blank localHumanizedTimestampBasicWithoutTZ

        let
          dmDelegateInfo = preview (_Just . bakerDetails_delegateInfo . _Just . to unJson) <$> details'
        elClass "table" "baker-balance" $ do
          el "tr" $ do
            el "td" (text "Available Balance")
            elClass "td" "baker-balance-whole" $ withPlaceholder $ ffor dmDelegateInfo $ fmap $ \t -> do
              let (w, _p, _tz) = tez' $ _cacheDelegateInfo_balance t - _cacheDelegateInfo_frozenBalance t
              text w
            elClass "td" "baker-balance-part" $ withPlaceholder' "" $ ffor dmDelegateInfo $ fmap $ \t -> do
              let (_w, p, tz) = tez' $ _cacheDelegateInfo_balance t - _cacheDelegateInfo_frozenBalance t
              text p
              elClass "span" "tez" $ text tz

          el "tr" $ do
            el "td" (text "Staking Balance")
            elClass "td" "baker-balance-whole" $ withPlaceholder $ ffor dmDelegateInfo $ fmap $ \t -> do
              let (w, _p, _tz) = tez' $ _cacheDelegateInfo_stakingBalance t
              text w
            elClass "td" "baker-balance-part" $ withPlaceholder' "" $ ffor dmDelegateInfo $ fmap $ \t -> do
              let (_w, p, tz) = tez' $ _cacheDelegateInfo_stakingBalance t
              text p
              elClass "span" "tez" $ text tz

        dyn_ $ ffor isGatheringData $ \case
          False -> blank
          True -> divClass "ui active inline loader mini blue" blank
              *> text "Gathering baker data."

renderResolvableSplashAlert :: (MonadRhyoliteFrontendWidget Bake t m)
  => NonEmpty ErrorLogView
  -> m () -- ^ Alert icon
  -> m () -- ^ Title
  -> Maybe (m ()) -- ^ Entity
  -> m () -- ^ Description body
  -> m ()
renderResolvableSplashAlert es@((etag :=> _) :| _) splashIcon title entity desc = do
  renderSplashAlert splashIcon title entity $ do
    desc
    when (isUserResolvable etag) $ do
      resolve <- divClass "buttons" $ uiButtonM "primary" $ do
        icon "icon-check"
        text "Resolve"
      void $ requestingIdentity $ public (PublicRequest_ResolveAlerts es') <$ resolve
  where es' = (\e -> etag :=> (Const $ errorLogIdForErrorLogView e)) <$> NEL.toList es

renderSplashAlert :: (MonadRhyoliteFrontendWidget Bake t m)
  => m () -- ^ Alert icon
  -> m () -- ^ Title
  -> Maybe (m ()) -- ^ Entity
  -> m () -- ^ Description body
  -> m ()
renderSplashAlert splashIcon title entity desc = do
  elClass "div" "dashboard-section-overview-icon" $ splashIcon
  elClass "div" "dashboard-section-overview-body" $ do
    divClass "ui header" $ title
    for_ entity $ divClass "alert-entity"
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
removeItemModal name = reminderModal
  ("Remove this " <> name <> "?")
  ("You can always add this " <> name <> " again from the \"Add " <> pluralOf (T.toTitle name) <> "\" button.")
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

waitingForResponse :: DomBuilder t m => m ()
waitingForResponse = divClass "ui basic segment" $ divClass "ui active centered inline text loader" $ text "Waiting for response"

semuiTab :: (DomBuilder t m, PostBuild t m, Eq k) => m () -> k -> Demux t k -> Dynamic t Enabled -> m (Event t k)
semuiTab label k currentTab enabled =
  fmap ((k <$) . gate (isEnabled <$> current enabled) . domEvent Click . fst) $
    elDynAttr' "a" `flip` label $ ffor (zipDyn enabled $ demuxed currentTab k) $ \(e,b) ->
      "class" =: T.unwords (["item"] ++ ["disabled" | isDisabled e] ++ ["active" | b])

withAmendmentPeriodProgress :: (HasTimer t r, MonadReader r m, MonadRhyoliteFrontendWidget Bake t m)
                     => RawLevel -> (Dynamic t Time.NominalDiffTime -> m ()) -> m ()
withAmendmentPeriodProgress expectedVotingPeriod w = do
  currentTime <- asks (^. timer)
  mProtoInfo <- maybeDyn =<< watchProtoInfo
  amendments <- watchAmendment
  mAmendment <- maybeDyn $ fmap snd . Map.lookupMax <$> amendments
  dyn_ $ ffor ((liftA2 . liftA2) (,) mProtoInfo mAmendment) $ \case
    Nothing -> divClass "loading" blank
    Just (protoInfo, amendment) -> do
      (period, votingPeriod) <- fmap splitDynPure $
        holdUniqDyn $ (_amendment_period &&& _amendment_votingPeriod) <$> amendment
      let
        thisPeriodEndTime = fmap fst $ getEndTimeForPeriod <$> period <*> amendment <*> amendments <*> protoInfo
        periodEndsIn =
          fmap (expectedVotingPeriod ==) votingPeriod >>= \case
            True -> liftA2 Time.diffUTCTime thisPeriodEndTime currentTime
            False -> pure 0 -- The latest period is not the same as the expected one, so we assume it's over.
      w periodEndsIn
