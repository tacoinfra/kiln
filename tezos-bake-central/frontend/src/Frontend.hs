{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend where

import Control.Lens (imap, to, (<>~))
import Control.Monad (unless)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader (ReaderT)
import Data.Aeson.Lens
import Data.Bool (bool)
import Data.Constraint.Extras
import Data.Default
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Infix hiding ((<&>))
import Data.GADT.Compare
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as Time
import Data.Version
import Data.Word (Word64)
import Database.Id.Class
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.File as File
import qualified GHCJS.DOM.Location as Location
import GHCJS.DOM.Types (MonadJSM, liftJSM)
import qualified GHCJS.DOM.Window as Window
import qualified Obelisk.ExecutableConfig.Lookup
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static (static)
import Obelisk.Route (R)
import Prelude hiding (log)
import Reflex.Dom.Core
import Reflex.Dom.Form.Widgets (formItem, formItem')
import qualified Reflex.Dom.SemanticUI as SemUi
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (AppWebSocket (..), functorToWire, runRhyoliteWidget)
import Rhyolite.Schema (Json (..))
import Safe (headMay)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.Common.Chain
import Tezos.Types



import Common (humanBytes, unixEpoch, uriHostPortPath)
import Common.Alerts (AlertsFilter (..), BakerErrorDescriptions (..), badNodeHeadMessage,
                      bakerAccusedDescriptions, bakerDeactivatedDescriptions,
                      bakerDeactivationRiskDescriptions, bakerGroupedMissedBonusDescriptions,
                      bakerGroupedMissedDescriptions, bakerInsufficientFundsDescriptions,
                      bakerLedgerDisconnectedDescriptions, bakerMissedDescriptions,
                      bakerMissedEndorsementBonusDescriptions, bakerVotingReminderDescriptions,
                      bakerNeedToResetHWMDescriptions, standardTimeFormat, bakerNotEnoughStakedBalanceDescriptions,
                      bakerNeedToFinalizeUnstakeDescriptions)
import Common.Api
import Common.App
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import Common.Config
  (FrontendConfig (..), HasFrontendConfig (frontendConfig), UsingNodeOption(..), frontendConfig_appVersion,
  frontendConfig_chain, frontendConfig_chainId, frontendConfig_usingNodeOption, frontendConfig_logExportAvailable,
  _UsingCustomNode)
import qualified Common.Config as Config
import Common.HeadTag (headTag)
import Common.Route
import Common.Schema
import Common.Snapshot
import Common.Tez
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
  , GEq r, Has' Eq r Identity
  )

frontend :: Frontend (R AppRoute)
frontend = Frontend
  { _frontend_head = headTag
  , _frontend_body = prerender_ blank frontendBody
  }

frontendBody
  :: forall m js t.
    ( MonadWidget t m
    , PrimMonad m
    , RouteConstraints t AppRoute m
    , Prerender js t m
    )
  => m ()
frontendBody = void $ do
  wsUri :: Dynamic t Text <- prerender (pure "") $
    fmap (maybe (error "Invalid WS URL") Uri.render) $ getBackendPath (BackendRoute_Listen :/ ()) True
  dyn_ $ ffor wsUri $ \ws -> do
    rec
      (socketState, _) <- runRhyoliteWidget functorToWire ws $ do
        withFrontendContext $
          withConnectivityModal socketState $
            runModalT (ModalBackdropConfig $ "class"=:"modal-backdrop")
              appMain
    pure ()

getBackendPath :: MonadJSM m => R BackendRoute -> Bool -> m (Maybe URI)
getBackendPath (backendRoute :/ a) isWebsocket = do
  configs <- liftIO Obelisk.ExecutableConfig.Lookup.getConfigs
  let getExecutableConfig f = Map.lookup f configs
  route :: URI <- case getExecutableConfig (T.pack Config.route) of
    Just r -> return $ fromMaybe (error $ "Unable to parse injected route: " <> show r) $ Uri.mkURI $ T.strip $ T.decodeUtf8 r
    Nothing ->
      Config.parseRootURIUnsafe <$> (Location.getHref =<< Window.getLocation =<< DOM.currentWindowUnchecked)
  let
    url = do
      encoder <- either (const Nothing) Just $ checkEncoder fullRouteEncoder
      let path = fst $ encode encoder (FullRoute_Backend backendRoute :/ a)
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
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m)
  => Dynamic t (AppWebSocket t app) -> m () -> m ()
withConnectivityModal socketState f = do
  connectionChanged <- updatedWithInit =<< holdUniqDyn (_appWebSocket_connected =<< socketState)
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

withFrontendContext :: (MonadAppWidget js t m) => ReaderT (FrontendContext t) m () -> m ()
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

appMain
  :: forall r m t js.
    ( MonadAppWidget js t m
    , MonadAppWidget js t (ModalM m), HasModal t m
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
    sideBarOpened <- holdDyn SemUi.Out $ fmapMaybe id openness
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
          dyn_ $ ffor sideBarOpened $ \o -> when (o == SemUi.In) liveErrorsWidget
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
  :: ( MonadAppWidget js t m
     , MonadAppWidget js t (ModalM m)
     , MonadJSM (ModalM m)
     , MonadJSM (Performable (ModalM m))
     , HasJSContext (Performable (ModalM m))
     , HasFrontendConfig r, MonadReader r m, HasModal t m
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
          displayLatestRelease
  where
    ppMajorMinor (Just (MajorMinorVersion major minor mextra ai)) = case ai of
        Release -> Just $ T.pack $
            show major
            <> "."
            <> show minor
            <> maybe mempty (\extra -> "." <> show extra) mextra
        _ -> Nothing
    ppMajorMinor Nothing = Nothing

    displayLatestRelease = do

        let gitLink = "https://gitlab.com/tezos/tezos/-/releases"

        latestTezosRelease' <- watchLatestTezosRelease
        let latestTezosRelease = ppMajorMinor <$> latestTezosRelease'

        dyn_ $ ffor latestTezosRelease $ elAttr "div" ("style" =: "margin-bottom: 1rem;") . maybe (text "Latest Tezos Release: Unavailable.")
            (\v -> hrefLink (gitLink <> "/v" <> v) $
                   elAttr "small" ("style" =: "position: absolute; left:30px;") $
                   text $ "Latest Tezos Release: " <> v)

routeSelector' :: (Prerender js t m, DomBuilder t m, SemUi.HasElConfig t e, RouteConstraints t r m)
               => R r -> (e -> ch -> m a) -> e -> ch -> m a
routeSelector' dest con cfg child = do
  r <- askRoute
  let activated = ffor r $ bool "" "active" . (== dest)
  routeLink dest $ con (cfg & SemUi.classes <>~ SemUi.Dyn activated) child

routeSelector :: (Prerender js t m, DomBuilder t m, SemUi.HasElConfig t e, RouteConstraints t r m)
              => R r -> (e -> ch -> m (a,b)) -> e -> ch -> m b
routeSelector dest con cfg child = snd <$> routeSelector' dest con cfg child

appSideHeader :: (MonadAppWidget js t m, RouteConstraints t AppRoute m) => m ()
appSideHeader =
  SemUi.segment
    (def
      & SemUi.classes SemUi.|~ "app-side-header"
      & SemUi.segmentConfig_basic SemUi.|~ True
      )
    $ do
        SemUi.header def $ do
          kilnLogo
          blackhrefLink "https://tezos-kiln.org/" $ text appName
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
  :: ( MonadAppWidget js t m
     , MonadAppWidget js t (ModalM m)
     , MonadJSM (ModalM m)
     , MonadJSM (Performable (ModalM m))
     , HasJSContext (Performable (ModalM m))
     , HasModal t m
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

appSideFooter
  :: (MonadAppWidget js t m
     , RouteConstraints t AppRoute m
     , MonadReader r m
     , HasFrontendConfig r
     )
     => m ()
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

appHeader
  :: forall r m t js.
    ( MonadAppWidget js t m, MonadJSM (Performable m)
    , MonadReader r m, HasTimer t r, HasFrontendConfig r, HasTimeZone r
    )
  => m (Event t ())
appHeader = SemUi.segment (def & SemUi.segmentConfig_vertical SemUi.|~ True) $ do
  collectedNodesStatus <- watchCollectiveNodesStatus everythingWindow
  let disconnected = ffor collectedNodesStatus $ \case
        Left CollectiveNodesFailure_NoNodes -> True
        Left (CollectiveNodesFailure_AllNodesDownSince _) -> True
        Right () -> False

  divClass "topbar" $ do
    divClass "ui horizontal list" $ do
      (latestHead, knownProto) <- watchHeadWithProtocol

      let infoItem faded title body = divClass "item" $
            elDynAttr "div" (bool Map.empty ("class" =: "faded") <$> faded) $ divClass "content" $ do
              divClass "header" $ text title
              divClass "description" body

      divClass "item" $ divClass "withRightIcon" $ do
        divClass "content" $ do
          divClass "header" $ text "Network"
          divClass "description" $ do
            customProtocol <- asks (^? frontendConfig
                       . frontendConfig_usingNodeOption
                       . _Just
                       . _UsingCustomNode
                       . key "network" . key "genesis" . key "protocol"
                       . _String)
            tooltipped TooltipPos_BottomLeft (protocolTooltip customProtocol latestHead) $
              text . showChain =<< asks (^. frontendConfig . frontendConfig_chain)

{-
        divClass "iconDiv" $
          dyn_ $ ffor disconnected $ flip when $ tooltipped TooltipPos_BottomCenter disconnectedTooltip $
            SemUi.icon "icon-disconnected"
            (def
              & SemUi.iconConfig_color SemUi.|?~ SemUi.Red
              & SemUi.iconConfig_size SemUi.|?~ SemUi.Big
              )
-}

      cyc <- holdUniqDyn $ (fmap . fmap) (view branchInfo_cycle) latestHead
      whenJustDyn cyc $ \c -> infoItem disconnected "Cycle" $ text $ tshow $ unCycle c

      whenJustDyn latestHead $ \b -> infoItem disconnected "Block" $ el "span" $ do
        text $ tshow (unRawLevel $ b ^. level)
        elClass "span" "metadescription" $ text " Baked "
        localHumanizedTimestampBasic $ pure $ b ^. timestamp

      amendments <- watchAmendment
      mKnownProto <- maybeDyn knownProto
      mAmendment <- maybeDyn $ fmap snd . Map.lookupMax <$> amendments
      whenJustDyn (liftA2 . (,,) <$> disconnected <*> ((fmap . fmap) _protocolIndex_constants <$> mKnownProto) <*> mAmendment) $ \(dc, protoInfo, amendment) -> unless dc $ do
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

      internalBakerMayDyn <- watchInternalBaker

      mbConnectedLedgerDyn <- watchConnectedLedger
      dHasLedgerConnectedChecks <- fmap (maybe False _frontendConfig_ledgerConnectedChecks) <$> watchFrontendConfig
      let mbLedgerPollingStateDyn = _connectedLedger_ledgerPollingState <$$> mbConnectedLedgerDyn
          showLedgerIndicatorDyn  = ffor2 dHasLedgerConnectedChecks mbLedgerPollingStateDyn $
            \hasLedgerConnectedChecks mbLedgerPollingState ->
              -- If ledger polling is disabled, we show ledger indicator only while Kiln Baker
              -- is baking to be sure that it displays the correct info (see #186)
              --
              -- Having 'ledgerPollingState == Just LedgerPollingState_Disabled' indicates that
              -- Kiln Baker is baking now.
              -- For more context, see 'Backend.Workers.LedgerPolling.ledgerPollingStateWorker'.
              hasLedgerConnectedChecks || mbLedgerPollingState == Just LedgerPollingState_Disabled

      dyn_ $ ffor2 internalBakerMayDyn showLedgerIndicatorDyn $ \internalBakerMay showLedgerIndicator -> do
        whenJust internalBakerMay $ \(intBakerPkh, _) -> when showLedgerIndicator $ do
          elAttr "div" ("class" =: "item" <> "style" =: "position: relative") $ divClass "content" $ do
            bakerAlertsDyn <- watchBakerAlerts
            let
              -- We treat ledger as connected if there are no unresolved ledger disconnection alerts
              disconnectionAlertsDyn = ffor bakerAlertsDyn $ \bakerAlertsMap ->
                case MMap.lookup intBakerPkh bakerAlertsMap of
                  Nothing -> []
                  Just alertsList -> flip filter (toList alertsList) $ \case
                    BakerAlert_Alert (BakerLogTag_BakerLedgerDisconnected :=> _) -> True
                    _ -> False
            let dIsLedgerConnected = null <$> disconnectionAlertsDyn
            divClass "header" $ do
              iconDyn $ ffor dIsLedgerConnected $ bool "red x" "green check"
              elAttr "img" ("src" =: $(static "images/ledger.svg") <> "class" =: "ledger") blank
            divClass "description" $ do
              text "Ledger Device "
              dynText $ ffor dIsLedgerConnected $ bool "Disconnected" "Connected"

  headerBell

  where
    _disconnectedTooltip = divClass "disconnected-tooltip" $ do
      el "p" $ divClass "tooltip-title" $ text "Disconnected from the blockchain."
      divClass "tooltip-description" $ do
        el "p" $ text "Kiln cannot gather data if no monitored nodes are synced with the blockchain. Data shown is stale."
        el "p" ensureHealthyNodes

    protocolTooltip mCustomProtocol dmLatestHead = divClass "protocol-tooltip" $ do
      divClass "tooltip-title" $ text "Current Protocol"
      let dProtoText = dmLatestHead <&> \mLatestHead ->
            case (mLatestHead, mCustomProtocol) of
              (_, Just customProtocol) -> customProtocol
              (Just latestHead', _) -> latestHead' ^. protocolHash. to protocolHashToBase58Text
              _ -> "Unknown"
      divClass "tooltip-description" $ el "p" $ do
        whenJustDyn dmLatestHead $ \_ -> copyButton (current dProtoText)
        dynText dProtoText

headerBell :: forall t m js . MonadAppWidget js t m => m (Event t ())
headerBell = do
  alertCount <- watchAlertCount
  let
    alertData :: Dynamic t (Int, Maybe AlertSeverity)
    alertData = maybe (0, Nothing) (DMap.foldlWithKey (\(v1, v2) l (Const c) -> (,) (c + v1)
      (if c > 0 then max v2 (Just $ _alertMetaData_severity $ getAlertMetaData l) else v2)) (0, Nothing)) <$> alertCount
  totalAlertCount <- holdUniqDyn (fst <$> alertData)
  maxSeverity <- holdUniqDyn (snd <$> alertData)
  let
    hasAlerts = fmap (> 0) totalAlertCount
    color = maybe "basic" severityColor <$> maxSeverity
  (e,_) <- SemUi.ui' "span"
    (def & SemUi.classes .~ SemUi.Dyn (fmap ((<>) "ui circular label link ") color))
    $ do
        dynText $ ffor totalAlertCount $ (fromMaybe <*> T.stripPrefix "0") . tshow
        text " "
        SemUi.icon "icon-bell"
          (def
            & SemUi.iconConfig_size SemUi.|?~ SemUi.Large
            & SemUi.iconConfig_color .~ SemUi.Dyn (ffor hasAlerts $ bool (Just SemUi.Grey) Nothing)
            & SemUi.iconConfig_link SemUi.|~ True
            & SemUi.iconConfig_fitted .~ SemUi.Dyn hasAlerts
            )
  return $ domEvent Click e

appContentArea
  :: forall r m t js.
    ( MonadAppWidget js t m
    , MonadJSM (Performable m)
    , MonadJSM m
    , MonadReader r m, HasFrontendConfig r, HasTimer t r, HasTimeZone r
    , Routed t (R AppRoute) m
    , HasModal t m
    , MonadAppWidget js t (ModalM m)
    , MonadJSM (ModalM m)
    , MonadJSM (Performable (ModalM m))
    , HasJSContext (Performable (ModalM m))
    , MonadReader r (ModalM m)
    )
  => m ()
appContentArea = do
  r <- askRoute
  flip runRoutedT r $ subRoute_ $ \case
    AppRoute_Index -> nodesTabOrWelcome
    AppRoute_Nodes -> nodesTabOrWelcome
    AppRoute_Options -> divClass "app-content" settingsTab

nodesTabOrWelcome
  :: forall r m t js.
    ( MonadAppWidget js t m
    , MonadReader r m, HasFrontendConfig r, HasTimeZone r, HasTimer t r
    , MonadJSM m
    , HasModal t m, MonadAppWidget js t (ModalM m)
    , MonadJSM (ModalM m)
    , MonadJSM (Performable (ModalM m))
    , HasJSContext (Performable (ModalM m))
    , MonadReader r (ModalM m)
    )
  => m ()
nodesTabOrWelcome = do
  -- _clientAddresses <- watchClientAddresses
  bakersMaybe <- watchBakerAddressesValid
  nodesMaybe <- watchNodeAddressesValid
  mUsingNodeOption <- (fmap . fmap) _frontendConfig_usingNodeOption <$> watchFrontendConfig

  -- doing some straightforward calculations, but inside a Dynamic and a Maybe
  let haveBakersMaybe = (fmap . fmap) (not . null) bakersMaybe
      haveNodesMaybe = (fmap . fmap) (not . null) nodesMaybe

  haveBakersHaveNodesMaybe <- holdUniqDyn $
    (liftA3 . liftA3) (,,) haveBakersMaybe haveNodesMaybe mUsingNodeOption

  globalAlerts

  dyn_ $ ffor haveBakersHaveNodesMaybe $ \case
    Nothing -> divClass "app-content app-welcome" waitingForResponse
    Just (False, False, Nothing) -> divClass "app-content app-welcome" welcomeScreen
    Just (haveBakers, haveNodes, usingNodeOption) -> divClass "app-content" $ do
      let isCustomNode = \case
            Just (UsingCustomNode _) -> True
            _ -> False
      when (isCustomNode usingNodeOption && not haveBakers) welcomeScreen
      when haveBakers bakersTab
      when haveNodes $ do
        nodesTab usingNodeOption

everythingWindow :: Applicative f => f (Set (ClosedInterval (WithInfinity a)))
everythingWindow = pure $ Set.singleton $ ClosedInterval LowerInfinity UpperInfinity

globalAlerts
  :: forall r m t js.
    ( MonadAppWidget js t m
    , MonadReader r m, HasFrontendConfig r
    , HasModal t m, MonadAppWidget js t (ModalM m)
    , MonadJSM (ModalM m)
    , MonadJSM (Performable (ModalM m))
    , HasJSContext (Performable (ModalM m))
    )
  => m ()
globalAlerts = do
  nodeAlerts <- do
    relevantAlerts <- watchErrorsByTag
      (pure $ Just AlertsFilter_UnresolvedOnly)
      (pure $ DMap.fromList [LogTag_InternalNodeFailed :=> Const ()])
      everythingWindow

    internalNodeFailedLog :: Dynamic t (Maybe ErrorLogInternalNodeFailed) <-
      holdUniqDyn $ ffor relevantAlerts $ \xs -> listToMaybe $ toList $ fforMaybe xs $ \case
        (ErrorLog { _errorLog_stopped = Nothing }, LogTag_InternalNodeFailed :=> Identity e) -> Just e
        _ -> Nothing

    pure [fmap internalNodeFailedAlertBanner <$> internalNodeFailedLog]

  currentVersion <- asks (^. frontendConfig . frontendConfig_appVersion)
  upstreamVersion <- watchUpstreamVersion
  let
    updateAlert :: Dynamic t (Maybe (m ()))
    updateAlert = ffor upstreamVersion $ \case
      Just uv
        | Just v <- _upstreamVersion_version uv
        , v > currentVersion
        , not (_upstreamVersion_dismissed uv) -> Just $ kilnUpdateAlert v
      _ -> Nothing

    allAlerts :: Dynamic t [m ()]
    allAlerts = catMaybes <$> sequence (updateAlert : nodeAlerts)
  dyn_ $ ffor allAlerts $ traverse_ $ divClass "dashboard-section dashboard-section-global-alerts" . \m -> do
    SemUi.segment (def & SemUi.classes SemUi.|~ "dashboard-section-overview") m

internalNodeFailedAlertBanner
  :: ( MonadAppWidget js t m
     , HasModal t m, MonadAppWidget js t (ModalM m)
     , MonadJSM (ModalM m)
     , MonadJSM (Performable (ModalM m))
     , HasJSContext (Performable (ModalM m))
     )
  => ErrorLogInternalNodeFailed -> m ()
internalNodeFailedAlertBanner e = case _errorLogInternalNodeFailed_reason e of
  InternalNodeFailureReason_CarthageUpgrade -> renderSplashAlert
    (icon "icon-warning big red")
    (text "Kiln Node is Outdated and Must be Recreated")
    Nothing
    (do
      el "p" $ text "With the last update, the Tezos Node uses a new file format for storage which requires less disk space."
      el "p" $ text
        "Your Kiln Node must be removed and recreated to continue running and baking. We recommend recreating \
        \your node with a snapshot to most quickly sync with the network."

      resolve <- divClass "buttons" $ uiButtonM "primary" $ text "Remove and Recreate Node"
      deleted <- requestingIdentity $ resolve $> public (PublicRequest_RemoveNode $ Right ())
      tellModal $ deleted $> cancelableModalWithClasses addNodeModal
    )

  InternalNodeFailureReason_Unknown _ -> pure () -- TODO: Might be useful...

kilnUpdateAlert :: (MonadAppWidget js t m) => Version -> m ()
kilnUpdateAlert v = do
  let
    header = "Kiln " <> T.pack (showVersion v) <> " is available!"
    body = el "div" $ do
      el "p" $ do
        text "This may be a crucial update that provides functionality to support upcoming Tezos protocol changes. Please check the release notes for details on the importance of this update: "
        let url = "https://gitlab.com/tezos-kiln/kiln/-/releases"
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

welcomeScreen :: forall t m js. MonadAppWidget js t m => m ()
welcomeScreen = mdo
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
        el "p" $ text "Click \"Add Nodes\" to start or monitor a node. Adding external nodes is recommended to provide network context."
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

data AlertSeverity
  = AlertSeverity_Info
  | AlertSeverity_Warning
  | AlertSeverity_Error
  deriving (Eq, Ord, Show, Enum, Bounded)

severityColor :: (IsString t) => AlertSeverity -> t
severityColor = \case
  AlertSeverity_Info -> "blue"
  AlertSeverity_Warning -> "orange"
  AlertSeverity_Error -> "red"

-- | Meta info for alerts to customize their appearance/behaviour
data AlertMetaData = AlertMetaData
  { _alertMetaData_isEventBased :: !Bool
  , _alertMetaData_isUserResolvable :: !Bool
  , _alertMetaData_severity :: !AlertSeverity
  }

class HasAlertMetaData a where
  getAlertMetaData :: a -> AlertMetaData

isUserResolvable :: (HasAlertMetaData a) => a -> Bool
isUserResolvable = _alertMetaData_isUserResolvable . getAlertMetaData

instance Default AlertMetaData where
  def = AlertMetaData
    { _alertMetaData_isEventBased = False
    , _alertMetaData_isUserResolvable = False
    , _alertMetaData_severity = AlertSeverity_Error
    }

instance HasAlertMetaData ErrorLogView' where
  getAlertMetaData (ErrorLogView' l _) = getAlertMetaData l

instance HasAlertMetaData (DSum LogTag a) where
  getAlertMetaData (logTag :=> _) = getAlertMetaData logTag

instance HasAlertMetaData (LogTag a) where
  getAlertMetaData = \case
    LogTag_Node nlt -> getAlertMetaData nlt
    LogTag_Baker blt -> getAlertMetaData blt
    LogTag_BakerNoHeartbeat -> def { _alertMetaData_isUserResolvable = True }
    LogTag_NetworkUpdate ->
      def { _alertMetaData_isEventBased = True
          , _alertMetaData_isUserResolvable = True
          , _alertMetaData_severity = AlertSeverity_Info
          }
    LogTag_InternalNodeFailed ->
      def { _alertMetaData_isEventBased = True
          , _alertMetaData_isUserResolvable = False
          , _alertMetaData_severity = AlertSeverity_Error
          }

instance HasAlertMetaData (NodeLogTag a) where
  getAlertMetaData = \case
    NodeLogTag_InaccessibleNode -> def
    NodeLogTag_NodeWrongChain -> def
    NodeLogTag_NodeInsufficientPeers -> def
    NodeLogTag_NodeInvalidPeerCount -> def { _alertMetaData_isUserResolvable = True }
    NodeLogTag_BadNodeHead -> def

instance HasAlertMetaData (DSum BakerLogTag a) where
  getAlertMetaData (logTag :=> _) = getAlertMetaData logTag

instance HasAlertMetaData (BakerLogTag a) where
  getAlertMetaData = \case
    BakerLogTag_BakerLedgerDisconnected ->
      def { _alertMetaData_isEventBased = False
          , _alertMetaData_isUserResolvable = False
          , _alertMetaData_severity = AlertSeverity_Error
          }
    BakerLogTag_BakerMissed ->
      def { _alertMetaData_isEventBased = False, _alertMetaData_isUserResolvable = True }
    BakerLogTag_MissedEndorsementBonus ->
      def { _alertMetaData_isEventBased = True, _alertMetaData_isUserResolvable = True }
    BakerLogTag_NeedToResetHWM -> def { _alertMetaData_severity = AlertSeverity_Warning }
    BakerLogTag_BakerDeactivated -> def
    BakerLogTag_BakerDeactivationRisk -> def { _alertMetaData_severity = AlertSeverity_Warning }
    BakerLogTag_BakerAccused ->
      def { _alertMetaData_isEventBased = True, _alertMetaData_isUserResolvable = True }
    BakerLogTag_InsufficientFunds -> def { _alertMetaData_severity = AlertSeverity_Warning }
    BakerLogTag_NotEnoughStakedBalance -> def
    BakerLogTag_NeedToFinalizeUnstake -> def
      { _alertMetaData_severity = AlertSeverity_Info
      }
    BakerLogTag_VotingReminder -> def
      { _alertMetaData_isEventBased = True
      , _alertMetaData_isUserResolvable = True
      , _alertMetaData_severity = AlertSeverity_Info
      }

instance HasAlertMetaData SynthError where
  getAlertMetaData (SynthError_BakersInformationDown _) = def

instance HasAlertMetaData CollectiveNodesFailure where
  getAlertMetaData _ = def

instance HasAlertMetaData BakerAlert where
  getAlertMetaData = \case
    BakerAlert_Alert a -> getAlertMetaData a
    BakerAlert_GroupedAlert GroupedBakerAlert{..} ->
      case _groupedBakerAlert_type of
        GroupedAlertType_MissedBake -> getAlertMetaData BakerLogTag_BakerMissed
        GroupedAlertType_MissedEndorsementBonus -> getAlertMetaData BakerLogTag_MissedEndorsementBonus

instance (HasAlertMetaData a, HasAlertMetaData b) => HasAlertMetaData (Either a b) where
  getAlertMetaData (Left v) = getAlertMetaData v
  getAlertMetaData (Right v) = getAlertMetaData v

{-# ANN liveErrorsWidget ("HLint: ignore Evaluate" :: String) #-}
liveErrorsWidget
  :: forall r m t js.
    ( MonadAppWidget js t m
    , MonadReader r m, HasFrontendConfig r, HasTimeZone r, HasTimer t r
    )
  => m ()
liveErrorsWidget = void $ do
  chainId <- asks (^. frontendConfig . frontendConfig_chainId)
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
      \bakers nowTime allNodesDownTime ->
        fromMaybe mempty $ do
          let getBaker (k, e) = case e of
                Left v -> Just (k, v)
                Right _ -> Nothing
          keys1 <- NEL.nonEmpty $ fmapMaybe getBaker $ MMap.toList $ MMap.map _bakerSummary_baker bakers
          since <- allNodesDownTime
          let k = SynthError_BakersInformationDown keys1
          pure $ Map.singleton k $ (, k) $
            ErrorLog
              { _errorLog_started = since
              , _errorLog_stopped = Nothing
              , _errorLog_lastSeen = nowTime
              , _errorLog_noticeSentAt = Nothing
              , _errorLog_chainId = chainId
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
          severity w = case _alertMetaData_severity $ getAlertMetaData w of
            AlertSeverity_Info -> "info"
            AlertSeverity_Warning -> "warning"
            AlertSeverity_Error -> "error"
      wDyn <- holdUniqDyn widgetDyn
      elDynAttr "div" (ffor2 resolvedDyn wDyn $ \resolved w ->
        "class" =: ("app-notification ui message " <> if resolved then "success" else severity w)) $ do
        dyn_ $ ffor wDyn $ either logEntry synthEntry
        let isEv = _alertMetaData_isEventBased . getAlertMetaData <$> wDyn
        el "div" $ do
          el "label" $ dynText $ ffor isEv $ bool "First Detected" "Detected"
          localTimestamp' $ _errorLog_started <$> logDyn
        dyn_ $ ffor isEv $ \b -> unless b $ el "div" $ do
          el "label" $ dynText $ ffor logDyn $ \log -> case _errorLog_stopped log of
            Nothing -> "Last Detected"
            Just _ -> "Stopped"
          localTimestamp' $ ffor logDyn $ \log -> fromMaybe (_errorLog_lastSeen log) (_errorLog_stopped log)
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
              header "Node has too few peers."
              nodeLabel n
              el "div" $ text $
                "This node has fewer peers than the configured minimum of " <> tshow minPeerCount <> "."

            NodeLogTag_NodeInsufficientPeers -> do
              let ErrorLogNodeInsufficientPeers _ _ syncThreshold _ = log
              header "Node has insufficient amount of peers"
              nodeLabel n
              el "div" $ text $
                "This node has fewer peers than the configured synchronization threshold " <> tshow syncThreshold <>
                  " to determine its status"

        LogTag_Baker blt -> case blt of
          BakerLogTag_BakerLedgerDisconnected -> renderBakerError
            (bakerLedgerDisconnectedDescriptions log)
            pkh
          BakerLogTag_BakerDeactivated -> renderBakerError
            (bakerDeactivatedDescriptions log)
            pkh
          BakerLogTag_BakerDeactivationRisk -> renderBakerError
            (bakerDeactivationRiskDescriptions log)
            pkh
          BakerLogTag_BakerAccused -> renderBakerError
            (bakerAccusedDescriptions log)
            pkh
          BakerLogTag_BakerMissed | _errorLogBakerMissed_count log > 1 -> do
            let cnt = _errorLogBakerMissed_count log
                firstLevel = _errorLogBakerMissed_firstLevel log
                firstTime = _errorLogBakerMissed_firstBakeTime log
                lastLevel = _errorLogBakerMissed_lastLevel log
                lastTime = _errorLogBakerMissed_lastBakeTime log
                right = _errorLogBakerMissed_right log
            tz <- asks (^. timeZone)
            flip renderBakerError pkh $ bakerGroupedMissedDescriptions
              tz cnt (firstLevel, firstTime) (lastLevel, lastTime) right
          BakerLogTag_BakerMissed -> renderBakerError
            (bakerMissedDescriptions log)
            pkh
          BakerLogTag_MissedEndorsementBonus -> renderBakerError
            (bakerMissedEndorsementBonusDescriptions log)
            pkh
          BakerLogTag_NeedToResetHWM -> renderBakerError
            (bakerNeedToResetHWMDescriptions log)
            pkh
          BakerLogTag_InsufficientFunds -> do
              dMinimalStake <- _protoInfo_minimalStake <$$$> watchLatestProtoInfo
              dyn_ $ ffor dMinimalStake $ \mMinimalStake -> renderBakerError (bakerInsufficientFundsDescriptions mMinimalStake log) pkh
          BakerLogTag_NotEnoughStakedBalance -> renderBakerError
              bakerNotEnoughStakedBalanceDescriptions
              pkh
          BakerLogTag_NeedToFinalizeUnstake -> renderBakerError
              (bakerNeedToFinalizeUnstakeDescriptions log)
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

          header "New Tezos version."
          el "div" $ do
            text "There is a new version of the Tezos software available on GitLab."

        LogTag_InternalNodeFailed -> case _errorLogInternalNodeFailed_reason log of
          InternalNodeFailureReason_CarthageUpgrade -> do
            header "Kiln node out-of-date for Carthage"
            el "div" $ text "The Kiln node must be rebuilt to support the new storage framework in Carthage."
          InternalNodeFailureReason_Unknown reason -> do
            header "Kiln node failed"
            unless (T.null reason) $ el "div" $ text $ "The Kiln node failed: " <> reason

    renderBakerError dsc pkh = do
      bakersDyn <- watchBakerAddresses
      header $ _bakerErrorDescriptions_title dsc <> "."
      divClass "alert-entity" $ dyn_ $ ffor bakersDyn $ maybe blank (bakerSummaryLabel pkh) . MMap.lookup pkh
      el "div" $ text $ _bakerErrorDescriptions_notification dsc

printMajorMinor :: NodeVersion -> Text
printMajorMinor n = case _nodeVersion_version n of
  MajorMinorVersion major minor mextra additionalinfo ->
    let extra = maybe mempty show mextra in
    case additionalinfo of
      Development -> T.pack (show major) <> "." <> T.pack (show minor) <> "." <> T.pack extra <> "-dev"
      ReleaseCandidate rc ->
          T.pack (show major) <> "." <> T.pack (show minor) <> "." <> T.pack extra <> "-rc" <> T.pack (show rc)
      Release -> T.pack (show major) <> "." <> T.pack (show minor) <> "." <> T.pack extra
      Unknown -> "unknown"

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

sidebarList :: forall t m k js.
  ( MonadAppWidget js t m
  , MonadAppWidget js t (ModalM m)
  , HasModal t m
  , Ord k
  )
  => Text
  -> Dynamic t (MonoidalMap k ((Text, Maybe Text), MonitoredStatus, Bool))
  -> (Event t () -> ModalM m (Dynamic t [Text], Event t ())) -> m ()
sidebarList name items' modal = do
  let items :: Dynamic t (Map.Map k ((Text, Maybe Text), MonitoredStatus, Bool)) = coerceDynamic items'
      isBakerList = name == "Baker"
  divClass "ui sub header" $ text (pluralOf name)
  divClass "ui list" $ do
    _ <- listWithKey items $ \_ item -> divClass "item bullet-before" $ do

      let color = (\(_,s,_) -> statusColor s) <$> item
      _ <- SemUi.ui' "i" (def & SemUi.elConfigClasses .~ "icon circle tiny" <> SemUi.Dyn color) blank
      divClass "content" $ do
        let (title, subtitle) = splitDynPure $ ffor item $ \(tst, _, _) -> tst
        -- If we render the list of bakers, the either title or subtitle
        -- is baker's PKH, and we need to use monospaced font for it.
        --
        -- If the subtitle is empty, the title is PKH. Otherwise, the
        -- subtitle is PKH.
        let isTitleMonospacedDyn    = fmap ((&&) isBakerList) (isNothing <$> subtitle)
            isSubtitleMonospacedDyn = fmap ((&&) isBakerList) (isJust    <$> subtitle)
        divClass "header" $ do
          dyn_ $ ffor isTitleMonospacedDyn $ \isTitleMonospaced ->
            let cssClass = if isTitleMonospaced then " monospaced-text" else ""
            in divClass ("title" <> cssClass) $ dynText title
          useSymbol <- holdUniqDyn $ view _3 <$> item
          let tooltippedKilnLogo = tooltipped
                TooltipPos_BottomRight
                (text $ "This " <> name <> " is run by Kiln.")
                (divClass "ui image right floated" kilnLogo)
          dyn_ $ bool blank tooltippedKilnLogo <$> useSymbol

        dyn_ $ ffor isSubtitleMonospacedDyn $ \isSubtitleMonospaced ->
          let cssClass = if isSubtitleMonospaced then " monospaced-text" else ""
          in divClass ("description" <> cssClass) $ dynText $ fromMaybe "" <$> subtitle

    openAddItemOptions <- buttonIconWithInfoCls "icon-plus" "modalopener fluid" ("Add " <> pluralOf name)
    tellModal $ (openAddItemOptions $>) $ cancelableModalWithClasses modal

bakerStatus :: Either CollectiveNodesFailure (BakerSummary, Maybe BakerDetails) -> MonitoredStatus
bakerStatus = \case
  Left e -> case e of
    CollectiveNodesFailure_NoNodes -> MonitoredStatus_Unhealthy
    CollectiveNodesFailure_AllNodesDownSince _ -> MonitoredStatus_Unhealthy
  Right (bakerSummary, mbBakerDetails)
    | Right bid <- _bakerSummary_baker bakerSummary, not (isBakerRunning bid) -> MonitoredStatus_Stopped
    | _bakerSummary_alertCount bakerSummary > 0 || fmap _bakerDetails_missedRightsInRow mbBakerDetails >= Just 5 -> MonitoredStatus_Unhealthy
    | _bakerSummary_nextRight bakerSummary == BakerNextRight_GatheringData -> MonitoredStatus_Unknown
    | otherwise -> MonitoredStatus_Healthy

bakersList ::
  ( MonadAppWidget js t m
  , MonadAppWidget js t (ModalM m)
  , MonadJSM (ModalM m)
  , MonadJSM (Performable (ModalM m))
  , HasJSContext (Performable (ModalM m))
  , HasModal t m
  )
  => m ()
bakersList = do
  dCollectedNodesStatus <- watchCollectiveNodesStatus everythingWindow
  bakerAddrsDyn <- watchBakerAddresses
  bakerDetailsDyn <- watchBakerDetailsFull

  let
    bakerAddrsDetails = ffor2 bakerAddrsDyn bakerDetailsDyn $ \(MMap.MonoidalMap bakerAddrs) (MMap.MonoidalMap bakerDetails) ->
      flip Map.mapWithKey bakerAddrs $ \pkh bs -> (bs, Map.lookup pkh bakerDetails)

  let bakers = ffor2 dCollectedNodesStatus bakerAddrsDetails
        $ \collectiveNodeStatus -> imap $ \pkh (bs, bd) ->
          ( bakerSummaryIdentification (pkh, bs)
          , bakerStatus $ (bs, bd) <$ collectiveNodeStatus
          , isRight $ _bakerSummary_baker bs -- Is this an internal baker?
          )
  sidebarList "Baker" (MMap.MonoidalMap <$> bakers) addBakerModal

addBakerModal :: forall t m js.
  ( MonadAppWidget js t m
  , MonadJSM m
  , MonadJSM (Performable m)
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
          <- watchErrorsByNode everythingWindow
        node <- watchInternalNode
        nodeDetails <- fmap join $ holdDyn (constDyn Nothing) <=< dyn $ ffor node $ \case
          Nothing -> pure $ constDyn Nothing
          Just (nid, pd) -> (fmap . fmap . fmap) (nid, pd,) $ watchNodeDetails nid
        let isBootstrapLevel = (< 2)
            nodeNotReady = handleClientErrorWorkflow splash ClientError_NodeNotReady
            f Nothing _ _ = launchNode -- With no internal node, we prompt the user to launch a kiln node
            f _ Nothing _ = nodeNotReady
            f _ (Just (nid, pd, nd)) es
              -- If we have errors associated with the internal node, or the process isn't running, we redirect to node-not-ready modal
              | MMap.member nid es = nodeNotReady
              | ProcessControl_Stop == _processData_control pd = nodeNotReady
              | isProcessStateNode (_processData_state pd) = nodeNotReady
              | maybe True isBootstrapLevel (_nodeDetailsData_headLevel nd) = nodeNotReady
              | otherwise = Workflow $ do
                result <- ledgerSetupSteps
                let (err, done) = fanEither result
                pure ((["ledger-setup-steps"], close <> done), handleClientErrorWorkflow splash <$> err)
            afterDisclaimer = tag $ current (liftA3 f node nodeDetails ebn)
        pure (([], close'), disclaimer afterDisclaimer <$ start)

    startBaking = divClass "start-baking column" $ do
      elClass "h5" "ui header" $ text "Start Baking"
      divClass "explanation" $ do
        text "Bake and attest on the Tezos blockchain using a baker that is managed from within Kiln. Requires using a "
        hrefLink "https://www.ledger.com/products/ledger-nano-s" $ text "Ledger Device" -- TODO is the link correct?
        text ". Kiln only supports running a single baker."
      baker <- maybeDyn =<< watchInternalBaker
      switchHold never <=< dyn $ ffor baker $ \case
        Nothing -> uiButton "primary fluid" "Start Baking"
        Just bid -> do
          kilnLogo
          let spacing = " "
          dynText $ ffor (isBakerRunning . snd <$> bid) $ (spacing <>) . \case
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
      pure ((["launch-node"], never), startNodeWorkflow launchNode close <$ start)

    disclaimer next = Workflow $ do
      elClass "h5" "ui header" $ text "Kiln Baking Disclaimer"
      divClass "explanation" $ do
        el "p" $ text "The development team has taken great care to create a baking product which is robust and effective. However, we cannot make any guarantees in regards to baking success."

        el "p" $ text "To the maximum extent permitted by applicable law, we are not liable to any extent for any loss, damage, liability, expense or claim you suffer as a result of, but not limited to:"
        el "ul" $ traverse_ (el "li" . text)
          [ "missed rewards due to missed baking or attestation opportunities"
          , "missed rewards due to failure to reveal a nonce"
          , "loss of funds due to double baking or double attesting"
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

setDelegateParamsModal :: forall r t m js.
    ( MonadAppWidget js t m
    , MonadReader r m
    , HasFrontendConfig r
    , MonadJSM (Performable m)
    , HasTimer t r, HasTimeZone r
    )
  => SecretKey
  -> Dynamic t (Maybe BakerDetails)
  -> Event t ()
  -> m (Event t ())
setDelegateParamsModal sk detailsDyn close = ffor (workflow setDelegateParams) $ \e -> close <> switch (current e)
  where
    walletAppExtraText = el "p" $ text
      "If you are using Tezos Wallet app of version 3.0.0 or higher, you need to enable \"expert mode\" and \"blind signing\" features in the Tezos Wallet app settings on the Ledger device."
    waitForWalletApp = waitForWalletAppFlow "Setting delegate parameters" walletAppExtraText sk

    waitForBakingApp = waitForBakingAppFlow sk "Delegate parameters have been set."

    setDelegateParams :: Workflow t m (Event t ())
    setDelegateParams = Workflow $ divClass "vote-buttons" $ do
      let dmBakingEdge = _bakerDetails_bakingEdge <$$> detailsDyn
          dmStakingLimit = _bakerDetails_stakingLimit <$$> detailsDyn
      divClass "header" $ text "Set delegate parameters"
      elAttr "div" ("class" =: "detail" <> "style" =: "margin-bottom: 20px") $ do
        text "Delegates can configure their staking policy by setting the following parameters. The new parameter values will be applied after 5 cycles. Please read more details in the "
        hrefLink "https://tezos.gitlab.io/paris/adaptive_issuance.html#staking-policy-configuration" $ text "documentation"
      elAttr "div" ("class" =: "detail" <> "style" =: "margin-bottom: 20px") $ do
        -- In Kiln UI we display and set the edge of baking over staking
        -- in percents and limit of staking over baking as integer for
        -- convenience, despite the fact that 'octez-client' allows more
        -- accurate fractional values.
        --
        -- If one needs to set these values with bigger precision,
        -- they can still use 'octez-client' command for it.
        el "p" $ text "The current active delegate parameters:"
        el "p" $ do
          text "Edge of Baking over Staking: "
          withPlaceholder . ffor dmBakingEdge . fmap $ \e -> do
            let edgePercents = e `div` 10000000
            elClass "span" "monospaced-text" $ text $ tshow edgePercents <> "%"
        el "p" $ do
          text "Limit of Staking over Baking: "
          withPlaceholder . ffor dmStakingLimit . fmap $ \l -> do
            let limit = l `div` 1000000
            elClass "span" "monospaced-text" $ text $ tshow limit

      dynEiBakingEdge <- elAttr "div" ("style" =: "margin-bottom: 10px") $ formItem setBakingEdgeField
      dynEiStakingLimit <- elAttr "div" ("style" =: "margin-bottom: 10px") $ formItem stakingLimitField
      let params = liftA2 (,) dynEiBakingEdge dynEiStakingLimit

      let disabledButton = uiButton "primary disabled" "Set" $> never
      setDelegateParams' :: Event t (Workflow t m (Event t ())) <-
        switchHold never <=< dyn $ ffor params $ \case
            (Right (Just bakingEdge), Right (Just stakingLimit)) -> do
              setDelegateParamsBtn <- uiButton "primary" "Set"
              pure $ setDelegateParamsBtn $> confirmSetDelegateParamsFlow False bakingEdge stakingLimit mempty
            _ -> disabledButton

      pure (never, waitForWalletApp <$> setDelegateParams')

    confirmSetDelegateParamsFlow
      :: Bool
      -> Integer
      -> Integer
      -> Text
      -> Workflow t m (Event t ())
    confirmSetDelegateParamsFlow isTimedOut bakingEdge stakingLimit errLog = Workflow $ divClass "vote-buttons" $ do
      ledgerStatus <- ledgerDeviceIcon LedgerApp_Wallet sk
      when isTimedOut $ do
        divClass "ui message" $ do
          el "div" $ do
            icon "icon-x red"
          divClass "title" $ do
            text "The Ledger prompt was rejected or timed out. Please try again."
        unless (T.null errLog) $ do
          divClass "bigtitle" $ text "Octez-client error log:"
          elClass "div" "vote-error-log monospaced-text" $ text errLog
      divClass "bigtitle" $ text "Set delegate parameters?"
      el "p" $ do
          text "Edge of Baking over Staking: "
          elClass "span" "monospaced-text" $ text $ tshow bakingEdge <> "%"
      el "p" $ do
        text "Limit of Staking over Baking: "
        elClass "span" "monospaced-text" $ text $ tshow stakingLimit
      el "p" $ text "The new parameter values will be applied after 5 cycles."
      setBtn <- uiDynButton (pure "primary") $ text "Set"
      _ <- requestingIdentity $ public (PublicRequest_SetDelegateParams sk bakingEdge stakingLimit) <$ setBtn
      let appLost = ffilter ((/=) (Just True)) $ updated ledgerStatus
          retryFlow = waitForWalletApp $ confirmSetDelegateParamsFlow isTimedOut bakingEdge stakingLimit errLog
      pure (never, leftmost [respondToPromptFlow retryFlow bakingEdge stakingLimit <$ setBtn, ledgerDisconnectedFlow sk retryFlow <$ appLost])

    respondToPromptFlow
      :: Workflow t m (Event t ())
      -- ^ Workflow to return to after retry
      -> Integer
      -> Integer
      -> Workflow t m (Event t ())
    respondToPromptFlow retryFlow bakingEdge stakingLimit = Workflow $ do
      ledgerStatus <- ledgerDeviceIcon LedgerApp_Wallet sk
      pb <- getPostBuild
      promptStep <- watchPrompting sk
      let changed = leftmost [updated promptStep, tag (current promptStep) pb]
          next = fforMaybe changed $ \case
            Just ss | Just (First setDelegateParamsStep) <- _setupState_setDelegateParams ss -> case setDelegateParamsStep of
              SetDelegateParamsStep_Prompting -> Nothing
              SetDelegateParamsStep_Done -> Just $ waitForBakingApp $ setParamsSuccessFlow $ Left ()
              SetDelegateParamsStep_Declined -> Just $ confirmSetDelegateParamsFlow True bakingEdge stakingLimit mempty
              SetDelegateParamsStep_Disconnected -> Just $ ledgerDisconnectedFlow sk retryFlow
              SetDelegateParamsStep_Failed errLog -> Just $ confirmSetDelegateParamsFlow True bakingEdge stakingLimit errLog
            _ -> Nothing
      divClass "bigtitle" $ do
        elClass "span" "icon" $ elClass "span" "ui active inline loader small blue" blank
        text "Respond to the prompt on your Ledger Device..."
      let appLost = ffilter ((/=) (Just True)) $ updated ledgerStatus
      pure (never, leftmost [next, ledgerDisconnectedFlow sk retryFlow <$ appLost])

    setParamsSuccessFlow
      :: Either () (Workflow t m (Event t ())) -- ^ Upon success, either close the dialog or redirect to another workflow
      -> Workflow t m (Event t ())
    setParamsSuccessFlow whereToGo = Workflow $ divClass "vote-buttons" $ do
      divClass "bigtitle" $ text "Setting delegate parameters completed succesfully."
      divClass "bigtitle" $
        text "Kiln Baker will be automatically restarted to continue bake and attest blocks."
      closeButton <- uiDynButton (pure "primary") $ text "Close"
      pure $ fanEither $ whereToGo <$ closeButton

finalizeUnstakeModal :: forall r t m js.
    ( MonadAppWidget js t m
    , MonadReader r m
    , HasFrontendConfig r
    , MonadJSM (Performable m)
    , HasTimer t r, HasTimeZone r
    )
  => SecretKey
  -> Dynamic t (Maybe BakerDetails)
  -> Event t ()
  -> m (Event t ())
finalizeUnstakeModal sk detailsDyn close = ffor (workflow finalizeUnstake) $ \e -> close <> switch (current e)
  where
    walletAppExtraText = el "p" $ text
      "If you are using Tezos Wallet app of version 3.0.0 or higher, you need to enable \"expert mode\" and \"blind signing\" features in the Tezos Wallet app settings on the Ledger device."
    waitForWalletApp = waitForWalletAppFlow "Finalizing unstake" walletAppExtraText sk

    waitForBakingApp = waitForBakingAppFlow sk "The unstaked frozen funds have been finalized."

    finalizeUnstake :: Workflow t m (Event t ())
    finalizeUnstake = Workflow $ divClass "vote-buttons" $ do
      let dmUnstakedFinalizableBalance = fmap join $ _bakerDetails_unstakedFinalizableBalance <$$> detailsDyn
      divClass "header" $ text "Finalize unstaked funds"
      elAttr "div" ("class" =: "detail" <> "style" =: "margin-bottom: 20px") $ do
        text "The previously unstaked funds needs to be finalized to become unfrozen. Please read more details in the "
        hrefLink "https://tezos.gitlab.io/paris/adaptive_issuance.html#new-staking-mechanism" $ text "documentation"
      elAttr "div" ("class" =: "detail" <> "style" =: "margin-bottom: 20px") $ do
        text "The finalizable unstaked balance is "
        withPlaceholder . ffor dmUnstakedFinalizableBalance . fmap $ \t -> do
          let (w, p, tz) = tez' t
          text $ w <> p
          elClass "span" "monospaced-text tez" $ text tz

      let noFundsToFinalizeText = text "Kiln Baker currently doesn't have any unstaked finalizable funds. If some funds were previously unstaked, Kiln will produce a notification when they become finalizable."
      finalizeUnstake' :: Event t (Workflow t m (Event t ())) <- switchHold never <=< dyn $ ffor dmUnstakedFinalizableBalance $ \case
          Nothing -> noFundsToFinalizeText $> never
          Just 0 -> noFundsToFinalizeText $> never
          Just _ -> do
            flnalizeUnstakeBtn <- uiButton "primary" "Finalize Unstake"
            pure $ flnalizeUnstakeBtn $> confirmFinalizeUnstakeFlow False mempty

      pure (never, waitForWalletApp <$> finalizeUnstake')

    confirmFinalizeUnstakeFlow
      :: Bool
      -> Text
      -> Workflow t m (Event t ())
    confirmFinalizeUnstakeFlow isTimedOut errLog = Workflow $ divClass "vote-buttons" $ do
      ledgerStatus <- ledgerDeviceIcon LedgerApp_Wallet sk
      when isTimedOut $ do
        divClass "ui message" $ do
          el "div" $ do
            icon "icon-x red"
          divClass "title" $ do
            text "The Ledger prompt was rejected or timed out. Please try again."
        unless (T.null errLog) $ do
          divClass "bigtitle" $ text "Octez-client error log:"
          elClass "div" "vote-error-log monospaced-text" $ text errLog
      divClass "bigtitle" $ text "Finalize unstaked funds?"
      finalizeUnstakeBtn <- uiDynButton (pure "primary") $ text "Finalize Unstake"
      _ <- requestingIdentity $ public (PublicRequest_FinalizeUnstake sk) <$ finalizeUnstakeBtn
      let appLost = ffilter ((/=) (Just True)) $ updated ledgerStatus
          retryFlow = waitForWalletApp $ confirmFinalizeUnstakeFlow isTimedOut errLog
      pure (never, leftmost [respondToPromptFlow retryFlow <$ finalizeUnstakeBtn, ledgerDisconnectedFlow sk retryFlow <$ appLost])

    respondToPromptFlow
      :: Workflow t m (Event t ())
      -- ^ Workflow to return to after retry
      -> Workflow t m (Event t ())
    respondToPromptFlow retryFlow = Workflow $ do
      ledgerStatus <- ledgerDeviceIcon LedgerApp_Wallet sk
      pb <- getPostBuild
      promptStep <- watchPrompting sk
      let changed = leftmost [updated promptStep, tag (current promptStep) pb]
          next = fforMaybe changed $ \case
            Just ss | Just (First finalizeUnstakeStep) <- _setupState_finalizeUnstake ss -> case finalizeUnstakeStep of
              FinalizeUnstakeStep_Prompting -> Nothing
              FinalizeUnstakeStep_Done -> Just $ waitForBakingApp $ finalizeUnstakeSuccessFlow $ Left ()
              FinalizeUnstakeStep_Declined -> Just $ confirmFinalizeUnstakeFlow True mempty
              FinalizeUnstakeStep_Disconnected -> Just $ ledgerDisconnectedFlow sk retryFlow
              FinalizeUnstakeStep_Failed errLog -> Just $ confirmFinalizeUnstakeFlow True errLog
            _ -> Nothing
      divClass "bigtitle" $ do
        elClass "span" "icon" $ elClass "span" "ui active inline loader small blue" blank
        text "Respond to the prompt on your Ledger Device..."
      let appLost = ffilter ((/=) (Just True)) $ updated ledgerStatus
      pure (never, leftmost [next, ledgerDisconnectedFlow sk retryFlow <$ appLost])

    finalizeUnstakeSuccessFlow
      :: Either () (Workflow t m (Event t ())) -- ^ Upon success, either close the dialog or redirect to another workflow
      -> Workflow t m (Event t ())
    finalizeUnstakeSuccessFlow whereToGo = Workflow $ divClass "vote-buttons" $ do
      divClass "bigtitle" $ text "Finalizing unstake completed succesfully."
      divClass "bigtitle" $
        text "Kiln Baker will be automatically restarted to continue bake and attest blocks."
      closeButton <- uiDynButton (pure "primary") $ text "Close"
      pure $ fanEither $ whereToGo <$ closeButton

unstakeModal :: forall r t m js.
    ( MonadAppWidget js t m
    , MonadReader r m
    , HasFrontendConfig r
    , MonadJSM (Performable m)
    , HasTimer t r, HasTimeZone r
    )
  => SecretKey
  -> Dynamic t (Maybe BakerDetails)
  -> Event t ()
  -> m (Event t ())
unstakeModal sk detailsDyn close = ffor (workflow unstake) $ \e -> close <> switch (current e)
  where
    walletAppExtraText = el "p" $ text
      "If you are using Tezos Wallet app of version 3.0.0 or higher, you need to enable \"expert mode\" in the Tezos Wallet app settings on the Ledger device."
    waitForWalletApp = waitForWalletAppFlow "Unstaking" walletAppExtraText sk

    waitForBakingApp = waitForBakingAppFlow sk "The funds have been unstaked."

    unstake :: Workflow t m (Event t ())
    unstake = Workflow $ divClass "vote-buttons" $ do
      divClass "header" $ text "Unstake tez from Kiln Baker's staked balance"
      elAttr "div" ("class" =: "detail" <> "style" =: "margin-bottom: 20px") $ do
        text "The staked funds can be unstaked and added to baker's unfrozen balance. After unstaking, the funds will remain frozen for 4 cycles, and then can be unfrozen with 'finalize unstake' operation. Please read more details in the "
        hrefLink "https://tezos.gitlab.io/paris/adaptive_issuance.html#new-staking-mechanism" $ text "documentation"
      elAttr "div" ("class" =: "detail" <> "style" =: "margin-bottom: 20px") $ do
        text "The current staked balance is "
        let dmStakedBalance = fmap join $ _bakerDetails_stakedBalance <$$> detailsDyn
        withPlaceholder . ffor dmStakedBalance . fmap $ \t -> do
          let (w, p, tz) = tez' t
          text $ w <> p
          elClass "span" "monospaced-text tez" $ text tz

      dynEiTez <- elAttr "div" ("style" =: "margin-bottom: 10px") $ formItem unstakeTezField
      let disabledButton = uiButton "primary disabled" "Unstake" $> never
      unstake' :: Event t (Workflow t m (Event t ())) <- switchHold never <=< dyn $ ffor dynEiTez $ \case
          Left _ -> disabledButton
          Right Nothing -> disabledButton
          Right (Just tz) -> do
            unstakeBtn <- (tz <$) <$> uiButton "primary" "Unstake"
            pure $ ffor unstakeBtn $ \t -> confirmUnstakeFlow False t mempty
      pure (never, waitForWalletApp <$> unstake')

    confirmUnstakeFlow
      :: Bool
      -> Integer
      -> Text
      -> Workflow t m (Event t ())
    confirmUnstakeFlow isTimedOut unstakeTez errLog = Workflow $ divClass "vote-buttons" $ do
      ledgerStatus <- ledgerDeviceIcon LedgerApp_Wallet sk
      when isTimedOut $ do
        divClass "ui message" $ do
          el "div" $ do
            icon "icon-x red"
          divClass "title" $ do
            text "The Ledger prompt was rejected or timed out. Please try again."
        unless (T.null errLog) $ do
          divClass "bigtitle" $ text "Octez-client error log:"
          elClass "div" "vote-error-log monospaced-text" $ text errLog
      divClass "bigtitle" $ text $ "Unstake " <> tshow unstakeTez  <> "ꜩ from Kiln Baker's staked balance?"
      unstakeBtn <- uiDynButton (pure "primary") $ text "Unstake"
      _ <- requestingIdentity $ public (PublicRequest_Unstake sk unstakeTez) <$ unstakeBtn
      let appLost = ffilter ((/=) (Just True)) $ updated ledgerStatus
          retryFlow = waitForWalletApp $ confirmUnstakeFlow isTimedOut unstakeTez errLog
      pure (never, leftmost [respondToPromptFlow retryFlow unstakeTez <$ unstakeBtn, ledgerDisconnectedFlow sk retryFlow <$ appLost])

    respondToPromptFlow
      :: Workflow t m (Event t ())
      -- ^ Workflow to return to after retry
      -> Integer -> Workflow t m (Event t ())
    respondToPromptFlow retryFlow unstakeTez = Workflow $ do
      ledgerStatus <- ledgerDeviceIcon LedgerApp_Wallet sk
      pb <- getPostBuild
      promptStep <- watchPrompting sk
      let changed = leftmost [updated promptStep, tag (current promptStep) pb]
          next = fforMaybe changed $ \case
            Just ss | Just (First unstakeStep) <- _setupState_unstake ss -> case unstakeStep of
              UnstakeStep_Prompting -> Nothing
              UnstakeStep_Done -> Just $ waitForBakingApp $ unstakeSuccessFlow $ Left ()
              UnstakeStep_Declined -> Just $ confirmUnstakeFlow True unstakeTez mempty
              UnstakeStep_Disconnected -> Just $ ledgerDisconnectedFlow sk retryFlow
              UnstakeStep_Failed errLog -> Just $ confirmUnstakeFlow True unstakeTez errLog
            _ -> Nothing
      divClass "bigtitle" $ do
        elClass "span" "icon" $ elClass "span" "ui active inline loader small blue" blank
        text "Respond to the prompt on your Ledger Device..."
      let appLost = ffilter ((/=) (Just True)) $ updated ledgerStatus
      pure (never, leftmost [next, ledgerDisconnectedFlow sk retryFlow <$ appLost])

    unstakeSuccessFlow
      :: Either () (Workflow t m (Event t ())) -- ^ Upon success, either close the dialog or redirect to another workflow
      -> Workflow t m (Event t ())
    unstakeSuccessFlow whereToGo = Workflow $ divClass "vote-buttons" $ do
      divClass "bigtitle" $ text "Unstaking completed succesfully."
      el "p" $ text
        "The unstaked funds will remain frozen for the time being. After 4 cycles, you can unfreeze them with 'finalize unstake' operation. Kiln will notify you when these funds can be unfrozen."
      divClass "bigtitle" $
        text "Kiln Baker will be automatically restarted to continue bake and attest blocks."
      closeButton <- uiDynButton (pure "primary") $ text "Close"
      pure $ fanEither $ whereToGo <$ closeButton

stakeModal :: forall r t m js.
    ( MonadAppWidget js t m
    , MonadReader r m
    , HasFrontendConfig r
    , MonadJSM (Performable m)
    , HasTimer t r, HasTimeZone r
    )
  => SecretKey
  -> Dynamic t (Maybe BakerDetails)
  -> Event t ()
  -> m (Event t ())
stakeModal sk detailsDyn close = ffor (workflow stake) $ \e -> close <> switch (current e)
  where
    walletAppExtraText = el "p" $ text
      "If you are using Tezos Wallet app of version 3.0.0 or higher, you need to enable \"expert mode\" in the Tezos Wallet app settings on the Ledger device."
    waitForWalletApp = waitForWalletAppFlow "Staking" walletAppExtraText sk

    waitForBakingApp = waitForBakingAppFlow sk "The funds have been staked."

    stake :: Workflow t m (Event t ())
    stake = Workflow $ divClass "vote-buttons" $ do
      divClass "header" $ text "Stake tez for Kiln Baker"
      elAttr "div" ("class" =: "detail" <> "style" =: "margin-bottom: 20px") $ do
        text "After the activation of adaptive issuance, the delegate's baking and voting power depends on its staked balance which can be increased using 'stake' operation. Please read more details in the "
        hrefLink "https://tezos.gitlab.io/paris/adaptive_issuance.html#new-staking-mechanism" $ text "documentation"
      elAttr "div" ("class" =: "detail" <> "style" =: "margin-bottom: 20px") $ do
        text "The current available balance is "
        let dmDelegateInfo = preview (_Just . bakerDetails_delegateInfo . _Just . to unJson) <$> detailsDyn
        withPlaceholder . ffor dmDelegateInfo . fmap $ \t -> do
          let (w, p, tz) = tez' $ _cacheDelegateInfo_balance t - _cacheDelegateInfo_frozenBalance t
          text $ w <> p
          elClass "span" "monospaced-text tez" $ text tz
      dynEiTez <- elAttr "div" ("style" =: "margin-bottom: 10px") $ formItem stakeTezField
      let disabledButton = uiButton "primary disabled" "Stake" $> never
      stake' :: Event t (Workflow t m (Event t ())) <- switchHold never <=< dyn $ ffor dynEiTez $ \case
          Left _ -> disabledButton
          Right Nothing -> disabledButton
          Right (Just tz) -> do
            stakeBtn <- (tz <$) <$> uiButton "primary" "Stake"
            pure $ ffor stakeBtn $ \t -> confirmStakeFlow False t mempty
      pure (never, waitForWalletApp <$> stake')

    confirmStakeFlow
      :: Bool
      -> Integer
      -> Text
      -> Workflow t m (Event t ())
    confirmStakeFlow isTimedOut stakeTez errLog = Workflow $ divClass "vote-buttons" $ do
      ledgerStatus <- ledgerDeviceIcon LedgerApp_Wallet sk
      when isTimedOut $ do
        divClass "ui message" $ do
          el "div" $ do
            icon "icon-x red"
          divClass "title" $ do
            text "The Ledger prompt was rejected or timed out. Please try again."
        unless (T.null errLog) $ do
          divClass "bigtitle" $ text "Octez-client error log:"
          elClass "div" "vote-error-log monospaced-text" $ text errLog
      divClass "bigtitle" $ text $ "Stake " <> tshow stakeTez  <> "ꜩ for Kiln Baker?"
      stakeBtn <- uiDynButton (pure "primary") $ text "Stake"
      _ <- requestingIdentity $ public (PublicRequest_Stake sk stakeTez) <$ stakeBtn
      let appLost = ffilter ((/=) (Just True)) $ updated ledgerStatus
          retryFlow = waitForWalletApp $ confirmStakeFlow isTimedOut stakeTez errLog
      pure (never, leftmost [respondToPromptFlow retryFlow stakeTez <$ stakeBtn, ledgerDisconnectedFlow sk retryFlow <$ appLost])

    respondToPromptFlow
      :: Workflow t m (Event t ())
      -- ^ Workflow to return to after retry
      -> Integer -> Workflow t m (Event t ())
    respondToPromptFlow retryFlow stakeTez = Workflow $ do
      ledgerStatus <- ledgerDeviceIcon LedgerApp_Wallet sk
      pb <- getPostBuild
      promptStep <- watchPrompting sk
      let changed = leftmost [updated promptStep, tag (current promptStep) pb]
          next = fforMaybe changed $ \case
            Just ss | Just (First stakeStep) <- _setupState_stake ss -> case stakeStep of
              StakeStep_Prompting -> Nothing
              StakeStep_Done -> Just $ waitForBakingApp $ stakeSuccessFlow $ Left ()
              StakeStep_Declined -> Just $ confirmStakeFlow True stakeTez mempty
              StakeStep_Disconnected -> Just $ ledgerDisconnectedFlow sk retryFlow
              StakeStep_NotEnoughBalance -> Just $ notEnoughBalanceFlow stakeTez $ Left ()
              StakeStep_Failed errLog -> Just $ confirmStakeFlow True stakeTez errLog
            _ -> Nothing
      divClass "bigtitle" $ do
        elClass "span" "icon" $ elClass "span" "ui active inline loader small blue" blank
        text "Respond to the prompt on your Ledger Device..."
      let appLost = ffilter ((/=) (Just True)) $ updated ledgerStatus
      pure (never, leftmost [next, ledgerDisconnectedFlow sk retryFlow <$ appLost])

    stakeSuccessFlow
      :: Either () (Workflow t m (Event t ())) -- ^ Upon success, either close the dialog or redirect to another workflow
      -> Workflow t m (Event t ())
    stakeSuccessFlow whereToGo = Workflow $ divClass "vote-buttons" $ do
      divClass "bigtitle" $ text "Staking completed succesfully."
      divClass "bigtitle" $
        text "Kiln Baker will be automatically restarted to continue bake and attest blocks."
      closeButton <- uiDynButton (pure "primary") $ text "Close"
      pure $ fanEither $ whereToGo <$ closeButton

    notEnoughBalanceFlow stakeTez whereToGo = Workflow $ divClass "vote-buttons" $ do
      divClass "bigtitle" $ text $ "Kiln Baker doesn't have enough balance to stake " <> tshow stakeTez <> "ꜩ."
      divClass "bigtitle" $
        text "Please try again with smaller amount of funds."
      closeButton <- uiDynButton (pure "primary") $ text "Close"
      pure $ fanEither $ whereToGo <$ closeButton

authorizeLedgerToBakeModal
  :: MonadAppWidget js t m
  => SecretKey -> PublicKeyHash -> Event t () -> m (Dynamic t [Text], Event t ())
authorizeLedgerToBakeModal sk pkh close = ffor (workflow auth) $ \d -> let (c, e) = splitDynPure d in (("add-baker":) <$> c, close <> switch (current e))
  where
    ledger = _secretKey_ledgerIdentifier sk
    auth = Workflow $ do
      ledgerCheckImg ledger
      elClass "h5" "ui header" $ text "Authorize this Ledger Device to bake for the following account?"
      elClass "h6" "ui header monospaced-text" $ text $ toPublicKeyHashText pkh
      authorize <- uiButton "primary" "Authorize"
      pure ((["ledger-prompt"], never), waiting <$ authorize)
    waiting = Workflow $ do
      ledgerCheckImg ledger
      respondToPrompt $ do
        el "span" $ text "Authorize Baking With Public Key? Public Key Hash "
        monospacedPkhText pkh
      pb <- getPostBuild
      _ <- requestingIdentity $ public (PublicRequest_SetupLedgerToBake sk) <$ pb
      promptStep <- watchPrompting sk
      let changed = leftmost [updated promptStep, tag (current promptStep) pb]
          next = fforMaybe changed $ \case
            Just ss | Just (First setupStep) <- _setupState_setup ss -> case setupStep of
              SetupLedgerToBakeStep_Done -> Just authorized
              SetupLedgerToBakeStep_DoneAndRegistered -> Just authorized
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
      elClass "h6" "ui header" $ do
        el "span" $ text "This Ledger Device is now authorized to bake for the address: "
        monospacedPkhText pkh
      continue <- uiButton "primary" "Continue"
      pure ((["ledger-prompt"], continue), never)

setHighWaterMark
  :: MonadAppWidget js t m
  => Dynamic t RawLevel -> SecretKey -> PublicKeyHash -> Event t () -> m (Dynamic t [Text], Event t ())
setHighWaterMark latestBlockLevelDyn secretKey pkh close = ffor (workflow set) $ \d -> let (c, e) = splitDynPure d in (("add-baker":) <$> c, close <> switch (current e))
  where
    ledger = _secretKey_ledgerIdentifier secretKey
    set = Workflow $ do
      ledgerCheckImg ledger
      elClass "h5" "ui header" $ text "Set the high-water mark for this account on the connected Ledger Device?"
      elClass "h6" "ui header monospaced-text" $ text $ toPublicKeyHashText pkh
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

setLiquidityBakingToggleModal
  :: MonadAppWidget js t m
  => (SecretKey, PublicKeyHash)
  -> Event t ()
  -> m (Dynamic t [Text], Event t ())
setLiquidityBakingToggleModal (sk, pkh) close = ffor (workflow set) $ \d -> let (c, e) = splitDynPure d in (("add-baker":) <$> c, close <> switch (current e))
  where
    set = Workflow $ do
      evt <- setLiquidityBakingToggle (sk, pkh) True
      pure ((["set-liquidity-baking-toggle"], void evt), never)

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
      elAttr "img" ("src" =: $(static "images/ledger.svg") <> "class" =: "ledger") blank
      elClass "h5" "ui header" $ do
        icon "red icon-x"
        text "The request was declined by the Ledger Device."
      divClass "explanation" $ text "If you did not intend to reject the prompt on the Ledger Device you may click retry."
      retry <- uiButton "primary" "Retry"
      pure ((["ledger-declined"], never), tryAgain <$ retry)

    ledgerDisconnected tryAgain = Workflow $ do
      elAttr "img" ("src" =: $(static "images/ledger.svg") <> "class" =: "ledger") blank
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
    elAttr "img" ("src" =: $(static "images/ledger.svg")) blank
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
  ( MonadAppWidget js t m
  , MonadAppWidget js t (ModalM m)
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
  ( MonadAppWidget js t m
  , MonadJSM m
  , MonadJSM (Performable m)
  , HasJSContext (Performable m)
  )
  => Event t () -> m (Dynamic t [Text], Event t ())
addNodeModal close = ffor (workflow splash) $ \d ->
  let (c, e) = splitDynPure d in (c, switch (current e))

  where
    splash = Workflow $ do
      divClass "ui header" $ text "Add Nodes"
      divClass "ui grid stackable divided" $ do
        startNodeEv <- addInternal
        e <- addExternal
        pure ((pure "add-node", e), startNodeWorkflow splash close <$ startNodeEv)

      where
        section header explanation = do
          elClass "h5" "ui header" $ text header
          divClass "explanation" $ text explanation

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

data NodeBootstrapMethod
  = NodeBootstrapMethod_PeerToPeer
  | NodeBootstrapMethod_SnapshotFile (Maybe File.File)
  | NodeBootstrapMethod_SnapshotFilePath (Maybe FilePath)
  | NodeBootstrapMethod_URI (Maybe URI)
  | NodeBootstrapMethod_KnownSnapshotProvider KnownSnapshotProvider

data SnapshotProviderOption
  = TzInit (Maybe TzInitRegion)
  | Custom (Maybe URI)
  deriving (Show, Eq, Ord)

startNodeWorkflow :: forall m t js.
  ( MonadAppWidget js t m
  , MonadJSM m
  , MonadJSM (Performable m)
  , HasJSContext (Performable m)
  )
  => Workflow t m ([Text], Event t ()) -> Event t () -> Workflow t m ([Text], Event t ())
startNodeWorkflow backWF close = Workflow $ do
  backEv <- backButton
  divClass "ui header" $ text "Start a Kiln Node"
  divClass "ui two column centered grid divided" $ do
  rec
    let
      radioItems =
        [ useSnapshotFileEv
        , useSnapshotPeerToPeerEv
        , useSnapshotFilePathEv
        , useSnapshotProviderEv
        ]

    useSnapshotProvider <- isRadioItemSelected radioItems useSnapshotProviderEv True
    useSnapshotFile     <- isRadioItemSelected radioItems useSnapshotFileEv False
    useSnapshotFilePath <- isRadioItemSelected radioItems useSnapshotFilePathEv False

    (useSnapshotProviderEv, selectedProviderDyn) <- divClass "column" $ do

      (useSnapshotProviderEv', selectedProviderDyn') <- fakeRadioItem useSnapshotProvider $ el "div" $ mdo
        el "div" $ text "Download latest rolling snapshot from the snapshot provider (Recommended)"

        divClass "explanation" $ text ""

        let
          providerOptions =
            [ TzInit Nothing
            , Custom Nothing
            ]

          providerText = text . \case
            TzInit _ -> "Tzinit"
            Custom _ -> "Custom snapshot provider URL"

        providerDropdown <- divClass "ui field" $ do
          el "label" $ text "Select snapshot provider"
          ddDyn <- value <$> SemUi.dropdown (def & SemUi.dropdownConfig_fluid SemUi.|~ True) (Identity (TzInit Nothing)) never (SemUi.TaggedStatic $
            Map.fromList $ ffor providerOptions $ \ p -> (p, providerText p))
          rsEvent <- dyn $ tzInitRegionSelect <$> ddDyn
          rsDyn <- switchHold never rsEvent >>= holdDyn (Just TzInitAsia)
          let
            zipFn (Identity (TzInit _)) (Just region) = Identity (TzInit (Just region))
            zipFn p _ = p
          pure $ zipDynWith zipFn ddDyn rsDyn

        let
          tzInitRegionSelect p = case p of
            (Identity (TzInit _)) -> mdo

              let
                regionRadioItems =
                  [ useAsianRegionEv
                  , useEuropeRegionEv
                  , useUSRegionEv
                  ]

              useAsianRegion <- isRadioItemSelected regionRadioItems useAsianRegionEv True
              useEuropeRegion <- isRadioItemSelected regionRadioItems useEuropeRegionEv False
              useUSRegion <- isRadioItemSelected regionRadioItems useUSRegionEv False

              el "br" blank
              el "div" $ text "Choose the snapshot server closest to your location"
              divClass "explanation" $  do
                text "Tzinit provider has several snapshot services located in different regions. Using the service from the closest region will reduce the download time"
              (useAsianRegionEv, _) <- fakeRadioItem useAsianRegion $ el "div" $ text "Asia"
              (useEuropeRegionEv, _) <- fakeRadioItem useEuropeRegion $ el "div" $ text "Europe"
              (useUSRegionEv, _) <- fakeRadioItem useUSRegion $ el "div" $ text "US"
              updated <$> do
                let
                  zipFn True (False, False) = Just TzInitAsia
                  zipFn False (True, False) = Just TzInitEurope
                  zipFn False (False, True) = Just TzInitUs
                  zipFn _ (_, _) = Nothing
                pure $ zipDynWith zipFn useAsianRegion (zipDyn useEuropeRegion useUSRegion)
            _ -> do
              pure $ updated $ constDyn (Just TzInitAsia)

        uriEv <- dyn $ ffor providerDropdown $ \(Identity v) -> case v of
          Custom _ -> do
            divClass "explanation" $  do
              text "You can enter either the URL of the provider's snapshot list file"
              text " or a link to a specific snapshot (e.g. "
              hrefLink "https://snapshots.eu.tzinit.org/mainnet/rolling" $ text "https://snapshots.eu.tzinit.org/mainnet/rolling"
              text ").  "
              text "Kiln will download the latest rolling snapshot from the provider or a specific snapshot depending on the given URL."
            elAttr "div" ("style" =: "margin-top: 10px") $
             either (const Nothing) Just <$$$> formItem' "" $ uriField "" ""
          _ -> blank $> constDyn Nothing
        uriEv' <- switchHold never $ updated <$> uriEv
        uriDyn <- holdDyn Nothing uriEv'

        let
          selectedProviderDyn' :: Dynamic t NodeBootstrapMethod
          selectedProviderDyn' = ffor2 providerDropdown uriDyn $ \(Identity dd) u -> case dd of
            TzInit mRegion -> NodeBootstrapMethod_KnownSnapshotProvider $
              KnownSnapshotProvider_TzInit $ fromMaybe def mRegion
            Custom _ -> NodeBootstrapMethod_URI u

        pure selectedProviderDyn'
      pure (useSnapshotProviderEv', selectedProviderDyn')

    ((useSnapshotFilePathEv, mSnapshotFilePath), useSnapshotPeerToPeerEv, (useSnapshotFileEv, mSelectedSnapshot))
      <- divClass "column" $ do
        (useSnapshotFilePathEv', mSnapshotFilePath') <- fakeRadioItem useSnapshotFilePath $ el "div" $ do
          el "div" $ text "Provide path to snapshot file"
          divClass "explanation" $ do
            el "p" $ text "You can provide a path to the snapshot file that is stored locally on the machine that is running Kiln."
          filePath <- formItem textField
          let
            mFilePath = ffor filePath $ \case
              Right fp -> fp
              Left _   -> Nothing
          return mFilePath
        let
          usep2p = do
            useSnapshotFile' <- useSnapshotFile
            useSnapshotFilePath' <- useSnapshotFilePath
            useSnapshotProvider' <- useSnapshotProvider
            return $ not $ useSnapshotFile' || useSnapshotFilePath' || useSnapshotProvider'

        (useSnapshotFileEv', mSelectedSnapshot') <- fakeRadioItem useSnapshotFile $ el "div" $ do
          el "div" $ text "Provide snapshot file stored locally"
          divClass "explanation" $ do
            el "p" $ text "As an alternative you can provide a snapshot file that is stored locally."
          divClass "file-selection" $ do
            rec
              let fileName = headMay <$> _inputElement_files fi
              dyn_ $ ffor fileName $ mapM $ \file -> do
                name <- liftJSM $ File.getName file
                divClass "file-name" $ text name
              elAttr "label" ("for" =: "fileId" <> "class" =: "ui button") $ text "Select Snapshot File"
              fi <- fileInput' $ constDyn ("id" =: "fileId")
            pure fileName

        (useSnapshotPeerToPeerEv', _) <- fakeRadioItem usep2p $
          divClass "" $ do
            divClass "" $ text "Peer to Peer Download"
            divClass "explanation" $ do
              el "p" $ text "Download the chain history from Genesis to the current head via peer to peer download (as nodes normally communicate on the blockchain)."
              el "p" $ text "Note that it may take a long time."
        pure ((useSnapshotFilePathEv', mSnapshotFilePath'), useSnapshotPeerToPeerEv', (useSnapshotFileEv', mSelectedSnapshot'))

  let
    selectedMethodDyn :: Dynamic t NodeBootstrapMethod
    selectedMethodDyn = do
      useSnapshotFile' <- useSnapshotFile
      useSnapshotFilePath' <- useSnapshotFilePath
      useSnapshotProvider' <- useSnapshotProvider
      if useSnapshotFile'
      then NodeBootstrapMethod_SnapshotFile <$> mSelectedSnapshot
      else if useSnapshotFilePath'
      then NodeBootstrapMethod_SnapshotFilePath . fmap T.unpack <$> mSnapshotFilePath
      else if useSnapshotProvider'
      then selectedProviderDyn
      else return NodeBootstrapMethod_PeerToPeer

    disabledFlag :: Dynamic t Text
    disabledFlag = do
      method <- selectedMethodDyn
      case method of
        NodeBootstrapMethod_PeerToPeer -> ""
        NodeBootstrapMethod_SnapshotFile (Just _) -> ""
        NodeBootstrapMethod_SnapshotFilePath (Just _) -> ""
        NodeBootstrapMethod_URI (Just _) -> ""
        NodeBootstrapMethod_KnownSnapshotProvider _ -> ""
        _ -> "disabled"

  rec
    _ <- divClass "row" $ do
      runWithReplace blank $ ffor invalidSnapshotFilePath $ \case
        AddInternalNodeError_SnapshotImportError SnapshotImportError_FileNotFound ->
          divClass "ui error message" $ text "File does not exist"
        AddInternalNodeError_SnapshotImportError SnapshotImportError_InvalidSnapshot ->
          divClass "ui error message" $ do
            el "p" $ text "The provided file is not a valid node snapshot"
            el "p" $ text "Check Kiln logs for a more detailed error message"
        AddInternalNodeError_SnapshotImportError SnapshotImportError_PermissionDenied ->
          divClass "ui error message" $ do
            el "p" $ text "Permission denied"
            el "p" $ text "If you run Kiln on macOS, try to move the snapshot file from Desktop/Downloads/Documents to another folder"
        AddInternalNodeError_SnapshotImportError SnapshotImportError_UnknownError ->
          divClass "ui error message" $ do
            el "p" $ text "Failed to validate file"
            el "p" $ text "'octez-node snapshot info' command failed. Check Kiln logs for a more detailed error message"


    addNodeEv <- divClass "row" $
      uiDynButton (T.unwords . (:["primary"]) <$> disabledFlag) (text "Add Node")

    let
      selectedMethodEv :: Event t NodeBootstrapMethod
      selectedMethodEv = tag (current selectedMethodDyn) addNodeEv

      startNodePeerToPeerEv = flip mapMaybe selectedMethodEv $ \case
        NodeBootstrapMethod_PeerToPeer -> Just ()
        _ -> Nothing
      uploadSnapshotEv = flip mapMaybe selectedMethodEv $ \case
        NodeBootstrapMethod_SnapshotFile f -> f
        _ -> Nothing
      uploadSnapshotFromPathEv = flip mapMaybe selectedMethodEv $ \case
        NodeBootstrapMethod_SnapshotFilePath f -> f
        _ -> Nothing
      downloadSnapshotEv = flip mapMaybe selectedMethodEv $ \case
        NodeBootstrapMethod_URI u -> u
        _ -> Nothing
      downloadFromProviderEv = flip mapMaybe selectedMethodEv $ \case
        NodeBootstrapMethod_KnownSnapshotProvider p -> Just p
        _ -> Nothing

    formEv <- performEvent $ ffor uploadSnapshotEv fileToFormValue

    mUri <- getBackendPath (BackendRoute_SnapshotUpload :/ ()) False
    let
      formUploadEv = (: []) . Map.singleton "snapshot-file" <$> formEv
    for_ mUri $ \uri -> postForms (Uri.render uri) formUploadEv

    uploadSnapshotResEv <- requestingIdentity $ formUploadEv $> public (PublicRequest_AddInternalNode (Just (NodeProcessState_ImportingSnapshot, SnapshotImportSource_FileSource)))
    downloadSnapshotResEv <- requestingIdentity $ ffor downloadSnapshotEv $ \u ->
      public (PublicRequest_AddInternalNode $ Just (NodeProcessState_DownloadingSnapshot, SnapshotImportSource_UriSource u))
    (invalidSnapshotFilePath, uploadSnapshotFromPathResEv) <- fmap fanEither $
      requestingIdentity $ ffor uploadSnapshotFromPathEv $ \fp ->
      public (PublicRequest_AddInternalNode (Just (NodeProcessState_ImportingSnapshot, SnapshotImportSource_FilePathSource fp)))
    startNodePeerToPeerResEv <- requestingIdentity $ startNodePeerToPeerEv $> public (PublicRequest_AddInternalNode Nothing)
    downloadFromProviderResEv <- requestingIdentity $ ffor downloadFromProviderEv $ \p ->
      public (PublicRequest_AddInternalNode $ Just (NodeProcessState_DownloadingSnapshot, SnapshotImportSource_KnownSnapshotProviderSource p))
    let
      events = map void [startNodePeerToPeerResEv, uploadSnapshotResEv, downloadSnapshotResEv, downloadFromProviderResEv]
        <> [uploadSnapshotFromPathResEv]

  pure ( (["start-node"], leftmost events <> close)
       , leftmost [backWF <$ backEv]
       )
  where
    fileInput' config = do
      let insertType = Map.insert "type" "file"
          dAttrs = insertType <$> config
      modifyAttrs <- dynamicAttributesToModifyAttributes dAttrs
      let filters = DMap.singleton Change . GhcjsEventFilter $ \_ -> do
            return . (,) mempty $ return . Just $ EventResult ()
          elCfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
            & modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
            & elementConfig_eventSpec . ghcjsEventSpec_filters .~ filters
          cfg = (def :: InputElementConfig EventResult t (DomBuilderSpace m)) & inputElementConfig_elementConfig .~ elCfg
      inputElement cfg

verifySnapshotModal ::
  ( MonadReader r m
  , HasTimer t r
  , HasTimeZone r
  , MonadJSM (Performable m)
  , MonadAppWidget js t m
  )
  => SnapshotMeta -> Event t () -> m (Event t ())
verifySnapshotModal smd = cancelableModalWithClasses $ \close -> do
  divClass "ui header" $ text "Verify Snapshot"
  divClass "verify-top-message" $ do
    icon "large orange icon-warning"
    case smd ^. snapshotMeta_headBlock of
      Just _ -> do
        divClass "header" $ text "Verifying the Block Hash"
        divClass "explanation" $ do
          el "p" $ text "It is highly recommended to verify the hash of the highest block level of the snapshot. Use a third-party source that you trust to verify the data below."
          el "p" $ text "Copy the block hash and search for it on a block explorer. Make sure the block is valid."
          el "p" $ text "If you are in doubt that the snapshot is valid, close this window, remove the node and restart using a snapshot you trust."
      Nothing -> do
        divClass "header" $ text "Check your snapshot"
        divClass "explanation" $ do
          el "p" $ text "The provided snapshot probably has the old v1 legacy format and doesn't provide information about its head block."

  whenJust (smd ^. snapshotMeta_headBlock) $ \headBlock -> do
    divClass "field" $ do
      divClass "detail" $ text "Snapshot's Highest Block Hash:"
      let hashText = blockHashToBase58Text headBlock
      divClass "proposal-hash" $ do
        copyButton $ pure hashText
        text hashText
    whenJust (smd ^. snapshotMeta_headBlockLevel) $ \headBlockLevel ->
      divClass "field" $ do
        divClass "detail" $ text "Highest Block Level:"
        divClass "" $ text $ (tshow . unRawLevel) headBlockLevel
    whenJust (smd ^. snapshotMeta_headBlockBakeTime) $ \headBlockTimestamp ->
      divClass "field" $ do
        divClass "detail" $ text "Date Baked:"
        divClass "" $ (localHumanizedTimestampBasic . constDyn) headBlockTimestamp
  start <- divClass "buttons" $ uiButton "primary" "Start Node"
  response <- requestingIdentity $ public (PublicRequest_UpdateInternalDaemon DaemonType_Node True) <$ start
  pure (pure ["confirmation"], leftmost [void response, close])

showErrorLogModal ::
  ( MonadReader r m
  , MonadAppWidget js t m
  )
  => Text -> Text -> Event t () -> m (Event t ())
showErrorLogModal title errorLog = cancelableModalWithClasses $ \close -> do
  divClass "ui header" $ text title
  divClass "log-message" $ el "pre" $ text errorLog
  close1 <- uiButton "primary" "Close"
  pure (pure ["show-error-log"], leftmost [close1, close])

thirtySixHoursToInfinity
  ::
  ( MonadReader r m, HasTimer t r
  , MonadAppWidget js t m
  )
  => m (Dynamic t (ClosedInterval (WithInfinity Time.UTCTime)))
thirtySixHoursToInfinity = do
  let thirtySixHoursAgo = (-1.5) * Time.nominalDay
  let oneHour = 60 * 60
  let quantize = flip Time.addUTCTime unixEpoch . (* oneHour) . fromIntegral @Integer . floor . (/ oneHour) . flip Time.diffUTCTime unixEpoch
  time <- holdUniqDyn . fmap quantize =<< asks (view timer)

  return $ fmap (flip ClosedInterval UpperInfinity . Bounded . Time.addUTCTime thirtySixHoursAgo) time

tileMenuEntry :: (DomBuilder t m, MonadFix m
                 , PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadHold t m, Prerender js t m)
              => Text -> m (Event t ())
tileMenuEntry = fmap (domEvent Click . fst) . SemUi.listItem' def . text

tileMenuEntryModal :: (DomBuilder t m, MonadFix m
                      , PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadHold t m
                      , HasModal t m, Prerender js t m)
                   => Text -> (Event t () -> ModalM m (Event t ())) -> m ()
tileMenuEntryModal txt modal = do
  open <- tileMenuEntry txt
  tellModal $ open $> modal

ppTezosVersion :: TezosVersion -> Text
ppTezosVersion = either id showV . getTezosVersion
  where
    showV x = getMajor x <> "." <> getMinor x <> getAdditionalInfo x
    getMajor = T.pack . show . _majorMinorVersion_major . _nodeVersion_version
    getMinor = T.pack . show . _majorMinorVersion_minor . _nodeVersion_version
    getAdditionalInfo = flip (.) (_majorMinorVersion_additional_info . _nodeVersion_version) $ \case
        Development -> "-dev"
        ReleaseCandidate rc -> "-rc" <> T.pack (show rc)
        Release -> mempty
        Unknown -> mempty

{-# ANN nodesTab ("HLint: ignore Use &&" :: String) #-}
nodesTab
  :: forall r m t js.
    ( MonadAppWidget js t m
    , MonadReader r m
    , MonadReader r (ModalM m)
    , MonadJSM m
    , MonadJSM (Performable (ModalM m))
    , HasFrontendConfig r, HasTimeZone r, HasTimer t r
    , HasModal t m, MonadAppWidget js  t (ModalM m)
    )
  => Maybe UsingNodeOption -> m ()
nodesTab usingNodeOption =
  divClass "dashboard-section dashboard-section-nodes" $ do
    elClass "h4" "dashboard-section-title" $ text "Nodes"
    nodesDyn <- watchNodeAddresses
    nodeTilesWidget nodesDyn
  where
    nodeTilesWidget :: Dynamic t (MonoidalMap (Id Node) NodeSummary) -> m ()
    nodeTilesWidget nodesDyn = do
      let
        (external, internal) = splitDynPure $
          (filterLeft &&& filterRight) . fmap _nodeSummary_node . MMap.getMonoidalMap <$> nodesDyn
        kilnNodeState = fmap _processData_state . headMay . Map.elems <$> internal
        kilnNodeRestartAt = (_processData_restartAt <=< (headMay . Map.elems)) <$> internal
      useBlocker <- holdUniqDyn $ ffor nodesDyn $ \n -> MMap.null n
      -- let alertWindow = ClosedInterval LowerInfinity UpperInfinity
      kilnNodeStateD <- holdUniqDyn kilnNodeState
      kilnNodeRestartAtDyn <- holdUniqDyn kilnNodeRestartAt
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
              <> maybe "" (\t -> "at " <> time t <> ", ") (_snapshotMeta_importCompleteTime =<< sm)
              <> "and a Kiln Node has been created. Before starting the node you must verify the snapshot."
            btn = do
              ev <- divClass "buttons" $ uiButtonM "primary" $ do
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
              el "p" $ do
                el "strong" $ text "Fix: "
                text "Logs may provide insight as to why this happened. Click the menu on the Kiln Node tile and select “Show import log”. Alternatively, removing and starting the Kiln Node again may fix the issue, but is not guaranteed. You may want to verify the snapshot you are using is valid."
          renderSplashAlert i title Nothing desc

        snapshotDownloadFailedAlert = SemUi.segment (def & SemUi.classes SemUi.|~ "dashboard-section-overview") $ do
          let
            i = icon "icon-warning big red"
            title = text "Snapshot download failed."
            desc = do
              el "p" $ text "An error has occured during snapshot download. Check your network connection and provided snapshot URL."
              el "p" $ text "Logs may provide insight as to why this happened. Click the menu on the Kiln Node tile and select “Show error log”."
          renderSplashAlert i title Nothing desc

        internalNodeFailedAlert = SemUi.segment (def & SemUi.classes SemUi.|~ "dashboard-section-overview") $ do
          let
            i = icon "icon-warning big red"
            title = text "Internal node failed."
            desc = do
              el "p" $ text "Kiln node failed during work. Check Kiln command-line arguments that affect it."
              dyn_ $ ffor kilnNodeRestartAtDyn $ \mbRestartAt -> do
                whenJust mbRestartAt $ \restartAt -> el "p" $ do
                  text "Kiln node will be automatically restarted "
                  localHumanizedTimestampBasic $ pure restartAt
              el "p" $ text "Logs may provide insight as to why this happened. Click the menu on the Kiln Node tile and select “Show error log”."
          renderSplashAlert i title Nothing desc

      dyn_ $ ffor kilnNodeStateD $ traverse_ $ \case
        ProcessState_Node NodeProcessState_ImportComplete -> verifySnapshotAlert
        ProcessState_Node NodeProcessState_ImportCanceled -> pure ()
        ProcessState_Node NodeProcessState_ImportFailed -> snapshotImportFailedAlert
        ProcessState_Node NodeProcessState_ImportTimeout -> snapshotImportFailedAlert
        ProcessState_Node NodeProcessState_ImportingSnapshot -> pure ()
        ProcessState_Node NodeProcessState_GeneratingIdentity -> pure ()
        ProcessState_Node NodeProcessState_DownloadingSnapshot -> pure ()
        ProcessState_Node NodeProcessState_DownloadFailed -> snapshotDownloadFailedAlert
        ProcessState_Node NodeProcessState_DownloadCanceled -> pure ()
        ProcessState_Node NodeProcessState_DownloadComplete -> pure ()
        ProcessState_Initializing -> pure ()
        ProcessState_Failed -> internalNodeFailedAlert
        ProcessState_Starting -> pure ()
        ProcessState_Stopped -> pure ()
        ProcessState_Running -> pure ()

      -- Node tiles
      dyn_ $ ffor useBlocker $ \case
        True -> case usingNodeOption of
            Just (UsingCustomNode _) -> divClass "app-content app-welcome" welcomeScreen
            _ -> waitingForResponse
        False -> divClass "ui stackable cards" $ do
          ebn <- snd <$$$$> watchErrorsByNode everythingWindow

          let
            withSeverity e = \case
                Just m -> Just (_alertMetaData_severity $ getAlertMetaData e, m)
                Nothing -> Nothing

            errorMessages nodeId = do
              unresolvedAlertsForThisNode <- holdUniqDyn $ foldMap toList . MMap.lookup nodeId <$> ebn
              pure $ ffor unresolvedAlertsForThisNode $ mapMaybe $ \(lTag :=> Identity log) -> withSeverity lTag $ case lTag of
                NodeLogTag_InaccessibleNode -> Just $ text "Unable to connect."
                NodeLogTag_NodeWrongChain -> Just $ text "On wrong network."
                NodeLogTag_NodeInsufficientPeers -> Just $ text "Insufficient peers."
                NodeLogTag_NodeInvalidPeerCount -> Just $ text "Node has too few peers."
                NodeLogTag_BadNodeHead -> Just $ text $
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
            tezosVersion <- watchTezosVersion nodeId

            standardNodeTile
              (dynText titleUniq)
              (dynText $ fromMaybe nbsp <$> subtitleUniq)
              externalNodeMenu
              (>>= getNodeHeadBlock)
              (Just errors)
              (Just $ maybe True (all (\(t :=> _) -> case t of NodeLogTag_InaccessibleNode -> False; _ -> True)) . MMap.lookup nodeId <$> ebn)
              Nothing
              (Just $ (=<<) _nodeDetailsData_peerCount)
              (Just $ maybe (NetworkStat 0 0 0 0) _nodeDetailsData_networkStat)
              nodeDetails
              tezosVersion

          void $ listWithKey internal $ \nodeId nodeData -> do
            errors <- errorMessages nodeId
            processData <- holdUniqDyn nodeData
            state <- holdUniqDyn $ _processData_state <$> nodeData

            bakerRunning <- fmap ((== Just True) . fmap (isBakerRunning . snd))
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
                    tileMenuEntryModal "Stop Node" $ stopModal bRunning (PublicRequest_UpdateInternalDaemon DaemonType_Node False <$)
                  _ -> do
                    start <- tileMenuEntry "Start Node"
                    void $ requestingIdentity $ public (PublicRequest_UpdateInternalDaemon DaemonType_Node True) <$ start

              verifyAndStartMenu sm = do
                tileMenuEntryModal "Verify and start node" (verifySnapshotModal sm)

              showLogMenu errorLog = do
                tileMenuEntryModal "Show Error Log" $ showErrorLogModal "Snapshot import log" errorLog

              exportLogsMenu = do
                isExportAvailable <- asks (^. frontendConfig . frontendConfig_logExportAvailable)
                when isExportAvailable $ do
                  mUri <- getBackendPath (BackendRoute_ExportLogs :/ ExportLog_Node :/ ()) False
                  for_ mUri $ \uri -> elAttr "a" ("download" =: "KilnNode.log" <> "href" =: Uri.render uri) $
                    SemUi.listItem' def $ text "Export Logs"

              cancelSnapshotModal = warningModal "Cancel Node Setup?"
                [ "This will exit the snapshot import and remove the Kiln Node. You may create a new Kiln Node at any time." ]
                "Cancel Setup"
                (PublicRequest_CancelSnapshotImport <$)
              cancelSnapshotMenu = do
                tileMenuEntryModal "Cancel Setup" cancelSnapshotModal
              cancelSnapshotDownloadModal = warningModal "Cancel Snapshot download?"
                [ "This will stop the snapshot download and remove the Kiln Node. You may create a new Kiln Node at any time." ]
                "Cancel Download"
                (PublicRequest_CancelSnapshotDownload <$)
              cancelSnapshotDownloadMenu = do
                tileMenuEntryModal "Cancel Download" cancelSnapshotDownloadModal

              removeNodeMenu = do
                let
                  epilogue = "All data for this node will be deleted from Kiln."
                  removeInternalNodeModal running = warningModal "Remove Node?"
                    [preface, body running "Removing", epilogue]
                    "Stop and Remove Node"
                dyn_ $ ffor bakerRunning $ \running ->
                  tileMenuEntryModal "Remove Node" $ removeInternalNodeModal running
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
                      NodeProcessState_ImportCanceled -> "SETUP"
                      NodeProcessState_ImportFailed -> "FAILED"
                      NodeProcessState_ImportTimeout -> "FAILED"
                      NodeProcessState_GeneratingIdentity -> "STARTING"
                      NodeProcessState_DownloadingSnapshot -> "SETUP"
                      NodeProcessState_DownloadFailed -> "FAILED"
                      NodeProcessState_DownloadCanceled -> "SETUP"
                      NodeProcessState_DownloadComplete -> "SETUP"
                    ProcessState_Starting -> "Starting"
                    ProcessState_Running -> "Running"
                    ProcessState_Failed -> "Failed"

              workingTile :: m ()
              workingTile = do

                version <- watchTezosVersion nodeId

                nodeDetails <- watchNodeDetails nodeId

                standardNodeTile @(ProcessData, Maybe NodeDetailsData)
                  title
                  subtitle
                  (exportLogsMenu *> startStopNodeMenu *> removeNodeMenu)
                  ((=<<) getNodeHeadBlock . snd)
                  (Just errors)
                  (Just $ ffor2 ebn nodeData $ \es nd -> and
                    [ maybe True (all (\(t :=> _) -> case t of NodeLogTag_InaccessibleNode -> False; _ -> True)) (MMap.lookup nodeId es)
                    , _processData_state nd == ProcessState_Running
                    ])
                  (Just $ _processData_state . fst)
                  (Just $ (=<<) _nodeDetailsData_peerCount . snd)
                  (Just $ maybe (NetworkStat 0 0 0 0) _nodeDetailsData_networkStat . snd)
                  ((,) <$> nodeData <*> nodeDetails)
                  version

              failedNodeTile :: ProcessData -> m ()
              failedNodeTile pd = nodeTileWithSections
                [ tileHeader title subtitle menu badge Nothing (pure Nothing)
                , divClass "internal-node-tile-body" $ do
                    divClass "ui row" $ case _processData_state pd of
                      ProcessState_Failed -> divClass "ui sub header" $ text "Internal node failed"
                      _ -> blank
                ]
                where
                  menu = case _processData_state pd of
                    ProcessState_Failed -> Just $ do
                      restart <- tileMenuEntry "Restart Node"
                      void $ requestingIdentity $ public (PublicRequest_UpdateInternalDaemon DaemonType_Node True) <$ restart
                      for_ (_processData_errorLog pd) $ \errLog ->
                        tileMenuEntryModal "Show Error Log" $ showErrorLogModal "Kiln node error log" errLog
                      removeNodeMenu
                    _ -> Nothing

                  badge :: m ()
                  badge = tileBadgeImpliedByErrors (Just errors) (Just state)

              nodeStartTile :: NodeProcessState -> Dynamic t (Maybe SnapshotMeta) -> m ()
              nodeStartTile nodeState dSnapshotMeta = nodeTileWithSections
                [ tileHeader title subtitle menu badge Nothing (pure Nothing)
                , divClass "internal-node-tile-body" $ do
                    -- when (nodeState == NodeProcessState_ImportingSnapshot || nodeState == NodeProcessState_GeneratingIdentity) $
                    case nodeState of
                      NodeProcessState_ImportingSnapshot -> divClass "generating-icons" $ do
                        icon "icon-install big"
                        divClass "ui active tiny inline loader blue small" blank
                      NodeProcessState_GeneratingIdentity -> divClass "generating-icons" $ do
                        icon "icon-id-badge big"
                        divClass "ui active tiny inline loader blue small" blank
                      NodeProcessState_DownloadingSnapshot -> divClass "generating-icons" $ do
                        icon "icon-id-badge big"
                        divClass "ui active tiny inline loader blue small" blank
                      _ -> blank
                    let
                      subHeader t = divClass "ui sub header" $ text t
                      errorMessage t = divClass "ui error message" $ text t
                    divClass "ui row" $ case nodeState of
                      NodeProcessState_ImportingSnapshot -> subHeader "Importing snapshot"
                      NodeProcessState_ImportComplete -> subHeader "Verify snapshot"
                      NodeProcessState_ImportCanceled -> subHeader "Snapshot import canceled"
                      NodeProcessState_ImportFailed -> errorMessage "Snapshot import failed"
                      NodeProcessState_ImportTimeout -> errorMessage "Snapshot import failed"
                      NodeProcessState_GeneratingIdentity -> subHeader "Generating identity"
                      NodeProcessState_DownloadingSnapshot -> subHeader "Downloading snapshot"
                      NodeProcessState_DownloadFailed -> errorMessage "Snapshot download failed"
                      NodeProcessState_DownloadCanceled -> subHeader "Snapshot download canceled"
                      NodeProcessState_DownloadComplete -> subHeader "Snapshot download complete"
                    when (nodeState == NodeProcessState_ImportingSnapshot) $ do
                      withSnapshotMeta $ \mSnapshotMeta -> for_ mSnapshotMeta $ \sm ->
                        whenJust (sm ^. snapshotMeta_importLog) $ \log -> divClass "import progress" $ text log
                    when (nodeState == NodeProcessState_DownloadingSnapshot) $ do
                      withSnapshotMeta $ \mSnapshotMeta -> for_ mSnapshotMeta $ \sm ->
                        whenJust (sm ^. snapshotMeta_downloadProgress) $ \progress -> divClass "import progress" $ text $
                          "Download progress: " <> tshow progress <> "%"
                    divClass "ui row" $ divClass "explanation" $ text $ case nodeState of
                      NodeProcessState_ImportingSnapshot -> "Depending on your hardware, importing a snapshot may take up to a few hours."
                      NodeProcessState_ImportComplete -> "You must verify this snapshot before starting the node."
                      NodeProcessState_ImportCanceled -> ""
                      NodeProcessState_ImportFailed -> ""
                      NodeProcessState_ImportTimeout -> ""
                      NodeProcessState_GeneratingIdentity -> "Before the node can run it must generate a secure identity to use on the network. This may take several minutes."
                      NodeProcessState_DownloadingSnapshot -> "Downloading snapshot from the given URL"
                      NodeProcessState_DownloadFailed -> ""
                      NodeProcessState_DownloadCanceled -> ""
                      NodeProcessState_DownloadComplete -> ""
                    when (nodeState == NodeProcessState_ImportComplete) $ withSnapshotMeta $ \mSnapshotMeta -> for_ mSnapshotMeta $ \sm -> do
                      ev <- divClass "buttons" $ uiButtonM "" $ do
                        icon "icon-angle-right"
                        text "Start Verification"
                      tellModal $ ev $> verifySnapshotModal sm
                    when (nodeState == NodeProcessState_ImportingSnapshot) $ elClass "p" "explanation" $ do
                      text "Taking too Long? "
                      (e, _) <- el' "a" $ text "Cancel import"
                      let ev = domEvent Click e
                      tellModal $ ev $> cancelSnapshotModal
                    when (nodeState == NodeProcessState_DownloadingSnapshot) $ elClass "p" "explanation" $ do
                      text "Taking too Long? "
                      (e, _) <- el' "a" $ text "Cancel download"
                      let ev = domEvent Click e
                      tellModal $ ev $> cancelSnapshotDownloadModal
                ]
                where
                  withSnapshotMeta action = dyn_ $ ffor dSnapshotMeta $ \mSnapshotMeta -> action mSnapshotMeta
                  menu = case nodeState of
                    NodeProcessState_ImportingSnapshot -> Just cancelSnapshotMenu
                    NodeProcessState_ImportCanceled -> Nothing
                    NodeProcessState_ImportComplete -> Just $ withSnapshotMeta $ \mSnapshotMeta -> do
                      mapM_ verifyAndStartMenu mSnapshotMeta
                      removeNodeMenu
                    NodeProcessState_ImportFailed -> Just $ withSnapshotMeta $ \mSnapshotMeta -> do
                      mapM_ showLogMenu (_snapshotMeta_importError =<< mSnapshotMeta)
                      removeNodeMenu
                    NodeProcessState_ImportTimeout -> Just removeNodeMenu
                    NodeProcessState_GeneratingIdentity -> Just $ startStopNodeMenu *> removeNodeMenu
                    NodeProcessState_DownloadingSnapshot -> Just cancelSnapshotDownloadMenu
                    NodeProcessState_DownloadFailed -> Just $ withSnapshotMeta $ \mSnapshotMeta -> do
                      mapM_ showLogMenu (_snapshotMeta_downloadError =<< mSnapshotMeta)
                      removeNodeMenu
                    NodeProcessState_DownloadCanceled -> Nothing
                    NodeProcessState_DownloadComplete -> Nothing
                  badge :: m ()
                  badge = tileBadgeImpliedByErrors (Just errors) (Just state)
            dSnapshotMeta <- watchSnapshotMeta
            dyn_ $ ffor processData $ \pd ->
              case _processData_state pd of
                ProcessState_Node s -> nodeStartTile s dSnapshotMeta
                ProcessState_Failed -> failedNodeTile pd
                _ -> workingTile

    tileHeader
      :: m () -- ^ Title
      -> m () -- ^ Subtitle
      -> Maybe (m ()) -- ^ Tile menu contents
      -> m () -- ^ Status badge
      -> Maybe (Dynamic t [(AlertSeverity, m ())]) -- ^ (Optional) Function to build list of error messages for this node
      -> Dynamic t (Maybe (Maybe TezosVersion))
      -> m ()
    tileHeader title subtitle menuContents badge errors' version = do
      case menuContents of
        Nothing -> divClass "tile-header-spacing" blank
        Just c -> tileMenu c
      divClass "title" $ do
        badge
        title
        divClass "secondary-name" subtitle
      divClass "ui left aligned container" $ tileVersion version
      tileErrors errors'

    tileVersion :: Dynamic t (Maybe (Maybe TezosVersion)) -> m ()
    tileVersion vv = do
        v <- maybeDyn vv
        let gitLink = "https://gitlab.com/tezos/tezos/-/releases/"
            commitHash = either id (_commitInfo_commitHash . _nodeVersion_commitInfo) . getTezosVersion
            commitLink = "https://gitlab.com/tezos/tezos/-/commit/"
            -- withQuestionMark f txt
                --- | txt == "-" = "?"
            wrapParens ver c
                | ver /= c = "(" <> T.take 8 c <> ")"
                | otherwise = T.take 8 c
            displayLinkText vc = dyn_ $ ffor vc $ \case
                Right (ver,c) -> do
                    -- if ver == c, then ver would be a commit hash
                    when (ver /= c) $ do
                      hrefLink (gitLink <> "v" <> ver) (text ver)
                      text " "
                    hrefLink (commitLink <> c) (text $ wrapParens ver c)
                Left t -> text t
            pp = maybe (Left "?") (Right  . ((,) <$> ppTezosVersion <*> commitHash))


        el "div" $ withPlaceholder $ withMaybeDyn v displayLinkText pp

    tileErrors = traverse_ $ \errors ->
      dyn_ $ ffor errors $ traverse_ $ \(severity, m) ->
        divClass ("ui message " <> severityColor severity) m

    tileBadgeImpliedByErrors
      :: Maybe (Dynamic t [a])
      -> Maybe (Dynamic t ProcessState)
      -> m ()
    tileBadgeImpliedByErrors mErrors mInternalState = do
      let color = fmap statusColor $ nodeStatus
            <$> sequence mInternalState
            <*> maybe (pure 0) (fmap length) mErrors
      iconDyn $ ("tiny circle " <>) <$> color

    tileBlockStats getBlock node = do
      b <- maybeDyn $ getBlock <$> node

      divClass "soft-heading" $
        withPlaceholder' "<unavailable>" $ withMaybeDyn b display (unRawLevel . view level)
      text "#"
      withPlaceholder $ withMaybeDyn b blockHashLink (view hash)

      el "dl" $ do

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
            showSpeed c n = dynText <=< holdUniqDyn $ ffor2 c n $ \c' n' -> if c' && n' > 0 then n' & fromIntegral & humanBytes & (<> "/s") else "-"
            showTotal c n = dynText <=< holdUniqDyn $ ffor2 c n $ \c' n' -> if c' && n' > 0 then n' & unTezosWord64 & fromIntegral & humanBytes else "-"

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
      -> Maybe (Dynamic t [(AlertSeverity, m ())]) -- ^ (Optional) Function to build list of error messages for this node
      -> Maybe (Dynamic t Bool) -- ^ (Optional) Are we connected to the node?
      -> Maybe (a -> ProcessState) -- ^ (Optional) Function to build list of error messages for this node
      -> Maybe (a -> Maybe Word64) -- ^ (Optional) Function to get the peer count of the node
      -> Maybe (a -> NetworkStat) -- ^ (Optional) Function to get the network stats of the node
      -> Dynamic t a -- ^ Node
      -> Dynamic t (Maybe (Maybe TezosVersion))
      -> m ()
    standardNodeTile title subtitle menuContents getBlock errors' connected internalState getPeerCount' getNetworkStats' node nodeVersion = do
      let badge = tileBadgeImpliedByErrors errors' $ fmap (<$> node) internalState
      nodeTileWithSections $
        [ tileHeader title subtitle (Just menuContents) badge errors' nodeVersion
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

bakersTab
  :: forall r m t js.
    ( MonadAppWidget js t m
    , MonadAppWidget js t (ModalM m)
    , MonadReader r m
    , MonadReader r (ModalM m)
    , HasTimer t r
    , HasTimeZone r
    , HasFrontendConfig r
    , MonadJSM m
    , MonadJSM (Performable (ModalM m))
    , HasModal t m
    )
  => m ()
bakersTab =
  divClass "dashboard-section dashboard-section-bakers" $ do
    tilesWidget =<< watchBakerAddresses
  where
    tilesWidget :: Dynamic t (MonoidalMap PublicKeyHash BakerSummary) -> m ()
    tilesWidget tilesDyn = do
      useBlocker <- holdUniqDyn $ MMap.null <$> tilesDyn
      dEbb :: Dynamic t (MonoidalMap PublicKeyHash (NonEmpty BakerAlert)) <- watchBakerAlerts
      dCollectiveNodesStatus <- watchCollectiveNodesStatus everythingWindow
      dyn_ $ ffor useBlocker $ \case
        True -> waitingForResponse
        False -> mdo
          let
            toLogTag ba = if isUserResolvable ba
              then Just $ case ba of
                BakerAlert_Alert (btag :=> Identity blog) ->
                  (LogTag_Baker btag :=> Const (errorLogIdForBakerLogTag btag blog)) :| []
                BakerAlert_GroupedAlert GroupedBakerAlert{..} ->
                  case _groupedBakerAlert_type of
                    GroupedAlertType_MissedBake ->
                      fmap (\i -> LogTag_Baker BakerLogTag_BakerMissed :=> Const i) _groupedBakerAlert_logs
                    GroupedAlertType_MissedEndorsementBonus ->
                      fmap (\i -> LogTag_Baker BakerLogTag_MissedEndorsementBonus :=> Const i) _groupedBakerAlert_logs
              else Nothing

            resolvable = concatMap toList . concatMap (mapMaybe toLogTag . NEL.toList) . MMap.elems <$> dEbb
            anyErrors = not . null <$> resolvable
          resolveAll <- uiDynButton ((<>) "primary right floated " . bool "transition hidden" "" <$> anyErrors) $ do
            icon "icon-check"
            text "Resolve All"
          _ <- requestingIdentity $ attachWith (\as () -> public $ PublicRequest_ResolveAlerts as) (current resolvable) resolveAll
          elClass "h4" "dashboard-section-title" $ text "Bakers"

          let
            bakerSummaryDetails = ffor2 tilesDyn (joinDynThroughMap bakersDetails) $ \(MMap.MonoidalMap bakerSummary) bakerDetails ->
              flip Map.mapWithKey bakerSummary $ \pkh bs -> (bs, join $ Map.lookup pkh bakerDetails)

            bakerStatus' = ffor2 dCollectiveNodesStatus bakerSummaryDetails $ \cns ->
              fmap $ \(bs, bd) ->
                bakerStatus $ (bs, bd) <$ cns
            wantBakerData = (||)
              <$> (elem MonitoredStatus_Unknown <$> bakerStatus')
              <*> (any isNothing <$> joinDynThroughMap bakersDetails)
          (bakersBanner :: Dynamic t (Maybe BakersBanner)) <-
            holdUniqDyn $ ffor2 dCollectiveNodesStatus wantBakerData $ \case
              Left _ -> \_ -> Just BakersBanner_CannotGather
              Right () -> \cond -> BakersBanner_Gathering <$ guard cond
          dyn_ $ ffor bakersBanner mkBakersBanner

          -- Show alert banner if baker process has failed.
          -- We don't unify it with other baker alerts to not make logic too polymorphic.
          dyn_ $ ffor tilesDyn $ \bakerSummaryMap ->
            for_ bakerSummaryMap $ \bakerSummary -> case _bakerSummary_baker bakerSummary of
              Left _ -> pure ()
              Right bid ->
                let bakerProcessData = _bakerInternalData_processData bid
                in case _processData_state bakerProcessData of
                  ProcessState_Failed -> failedBakerBanner bakerProcessData
                  _ -> pure ()

          let notifications :: Dynamic t (Map.Map (Down BakerAlert) ())
              notifications = Map.fromList . fmap (\k -> (Down k, ())) . foldMap toList . MMap.elems . fmap NEL.toList <$> dEbb
          _ <- listWithKey notifications $ \(Down k) _ -> splashAlert tilesDyn k

          (bakersDetails :: Dynamic t (Map.Map PublicKeyHash
                                               (Dynamic t (Maybe BakerDetails)))) <- divClass "ui stackable cards" $ do
            listWithKey (coerceDynamic tilesDyn) $ \pkh vDyn -> do
              unresolvedAlerts <- holdUniqDyn $ foldMap toList . MMap.lookup pkh <$> dEbb

              let
                renderBakerError = text . _bakerErrorDescriptions_tile

                bakerAlerts = (++)
                  <$> ffor dCollectiveNodesStatus (\case
                          Left e -> [Left e]
                          Right _ -> [])
                  <*> (map Right <$> unresolvedAlerts)

                withSeverity e = fmap $ \m -> (_alertMetaData_severity $ getAlertMetaData e, m)
                errorMessages = ffor bakerAlerts $ mapMaybe $ \e -> withSeverity e $ case e of
                  Left (_ :: CollectiveNodesFailure) -> Just $ text "Cannot gather baker data."
                  Right (BakerAlert_Alert (lTag :=> Identity log)) -> case lTag of
                    BakerLogTag_BakerMissed -> Just $ text $ "Missed " <> aRight <> "."
                      where
                        aRight = case _errorLogBakerMissed_right log of
                          RightKind_Baking -> "a bake"
                          RightKind_Endorsing -> "an attestation"
                    BakerLogTag_MissedEndorsementBonus -> Just $ text "Missed attestation bonus."
                    BakerLogTag_NeedToResetHWM -> Just $ renderBakerError $ bakerNeedToResetHWMDescriptions log
                    BakerLogTag_BakerLedgerDisconnected -> Just $ renderBakerError $ bakerLedgerDisconnectedDescriptions log
                    BakerLogTag_BakerDeactivated -> Just $ renderBakerError $ bakerDeactivatedDescriptions log
                    BakerLogTag_BakerDeactivationRisk -> Just $ renderBakerError $ bakerDeactivationRiskDescriptions log
                    BakerLogTag_BakerAccused -> Just $ renderBakerError $ bakerAccusedDescriptions log
                    BakerLogTag_InsufficientFunds -> Just $ do
                        dMinimalStake <- _protoInfo_minimalStake <$$$> watchLatestProtoInfo
                        dyn_ $ ffor dMinimalStake $ \mMinimalStake -> renderBakerError $ bakerInsufficientFundsDescriptions mMinimalStake log
                    BakerLogTag_NotEnoughStakedBalance -> Just $
                      renderBakerError bakerNotEnoughStakedBalanceDescriptions
                    BakerLogTag_NeedToFinalizeUnstake -> Nothing
                    BakerLogTag_VotingReminder -> Nothing
                  Right (BakerAlert_GroupedAlert GroupedBakerAlert{..}) ->
                    Just $ el "span" $ do
                      elClass "span" "ui label circular" $ text $ tshow _groupedBakerAlert_count
                      text nbsp
                      text $ "Missed " <> subj <> "."
                      where
                        subj = case _groupedBakerAlert_type of
                          GroupedAlertType_MissedBake -> case _groupedBakerAlert_right of
                            Just RightKind_Baking -> "a bake"
                            Just RightKind_Endorsing -> "an attestation"
                            Nothing -> error "Inconsistent state of grouped alert. 'right' should be 'Just' value for 'MissedBake' alert."
                          GroupedAlertType_MissedEndorsementBonus -> "the attestation bonus"

              let (title, subtitle) = splitDynPure $ bakerSummaryIdentification . (pkh,) <$> vDyn
              titleUniq <- holdUniqDyn title
              subtitleUniq <- holdUniqDyn subtitle
              details <- watchBakerDetails pkh

              -- If subtitle is 'Nothing' (in other words, if the baker has no alias)
              -- then the title is baker's pkh, and we should use monospaced font there
              let isTitleMonospaced = isNothing <$> subtitleUniq
              let
                title' = dyn_ $ ffor isTitleMonospaced $ \case
                  False -> dynText titleUniq
                  True  -> elClass "span" "monospaced-text" $ dynText titleUniq
              let
                standardBakerTile =
                  tile
                    title'
                    pkh
                    subtitleUniq
                    (\ev -> PublicRequest_RemoveBaker pkh <$ ev)
                    -- if you have both a bake and attest for the same level, you
                    -- must *first* bake the block at that level, then you may
                    -- immediately attest that block.  the times are the same,
                    -- baking happens first.
                    (Just errorMessages)
                    vDyn
                    details
                    dCollectiveNodesStatus

              isInternal <- holdUniqDyn $ isRight . _bakerSummary_baker <$> vDyn
              dyn_ $ ffor isInternal $ \case
                False -> standardBakerTile
                True -> do
                  bid <- watchInternalBaker
                  -- show 'failedBakerTile' if baker has failed process state
                  dyn_ $ ffor bid $ \bid' -> case fmap (_bakerInternalData_processData . snd) bid' of
                    Just bakerPd | _processData_state bakerPd == ProcessState_Failed ->
                      failedBakerTile (dynText titleUniq) bakerPd (\ev -> PublicRequest_RemoveBaker pkh <$ ev)
                    _ -> standardBakerTile

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

    failedBakerBanner :: ProcessData -> m ()
    failedBakerBanner intBakerData =
      SemUi.segment (def & SemUi.classes SemUi.|~ "dashboard-section-overview") $ do
        let
          i = icon "icon-warning big red"
          title = text "Kiln Baker failed."
          desc = do
            el "p" $ text "Kiln baker failed during work. Check 'kiln-baker-custom-args' Kiln argument."
            let mbRestartAt = _processData_restartAt intBakerData
            whenJust mbRestartAt $ \restartAt -> el "p" $ do
              text "Kiln baker will be automatically restarted "
              localHumanizedTimestampBasic $ pure restartAt
            el "p" $ text "Logs may provide insight as to why this happened. Click the menu on the Kiln Baker tile and select “Show error log”."
        renderSplashAlert i title Nothing desc

    splashAlert :: Dynamic t (MonoidalMap PublicKeyHash BakerSummary) -> BakerAlert -> m ()
    splashAlert tilesDyn = SemUi.segment (def & SemUi.classes SemUi.|~ "dashboard-section-overview") . \case
      BakerAlert_Alert errorView@(bTag :=> Identity log) ->
        let
          pkh = bakerIdForBakerErrorLogView errorView
          ev = (LogTag_Baker bTag :=> Const (errorLogIdForBakerLogTag bTag log)) :| []
        in case bTag of
          BakerLogTag_BakerLedgerDisconnected -> renderBakerError ev (pure $ bakerLedgerDisconnectedDescriptions log) pkh
          BakerLogTag_BakerMissed -> renderBakerError ev (pure $ bakerMissedDescriptions log) pkh
          BakerLogTag_MissedEndorsementBonus -> renderBakerError ev (pure $ bakerMissedEndorsementBonusDescriptions log) pkh
          BakerLogTag_NeedToResetHWM -> renderBakerError ev (pure $ bakerNeedToResetHWMDescriptions log) pkh
          BakerLogTag_BakerDeactivated -> renderBakerError ev (pure $ bakerDeactivatedDescriptions log) pkh
          BakerLogTag_BakerDeactivationRisk -> renderBakerError ev (pure $ bakerDeactivationRiskDescriptions log) pkh
          BakerLogTag_BakerAccused -> renderBakerError ev (pure $ bakerAccusedDescriptions log) pkh
          BakerLogTag_InsufficientFunds -> do
              dMinimalStake <- _protoInfo_minimalStake <$$$> watchLatestProtoInfo
              dyn_ $ ffor dMinimalStake $ \mMinimalStake -> renderBakerError ev (pure $ bakerInsufficientFundsDescriptions mMinimalStake log) pkh
          BakerLogTag_NotEnoughStakedBalance ->
            renderBakerError ev (pure bakerNotEnoughStakedBalanceDescriptions) pkh
          BakerLogTag_NeedToFinalizeUnstake ->
            renderBakerError ev (pure $ bakerNeedToFinalizeUnstakeDescriptions log) pkh
          BakerLogTag_VotingReminder ->
            withAmendmentPeriodProgress (_errorLogVotingReminder_votingPeriod log) $ \remaining ->
              renderBakerError ev (bakerVotingReminderDescriptions log <$> remaining) pkh

      BakerAlert_GroupedAlert GroupedBakerAlert{..} ->
        let
          logs = _groupedBakerAlert_logs
          first' = _groupedBakerAlert_first
          latest' = _groupedBakerAlert_latest
          pkh = unId _groupedBakerAlert_baker
        in do
          tz <- asks (^. timeZone)
          case _groupedBakerAlert_type of
            GroupedAlertType_MissedBake ->
              let
                ev = fmap (\l -> LogTag_Baker BakerLogTag_BakerMissed :=> Const l) logs
                errMsg = "Inconsistent state of grouped alert. 'right' should be 'Just' value for 'MissedBake' alert."
                rightKind = fromMaybe (error errMsg) _groupedBakerAlert_right
              in renderBakerError ev (pure $ bakerGroupedMissedDescriptions tz _groupedBakerAlert_count first' latest' rightKind) pkh
            GroupedAlertType_MissedEndorsementBonus ->
              let ev = fmap (\l -> LogTag_Baker BakerLogTag_MissedEndorsementBonus :=> Const l) logs
              in renderBakerError ev (pure $ bakerGroupedMissedBonusDescriptions tz (length logs) first' latest') pkh

      where
        renderBakerError :: NonEmpty (DSum LogTag (Const (Id ErrorLog))) -> Dynamic t BakerErrorDescriptions -> PublicKeyHash -> m ()
        renderBakerError ev dsc pkh = do
          let severity = _alertMetaData_severity $ getAlertMetaData $ NEL.head ev
              warning = _bakerErrorDescriptions_warning <$> dsc
          renderResolvableSplashAlert ev
            (icon $ "icon-warning big " <> severityColor severity)
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

    failedBakerTile
      :: m () -- ^ Title
      -> ProcessData -- ^ Baker process data
      -> (Event t () -> Event t (PublicRequest ())) -- ^ Construct an API request with an 'Event' to remove this baker.
      -> m ()
    failedBakerTile title bakerProcessData mkRemoveReq = do
      divClass "ui card dashboard-tile baker-tile" $ divClass "content" $ do
        tileMenu $ do
          let
            showLogMenu errorLog = do
              restart <- tileMenuEntry "Restart Baker"
              void $ requestingIdentity $ public (PublicRequest_UpdateInternalDaemon DaemonType_Baker True) <$ restart
              tileMenuEntryModal "Show Error Log" $ showErrorLogModal "Kiln baker error log" errorLog

            removeEntry modal = tileMenuEntryModal "Remove Baker" $ modal mkRemoveReq
            removeInternalBakerModal = warningModal "Remove Baker?"
              ["This baker will not be able to sign blocks or attestations once removed and all related baker data will be deleted."]
              "Remove Baker"

          let mbErrorLog = _processData_errorLog bakerProcessData
          traverse_ showLogMenu mbErrorLog
          removeEntry removeInternalBakerModal

        divClass "title" $ do
          icon "tiny circle red"
          title
          blank
          divClass "internal-subtitle" $ do
            kilnLogo
            divClass "ui sub header" $ text "Failed"

    tile
      :: m () -- ^ Title
      -> PublicKeyHash
      -> Dynamic t (Maybe Text) -- ^ Subtitle
      -> (Event t () -> Event t (PublicRequest ())) -- ^ Construct an API request with an 'Event' to remove this baker.
      -> Maybe (Dynamic t [(AlertSeverity, m ())]) -- ^ (Optional) Function to build list of error messages for this baker
      -> Dynamic t BakerSummary -- ^ Baker
      -> Dynamic t (Maybe BakerDetails) -- ^ Details
      -> Dynamic t (Either CollectiveNodesFailure ())
      -> m ()
    tile title pkh subtitle mkRemoveReq errors' bakerDyn details' dCollectiveNodesStatus = do
      let connected = isRight <$> dCollectiveNodesStatus
          dmDelegateInfo = preview (_Just . bakerDetails_delegateInfo . _Just . to unJson) <$> details'
          dmStakedBalance = fmap join $ _bakerDetails_stakedBalance <$$> details'
          dmUnstakedFrozenBalance = fmap join $ _bakerDetails_unstakedFrozenBalance <$$> details'

      mbLatestHeadDyn <- watchLatestHead
      mbAiCycleDyn <- watchAICycle

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
              vp | isVotingPeriod vp -> Just $ maybe notVoted hasVoted mBakerVote
              _ -> Nothing

        tileMenu $ do
          let
            removeEntry modal = tileMenuEntryModal "Remove Baker" $ modal mkRemoveReq
          dyn $ ffor bakerDyn $ \bs -> case _bakerSummary_baker bs of
            Left _ -> do -- not a kiln baker
              removeEntry $ removeItemModal "baker"
            Right bid -> do
              mPeriodKind_amendment <- maybeDyn . fmap Map.lookupMax =<< watchAmendment
              mbBakerVoteDyn <- watchBakerVote
              whenJustDyn mPeriodKind_amendment $ \periodKind_amendment -> do
                isTestingOrAdoptionPeriod <- holdUniqDyn $ not . isVotingPeriod . fst <$> periodKind_amendment
                dyn_ $ ffor2 isTestingOrAdoptionPeriod mbBakerVoteDyn $ \cond mbBakerVote -> case cond of
                  True -> pure ()
                  False | isJust mbBakerVote -> pure ()
                  False -> do
                    open <- tileMenuEntry "Vote"
                    let amendment = snd <$> periodKind_amendment
                    mProtoInfo <- maybeDyn =<< watchLatestProtoInfo
                    let baker = ffor (current bakerDyn) $ \summary -> case _bakerSummary_baker summary of
                          Left _ -> Nothing
                          Right b -> Just (pkh, _bakerInternalData_secretKey b)
                        xs = (liftA3 . liftA3) (,,) (current mProtoInfo) (pure . pure <$> current amendment) baker
                    tellModal $ attachWithMaybe (\ma () -> ffor ma $ \(p,a,b) -> cancelableModalWithClasses $ fmap (pure ["vote-modal"],) . voteModal b p a) xs open

              isExportAvailable <- asks (^. frontendConfig . frontendConfig_logExportAvailable)
              when isExportAvailable $ do
                mBakerUri <- getBackendPath (BackendRoute_ExportLogs :/ ExportLog_Baker :/ ()) False
                for_ mBakerUri $ \uri -> elAttr "a" ("download" =: "KilnBaker.log" <> "href" =: Uri.render uri) $
                  SemUi.listItem' def $ text "Export Baker Logs"

              let sk = _bakerInternalData_secretKey bid
              tileMenuEntryModal "Authorize Ledger Device" $ cancelableModalWithClasses $ authorizeLedgerToBakeModal sk pkh
              latestHead <- maybeDyn =<< watchLatestHead
              dyn_ $ ffor latestHead $ \case
                Nothing -> pure () -- no head to set high water mark
                Just bl -> tileMenuEntryModal "Set High-Water Mark" $ cancelableModalWithClasses $ setHighWaterMark (view level <$> bl) sk pkh
              tileMenuEntryModal "Set Liquidity Baking" $ cancelableModalWithClasses $ setLiquidityBakingToggleModal (sk, pkh)
              if isBakerRunning bid
              then do
                let stopModal = warningModal "Stop Baker?"
                      ["This baker will not be able to sign blocks or attestations once stopped. You can restart this baker at any time."]
                      "Stop Baker"
                tileMenuEntryModal "Stop Baker" $ stopModal (PublicRequest_UpdateInternalDaemon DaemonType_Baker False <$)
              else do
                start <- tileMenuEntry "Start Baker"
                void $ requestingIdentity $ public (PublicRequest_UpdateInternalDaemon DaemonType_Baker True) <$ start
              let
                removeInternalBakerModal = warningModal "Remove Baker?"
                  ["This baker will not be able to sign blocks or attestations once removed and all related baker data will be deleted."]
                  "Remove Baker"
              removeEntry removeInternalBakerModal

              let
                openModalEv btn modal = ffor btn $ \() ->
                  cancelableModalWithClasses $ fmap (pure ["vote-modal"],) . modal sk details'
              whenAIActivated mbLatestHeadDyn mbAiCycleDyn $ do
                stakeBtn <- tileMenuEntry "Stake"
                let openStakeEv = openModalEv stakeBtn stakeModal
                tellModal openStakeEv

                unstakeBtn <- tileMenuEntry "Unstake"
                let openUnstakeEv = openModalEv unstakeBtn unstakeModal
                tellModal openUnstakeEv

                finalizeUnstakeBtn <- tileMenuEntry "Finalize Unstake"
                let openFinalizeUnstakeEv = openModalEv finalizeUnstakeBtn finalizeUnstakeModal
                tellModal openFinalizeUnstakeEv

              -- Setting delegate parameters is available before Adaptive Issuance activation
              -- so we always show this button
              setParamsBtn <- tileMenuEntry "Set Delegate Parameters"
              let openSetDelegateParamsEv = openModalEv setParamsBtn setDelegateParamsModal
              tellModal openSetDelegateParamsEv

        divClass "title" $ do
          let bakerStatusDyn = (\b bd n -> bakerStatus $ (b, bd) <$ n) <$> bakerDyn <*> details' <*> dCollectiveNodesStatus
          for_ errors' $ \_ -> do
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
          divClass "secondary-name monospaced-text" $ dynText =<< holdUniqDyn (fromMaybe nbsp <$> subtitle)

        for_ errors' $ \errors -> do
          dyn_ $ ffor errors $ traverse_ $ \(severity, m) ->
            divClass ("ui message " <> severityColor severity) m

        let
          nextRightTxt = ffor (_bakerSummary_nextRight <$> bakerDyn) $ \case
            BakerNextRight_GatheringData -> Left $ text "-"
            BakerNextRight_WaitingForRights -> Left $ text "Waiting to receive baking rights"
            BakerNextRight_KnownNoRights -> Left $ text "None"
            BakerNextRight_BakeBlock lvl -> Right lvl
          wantToGatherData = (== BakerNextRight_GatheringData) . _bakerSummary_nextRight <$> bakerDyn
        isGatheringData <- holdUniqDyn $ (&&) <$> wantToGatherData <*> connected

        divClass "divider" blank

        el "dl" $ do
          (latestHead, knownProto) <- watchHeadWithProtocol
          let
            mbActiveConsensusPkhDyn = ffor dmDelegateInfo $ \mDelegateInfo ->
              mDelegateInfo >>= _cacheDelegateInfo_activeConsensusKey
            mbPendingConsensusPkhDyn = ffor dmDelegateInfo $ \mDelegateInfo ->
              mDelegateInfo >>= _cacheDelegateInfo_pendingConsensusKey
          whenJustDyn mbActiveConsensusPkhDyn $ \activeConsensusPkh -> el "div" $ do
            el "dt" (text "Active consensus key")
            elClass "dd" "monospaced-text" $ text $ toPublicKeyHashText activeConsensusPkh
          whenJustDyn mbPendingConsensusPkhDyn $ \PendingConsensusKey{..} -> el "div" $ do
            el "dt" (text "Pending consensus key")
            let cycleText = tshow $ unCycle _pendingConsensusKey_cycle
            el "dd" $ do
              tooltipped TooltipPos_BottomCenter (pkhTooltip pkh) $
                elClass "span" "monospaced-text" $ text $ shortenPkh _pendingConsensusKey_pkh
              el "span" $ text $ " at cycle " <> cycleText
          el "div" $ do
            el "dt" (text "Next Bake")
            elClass "dd" "monospaced-text" $ dyn_ $ ffor nextRightTxt $ \case
              Left t -> t
              Right lvl -> do
                text $ tshow $ unRawLevel lvl
                etaDyn <- maybeDyn $ getCompose $ predictFutureTimestamp
                  <$> Compose (view protocolIndex_constants <<$>> knownProto)
                  <*> Compose (fmap Just $ constDyn lvl)
                  <*> Compose latestHead
                text nbsp
                dyn_ $ ffor etaDyn $ maybe blank localHumanizedTimestampBasicWithoutTZ

        elClass "table" "baker-balance" $ do
          let availableBalanceTooltip = divClass "detail" $ text "The spendable balance, excluding frozen deposists."
          el "tr" $ do
            el "td" $ tooltipped TooltipPos_TopLeft availableBalanceTooltip (text "Available Balance")
            elClass "td" "baker-balance-whole monospaced-text" $ withPlaceholder $ ffor dmDelegateInfo $ fmap $ \t -> do
              let (w, _p, _tz) = tez' $ _cacheDelegateInfo_balance t - _cacheDelegateInfo_frozenBalance t
              text w
            elClass "td" "baker-balance-part monospaced-text" $ withPlaceholder' "" $ ffor dmDelegateInfo $ fmap $ \t -> do
              let (_w, p, tz) = tez' $ _cacheDelegateInfo_balance t - _cacheDelegateInfo_frozenBalance t
              text p
              elClass "span" "tez" $ text tz

          whenAIActivated mbLatestHeadDyn mbAiCycleDyn $ do
            let stakedBalanceTooltip = divClass "detail" $ text "The staked balance."
            el "tr" $ do
              el "td" $ tooltipped TooltipPos_TopLeft stakedBalanceTooltip (text "Staked Balance")
              elClass "td" "baker-balance-whole monospaced-text" $ withPlaceholder $ ffor dmStakedBalance $ fmap $ \t -> do
                let (w, _p, _tz) = tez' t
                text w
              elClass "td" "baker-balance-part monospaced-text" $ withPlaceholder' "" $ ffor dmStakedBalance $ fmap $ \t -> do
                let (_w, p, tz) = tez' t
                text p
                elClass "span" "tez" $ text tz

            let unstakedFrozenBalanceTooltip = divClass "detail" $ text "The unstaked balance which is still frozen."
            el "tr" $ do
              el "td" $ tooltipped TooltipPos_TopLeft unstakedFrozenBalanceTooltip (text "Unstaked Frozen Balance")
              elClass "td" "baker-balance-whole monospaced-text" $ withPlaceholder $ ffor dmUnstakedFrozenBalance $ fmap $ \t -> do
                let (w, _p, _tz) = tez' t
                text w
              elClass "td" "baker-balance-part monospaced-text" $ withPlaceholder' "" $ ffor dmUnstakedFrozenBalance $ fmap $ \t -> do
                let (_w, p, tz) = tez' t
                text p
                elClass "span" "tez" $ text tz

          let stakingBalanceTooltip = divClass "detail" $ text "The total balance including delegated funds, balance of delegate itself and frozen deposits."
          el "tr" $ do
            el "td" $ tooltipped TooltipPos_TopLeft stakingBalanceTooltip (text "Staking Balance")
            elClass "td" "baker-balance-whole monospaced-text" $ withPlaceholder $ ffor dmDelegateInfo $ fmap $ \t -> do
              let (w, _p, _tz) = tez' $ _cacheDelegateInfo_stakingBalance t
              text w
            elClass "td" "baker-balance-part monospaced-text" $ withPlaceholder' "" $ ffor dmDelegateInfo $ fmap $ \t -> do
              let (_w, p, tz) = tez' $ _cacheDelegateInfo_stakingBalance t
              text p
              elClass "span" "tez" $ text tz

        let
          dmParticipationInfo = preview (_Just . bakerDetails_participationInfo . _Just . to unJson) <$> details'

        dyn_ $ ffor dmParticipationInfo $ \case
          Nothing -> blank
          Just participationInfo  -> do
            divClass "divider" blank

            let
              infoTableRow rtitle label method = el "tr" $ do
                el "td" (text rtitle)
                elClass "td" (label <> " monospaced-text") (text . tshow . method $ participationInfo)

            elClass "table" "participation-info" $ do

              infoTableRow "Expected cycle activity" "expected-cycle-activity"
                _participationInfo_expectedCycleActivity

              infoTableRow "Minimal cycle activity" "minimal-cycle-activity"
                _participationInfo_minimalCycleActivity

              infoTableRow "Missed slots" "missed-slots" _participationInfo_missedSlots

              infoTableRow "Missed levels" "missed-levels" _participationInfo_missedLevels

              infoTableRow "Remaining allowed missed slots" "remaining-allowed-missed-slots"
                _participationInfo_remainingAllowedMissedSlots

              el "tr" $ do
                el "td" (text "Expected attestation rewards")
                elClass "td" "expected-endorsing-rewards monospaced-text" . withPlaceholder . ffor dmParticipationInfo .
                  fmap $ \t -> do
                    let (w, p, tz) = tez' $ _participationInfo_expectedEndorsingRewards t
                    text $ w <> p
                    elClass "span" "tez" $ text tz

        dyn_ $ ffor isGatheringData $ \case
          False -> blank
          True -> divClass "ui active inline loader mini blue" blank
              *> text "Gathering baker data."

renderResolvableSplashAlert :: (MonadAppWidget js t m)
  => NonEmpty (DSum LogTag (Const (Id ErrorLog)))
  -> m () -- ^ Alert icon
  -> m () -- ^ Title
  -> Maybe (m ()) -- ^ Entity
  -> m () -- ^ Description body
  -> m ()
renderResolvableSplashAlert es splashIcon title entity desc = do
  renderSplashAlert splashIcon title entity $ do
    desc
    when (isUserResolvable $ NEL.head es) $ do
      resolve <- divClass "buttons" $ uiButtonM "primary" $ do
        icon "icon-check"
        text "Resolve"
      void $ requestingIdentity $ public (PublicRequest_ResolveAlerts $ toList es) <$ resolve

renderSplashAlert :: (MonadAppWidget js t m)
  => m () -- ^ Alert icon
  -> m () -- ^ Title
  -> Maybe (m ()) -- ^ Entity
  -> m () -- ^ Description body
  -> m ()
renderSplashAlert splashIcon title entity desc = do
  elClass "div" "dashboard-section-overview-icon" splashIcon
  elClass "div" "dashboard-section-overview-body" $ do
    divClass "ui header" title
    for_ entity $ divClass "alert-entity"
    divClass "description" desc

withPlaceholder :: (DomBuilder t m, PostBuild t m) => Dynamic t (Maybe (m ())) -> m ()
withPlaceholder = withPlaceholder' "-"

withPlaceholder' :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t (Maybe (m ())) -> m ()
withPlaceholder' placeholder f' = dyn_ $ ffor f' $ \case
  Nothing -> text placeholder
  Just f -> f

withMaybeDyn :: (Eq b, MonadFix m, MonadHold t m, Reflex t) => Dynamic t (Maybe (Dynamic t a)) -> (Dynamic t b -> m ()) -> (a -> b) -> Dynamic t (Maybe (m ()))
withMaybeDyn d mkWidget f = (fmap.fmap) (mkWidget <=< holdUniqDyn . fmap f) d

removeItemModal :: MonadAppWidget js t m
                => Text
                -> (Event t () -> Event t (PublicRequest ()))
                -> Event t ()
                -> m (Event t ())
removeItemModal name = reminderModal
  ("Remove this " <> name <> "?")
  ("You can always add this " <> name <> " again from the \"Add " <> pluralOf (T.toTitle name) <> "\" button.")
  ("Remove " <> T.toTitle name)

tileMenu :: (DomBuilder t m, TriggerEvent t m, MonadIO (Performable m), PerformEvent t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => m b -> m ()
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

{-# ANN withAmendmentPeriodProgress ("HLint: ignore Redundant fmap" :: String) #-}
withAmendmentPeriodProgress :: (HasTimer t r, MonadReader r m, MonadAppWidget js t m)
                     => RawLevel -> (Dynamic t Time.NominalDiffTime -> m ()) -> m ()
withAmendmentPeriodProgress expectedVotingPeriod w = do
  currentTime <- asks (^. timer)
  mProtoInfo <- maybeDyn =<< watchLatestProtoInfo
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

-- | Only show the given widget if adaptive issuance is activated.
whenAIActivated
  :: MonadAppWidget js t m
  => Dynamic t (Maybe BranchInfo)
  -> Dynamic t (Maybe Cycle)
  -> m ()
  -> m ()
whenAIActivated mbLatestHeadDyn mbAiCycleDyn contents =
  dyn_ $ ffor2 mbLatestHeadDyn mbAiCycleDyn $ \mbHeadBlock mbAiCycle ->
    whenJust mbHeadBlock $ \headBlock -> whenJust mbAiCycle $ \aiCycle ->
      when (_branchInfo_cycle headBlock >= aiCycle) contents
