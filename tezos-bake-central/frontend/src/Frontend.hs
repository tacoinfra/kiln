{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend where

import Control.Applicative (Const (..), liftA2, (<|>))
import Control.Lens ((%~), (.~), _1, _2, _3)
import Control.Monad (join, when, (<=<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Either (isRight)
import Data.Either.Combinators (rightToMaybe)
import Data.Fixed (Micro)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor (void)
import Data.List (intersperse, sortBy)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (..), comparing)
import Data.Semigroup (First (..), (<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Traversable (for)
import Data.Version (Version, showVersion)
import qualified Form.Checks as Check
import qualified GHCJS.DOM as DOM
import GHCJS.DOM.Element (setInnerHTML)
import qualified GHCJS.DOM.Location as Location
import GHCJS.DOM.Types (MonadJSM)
import qualified GHCJS.DOM.Window as Window
import qualified Obelisk.ExecutableConfig
import Prelude hiding (log)
import Reflex.Dom.Core
import Reflex.Dom.Form.FieldWriter (tellFieldErr, withFormFieldsErr)
import qualified Reflex.Dom.Form.Validators as Validator
import Reflex.Dom.Form.Widgets (formItem, formItem', validatedInput)
import qualified Reflex.Dom.SemanticUI as SemUi
import qualified Reflex.Dom.TextField as Txt
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (MonadRhyoliteFrontendWidget, runRhyoliteWidget, watchViewSelector)
import Rhyolite.Schema (Email, Id, Json (..))
import Rhyolite.WebSocket (WebSocketUrl (..))
import Safe (maximumMay)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.NodeRPC.Sources (PublicNode (..), tzScanUri)
import Tezos.NodeRPC.Types
import Tezos.Types

import Common (maybeSomething, tshow, uriHostPortPath)
import Common.Alerts (badNodeHeadMessage)
import Common.Api
import Common.App
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import qualified Common.Config as Config
import Common.HeadTag (headTag)
import Common.Route
import Common.Schema hiding (Event)
import Common.Vassal
import Frontend.Common
import Obelisk.Frontend
import Obelisk.Route

frontend :: Frontend (R AppRoute)
frontend = Frontend
  { _frontend_head = headTag
  , _frontend_body = prerender (return ()) frontendBody
  , _frontend_notFoundRoute = const $ AppRoute_Index :/ ()
  }

frontendBody ::
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

  checkForUpgrade <-
    fmap (maybe False Config.parseBool) $
      liftIO $ getExecutableConfig $ T.pack Config.checkForUpgrade

  chain :: Either NamedChain ChainId <- ffor (liftIO $ getExecutableConfig $ T.pack Config.chain) $ \r ->
    maybe (error "No network name or ID provided") (parseChainOrError . T.strip) r

  let
    routeScheme = T.toLower . Uri.unRText <$> Uri.uriScheme route
    renderPathPieces pieces = T.intercalate "/" (map Uri.unRText $ toList pieces)
    routeAuthority = rightToMaybe $ Uri.uriAuthority route
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

    appCfg = Cfg
      { _cfg_checkForUpgrade = checkForUpgrade
      , _cfg_chain = chain
      }

  runRhyoliteWidget (Left $ fromMaybe (error "Invalid WS URL") wsUrl) $
    flip runReaderT appCfg appMain

validatingRange :: (View (RangeSelector e v) a -> b) -> (View (RangeSelector e v) a -> Maybe b)
validatingRange f v =
  if null $ _rangeView_support v
    then Nothing
    else Just $ f v

watchProtoInfo :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe ProtoInfo))
watchProtoInfo =
  (fmap . fmap) (getMaybeView . _bakeView_parameters) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_parameters = viewJust 1
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

watchNodeAddresses :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap (Id Node) (URI, Maybe Text)))
watchNodeAddresses = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodeAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> fmapMaybe getFirst $ getRangeView' (_bakeView_nodeAddresses v')

watchNodeAddressesValid :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (MonoidalMap (Id Node) (URI, Maybe Text))))
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

watchUpgradeNotice
  :: MonadRhyoliteFrontendWidget Bake t m
  => m (Dynamic t (Maybe (ErrorLog, Either UpgradeCheckError Version)))
watchUpgradeNotice =
  (fmap . fmap) (getMaybeView . _bakeView_upgrade) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_upgrade = viewJust 1 }



-- NB: The order of these constructors determines the order of the tabs in the UI.
data UITab = UITab_Summary
           | UITab_Nodes
           | UITab_Delegate PublicKeyHash
           | UITab_Client (Id Client) URI
           | UITab_Options
  deriving (Eq, Ord, Show)

appMain
  :: forall t m.
    ( MonadRhyoliteFrontendWidget Bake t m
    , MonadJSM (Performable m)
    , MonadJSM m
    , MonadReader Cfg m
    )
  => m ()
appMain = elAttr "div" ("style" =: "width: 80%; margin-left: auto; margin-right: auto;") $ do
  clientAddresses <- watchClientAddresses
  delegates <- watchDelegatePublicKeyHashes
  el "h1" $ text "Node Monitor"
  publicNodesMaybe <- watchPublicNodeConfigValid
  nodesMaybe <- watchNodeAddressesValid
  -- doing some straightforward calculations, but inside a Dynamic and a Maybe
  let nodesTabEnabledMaybe = (fmap.fmap) (\x -> if x then Enabled else Disabled) $
        (liftA2 . liftA2) ((||) . any _publicNodeConfig_enabled . toList) publicNodesMaybe $
        (fmap . fmap) (not . null) nodesMaybe
  initialSelection <- maybeDyn $
        (fmap . fmap) (bool UITab_Options UITab_Nodes . isEnabled) nodesTabEnabledMaybe
  dyn_ $ ffor initialSelection $ \case
    Nothing -> divClass "column" waitingForResponse
    Just aTab -> do
      initialTab <- sample (current aTab) -- only needed for initial value, don't want it to steal your focus
      rec selection <- elAttr "div" ("class" =: "ui top attached tabular menu") $ leftmost <$> sequenceA
            [ semuiTab (text "Nodes") UITab_Nodes currentTabD (fmap (fromMaybe Disabled) nodesTabEnabledMaybe)
            , fmap switch . hold never <=< dyn . ffor clientAddresses $ \cs ->
              fmap leftmost . for (MMap.toList cs) $ \(cid, name) ->
                semuiTab (text $ "B:" <> Uri.render name) (UITab_Client cid name) currentTabD (pure Enabled)
            , fmap switch . hold never <=< dyn . ffor delegates $ \ds ->
              fmap leftmost $ for (Set.toList ds) $ \pkh ->
                semuiTab (text $ "tz:" <> toPublicKeyHashText pkh) (UITab_Delegate pkh) currentTabD (pure Enabled)
            , semuiTab (text "Options") UITab_Options currentTabD (pure Enabled)
            ]
          currentTab <- holdDyn initialTab selection
          let currentTabD = demux currentTab

      divClass "ui bottom attached tab segment active" $
        divClass "ui one column grid" $ do
          upgradeNotice <- holdUniqDyn =<< watchUpgradeNotice
          dyn_ $ ffor upgradeNotice $ \case
            Nothing -> blank
            Just (_log, upgrade) -> elAttr "div" ("class"=:"column"<>"style"=:"padding-bottom:0px;") $
              case upgrade of
                Left _e -> divClass "ui red right ribbon label" $ text "Upgrade check failed"
                Right v -> do
                  let
                    versionText = T.pack (showVersion v)
                    versionAnchor = "anchor-" <> T.filter (/='.') versionText
                  elAttr "a"
                    (  "class"=:"ui green right ribbon label"
                    <> "href"=:(Config.changelogUrl <> "#" <> versionAnchor)
                    <> "target"=:"_blank") $
                      text $ "New version available: " <> versionText

          divClass "column" $
            dyn_ $ ffor currentTab $ \case
              UITab_Summary -> summaryTab
              UITab_Nodes -> nodesTab
              UITab_Options -> optionsTab
              UITab_Client cid addr -> clientTab cid addr
              UITab_Delegate pkh -> delegateTab pkh


whenJustDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t (Maybe a) -> (a -> m ()) -> m ()
whenJustDyn d f = dyn_ . ffor d $ \case
  Nothing -> blank
  Just x -> f x

summaryTab :: forall t m. (MonadRhyoliteFrontendWidget Bake t m, MonadJSM m, MonadReader Cfg m) => m ()
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
          el "td" . blockHashLink $ _bakedEvent_hash $ _event_detail b
          el "td" . dyn . ffor dparameters $ \case
            Nothing -> text "N/A"
            Just protoInfo -> text . tez $ blockRewards b protoInfo

  return ()

radioLabels :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, Eq k) => k -> [(k, m ())] -> m (Dynamic t k)
radioLabels k0 ks = mdo
  selectedDyn <- holdDyn k0 $ leftmost kClicks
  kClicks <- for ks $ \(k, label) -> do
    (element', ()) <- elDynAttr' "a" (ffor selectedDyn $ \selected -> "class"=:("ui " <> (if selected == k then "blue" else "") <> " tiny label link")) label
    pure $ k <$ domEvent Click element'
  pure selectedDyn

data AlertsFilter = AlertsFilter_All | AlertsFilter_UnresolvedOnly | AlertsFilter_ResolvedOnly
  deriving (Eq, Ord, Show, Enum, Bounded)


liveErrorsWidget
  :: forall t m. (MonadRhyoliteFrontendWidget Bake t m, MonadReader Cfg m)
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

  elAttr "div" ("style"=:"padding-top:1em; max-height: 60em; overflow-y: auto;") $
    listWithKey (errorsByTime Down <$> combinedErrors) $ \_ vDyn ->
      dyn_ $ ffor vDyn $ \v@(log, _, _) -> do
        divClass ("ui message " <> if isJust $ _errorLog_stopped log then "success" else "error") $ do
          logEntry v
          el "p" $ do
            text "First seen: " *> localTimestamp (_errorLog_started log) *> text " | "
            case _errorLog_stopped log of
              Nothing -> text "Last seen: " *> localTimestamp (_errorLog_lastSeen log)
              Just stopped -> text "Stopped: " *> localTimestamp stopped

  where
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
      let header txt = divClass "header" $ text $ case _errorLog_stopped log of
            Just _ -> "Resolved: " <> txt
            Nothing -> txt
      in case specificLog of
          ErrorLogView_InaccessibleNode (ErrorLogInaccessibleNode _ _ address alias) -> for_ node' $ const $ do
            header $ "Unable to connect to node" <> maybe "" (" " <>) alias <> " at " <> Uri.render address

          ErrorLogView_NodeWrongChain (ErrorLogNodeWrongChain _ _ address alias expectedChainId actualChainId) ->
            for_ node' $ const $ do
              header $ "Node on wrong network: " <> fromMaybe (Uri.render address) alias
              el "p" $
                text $ "The node is running on network " <> toBase58Text actualChainId <> " but is expected to be on " <> toBase58Text expectedChainId <> "."

          ErrorLogView_BakerNoHeartbeat (ErrorLogBakerNoHeartbeat _ lastLevel lastBlockHash _) -> do
            header "Baker lagging behind" -- TODO Show client address
            el "p" $ do
              text "Last block level seen: "
              blockHashLinkAs lastBlockHash (text $ tshow lastLevel)

          ErrorLogView_BadNodeHead l ->
            for_ node' $ \node -> do
            let (heading, message) = badNodeHeadMessage text blockHashLink l
            header $ heading <> ": " <> fromMaybe (Uri.render $ _node_address node) (_node_alias node)
            el "p" message

          ErrorLogView_MultipleBakersForSameDelegate ErrorLogMultipleBakersForSameDelegate{} -> do
            header "Multiple bakers for same delegate" -- TODO Fill this out

    errorsByTime direction errors = Map.fromList
      [ (direction (_errorLog_started l, _errorLog_lastSeen l, elId), row)
      | (elId, row@(l, _, _)) <- MMap.toList errors
      ]

optionsTab :: (MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m), MonadJSM m, MonadReader Cfg m) => m ()
optionsTab = divClass "ui two column stackable grid" $ do
  enableUpgradeCheck <- asks _cfg_checkForUpgrade

  _ <- divClass "column" $ traverse (divClass "ui basic segment") $
    [ currentChain
    , publicNodeOptions
    , nodesOptions
    ]
    ++ [ delegatesOptions | False ]
    ++ [ clientsOptions | False ]
    ++ [ upgradeOptions | enableUpgradeCheck ]
  divClass "column" $ do
    divClass "ui basic segment" mailServerOptions
    divClass "ui basic segment" notificationOptions
  where
    currentChain = do
      chain <- asks _cfg_chain
      elClass "h3" "ui header" $ do
        text "Network: "
        el "em" $ text $ showChain chain
      el "p" $ el "em" $ do
        text "You can monitor a different network by setting the "
        el "code" $ text $ T.pack Config.chain
        text " configuration. Run the server with "
        el "code" $ text "--help"
        text " for more information."

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

    clientsOptions = void $ do
      divClass "ui medium header" $ text "Clients"
      elClass "table" "ui celled striped compact table" $ do
        clients <- watchClientAddresses -- TODO
        _ <- listWithKey (coerce <$> clients) $ \_ dName -> el "tr" $ do
          el "td" $ dynText $ Uri.render <$> dName
          el "td" $ do
            eRemove <- buttonWithInfo "Remove" "Stop monitoring this client. It will continue running."
            requestingIdentity $ public . PublicRequest_RemoveClient <$> tag (current dName) eRemove

        addE <- urlInputRow validateUri "Add Baker" "Begin monitoring the baker at the address entered." "http://[host][:port]"
        void $ requestingIdentity $ ffor addE $ \(addr,alias) -> public (PublicRequest_AddClient addr alias)

    delegatesOptions = do
      divClass "ui medium header" $ text "Delegates"
      elClass "table" "ui celled striped compact table" $ do
        delegates <- watchDelegatePublicKeyHashes
        _ <- listWithKey (Map.fromSet (const ()) <$> delegates) $ \pkh _ -> el "tr" $ do
          el "td" $ publicKeyHashLink pkh
          el "td" $ do
            eRemove <- buttonWithInfo "Remove" "Stop monitoring this delegate."
            requestingIdentity $ public . PublicRequest_RemoveDelegate <$> tag (pure pkh) eRemove

        addE <- urlInputRow (Validator.Validator (first tshow . tryReadPublicKeyHashText) id) "Add Delegate" "Begin monitoring wallet address entered." "tz..."
        void $ requestingIdentity $ ffor addE $ \(pkh,alias) -> public (PublicRequest_AddDelegate pkh alias)

    publicNodeOptions = do
      divClass "ui medium header" $ text "Public Nodes"

      let
        showPublicNode = \case
          PublicNode_TzScan -> "tzscan.io"
          PublicNode_Blockscale -> "Foundation"
          PublicNode_Obsidian -> "Obsidian"

      pncDyn <- watchPublicNodeConfig
      for_ [minBound..maxBound] $ \pn -> do
        (element', ()) <- elDynAttr' "a" (ffor pncDyn $ \pnc -> "class"=:("ui " <> (if isPublicNodeEnabled pn pnc then "blue" else "") <> " button link")) $
          text $ showPublicNode pn
        let toggled = tag (current $ not . isPublicNodeEnabled pn <$> pncDyn) (domEvent Click element')
        void $ requestingIdentity $ ffor toggled $ \enabled -> public (PublicRequest_SetPublicNodeConfig pn enabled)

    nodesOptions = do
      divClass "ui medium header" $ text "Monitored Nodes"
      elClass "table" "ui celled striped compact table" $ do
        nodes <- watchNodeAddresses
        _ <- listWithKey (coerce <$> nodes) $ \_ node -> el "tr" $ do
          let dAddress = ffor node fst
          let dName = ffor node snd
          el "td" $ dynText $ ffor dAddress Uri.render
          el "td" $ dynText $ ffor dName $ fromMaybe ""
          el "td" $ do
            eRemove <- buttonWithInfo "Remove" "Stop monitoring this node. It will continue running."
            requestingIdentity $ public . PublicRequest_RemoveNode . fst <$> tag (current node) eRemove

        addE <- urlInputRow validateUri "Add Node" "Begin monitoring the node at the address entered." "http://[host][:port]"
        void $ requestingIdentity $ ffor addE $ \(addr,alias) -> public (PublicRequest_AddNode addr alias)

    upgradeOptions = mdo
      isLoading <- holdDyn False $ leftmost [False <$ result, True <$ checkUpgrade]
      checkUpgrade <- fmap (domEvent Click . fst) $ elDynAttr' "div"
        (ffor isLoading $ \loading -> "class"=:("ui large button" <> (if loading then " loading" else "")))
        $ text "Check for New Version"
      result <- requestingIdentity $ public PublicRequest_CheckForUpgrade <$ checkUpgrade
      widgetHold_ blank $ ffor result $ \(currentVersion, checkResult) -> do
        let currentVersionText = "You're currently using version " <> T.pack (showVersion currentVersion)
        case checkResult of
          Nothing -> divClass "ui success message" $
            text $ currentVersionText <> " which is the latest."
          Just (Right newVersion) -> divClass "ui success message" $
            text $ "A new version is available: " <> T.pack (showVersion newVersion) <> ". " <> currentVersionText <> "."
          Just (Left _) -> divClass "ui error message" $
            text $ "We had trouble checking for upgrades. " <> currentVersionText <> "."

    urlInputRow
      :: (MonadRhyoliteFrontendWidget Bake t m, Eq a)
      => Validator.Validator t m a -> Text -> Text -> Text -> m (Event t (a,Maybe Text))
    urlInputRow validator label info placeholder = el "tr" $ do
      (tdEl1, address) <- el' "td" $ formItem
        $ validatedInput validator
        $ def & Txt.setPlaceholder placeholder & Txt.setFluid
      (tdEl2, alias) <- el' "td" $ formItem
        $ validatedInput (Validator.optional Validator.validateText)
        $ def & Txt.setPlaceholder "alias" & Txt.setFluid
      addButton <- elClass "td" "right aligned collapsing" $ buttonWithInfo label info
      let namedAddress = liftA2 (liftA2 (,)) address alias
      return $ filterRight $ tag (current namedAddress) $ leftmost [addButton, keypress Enter tdEl1, keypress Enter tdEl2]


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
        tellFieldErr (_1 . mailServerView_hostName) <=< formItem' "eight wide"
          $ validatedInput Validator.validateText
          $ defTxt "Host" & Txt.setInitial (_mailServerView_hostName frm0)

        tellFieldErr (_1 . mailServerView_portNumber) <=< formItem' "four wide"
          $ validatedInput (Validator.validateNumeric "port" (Just 0, Just 65535) (Just 1))
          $ defTxt "Port" & Txt.setInitial (tshow $ _mailServerView_portNumber frm0)

        tellFieldErr (_1 . mailServerView_smtpProtocol) <=< formItem' "four wide"
          $ fmap (fmap (maybe (Left "Please select a protocol") Right) . SemUi._dropdown_value)
          $ do
            labeled "Protocol"
            SemUi.dropdown (def & SemUi.dropdownConfig_placeholder .~ "Protocol"
                                & SemUi.dropdownConfig_fluid SemUi.|~ True)
              (Just $ _mailServerView_smtpProtocol frm0)
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


data NodeTile
  = NodeTile_PlainNode (Id Node) Node
  | NodeTile_PublicNode PublicNodeHead
  deriving (Eq, Ord, Show)

nodeIdForErrorLogView :: ErrorLogView -> Maybe (Id Node)
nodeIdForErrorLogView = \case
  ErrorLogView_InaccessibleNode l -> Just $ _errorLogInaccessibleNode_node l
  ErrorLogView_NodeWrongChain l -> Just $ _errorLogNodeWrongChain_node l
  ErrorLogView_BadNodeHead l -> Just $ _errorLogBadNodeHead_node l
  _ -> Nothing

nodesTab :: forall t m. (MonadRhyoliteFrontendWidget Bake t m, MonadReader Cfg m) => m ()
nodesTab = divClass "ui stackable grid" $ do
  let alertWindow = ClosedInterval LowerInfinity UpperInfinity
  alertsDyn <- maybeDynLazy . fmap maybeSomething =<< watchErrors (pure $ Set.singleton alertWindow)

  nodesDyn <- watchNodes $ pure $ viewRangeAll ()

  dyn_ $ ffor alertsDyn $ \case
    Nothing -> divClass "column" $ nodeTilesWidget (constDyn MMap.empty) nodesDyn
    Just nonEmptyAlertsDyn -> do
      divClass "ten wide column" $ nodeTilesWidget nonEmptyAlertsDyn nodesDyn
      divClass "six wide column" $ do
        elClass "h3" "ui header" $ text "Alerts"
        liveErrorsWidget nonEmptyAlertsDyn nodesDyn

  where
    nodeTilesWidget alerts nodesDyn = do
      publicNodeConfigDyn <- watchPublicNodeConfig
      rawPublicNodesDyn <- watchPublicNodeHeads
      let
        publicNodesDyn = zipDynWith (\pnc ->
          MMap.filter (flip isPublicNodeEnabled pnc . _publicNodeHead_source)
          ) publicNodeConfigDyn rawPublicNodesDyn

      useBlocker <- holdUniqDyn $ ffor (zipDyn publicNodesDyn nodesDyn) $ \(pn,n) -> MMap.null pn && MMap.null n

      maxLevelOnPublicNodes <- holdUniqDyn $
        maximumMay . map _publicNodeHead_headLevel . toList <$> publicNodesDyn

      dyn_ $ ffor useBlocker $ \case
        True -> waitingForResponse
        False -> divClass "ui stackable cards" $ do
          void $ listWithKey (MMap.getMonoidalMap <$> publicNodesDyn) $ \_ vDyn -> do
            vDyn' <- holdUniqDyn vDyn
            divClass "ui card" $ divClass "content" $ dyn_ $ ffor vDyn' $ \node -> do
              let chain = getNamedChainOrChainId $ _publicNodeHead_chain node
              let nodeTitle = case _publicNodeHead_source node of
                    PublicNode_TzScan -> either (urlLink . tzScanUri) (flip const) chain $ text $ "tzscan (" <> showChain chain <> ")"
                    PublicNode_Blockscale -> text $ "Foundation Nodes (" <> showChain chain <> ")"
                    PublicNode_Obsidian -> text $ "Obsidian Systems (" <> showChain chain <> ")"
              headBlockLevelHeader
                nodeTitle
                (Just (_publicNodeHead_headBlockHash node, _publicNodeHead_headLevel node))
                (pure Nothing)
              divClass "description" $
                nodeDataTable
                  [ (text "Block Hash:", blockHashLink $ _publicNodeHead_headBlockHash node)
                  , (text "Block Fitness:", text $ fitnessText $ _publicNodeHead_headBlockFitness node)
                  , (text "Block Baked:", localTimestamp $ _publicNodeHead_headBlockBakedAt node)
                  ]

          void $ listWithKey (MMap.getMonoidalMap <$> nodesDyn) $ \nodeId vDyn -> do
            vDyn'' <- holdUniqDyn vDyn
            vDyn'start <- sample $ current vDyn''
            vDyn'update <- throttle 0.2 (updated vDyn'')
            vDyn' <- holdDyn vDyn'start vDyn'update
            divClass "ui card" $ divClass "content" $ dyn_ $ ffor vDyn' $ \node -> do
              fallingBehindBy <- case _node_headLevel node of
                Nothing -> pure (pure Nothing)
                Just nodeLevel -> do
                  let calcBehindBy maxLevel = if behindBy >= 5 then Just behindBy else Nothing
                        where behindBy = maxLevel - nodeLevel

                  holdUniqDyn $ (calcBehindBy =<<) <$> maxLevelOnPublicNodes

              headBlockLevelHeader
                (text $ fromMaybe (uriHostPortPath $ _node_address node) $ _node_alias node)
                (liftA2 (,) (_node_headBlockHash node) (_node_headLevel node))
                fallingBehindBy

              divClass "description" $ do
                let stat = _node_networkStat node
                nodeDataTable
                  [ (text "Block Hash:", maybe (text "N/A") blockHashLink $ _node_headBlockHash node)
                  , (text "Block Fitness:", text $ maybe "N/A" fitnessText $ _node_fitness node)
                  , (text "Block Baked:", maybe (text "N/A") localTimestamp $ _node_headBlockBakedAt node)
                  , (text "Peer Count:", text $ maybe "N/A" tshow $ _node_peerCount node)
                  , (text "Total Sent:", text $ tshow (unTezosWord64 $ _networkStat_totalSent stat) <> " bytes")
                  , (text "Total Received:", text $ tshow (unTezosWord64 $ _networkStat_totalRecv stat) <> " bytes")
                  , (text "Inflow:", text $ tshow (_networkStat_currentInflow stat) <> " bytes/sec")
                  , (text "Outflow:", text $ tshow (_networkStat_currentOutflow stat) <> " bytes/sec")
                  ]

                unresolvedAlertsForThisNode <-
                  holdUniqDyn $ foldMap toList . MMap.lookup nodeId . errorsByNode <$> alerts
                let errorMessage = divClass "ui error message" . divClass "header"
                dyn_ $ ffor unresolvedAlertsForThisNode $ traverse_ $ \case
                  ErrorLogView_InaccessibleNode{} ->  errorMessage $ text "Unable to connect."
                  ErrorLogView_NodeWrongChain{} -> errorMessage $ text "On wrong network."
                  ErrorLogView_BadNodeHead l -> errorMessage $ text $
                    fst (badNodeHeadMessage Const (Const . const "") l) <> "."
                  _ -> blank

    headBlockLevelHeader :: m () -> Maybe (BlockHash, RawLevel) -> Dynamic t (Maybe RawLevel) -> m ()
    headBlockLevelHeader title blockHashAndLevel blocksBehindDyn =
      elClass "h3" "ui center aligned header" $ do
        title
        elAttr "div" ("class"=:"sub header"<>"style"=:"padding-top:1em") $ do
          case blockHashAndLevel of
            Nothing -> text "Connecting..."
            Just (blockHash, lvl) -> dyn_ $ ffor blocksBehindDyn $ \blocksBehind -> do
              let styled = if isJust blocksBehind then errorStyle else id
              styled $ blockHashLinkAs blockHash $ text $ tshow $ unRawLevel lvl
          divClass "sub header" $ do
            text "Head Block Level"
            dyn_ $ ffor blocksBehindDyn $ traverse_ $ \numBehind ->
              elAttr "div" ("style"=:"padding-top:0.4em") $ errorStyle $
                text $ tshow (unRawLevel numBehind) <> " Blocks Behind"
      where
        errorStyle = elClass "span" "block-level-error"

    nodeDataTable rows = elAttr "table" ("class"=:"ui very basic compact stackable table") $
      for_ rows $ \(heading, val) -> el "tr" $ do
        _ <- elAttr "th" ("style"=:"text-align:left") heading
        elAttr "td" ("style"=:"text-align:left") val

    errorsByNode
      :: MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView)
      -> MonoidalMap (Id Node) (NonEmpty ErrorLogView)
    errorsByNode xs = MMap.fromListWith (<>)
      [ (k, pure t)
      | (ErrorLog{_errorLog_stopped = Nothing}, t) <- MMap.elems xs
      , Just k <- [nodeIdForErrorLogView t]
      ]


delegateTab
  :: (MonadRhyoliteFrontendWidget Bake t m, MonadReader Cfg m)
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

clientTab :: (MonadRhyoliteFrontendWidget Bake t m, MonadReader Cfg m) => Id Client -> URI -> m ()
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
            el "td" $ blockHashLink $ _bakedEvent_hash $ _event_detail b
            el "td" $ dyn_ $ ffor dparameters $ traverse $ \protoInfo ->
              text $ tez $ blockRewards b protoInfo

waitingForResponse :: DomBuilder t m => m ()
waitingForResponse = divClass "ui basic segment" $ divClass "ui active centered inline text loader" $ text "Waiting for response"

data Enabled = Disabled | Enabled
  deriving (Eq, Ord, Show, Read, Enum)

isDisabled :: Enabled -> Bool
isDisabled Disabled = True
isDisabled Enabled = False

isEnabled :: Enabled -> Bool
isEnabled Enabled = True
isEnabled Disabled = False

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
          & inputElementConfig_setValue .~ ("" <$ fmapMaybe rightToMaybe rsp)
          & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ validationAttrs
        addItemClick <- fmap (domEvent Click . fst) $ elClass' "span" "add-button" $ elClass "i" "fa fa-plus-circle fa-fw" blank
        return (itemInput, addItemClick)
      let v = value i
          validationResults = leftmost
            [ (\v' -> if T.null v' then Left () else Right (validate v')) <$> updated v
            , Right . isJust . rightToMaybe <$> rsp
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
