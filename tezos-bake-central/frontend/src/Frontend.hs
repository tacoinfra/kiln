{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Frontend where

import Control.Applicative (liftA2)
import Control.Lens ((<&>), _1, _2)
import Control.Monad (when, (<=<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Data.AppendMap (AppendMap, _unAppendMap)
import qualified Data.AppendMap as Map
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Data.Either.Combinators (rightToMaybe)
import Data.Fixed (Micro)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor (void)
import Data.List (intersperse, sortBy)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as BaseMap
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (comparing)
import Data.Semigroup (First (..), (<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Traversable (for)
import Data.Version (Version, showVersion)
import qualified Form.Checks as Check
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Types (MonadJSM)
import qualified Obelisk.ExecutableConfig
import Reflex.Dom.Core
import Reflex.Dom.Form.FieldWriter (tellFieldErr, withFormFieldsErr)
import qualified Reflex.Dom.Form.Validators as Validator
import Reflex.Dom.Form.Widgets (formItem, formItem', validatedInput)
import qualified Reflex.Dom.SemanticUI as SemUi
import qualified Reflex.Dom.TextField as Txt
import Rhyolite.Api (public)
import Rhyolite.App (getSingle)
import Rhyolite.Frontend.App (MonadRhyoliteFrontendWidget, runRhyoliteWidget, watchViewSelector)
import Rhyolite.Request.Common (decodeValue')
import Rhyolite.Route (RouteEnv)
import Rhyolite.Schema (Email, Id, Json (..))
import Rhyolite.WebSocket (websocketUrlFromRouteEnv)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.NodeRPC.Types
import Tezos.Types

import Common (maybeSomething, tshow, uriHostPortPath)
import Common.Api
import Common.App
import Common.AppendIntervalMap (AppendIntervalMap, ClosedInterval (..), WithInfinity (..))
import qualified Common.AppendIntervalMap as AppendIMap
import qualified Common.Config as Config
import Common.Schema hiding (Event)
import Common.URI (mkRootUri)
import Frontend.Common

frontend :: (StaticWidget x (), Widget x ())
frontend = (headTag,) $ void $ do
  let decodeViaJson = decodeValue' . LBS.fromStrict . T.encodeUtf8
  route :: RouteEnv <- liftIO (Obelisk.ExecutableConfig.get $ T.pack Config.route) >>= \case
    Just r -> return $ fromMaybe (error "Unable to parse injected route") (decodeViaJson r)
    Nothing -> do
      protocol <- getLocationProtocol
      hostWithPort <- getLocationHost
      return $ let (host, port) = T.breakOn ":" hostWithPort
                in (T.unpack protocol, T.unpack host, T.unpack port)

  blockExplorerUrl <- ffor (liftIO $ Obelisk.ExecutableConfig.get $ T.pack Config.blockExplorer) $ fmap $ \url ->
    case mkRootUri url of
      Left e -> error $ T.unpack $ "Error parsing injected block explorer URL " <> url <> ": " <> e
      Right rootUrl -> rootUrl

  checkForUpgrade <-
    fmap (Config.parseBool . fromMaybe (error $ "Missing " <> Config.checkForUpgrade <> " configuration")) $
      liftIO $ Obelisk.ExecutableConfig.get $ T.pack Config.checkForUpgrade

  chainId :: ChainId <- ffor (liftIO $ Obelisk.ExecutableConfig.get $ T.pack Config.chain) $ \r ->
    maybe (error "No chain ID given") (fromString . T.unpack . T.strip) r

  runRhyoliteWidget (Left $ websocketUrlFromRouteEnv route) $ runReaderT appMain Cfg
    { _cfg_blockExplorerUrl = blockExplorerUrl
    , _cfg_checkForUpgrade = checkForUpgrade
    , _cfg_chainId = chainId
    }

watchProtoInfo :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe ProtoInfo))
watchProtoInfo =
  (fmap . fmap) (getSingle . _bakeView_parameters) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_parameters = Just 1
    }

watchNodes :: (MonadRhyoliteFrontendWidget Bake t m) => Dynamic t (UniversalMap (Id Node) ()) -> m (Dynamic t (AppendMap (Id Node) Node))
watchNodes nidsDyn = do
  theView <- watchViewSelector $ ffor nidsDyn $ \nids -> mempty
    { _bakeViewSelector_nodes = fmap (const 1) nids
    }
  return $ ffor theView $ \v -> Map.mapMaybe (\(First n,_) -> n) (_bakeView_nodes v)

watchNodeAddresses :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Node) URI))
watchNodeAddresses = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodeAddresses = Just 1
    }
  return $ ffor theView $ \v' -> flip Map.mapMaybeWithKey (_bakeView_nodeAddresses v') $ \_ (First r, _) -> r


watchClient :: (MonadRhyoliteFrontendWidget Bake t m) => Dynamic t (Id Client) -> m (Dynamic t (AppendMap (Id Client) ClientInfo))
watchClient cidDyn = do
  theView <- watchViewSelector . ffor cidDyn $ \cid -> mempty
    { _bakeViewSelector_clients = Map.singleton cid 1
    }
  return $ ffor theView $ \v -> Map.mapMaybe (\(First n,_) -> n) (_bakeView_clients v)

watchDelegatePublicKeyHashes :: (MonadRhyoliteFrontendWidget Bake t m) => m (Dynamic t (Set PublicKeyHash))
watchDelegatePublicKeyHashes = do
  (fmap.fmap) (fromMaybe mempty . getSingle . _bakeView_delegates) $ watchViewSelector $ pure $ mempty {_bakeViewSelector_delegates = Just 1}

watchDelegateStats :: (MonadRhyoliteFrontendWidget Bake t m) => Dynamic t (Set PublicKeyHash) -> m (Dynamic t (AppendMap PublicKeyHash (BakeEfficiency, Account)))
watchDelegateStats delegates = do
  let levels :: RawLevel = 30
  theView <- watchViewSelector $ ffor delegates $ \ds -> mempty
    { _bakeViewSelector_delegateStats = Map.mapKeys (,levels) $ Map.fromSet (const 1) ds
    }
  return $ ffor theView $
      Map.mapKeys fst
    . Map.mapMaybeWithKey (\(pkh, lvl) x -> if lvl == levels then Just x else Nothing)
    . Map.mapMaybe (\(First r, _) -> r)
    . _bakeView_delegateStats

watchClientAddresses :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Client) URI))
watchClientAddresses = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_clientAddresses = Just 1
    }
  return $ ffor theView $ \v' -> flip Map.mapMaybeWithKey (_bakeView_clientAddresses v') $ \_ (First r, _) -> r

watchNotificatees :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Notificatee) Email))
watchNotificatees = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_notificatees = Just 1
    }
  return $ ffor theView $ \v -> fmapMaybe (getFirst . fst) (_bakeView_notificatees v)

watchSummary :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (Report, Int)))
watchSummary = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_summary = Just 1
    }
  improvingMaybe $ ffor theView $ \v -> getSingle $ _bakeView_summary v

watchSummaryGraph :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (Micro, Text)))
watchSummaryGraph = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_summary = Just 1
    }
  improvingMaybe $ ffor theView $ \v -> getSingle $ _bakeView_summaryGraph v

watchMailServer :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe MailServerView))
watchMailServer =
  (fmap . fmap) (getSingle . _bakeView_mailServer) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_mailServer = Just 1 }

watchErrors
  :: MonadRhyoliteFrontendWidget Bake t m
  => Dynamic t (Set (ClosedInterval (WithInfinity UTCTime)))
  -> m (Dynamic t (AppendIntervalMap (ClosedInterval (WithInfinity UTCTime)) (AppendMap (Id ErrorLog) (Maybe (ErrorLog, ErrorLogView)))))
watchErrors intervals = do
  theView <- watchViewSelector $ ffor intervals $ \ivals -> mempty
    { _bakeViewSelector_errors = AppendIMap.fromSet (const 1) ivals
    }
  pure $ ffor theView $ \v ->
    ffor (_bakeView_errors v) $ \(idsSet, _) -> getFirst <$> restrictKeys (_bakeView_errorsById v) idsSet

watchTzScan :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe TzScan))
watchTzScan =
  (fmap . fmap) (getSingle . _bakeView_tzscan) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_tzscan = Just 1 }

watchUpgradeNotice
  :: MonadRhyoliteFrontendWidget Bake t m
  => m (Dynamic t (Maybe (ErrorLog, Either UpgradeCheckError Version)))
watchUpgradeNotice =
  (fmap . fmap) (getSingle . _bakeView_upgrade) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_upgrade = Just 1 }

headTag :: DomBuilder t m => m ()
headTag = do
  traverse_ (\s -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank)
    [ "css/font-awesome.min.css"
    , "semantic-ui/semantic.min.css"
    , "css/main.css"
    ]
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0, maximum-scale=1.0") blank
  elAttr "meta" ("charset" =: "utf-8") blank


-- NB: The order of these constructors determines the order of the tabs in the UI.
data UITab = UITab_Summary
           | UITab_Nodes
           | UITab_Delegate PublicKeyHash
           | UITab_Client (Id Client) URI
           | UITab_Options
  deriving (Eq, Ord, Show)


appMain :: forall t m. (MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m), MonadJSM m, MonadReader Cfg m) => m ()
appMain = elAttr "div" ("style" =: "width: 80%; margin-left: auto; margin-right: auto;") $ do
  clientAddresses <- watchClientAddresses
  delegates <- watchDelegatePublicKeyHashes
  el "h1" $ text "Baker Central"
  rec selection <- elAttr "div" ("class" =: "ui top attached tabular menu") $ fmap leftmost $ sequenceA
        [ semuiTab (text "Nodes") UITab_Nodes currentTab
        , fmap switch . hold never <=< dyn . ffor clientAddresses $ \cs ->
          fmap leftmost . for (Map.toList cs) $ \(cid, name) ->
            semuiTab (text $ "B:" <> Uri.render name) (UITab_Client cid name) currentTab
        , fmap switch . hold never <=< dyn . ffor delegates $ \ds ->
          fmap leftmost $ for (Set.toList ds) $ \pkh ->
            semuiTab (text $ "tz:" <> toPublicKeyHashText pkh) (UITab_Delegate pkh) currentTab
        , semuiTab (text "Options") UITab_Options currentTab
        ]
      currentTab <- fmap demux (holdDyn UITab_Nodes selection)

  divClass "ui bottom attached tab segment active" $ do
    divClass "ui one column grid" $ do
      upgradeNotice <- holdUniqDyn =<< watchUpgradeNotice
      dyn_ $ ffor upgradeNotice $ \case
        Nothing -> blank
        Just (log, upgrade) -> elAttr "div" ("class"=:"column"<>"style"=:"padding-bottom:0px;") $
          case upgrade of
            Left e -> divClass "ui red right ribbon label" $ text "Upgrade check failed"
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
        widgetHold_ nodesTab $ ffor selection $ \case
          UITab_Summary -> summaryTab
          UITab_Nodes -> nodesTab
          UITab_Options -> optionsTab
          UITab_Client cid addr -> clientTab cid addr
          UITab_Delegate pkh -> delegateTab pkh


whenJustDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t (Maybe a) -> (a -> m ()) -> m ()
whenJustDyn d f = dyn_ . ffor d $ \case
  Nothing -> blank
  Just x -> f x

summaryTab :: forall t m. (MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m), MonadJSM m, MonadReader Cfg m) => m ()
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
    dyn . ffor mGraph $ \case
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

liveErrorsWidget
  :: forall t m. (MonadRhyoliteFrontendWidget Bake t m, MonadReader Cfg m)
  => Dynamic t (AppendIntervalMap (ClosedInterval (WithInfinity UTCTime)) (AppendMap (Id ErrorLog) (Maybe (ErrorLog, ErrorLogView))))
  -> m ()
liveErrorsWidget errors = do
  dyn_ $ ffor errors $ traverse_ $ traverse_ $ traverse_ $ \(log, specificLog) -> do
    let header txt = divClass "header" $ text $ case _errorLog_stopped log of
          Just _ -> "Resolved: " <> txt
          Nothing -> txt
    divClass ("ui message " <> if isJust $ _errorLog_stopped log then "success" else "error") $ do
      case specificLog of
        ErrorLogView_InaccessibleEndpoint (ErrorLogInaccessibleEndpoint _ endpointType address) -> do
          let endpointTypeName = case endpointType of
                EndpointType_Node -> "node"
                EndpointType_Client -> "client"
          header $ "Unable to connect to " <> endpointTypeName <> " at " <> Uri.render address

        ErrorLogView_BakerNoHeartbeat (ErrorLogBakerNoHeartbeat _ lastLevel lastBlockHash clientId) -> do
          header "Baker lagging behind" -- TODO Show client address
          el "p" $ do
            text "Last block level seen: "
            blockHashLinkAs lastBlockHash (text $ tshow lastLevel)

        ErrorLogView_NodeOnFork ErrorLogNodeOnFork{} ->
          header "Node is on fork" -- TODO Fill this out

        ErrorLogView_MultipleBakersForSameDelegate ErrorLogMultipleBakersForSameDelegate{} ->
          header "Multiple bakers for same delegate" -- TODO Fill this out

      el "p" $ do
        text $ "First seen: " <> tshow (_errorLog_started log) <> " | "
        case _errorLog_stopped log of
          Nothing -> text $ "Last seen: " <> tshow (_errorLog_lastSeen log)
          Just stopped -> text $ "Stopped: " <> tshow stopped


optionsTab :: (MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m), MonadJSM m, MonadReader Cfg m) => m ()
optionsTab = divClass "ui two column grid" $ do
  enableUpgradeCheck <- asks _cfg_checkForUpgrade

  divClass "column" $ traverse (divClass "ui basic segment") $
    [ currentChain
    , delegatesOptions
    , nodesOptions
    ]
    ++ [ clientsOptions | False ]
    ++ [ upgradeOptions | enableUpgradeCheck ]
  divClass "column" $ do
    divClass "ui basic segment" mailServerOptions
    divClass "ui basic segment" notificationOptions
  where
    currentChain = do
      chainId <- asks _cfg_chainId
      elClass "h3" "ui header" $ do
        text $ "Network: " <> toBase58Text chainId
        when (chainId == betanetChain) $ text " (betanet)"
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
          requestingIdentity . ffor removeN $ \(_, email) -> public (PublicRequest_RemoveNotificatee email)

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
        listWithKey (Map._unAppendMap <$> clients) $ \_ dName -> el "tr" $ do
          el "td" $ dynText $ Uri.render <$> dName
          el "td" $ do
            eRemove <- buttonWithInfo "Remove" "Stop monitoring this client. It will continue running."
            requestingIdentity $ public . PublicRequest_RemoveClient <$> tag (current dName) eRemove

        addE <- urlInputRow validateUri "Add Baker" "Begin monitoring the baker at the address entered." "http://[host][:port]"
        void $ requestingIdentity $ ffor addE $ \addr -> public (PublicRequest_AddClient addr)

    delegatesOptions = do
      divClass "ui medium header" $ text "Delegates"
      elClass "table" "ui celled striped compact table" $ do
        delegates <- watchDelegatePublicKeyHashes
        listWithKey (BaseMap.fromSet (const ()) <$> delegates) $ \pkh _ -> el "tr" $ do
          el "td" $ publicKeyHashLink pkh
          el "td" $ do
            eRemove <- buttonWithInfo "Remove" "Stop monitoring this delegate."
            requestingIdentity $ public . PublicRequest_RemoveDelegate <$> tag (pure pkh) eRemove

        addE <- urlInputRow (Validator.Validator (first tshow . tryReadPublicKeyHashText) id) "Add Delegate" "Begin monitoring wallet address entered." "tz..."
        void $ requestingIdentity $ ffor addE $ \pkh -> public (PublicRequest_AddDelegate pkh)

    nodesOptions = do
      divClass "ui medium header" $ text "Nodes"
      elClass "table" "ui celled striped compact table" $ do
        nodes <- watchNodeAddresses
        listWithKey (Map._unAppendMap <$> nodes) $ \_ node -> el "tr" $ do
          let dName = node
          el "td" $ dynText $ Uri.render <$> dName
          el "td" $ do
            eRemove <- buttonWithInfo "Remove" "Stop monitoring this node. It will continue running."
            requestingIdentity $ public . PublicRequest_RemoveNode <$> tag (current dName) eRemove

        addE <- urlInputRow validateUri "Add Node" "Begin monitoring the node at the address entered." "http://[host][:port]"
        let nodeIdent = Nothing -- either (const Nothing) Just . fromBase58 . T.encodeUtf8 <$> value idInput
        void $ requestingIdentity $ ffor addE $ \addr -> public (PublicRequest_AddNode addr nodeIdent)

    upgradeOptions = mdo
      isLoading <- holdDyn False $ leftmost [False <$ result, True <$ checkUpgrade]
      checkUpgrade <- fmap (domEvent Click . fst) $ elDynAttr' "div"
        (ffor isLoading $ \loading -> "class"=:("ui large button" <> (if loading then " loading" else "")))
        $ text "Check for New Version"
      result <- requestingIdentity $ public PublicRequest_CheckForUpgrade <$ checkUpgrade
      widgetHold_ blank $ ffor result $ \case
        Left _ -> divClass "ui error message" $ text "We had trouble checking for upgrades"
        Right v -> divClass "ui success message" $ text $ "A new version is available: " <> T.pack (showVersion v)

    urlInputRow
      :: (MonadRhyoliteFrontendWidget Bake t m
         , Eq a
         , Show a
         )
      => Validator.Validator t m a -> Text -> Text -> Text -> m (Event t a)
    urlInputRow validator label info placeholder = el "tr" $ do
      (tdEl, address) <- el' "td" $ formItem
        $ validatedInput validator
        $ def & Txt.setPlaceholder placeholder & Txt.setFluid
      addButton <- elClass "td" "right aligned collapsing" $ buttonWithInfo label info
      return $ filterRight $ tag (current address) $ leftmost [addButton, keypress Enter tdEl]


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
          $ validatedInput Validator.validateText
          $ defTxt "User name" & Txt.setInitial (_mailServerView_userName frm0)

        tellFieldErr _2 <=< formItem
          $ validatedInput validatePassword
          $ defTxt "Password"

    validatePassword = Validator.Validator (\x -> if T.null x then Left "Please enter a password" else Right x) Txt.setPasswordType
    defTxt txt = def & Txt.addLabel (labeled txt) & Txt.setPlaceholder txt
    labeled = el "label" . text


data NodeTile
  = NodeTile_PlainNode (Id Node) Node
  | NodeTile_TzScan TzScan
  | NodeTile_Foundation
  deriving (Eq, Ord, Show)

nodesTab :: (MonadRhyoliteFrontendWidget Bake t m, MonadReader Cfg m) => m ()
nodesTab = divClass "ui stackable grid" $ do
  alertsDyn <- maybeDynLazy . fmap maybeSomething =<<
    watchErrors (pure $ Set.singleton $ ClosedInterval LowerInfinity UpperInfinity)

  dyn_ $ ffor alertsDyn $ \case
    Nothing -> divClass "column" nodeTilesWidget
    Just nonEmptyAlertsDyn -> do
      divClass "ten wide column" nodeTilesWidget
      divClass "six wide column" $ do
        elClass "h3" "ui header" $ text "Alerts"
        liveErrorsWidget nonEmptyAlertsDyn

  where
    nodeTilesWidget = do
      tzscanDyn <- watchTzScan
      nodesDyn <- watchNodes $ pure $ universe ()
      let
        zipNodeTiles tzscan nodes =
          (case tzscan of
            Nothing -> id
            Just v -> (NodeTile_TzScan v :)
          ) -- if available, prepend the tzscan node to the list
          (uncurry NodeTile_PlainNode <$> Map.toAscList nodes)
      maybeTilesDyn <- maybeDynLazy $ nonEmpty <$> zipDynWith zipNodeTiles tzscanDyn nodesDyn
      dyn_ $ ffor maybeTilesDyn $ \case
        Nothing -> waitingForResponse
        Just tilesDyn -> divClass "ui stackable cards" $ void $ do
          listWithKey (BaseMap.fromList . zip [1..] . toList <$> tilesDyn) $ \_ vDyn -> do
            divClass "ui card" $ divClass "content" $ dyn_ $ ffor vDyn $ \case
              NodeTile_TzScan tzscan -> do
                headBlockLevelHeader (text "tzscan.io") $
                  Just (_tzScan_headBlockHash tzscan, _tzScan_headLevel tzscan)
                divClass "description" $ do
                  nodeDataTable
                    [ (text "Block hash:", blockHashLink $ _tzScan_headBlockHash tzscan)
                    , (text "Block fitness:", text $ fitnessText $ _tzScan_fitness tzscan)
                    ]
              NodeTile_Foundation -> text "foundation"
              NodeTile_PlainNode _ node -> do
                headBlockLevelHeader (text $ uriHostPortPath $ _node_address node) $
                  liftA2 (,) (_node_headBlockHash node) (_node_headLevel node)
                divClass "description" $ do
                  let stat = _node_networkStat node
                  nodeDataTable
                    [ (text "Block hash:", maybe (text "N/A") blockHashLink $ _node_headBlockHash node)
                    , (text "Block fitness:", text $ maybe "N/A" fitnessText $ _node_fitness node)
                    , (text "Peer Count:", text $ maybe "N/A" tshow $ _node_peerCount node)
                    , (text "Total Sent:", text $ tshow (unTezosWord64 $ _networkStat_totalSent stat) <> " bytes")
                    , (text "Total Received:", text $ tshow (unTezosWord64 $ _networkStat_totalRecv stat) <> " bytes")
                    , (text "Inflow:", text $ tshow (_networkStat_currentInflow stat) <> " bytes/sec")
                    , (text "Outflow:", text $ tshow (_networkStat_currentOutflow stat) <> " bytes/sec")
                    ]

    headBlockLevelHeader title blockHashAndLevel =
      elClass "h3" "ui center aligned header" $ do
        title
        elAttr "div" ("class"=:"sub header"<>"style"=:"padding-top:1em") $ do
          case blockHashAndLevel of
            Nothing -> text "Connecting..."
            Just (blockHash, blockLevel) ->
              blockHashLinkAs blockHash $ text $ tshow $ unRawLevel blockLevel
          divClass "sub header" $ text "Head Block Level"

    nodeDataTable rows = elAttr "table" ("class"=:"ui very basic compact stackable table") $
      for_ rows $ \(heading, value) -> el "tr" $ do
        elAttr "th" ("style"=:"text-align:left") heading
        elAttr "td" ("style"=:"text-align:left") value


delegateTab
  :: (MonadRhyoliteFrontendWidget Bake t m, MonadReader Cfg m)
  => PublicKeyHash
  -> m ()
delegateTab pkh = do
  delegates <- watchDelegateStats $ pure $ Set.singleton pkh
  dparameters <- watchProtoInfo
    -- TODO: this could be a maybeDyn of some sort so that we don't redraw the dom for each balance change/block baked.
  thisDelegate <- maybeDyn $ Map.lookup pkh <$> delegates
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
  dyn_ $ ffor (Map.lookup cid <$> clients) $ \case
    Nothing -> waitingForResponse
    Just clientInfo -> divClass "ui grid" $ do
      dparameters <- watchProtoInfo
      let report = unJson (_clientInfo_report clientInfo)
          baked = sortBy (flip (comparing _event_time)) (_report_baked report)
          errors = sortBy (flip (comparing _error_time)) (map mkErr (_report_errors report))
      divClass "eight wide column" $ do
        elClass "h3" "ui medium header" $ text $ Uri.render addr
        divClass "delegates" $ do
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

semuiTab :: (DomBuilder t m, PostBuild t m, Eq k) => m () -> k -> Demux t k -> m (Event t k)
semuiTab label k currentTab =
  fmap ((k <$) . domEvent Click . fst) $
    elDynAttr' "a" (ffor (demuxed currentTab k) $ \b -> "class" =: if b then "item active" else "item") label

-- | Control that allows the user to build a list of items.
listInput :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Ord k)
          => Text -- ^ Placeholder for input
          -> (Text -> Bool) -- ^ Input validation
          -> (Dynamic t Text -> m ()) -- ^ Widget builder for each item in the list
          -> Dynamic t (AppendMap k Text) -- ^ Items in list
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
      remove <- fmap (fmap (leftmost . BaseMap.elems)) $ elClass "ul" "list-input-items" $
        listWithKey (_unAppendMap <$> items) $ \k t -> el "li" $ do
          el "span" $ itemWidget t
          fmap ((,) k) . tag (current t) . domEvent Click . fst <$> el' "span" (elClass "i" "fa fa-fw fa-times-circle" blank)
  return (ffilter validate submit, switch . current $ remove)
