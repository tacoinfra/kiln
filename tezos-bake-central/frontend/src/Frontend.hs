{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Frontend where

import Control.Lens (_1, _2)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Data.AppendMap (AppendMap, _unAppendMap)
import qualified Data.AppendMap as Map
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Data.Either.Combinators (rightToMaybe)
import Data.Fixed
import Data.List
import qualified Data.Map as BaseMap
import Data.Maybe
import Data.Monoid ()
import Data.Ord
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Format
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Types (MonadJSM)
import qualified Obelisk.ExecutableConfig
import Reflex.Dom.Core
import Reflex.Dom.Form.FieldWriter (tellFieldErr, withFormFieldsErr)
import qualified Reflex.Dom.Form.Validators as Validator
import Reflex.Dom.Form.Widgets (formItem, validatedInput)
import qualified Reflex.Dom.SemanticUI as SemUi
import qualified Reflex.Dom.TextField as Txt
import Rhyolite.Api
import Rhyolite.App (getSingle)
import Rhyolite.Frontend.App
import Rhyolite.Request.Common (decodeValue')
import Rhyolite.Route
import Rhyolite.Schema
import Rhyolite.WebSocket

import Common.Api
import Common.App
import Common.Json (TezosWord64 (..))
import Common.PublicKeyHash
import Common.Schema hiding (Event)
import Common.TaggedHash
import Common.Tez
import Frontend.Common (buttonWithInfo, formWithSubmit, tooltip, tooltipPos, uiButton)


frontend :: (StaticWidget x (), Widget x ())
frontend =
  ( headTag
  , void $ do
      routeStr <- liftIO $ Obelisk.ExecutableConfig.get "route"
      route :: RouteEnv <- case routeStr of
        Just r -> return $ fromMaybe
          (error "Unable to parse injected route")
          (decodeValue' $ LBS.fromStrict $ T.encodeUtf8 r)
        Nothing -> do
          protocol <- getLocationProtocol
          hostWithPort <- getLocationHost
          return $ let (host, port) = T.breakOn ":" hostWithPort
                    in (T.unpack protocol, T.unpack host, T.unpack port)
      liftIO $ print route
      runRhyoliteWidget (Left $ websocketUrlFromRouteEnv route) appMain
  )

watchProtoInfo :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe ProtoInfo))
watchProtoInfo = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_parameters = Just 1
    }
  return $ fmap (getSingle . _bakeView_parameters) theView

watchNodes :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Node) Node))
watchNodes = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodes = Just 1
    }
  return $ ffor theView $ \v' -> fmapMaybe (getFirst . fst) (_bakeView_nodes v')


watchClient :: (MonadRhyoliteFrontendWidget Bake t m) => Dynamic t (Id Client) -> m (Dynamic t (AppendMap (Id Client) ClientInfo))
watchClient cidDyn = do
  theView <- watchViewSelector . ffor cidDyn $ \cid -> mempty
    { _bakeViewSelector_clients = Map.singleton cid 1
    }
  return . ffor theView $ \v -> Map.mapMaybe (\(First n,_) -> n) (_bakeView_clients v)

watchClientAddresses :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Client) ClientAddress))
watchClientAddresses = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_clientAddresses = Just 1
    }
  return . ffor theView $ \v' -> flip Map.mapMaybeWithKey (_bakeView_clientAddresses v') $ \_ (First r, _) -> r

watchNotificatees :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Notificatee) Email))
watchNotificatees = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_notificatees = Just 1
    }
  return . ffor theView $ \v -> fmapMaybe (getFirst . fst) (_bakeView_notificatees v)

watchSummary :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (Report, Int)))
watchSummary = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_summary = Just 1
    }
  improvingMaybe . ffor theView $ \v -> getSingle $ _bakeView_summary v

watchSummaryGraph :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (Micro, Text)))
watchSummaryGraph = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_summary = Just 1
    }
  improvingMaybe . ffor theView $ \v -> getSingle $ _bakeView_summaryGraph v


watchMailServer :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe MailServerView))
watchMailServer =
  (fmap . fmap) (getSingle . _bakeView_mailServer) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_mailServer = Just 1 }

headTag :: DomBuilder t m => m ()
headTag = do
  mapM_ (\s -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank)
    [ "css/font-awesome.min.css"
    , "semantic-ui/semantic.css"
    , "css/main.css"
    ]
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0, maximum-scale=1.0") blank
  elAttr "meta" ("charset" =: "utf-8") blank


tezzies :: Tezzies -> Text
tezzies (Tezzies n) = T.dropWhileEnd (=='.') (T.dropWhileEnd (== '0') (T.pack (show n))) <> "ꜩ"

-- NB: The order of these constructors determines the order of the tabs in the UI.
data UITab = UITab_Summary
           | UITab_Client (Id Client) Text
           | UITab_Options
  deriving (Eq, Ord, Show)

appMain :: forall t m. (MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m), MonadJSM m) => m ()
appMain = elAttr "div" ("style" =: "width: 80%; margin-left: auto; margin-right: auto;") $ do
  clientAddresses <- watchClientAddresses
  el "h1" $ text "Baker Central"
  rec selection <- elAttr "div" ("class" =: "ui top attached tabular menu") $ do
        summaryT <- semuiTab "Summary" UITab_Summary currentTab
        clientT <- fmap switch . hold never <=< dyn . ffor clientAddresses $ \cs ->
          fmap leftmost . forM (Map.toList cs) $ \(cid, name) ->
            semuiTab name (UITab_Client cid name) currentTab
        optionsT <- semuiTab "Options" UITab_Options currentTab
        return (leftmost [summaryT, clientT, optionsT])
      currentTab <- fmap demux (holdDyn UITab_Summary selection)
  elAttr "div" ("class" =: "ui bottom attached tab segment active") . widgetHold summaryTab . ffor selection $ \case
    UITab_Summary -> summaryTab
    UITab_Options -> optionsTab
    UITab_Client cid addr -> clientTab cid addr
  return ()


whenJustDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t (Maybe a) -> (a -> m ()) -> m ()
whenJustDyn d f = dyn_ . ffor d $ \case
  Nothing -> blank
  Just x -> f x

summaryTab :: forall t m. (MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m), MonadJSM m) => m ()
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
          text $ "Waiting: " <> T.pack (show n)
    mGraph <- watchSummaryGraph
    (graphEl, _) <- el' "div" blank
    dyn . ffor mGraph $ \case
      Nothing -> blank
      Just (total, graphText) -> do
        setInnerHTML (_element_raw graphEl) graphText
        text $ "Total rewards earned: " <> tezzies (Tezzies total)
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
        forM_ baked $ \b -> el "tr" $ do
          el "td" $ el "strong" $ text $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d at %H:%M" $ _event_time b
          el "td" . text . T.pack . show . blockLevel $ b
          el "td" . text . T.take 14 . toBase58Text . _bakedEvent_hash . _event_detail $ b
          el "td" . dyn . ffor dparameters $ \case
            Nothing -> text "N/A"
            Just protoInfo -> text . tezzies $ blockRewards b protoInfo
  return ()


optionsTab :: (MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m), MonadJSM m) => m ()
optionsTab = divClass "ui grid" $ do
  clients <- watchClientAddresses
  nodes <- watchNodes
  divClass "four wide column" $ do
    divClass "ui medium header" $ text "Notification Recipients"
    let isEmailAddress = const True
    notificatees <- watchNotificatees

    let
      emailWidget email = do
        dynText email
        text " "
        ev <- uiButton "mini compact orange" "Send test"
        void $ requestingIdentity $ public . PublicRequest_SendTestEmail <$> tag (current email) ev

    rec (addN, removeN) <- listInput "user@example.com" isEmailAddress emailWidget notificatees (Right "" <$ addedN)
        addedN <- requestingIdentity . ffor addN $ \email -> public (PublicRequest_AddNotificatee email)
        requestingIdentity . ffor removeN $ \(_, email) -> public (PublicRequest_RemoveNotificatee email)

    divClass "ui medium header" $ text "SMTP Mail Server"
    mailServer <- watchMailServer
    dyn_ $ ffor mailServer $ \cfg -> do
      let form0 = fromMaybe (MailServerView "" 587 SmtpProtocol_Ssl "") cfg
      updatedForm <- mailServerForm form0
      requestingIdentity $ public . uncurry PublicRequest_SetMailServerConfig <$> updatedForm

    return ()

  divClass "four wide column" $ do
    divClass "ui medium header" $ text "Monitored Clients"
    elAttr "table" ("class" =: "ui celled striped compact table") $ do
      listWithKey (Map._unAppendMap <$> clients) $ \_ dName -> el "tr" $ do
        el "td" $ dynText dName
        el "td" $ do
          eRemove <- buttonWithInfo "Remove" "Stop monitoring this baker. It will continue running."
          requestingIdentity $ public . PublicRequest_RemoveClient <$> tag (current dName) eRemove
      el "tr" $ do
        addressInput <- el "td" $ textInput def
        addButton <- el "td" $ buttonWithInfo "Add Baker" "Begin monitoring the baker at the address entered."
        let address = value addressInput
            addE = tag (current address) $ leftmost [addButton, keypress Enter addressInput]
        requestingIdentity . ffor addE $ \addr -> public (PublicRequest_AddClient addr)

    divClass "ui medium header" $ text "Nodes"
    elAttr "table" ("class" =: "ui celled striped compact table") $ do
      listWithKey (Map._unAppendMap <$> nodes) $ \_ node -> el "tr" $ do
        let dName = _node_address <$> node
        el "td" $ dynText dName
        el "td" $ do
          eRemove <- buttonWithInfo "Remove" "Stop monitoring this node. It will continue running."
          requestingIdentity $ public . PublicRequest_RemoveNode <$> tag (current dName) eRemove
      el "tr" $ do
        addressInput <- el "td" $ textInput def
        addButton <- el "td" $ buttonWithInfo "Add Node" "Begin monitoring the node at the address entered."
        let address = value addressInput
            addE = tag (current address) $ leftmost [addButton, keypress Enter addressInput]
        requestingIdentity . ffor addE $ \addr -> public (PublicRequest_AddNode addr)

  return ()

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
      tellFieldErr (_1 . mailServerView_hostName) <=< formItem
        $ validatedInput Validator.validateText
        $ defTxt "Host" & Txt.setInitial (_mailServerView_hostName frm0)

      tellFieldErr (_1 . mailServerView_portNumber) <=< formItem
        $ validatedInput (Validator.validateNumeric "port" (Just 0, Just 65535) (Just 1))
        $ defTxt "Port" & Txt.setInitial (T.pack $ show $ _mailServerView_portNumber frm0)

      tellFieldErr (_1 . mailServerView_smtpProtocol) <=< formItem
        $ fmap (fmap (maybe (Left "Please select a protocol") Right) . SemUi._dropdown_value)
        $ do
          labeled "Protocol"
          SemUi.dropdown (def & SemUi.dropdownConfig_placeholder .~ "Protocol")
            (Just $ _mailServerView_smtpProtocol frm0)
            $ SemUi.TaggedStatic
            $ SmtpProtocol_Plain=:text "Plain"
            <> SmtpProtocol_Ssl=:text "SSL"
            <> SmtpProtocol_Starttls=:text "STARTTLS"

      tellFieldErr (_1 . mailServerView_userName) <=< formItem
        $ validatedInput Validator.validateText
        $ defTxt "User name" & Txt.setInitial (_mailServerView_userName frm0)

      tellFieldErr _2 <=< formItem
        $ validatedInput validatePassword
        $ defTxt "Password"

    validatePassword = Validator.Validator (\x -> if T.null x then Left "Please enter a password" else Right x) Txt.setPasswordType
    defTxt txt = def & Txt.addLabel (labeled txt) & Txt.setPlaceholder txt
    labeled = el "label" . text


clientTab :: (MonadRhyoliteFrontendWidget Bake t m) => Id Client -> Text -> m ()
clientTab cid addr = do
  clients <- watchClient (pure cid)
  nodes <- watchNodes -- TODO: limit by client
  divClass "ui grid" . void . dyn . ffor (Map.lookup cid <$> clients) $ \case
    Nothing -> text "Waiting for response..."
    Just clientInfo -> do
      dparameters <- watchProtoInfo
      let report = unJson (_clientInfo_report clientInfo)
          baked = sortBy (flip (comparing _event_time)) (_report_baked report)
          errors = sortBy (flip (comparing _error_time)) (map mkErr (_report_errors report))
      divClass "eight wide column" $ do
        divClass "ui medium header" . text $ addr
        elAttr "div" ("class" =: "delegates") $ do
          text $ "ID: "
            <> T.intercalate " " (fmap toPublicKeyHashText $ _clientConfig_delegates $ unJson $ _clientInfo_config clientInfo)
        divClass "ui small header" . text $ "Node Statistics"
        elAttr "div" ("class" =: "client-node") $ do
          text $ "Node: "
            <> _clientConfig_nodeUri (unJson $ _clientInfo_config clientInfo)
        dyn . ffor nodes $ \ns -> case Nothing {- TODO: sort this out in a way that breaks for unreachable nodes Map.lookup (_clientInfo_node clientInfo) ns -} of
            Nothing -> text "Waiting..."
            Just n -> do
              el "div" . text $ "Head block level " <> case _node_headLevel n of
                Nothing -> "unknown"
                Just k -> T.pack (show k)
              el "div" . text $ "Peer count: " <> case _node_peerCount n of
                Nothing -> "unknown"
                Just k -> T.pack (show k)
              let stat = _node_networkStat n
              el "div" . text $ "Sent: " <> T.pack (show (unTezosWord64 $ _networkStat_totalSent stat)) <> " bytes"
              el "div" . text $ "Recv: " <> T.pack (show (unTezosWord64 $ _networkStat_totalRecv stat)) <> " bytes"
              el "div" . text $ "Inflow: " <> T.pack (show (_networkStat_currentInflow stat)) <> " bytes/sec"
              el "div" . text $ "Outflow: " <> T.pack (show (_networkStat_currentOutflow stat)) <> " bytes/sec"
        forM_ (_clientInfo_balance clientInfo) $ \tz -> do
          elAttr "div" ("class" =: "balance" <> "data-tooltip" =: "This is the current number of tezzies in the account that this baker is using.") $ do
            text "Current Balance: "
            text (tezzies tz)
          dyn . ffor dparameters $ \parameters -> forM_ parameters $ \protoInfo -> do
            let bSD = _protoInfo_blockSecurityDeposit protoInfo
                eSD = _protoInfo_endorsementSecurityDeposit protoInfo
                failures = ["baking or endorsement" | tz < min bSD eSD] <> ["baking" | tz < bSD] <> ["endorsement" | tz < eSD]
            case failures of
              (t:_) -> do
                text $ "The identity in use by this baker has not enough tezzies to pay the security deposit for " <> t <> ". "
                text $ "The security deposit for baking is currently " <> tezzies bSD <> " and for endorsement is currently " <> tezzies eSD <> ". "
                text $ "You'll need to transfer sufficient tezzies into the account before it can continue."
              [] | tz < 4 * (bSD + eSD) -> do
                text $ "The identity in use by this baker is running somewhat low on tezzies. "
                  <> "The security deposit for baking is currently " <> tezzies bSD <> " and for endorsement is currently " <> tezzies eSD <> ". "
                  <> "Be sure to keep enough tezzies in the account to pay the security deposits on blocks you'll be baking or endorsing."
              _ -> blank
        divClass "counts" $ do
          {-
          tooltip "This counts the number of times that a block was baked and injected into the blockchain by this baker since it began running." . text $
            "Blocks baked:" <> (T.pack . show $ length baked) -- incorrect
          -}
          tooltip "This counts the number of errors that this baker has encountered since it began running." . text $
            "Errors: " <> (T.pack . show $ length errors)
        case errors of
          [] -> blank
          _ -> divClass "errors" $ do
            divClass "ui medium header" $ text "Errors"
            elClass "table" "ui celled striped table" $ do
              el "thead" . el "tr" $ do
                elClass "th" "four wide" $ text "Time"
                el "th" $ text "Message"
              forM_ errors $ \e -> do
                el "tr" $ do
                  el "td" . el "strong" . text . T.pack . formatTime defaultTimeLocale "%Y-%m-%d at %H:%M" . _error_time $ e
                  el "td" $ do
                    forM_ (T.lines (_error_text e)) $ \t ->
                      divClass "errorLine" $ text t
      divClass "eight wide column" $ do
        divClass "ui medium header" $ text "Activity"
        elAttr "table" ("class" =: "ui celled striped table") $ do
          el "thead" . el "tr" $ do
            elClass "th" "four wide" $ text "Time"
            el "th" $ text "Level"
            el "th" $ text "Block Hash"
            el "th" $ text "Reward"
          forM_ baked $ \b -> el "tr" $ do
            el "td" . el "strong" $ text $ T.pack . formatTime defaultTimeLocale "%Y-%m-%d at %H:%M" . _event_time $ b
            el "td" . text . T.pack . show . blockLevel $ b
            el "td" . text . T.take 14 . toBase58Text . _bakedEvent_hash . _event_detail $ b
            el "td" . dyn . ffor dparameters $ \case
              Nothing -> blank
              Just protoInfo -> text . tezzies $ blockRewards b protoInfo

semuiTab :: (DomBuilder t m, PostBuild t m, Eq k) => Text -> k -> Demux t k -> m (Event t k)
semuiTab label k currentTab =
  fmap ((k <$) . domEvent Click . fst) $
    elDynAttr' "a" (ffor (demuxed currentTab k) $ \b -> "class" =: if b then "item active" else "item") $
      text label

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
        Left errs -> forM_ errs $ elClass "div" "modal-content__text-input-error" . text
        Right success -> elClass "div" "modal-content__text-input-success" $ text success
      remove <- fmap (fmap (leftmost . BaseMap.elems)) $ elClass "ul" "list-input-items" $
        listWithKey (_unAppendMap <$> items) $ \k t -> el "li" $ do
          el "span" $ itemWidget t
          fmap ((,) k) . tag (current t) . domEvent Click . fst <$> el' "span" (elClass "i" "fa fa-fw fa-times-circle" blank)
  return (ffilter validate submit, switch . current $ remove)
