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

import Control.Lens ((<&>), _1, _2)
import Control.Monad (when, (<=<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, runReaderT)
import Data.AppendMap (AppendMap, _unAppendMap)
import qualified Data.AppendMap as Map
import Data.Bifunctor
import qualified Data.ByteString.Base16 as BS16
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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Traversable (for)
import qualified Form.Checks as Check
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Types (MonadJSM)
import qualified Obelisk.ExecutableConfig
import Reflex.Dom.Core
import Reflex.Dom.Form.FieldWriter (tellFieldErr, withFormFieldsErr)
import qualified Reflex.Dom.Form.Validators as Validator
import Reflex.Dom.Form.Widgets (formItem, validatedInput)
import qualified Reflex.Dom.SemanticUI as SemUi
import qualified Reflex.Dom.TextField as Txt
import Rhyolite.Api (public)
import Rhyolite.App (getSingle)
import Rhyolite.Frontend.App (MonadRhyoliteFrontendWidget, runRhyoliteWidget, watchViewSelector)
import Rhyolite.Request.Common (decodeValue')
import Rhyolite.Route (RouteEnv)
import Rhyolite.Schema (Email, Id, Json (..))
import Rhyolite.WebSocket (websocketUrlFromRouteEnv)
import qualified Text.URI as Uri

import Common (tshow)
import Common.Api
import Common.App
import Common.AppendIntervalMap (AppendIntervalMap, ClosedInterval (..), WithInfinity (..))
import qualified Common.AppendIntervalMap as AppendIMap
import qualified Common.Config as Config
import Common.Fitness (unFitness)
import Common.Json (TezosWord64 (..))
import Common.PublicKeyHash (PublicKeyHash, toPublicKeyHashText, tryReadPublicKeyHashText)
import Common.Schema hiding (Event)
import Common.Tez (Tez (..))
import Common.URI (mkRootUri)
import Frontend.Common

urlInputRow
  :: (MonadRhyoliteFrontendWidget Bake t m
    , Eq a
    , Show a
    )
  => Validator.Validator t m a -> Text -> Text -> Text -> m (Event t a)
urlInputRow validator label info placeholder = el "tr" $ do
  (tdEl, address) <- el' "td" $ formItem
    $ validatedInput validator
    $ def & Txt.setPlaceholder placeholder
  addButton <- el "td" $ buttonWithInfo label info
  return $ filterRight $ tag (current address) $ leftmost [addButton, keypress Enter tdEl]

frontend :: (StaticWidget x (), Widget x ())
frontend =
  ( headTag
  , void $ do
      route :: RouteEnv <- liftIO (Obelisk.ExecutableConfig.get $ T.pack Config.route) >>= \case
        Just r -> return $ fromMaybe
          (error "Unable to parse injected route")
          (decodeValue' $ LBS.fromStrict $ T.encodeUtf8 r)
        Nothing -> do
          protocol <- getLocationProtocol
          hostWithPort <- getLocationHost
          return $ let (host, port) = T.breakOn ":" hostWithPort
                    in (T.unpack protocol, T.unpack host, T.unpack port)

      blockExplorerUrl <- ffor (liftIO $ Obelisk.ExecutableConfig.get $ T.pack Config.blockExplorer) $ fmap $ \url ->
        case mkRootUri url of
          Left e -> error $ T.unpack $ "Error parsing injected block explorer URL " <> url <> ": " <> e
          Right rootUrl -> rootUrl

      runRhyoliteWidget (Left $ websocketUrlFromRouteEnv route) $ runReaderT appMain (Cfg blockExplorerUrl)
  )


watchProtoInfo :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe ProtoInfo))
watchProtoInfo =
  (fmap . fmap) (getSingle . _bakeView_parameters) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_parameters = Just 1
    }

watchNode :: (MonadRhyoliteFrontendWidget Bake t m) => Dynamic t (Id Node) -> m (Dynamic t (AppendMap (Id Node) Node))
watchNode cidDyn = do
  theView <- watchViewSelector . ffor cidDyn $ \cid -> mempty
    { _bakeViewSelector_nodes = Map.singleton cid 1
    }
  return $ ffor theView $ \v -> Map.mapMaybe (\(First n,_) -> n) (_bakeView_nodes v)

watchNodeAddresses :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Node) ClientAddress))
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

watchDelegatePublicKeyHashes :: (MonadRhyoliteFrontendWidget Bake t m) => m (Dynamic t (AppendMap PublicKeyHash ()))
watchDelegatePublicKeyHashes = do
  (fmap.fmap) (void . _bakeView_delegates) $ watchViewSelector $ pure $ mempty {_bakeViewSelector_delegates = Just 1}
  -- return $ ffor theView $ \v' -> _

watchDelegateStats :: (MonadRhyoliteFrontendWidget Bake t m) => Dynamic t (Set PublicKeyHash) -> m (Dynamic t (AppendMap PublicKeyHash (BakeEfficiency, Account)))
watchDelegateStats delegates = do
  theView <- watchViewSelector $ ffor delegates $ \ds -> mempty
    { _bakeViewSelector_delegateStats = Map.fromSet (const 1) ds
    }
  return $ ffor theView $ Map.mapMaybe (\(First r, _) -> r) . _bakeView_delegateStats

watchClientAddresses :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Client) ClientAddress))
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


headTag :: DomBuilder t m => m ()
headTag = do
  traverse_ (\s -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank)
    [ "css/font-awesome.min.css"
    , "semantic-ui/semantic.css"
    , "css/main.css"
    ]
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0, maximum-scale=1.0") blank
  elAttr "meta" ("charset" =: "utf-8") blank


-- NB: The order of these constructors determines the order of the tabs in the UI.
data UITab = UITab_Summary
           | UITab_Delegate PublicKeyHash
           | UITab_Client (Id Client) Text
           | UITab_Node (Id Node)
           | UITab_Options
  deriving (Eq, Ord, Show)


appMain :: forall t m. (MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m), MonadJSM m, MonadReader Cfg m) => m ()
appMain = elAttr "div" ("style" =: "width: 80%; margin-left: auto; margin-right: auto;") $ do
  nodeAddresses <- watchNodeAddresses
  clientAddresses <- watchClientAddresses
  delegates <- watchDelegatePublicKeyHashes
  el "h1" $ text "Baker Central"
  rec selection <- elAttr "div" ("class" =: "ui top attached tabular menu") $ do
        summaryT <- semuiTab (text "Summary") UITab_Summary currentTab
        nodeT <- fmap switch . hold never <=< dyn . ffor nodeAddresses $ \cs ->
          fmap leftmost . for (Map.toList cs) $ \(cid, name) ->
            semuiTab (text $ "N:" <> name) (UITab_Node cid) currentTab
        clientT <- fmap switch . hold never <=< dyn . ffor clientAddresses $ \cs ->
          fmap leftmost . for (Map.toList cs) $ \(cid, name) ->
            semuiTab (text $ "B:" <> name) (UITab_Client cid name) currentTab
        delegateT <- fmap switch . hold never <=< dyn . ffor delegates $ \cs ->
          fmap leftmost . for (Map.toList cs) $ \(pkh, _) ->
            semuiTab (text $ "tz:" <> toPublicKeyHashText pkh) (UITab_Delegate pkh) currentTab
        optionsT <- semuiTab (text "Options") UITab_Options currentTab
        return (leftmost [summaryT, delegateT, clientT, nodeT, optionsT])
      currentTab <- fmap demux (holdDyn UITab_Summary selection)
  elAttr "div" ("class" =: "ui bottom attached tab segment active") . widgetHold summaryTab . ffor selection $ \case
    UITab_Summary -> summaryTab
    UITab_Options -> optionsTab
    UITab_Node nid -> nodeTab nid
    UITab_Client cid addr -> clientTab cid addr
    UITab_Delegate pkh -> delegateTab pkh
  return ()


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

      errors <- watchErrors (pure $ Set.singleton $ ClosedInterval LowerInfinity UpperInfinity)
      dyn_ $ ffor errors $ traverse_ $ traverse_ $ traverse_ $ \(log, specificLog) -> do
        let header = divClass "header" . text
        divClass "ui error message" $ do
          case specificLog of
            ErrorLogView_InaccessibleEndpoint (ErrorLogInaccessibleEndpoint _ endpointType address) -> do
              let endpointTypeName = case endpointType of
                    EndpointType_Node -> "node"
                    EndpointType_Client -> "client"
              header $ "Unable to connect to " <> endpointTypeName <> " at " <> address

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


optionsTab :: (MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m), MonadJSM m, MonadReader Cfg m) => m ()
optionsTab = divClass "ui grid" $ do
  clients <- watchClientAddresses
  nodes <- watchNodeAddresses
  delegates <- watchDelegatePublicKeyHashes
  divClass "four wide column" $ do
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

    divClass "ui medium header" $ text "SMTP Mail Server"
    mailServer <- watchMailServer
    dyn_ $ ffor mailServer $ \cfg -> do
      let form0 = fromMaybe (MailServerView "" 587 SmtpProtocol_Ssl "") cfg
      updatedForm <- mailServerForm form0
      requestingIdentity $ public . uncurry PublicRequest_SetMailServerConfig <$> updatedForm

  divClass "four wide column" $ do
    divClass "ui medium header" $ text "Monitored Clients"
    elAttr "table" ("class" =: "ui celled striped compact table") $ do
      listWithKey (Map._unAppendMap <$> clients) $ \_ dName -> el "tr" $ do
        el "td" $ dynText dName
        el "td" $ do
          eRemove <- buttonWithInfo "Remove" "Stop monitoring this baker. It will continue running."
          requestingIdentity $ public . PublicRequest_RemoveClient <$> tag (current dName) eRemove

      addE <- fmap Uri.render <$> urlInputRow validateUri "Add Baker" "Begin monitoring the baker at the address entered." "http://[host][:port]"
      void $ requestingIdentity $ ffor addE $ \addr -> public (PublicRequest_AddClient addr)

    divClass "ui medium header" $ text "Delegates"
    elAttr "table" ("class" =: "ui celled striped compact table") $ do
      listWithKey (Map._unAppendMap <$> delegates) $ \pkh _ -> el "tr" $ do
        el "td" $ publicKeyHashLink pkh
        el "td" $ do
          eRemove <- buttonWithInfo "Remove" "Stop monitoring this delegate."
          requestingIdentity $ public . PublicRequest_RemoveDelegate <$> tag (pure pkh) eRemove

      addE <- urlInputRow (Validator.Validator (first tshow . tryReadPublicKeyHashText) id) "Add Delegate" "Begin monitoring wallet address entered." "tz..."
      void $ requestingIdentity $ ffor addE $ \pkh -> public (PublicRequest_AddDelegate pkh)

    divClass "ui medium header" $ text "Nodes"
    elAttr "table" ("class" =: "ui celled striped compact table") $ do
      listWithKey (Map._unAppendMap <$> nodes) $ \_ node -> el "tr" $ do
        let dName = node
        el "td" $ dynText dName
        el "td" $ do
          eRemove <- buttonWithInfo "Remove" "Stop monitoring this node. It will continue running."
          requestingIdentity $ public . PublicRequest_RemoveNode <$> tag (current dName) eRemove

      addE <- fmap Uri.render <$> urlInputRow validateUri "Add Node" "Begin monitoring the node at the address entered." "http://[host][:port]"
      let nodeIdent = Nothing -- either (const Nothing) Just . fromBase58 . T.encodeUtf8 <$> value idInput
      void $ requestingIdentity $ ffor addE $ \addr -> public (PublicRequest_AddNode addr nodeIdent)

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


nodeTab :: (MonadRhyoliteFrontendWidget Bake t m, MonadReader Cfg m) => Id Node -> m ()
nodeTab nid = do
  dNode <- watchNode $ pure nid
  thisNode <- maybeDyn $ Map.lookup nid <$> dNode
  dyn_ $ ffor thisNode $ \case
    Nothing -> waitingForResponse
    Just nodeDyn -> dyn_ $ ffor nodeDyn $ \node -> do
      divClass "ui small header" . text $ "Node Statistics"
      elAttr "div" ("class" =: "client-node") $ do
        text $ "Node: " <> _node_address node
      el "div" $ do
        text "Head block level: "
        maybe id blockHashLinkAs (_node_headBlockHash node) (text $ maybe "N/A" tshow $ _node_headLevel node)
      el "div" $ text $ "Head block fitness: " <> case _node_fitness node of
        Nothing -> "N/A"
        Just k -> T.intercalate ":" $ toList $ fmap (T.decodeUtf8 . BS16.encode) $ unFitness k
      el "div" $ text $ "Peer count: " <> maybe "N/A" tshow (_node_peerCount node)
      let stat = _node_networkStat node
      el "div" $ text $ "Sent: " <> tshow (unTezosWord64 $ _networkStat_totalSent stat) <> " bytes"
      el "div" $ text $ "Recv: " <> tshow (unTezosWord64 $ _networkStat_totalRecv stat) <> " bytes"
      el "div" $ text $ "Inflow: " <> tshow (_networkStat_currentInflow stat) <> " bytes/sec"
      el "div" $ text $ "Outflow: " <> tshow (_networkStat_currentOutflow stat) <> " bytes/sec"

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
              text $ tshow $ (round (fromIntegral baked / fromIntegral rights * 100 :: Double) :: Int)
              text "%)"



clientTab :: (MonadRhyoliteFrontendWidget Bake t m, MonadReader Cfg m) => Id Client -> Text -> m ()
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
        elClass "h3" "ui medium header" $ text addr
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
