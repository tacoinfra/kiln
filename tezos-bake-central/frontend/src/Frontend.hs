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

import Common.Api
import Common.App
import Common.Schema hiding (Event)
import Control.Lens (firstOf)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Data.AppendMap (AppendMap, _unAppendMap)
import qualified Data.AppendMap as Map
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Combinators
import Data.Fixed
import Data.Foldable (foldl')
import Data.List
import qualified Data.Map as BaseMap
import Data.Maybe
import Data.Monoid hiding (First (..), (<>))
import Data.Ord
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Format
import Data.Word
import qualified Obelisk.ExecutableConfig
import Reflex.Dom
import Rhyolite.Api
import Rhyolite.Frontend.App
import Rhyolite.Request.Common (decodeValue')
import Rhyolite.Route
import Rhyolite.Schema
import Rhyolite.WebSocket

import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Types (MonadJSM)

import Common.PublicKeyHash
import Common.TaggedHash
import Common.Tez


frontend :: (StaticWidget x (), Widget x ())
frontend =
  ( headTag
  , void $ do
      Just routeStr <- liftIO $ Obelisk.ExecutableConfig.get "route"
      let route :: RouteEnv
          Just route = decodeValue' $ LBS.fromStrict $ T.encodeUtf8 routeStr
      liftIO $ print route
      runRhyoliteWidget (mapLeft websocketUrlFromRouteEnv (Left route)) appMain
  )

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

buttonWithInfo :: (DomBuilder t m) => Text -> Text -> m (Event t ())
buttonWithInfo label t =
  fmap (domEvent Click . fst) <$> elAttr' "button" ("class" =: "ui button" <> "data-tooltip" =: t) $ do
    text label

tooltip :: (DomBuilder t m) => Text -> m a -> m a
tooltip t = elAttr "div" ("data-tooltip" =: t)

tooltipPos :: (DomBuilder t m) => Text -> Text -> m a -> m a
tooltipPos p t = elAttr "div" ("data-tooltip" =: t <> "data-position" =: p)

-- NB: The order of these constructors determines the order of the tabs in the UI.
data UITab = UITab_Summary
           | UITab_Client (Id Client)
           | UITab_Options
  deriving (Eq, Ord, Show)

appMain :: forall t m. (MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m)) => m ()
appMain = elAttr "div" ("style" =: "width: 80%; margin-left: auto; margin-right: auto;") $ do
  clients <- watchClients
  el "h1" $ text "Baker Central"
  rec selection <- elAttr "div" ("class" =: "ui top attached tabular menu") $ do
        summaryT <- semuiTab "Summary" UITab_Summary currentTab
        clientT <- fmap switch . hold never <=< dyn . ffor clients $ \cs ->
          fmap leftmost . forM (Map.toList cs) $ \(cid, (name, _)) ->
            semuiTab name (UITab_Client cid) currentTab
        optionsT <- semuiTab "Options" UITab_Options currentTab
        return (leftmost [summaryT, clientT, optionsT])
      currentTab <- fmap demux (holdDyn UITab_Summary selection)
  elAttr "div" ("class" =: "ui bottom attached tab segment active") . widgetHold summaryTab . ffor selection $ \case
    UITab_Summary -> summaryTab
    UITab_Options -> optionsTab
    UITab_Client cid -> clientTab cid (Map.lookup cid <$> clients)
  return ()

summaryTab :: forall t m. (MonadRhyoliteFrontendWidget Bake t m, MonadJSM (Performable m)) => m ()
summaryTab = divClass "ui grid" $ do
  dlevel <- watchTezosLevel
  clients <- watchClients
  rewards <- watchRewards
  dparameters <- watchProtoInfo
  let totalRewards :: Dynamic t (AppendMap Integer Micro)
      totalRewards = foldl' (Map.unionWith (+)) Map.empty <$> rewards
      cumulate :: (Ord a, Integral a, Num b) => a -> b -> AppendMap a b -> [(a,b)]
      cumulate l p m = foldr (\(x,y) xs s -> let y' = s + y in (x - l, s) : (x - l, y') : xs y') (\_ -> []) (Map.toList m) p

      cumulativeRewards :: Dynamic t (Maybe (Micro, [(Integer, Micro)]))
      cumulativeRewards = do
        (mLevel :: Maybe Integer) <- fmap fromIntegral <$> dlevel
        totalRs <- totalRewards
        return . ffor mLevel $ \level ->
          let (before, x, after) = Map.splitLookup (level - 200) totalRs -- NB: we need to push this splitting into the backend
              principal = sum before + fromMaybe 0 x -- just in the sense of where the graph starts
              (past, x', _future) = Map.splitLookup level after
              total = principal + sum past + fromMaybe 0 x'
          in (total, cumulate level principal after)

  let aggCounts :: Either a ClientInfo -> (Count, Sum Int)
      aggCounts (Left _) = (mempty, Sum 1)
      aggCounts (Right ci) = (mkCount (unJson (_clientInfo_report ci)), Sum 0)
  divClass "six wide column" $ do
    divClass "ui medium header" $ text "Summary"
    text "These are the totals of various events across all monitored bakers."
    dyn . ffor (foldMap (aggCounts . snd) <$> clients) $ \(counts, e) -> do
      divClass "counts" $ el "ul" $ do
        tooltipPos "right center" "The number of blocks that have been baked." . text $ ("Blocks baked:" <>) . T.pack . show $ _count_injected counts
        tooltipPos "right center" "The number of errors that have occurred." . text $ ("Errors:" <>) . T.pack . show $ _count_errors counts
        tooltipPos "right center" "This is the number of bakers from which we're still awaiting any response." . text $ ("Waiting:" <>) . T.pack . show $ getSum e
    (graphEl, _) <- el' "div" blank
    graphText <- requestingIdentity . fforMaybe (updated cumulativeRewards) $ \case
      Nothing -> Nothing
      Just (_, cr) -> case drop 2 cr of
        [] -> Nothing
        _ -> Just $ public (PublicRequest_RenderGraph "Cumulative Rewards" cr)
    performEvent_ . ffor graphText $ \theSVG -> do
      setInnerHTML (_element_raw graphEl) theSVG
    dyn . ffor cumulativeRewards $ \case
      Nothing -> blank
      Just (total, _) -> text $ "Total rewards earned: " <> tezzies (Tezzies total)
  dyn . ffor clients $ \cs -> do
    let reports = fmapMaybe (\case (_, Right i) -> Just (unJson (_clientInfo_report i)); _ -> Nothing) cs
        baked = sortBy (comparing _event_time) (concat (_report_baked <$> reports))
    divClass "ten wide column" $ do
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
  return ()

optionsTab :: (MonadRhyoliteFrontendWidget Bake t m) => m ()
optionsTab = divClass "ui grid" $ do
  clients <- watchClients
  nodes <- watchNodes
  divClass "four wide column" $ do
    divClass "ui medium header" $ text "Notification Recipients"
    let isEmailAddress = const True
    notificatees <- watchNotificatees
    rec (addN, removeN) <- listInput "user@example.com" isEmailAddress notificatees (Right "" <$ addedN)
        addedN <- requestingIdentity . ffor addN $ \email -> public (PublicRequest_AddNotificatee email)
        requestingIdentity . ffor removeN $ \(_, email) -> public (PublicRequest_RemoveNotificatee email)
    return ()

  divClass "four wide column" $ do
    divClass "ui medium header" $ text "Monitored Clients"
    elAttr "table" ("class" =: "ui celled striped compact table") $ do
      listWithKey (Map._unAppendMap <$> clients) $ \_ dNameInfo -> el "tr" $ do
        let dName = fst <$> dNameInfo
        el "td" $ dynText dName
        el "td" $ do
          eRemove <- buttonWithInfo "Remove" "Stop monitoring this baker. It will continue running."
          requestingIdentity $ (public . PublicRequest_RemoveClient <$> tag (current dName) eRemove)
      el "tr" $ do
        addressInput <- el "td" $ textInput def
        addButton <- el "td" $ buttonWithInfo "Add Baker" "Begin monitoring the baker at the address entered."
        let address = value addressInput
            addE = tag (current address) $ leftmost [addButton, keypress Enter addressInput]
        requestingIdentity . ffor addE $ \addr -> public (PublicRequest_AddClient addr)

    divClass "ui medium header" $ text "Nodes"
    elAttr "table" ("class" =: "ui celled striped compact table") $ do
      listWithKey (Map._unAppendMap <$> nodes) $ \_ dNameInfo -> el "tr" $ do
        let dName = dNameInfo
        el "td" $ dynText dName
        el "td" $ do
          eRemove <- buttonWithInfo "Remove" "Stop monitoring this node. It will continue running."
          requestingIdentity $ (public . PublicRequest_RemoveNode <$> tag (current dName) eRemove)
      el "tr" $ do
        addressInput <- el "td" $ textInput def
        addButton <- el "td" $ buttonWithInfo "Add Node" "Begin monitoring the node at the address entered."
        let address = value addressInput
            addE = tag (current address) $ leftmost [addButton, keypress Enter addressInput]
        requestingIdentity . ffor addE $ \addr -> public (PublicRequest_AddNode addr)

  return ()

clientTab :: (MonadRhyoliteFrontendWidget Bake t m) => Id Client -> Dynamic t (Maybe (ClientAddress, Either Text ClientInfo)) -> m ()
clientTab _ mReportD = divClass "ui grid" . void . dyn . ffor mReportD $ \case
    Nothing -> text "Waiting for response..."
    Just (addr, mReport) -> do
      case mReport of
        Left e -> text e
        Right clientInfo -> do
          dparameters <- watchProtoInfo
          let report = unJson (_clientInfo_report clientInfo)
              counts = mkCount report
              baked = _report_baked report
          divClass "eight wide column" $ do
            divClass "ui medium header" . text $ addr
            elAttr "div" ("class" =: "delegates") $ do
              text $ "ID: "
              text $ (T.intercalate " " $ fmap toPublicKeyHashText $ _clientConfig_delegates $ unJson $ _clientInfo_config clientInfo)
            elAttr "div" ("class" =: "client-node") $ do
              text $ "Node: "
              text $ _clientConfig_nodeUri $ unJson $ _clientInfo_config clientInfo
            forM_ (_clientInfo_balance clientInfo) $ \tz -> do
              elAttr "div" ("class" =: "balance" <> "data-tooltip" =: "This is the current number of tezzies in the account that this baker is using.") $ do
                text "Current Balance: "
                text (tezzies tz)
              dyn . ffor dparameters $ \parameters -> forM_ (parameters) $ \protoInfo -> do
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
                    text $ "The security deposit for baking is currently " <> tezzies bSD <> " and for endorsement is currently " <> tezzies eSD <> ". "
                    text $ "Be sure to keep enough tezzies in the account to pay the security deposits on blocks you'll be baking or endorsing."
                  _ -> blank
            divClass "counts" $ do
              tooltip "This counts the number of times that a block was baked and injected into the blockchain by this baker since it began running." . text $
                "Blocks baked:" <> (T.pack . show $ _count_injected counts)
              tooltip "This counts the number of errors that this baker has encountered since it began running." . text $
                "Errors:" <> (T.pack . show $ _count_errors counts)
            case _report_errors report of
              [] -> blank
              es -> divClass "errors" $ do
                divClass "ui medium header" $ text "Errors"
                elClass "table" "ui celled striped table" $ do
                  el "thead" . el "tr" $ do
                    elClass "th" "four wide" $ text "Time"
                    el "th" $ text "Message"
                  forM_ es $ \e -> do
                    el "tr" $ do
                      el "td" . el "strong" . text . T.pack . formatTime defaultTimeLocale "%Y-%m-%d at %H:%M" . _error_time . mkErr $ e
                      el "td" $ do
                        forM_ (T.lines (_error_text . mkErr $ e)) $ \t ->
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
  fmap ((k <$) . domEvent Click . fst) .
    elDynAttr' "a" (ffor (demuxed currentTab k) $ \b -> "class" =: if b then "item active" else "item") $
      text label

watchProtoInfo :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe ProtoInfo))
watchProtoInfo = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_parameters = Just 1
    }
  return $ fmap (join . fmap (getFirst . fst) . firstOf traverse) (fmap _bakeView_parameters theView)

watchTezosLevel :: (MonadRhyoliteFrontendWidget Bake t m) => m (Dynamic t (Maybe Word64))
watchTezosLevel = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_level = Just 1
    }
  return $ fmap (join . fmap (getFirst . fst) . firstOf traverse) (fmap _bakeView_level theView)

watchNodes :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Node) ClientAddress))
watchNodes = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodes = Just 1
    }
  return . ffor theView $ \v' -> fmapMaybe (getFirst . fst) (_bakeView_nodes v')


watchClients :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Client) (ClientAddress, Either Text ClientInfo)))
watchClients = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_clients = Just 1
    }
  return . ffor theView $ \v' -> flip Map.mapMaybeWithKey (_bakeView_clients v') $ \_ (First r,_) ->
    case r of
      Nothing -> Nothing
      (Just (name, Nothing)) -> Just (name, Left "No response yet.")
      (Just (name, Just ci)) -> Just (name, Right ci)

watchRewards :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Client) (AppendMap Integer Micro)))
watchRewards = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_clients = Just 1
    }
  return . ffor theView $ \v -> Map.mapWithKey (\_ (First r,_) -> Map.mapKeys fromIntegral r) (_bakeView_rewards v)

watchNotificatees :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Notificatee) Email))
watchNotificatees = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_notificatees = Just 1
    }
  return . ffor theView $ \v -> fmapMaybe (getFirst . fst) (_bakeView_notificatees v)

-- | Control that allows the user to build a list of items.
-- TODO: Move this to Focus.JS.Widget
listInput :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Ord k)
          => Text -- ^ Placeholder for input
          -> (Text -> Bool) -- ^ Input validation
          -> Dynamic t (AppendMap k Text) -- ^ Items in list
          -> Event t (Either [Text] Text) -- ^ Event of error messages or successful submission
          -> m (Event t Text, Event t (k, Text)) -- ^ Add item event, remove item event
listInput ph validate items rsp = divClass "list-input" $ do
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
      widgetHold_ (return ()) $ ffor rsp $ \case
        Left errs -> forM_ errs $ elClass "div" "modal-content__text-input-error" . text
        Right success -> elClass "div" "modal-content__text-input-success" $ text success
      remove <-  fmap (fmap (leftmost . BaseMap.elems)) $ elClass "ul" "list-input-items" $
        listWithKey (_unAppendMap <$> items) $ \k t -> el "li" $ do
          el "span" $ dynText t
          fmap ((,) k) . tag (current t) . domEvent Click . fst <$> el' "span" (elClass "i" "fa fa-fw fa-times-circle" blank)
  return (ffilter validate submit, switch . current $ remove)
