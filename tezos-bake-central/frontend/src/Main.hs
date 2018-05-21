{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Common.Api
import Common.App
import Common.Schema hiding (Event)
import Control.Lens (firstOf)
import Control.Monad
import Control.Monad.Trans
import qualified Data.AppendMap as Map
import Data.AppendMap (AppendMap)
import Data.Either.Combinators
import Data.Fixed
import Data.Foldable (foldl')
import Data.Maybe
import Data.Monoid hiding (First(..), (<>))
import Data.Semigroup
import Data.Text (Text)
import Data.Time.Format
import Data.Word
import Focus.Api
import Focus.JS.App
import Focus.JS.Run
import Focus.Request
import Focus.Route
import Focus.Schema
import Focus.WebSocket
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Obelisk.ExecutableConfig
import Reflex.Dom

import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM.Element (setInnerHTML) -- for now

import Common.BlockHeader

main :: IO ()
main = do
  Just routeStr <- Obelisk.ExecutableConfig.get "route"
  let route :: RouteEnv
      Just route = decodeValue' $ LBS.fromStrict $ T.encodeUtf8 routeStr
  liftIO $ print route
  let frontendConfig = FrontendConfig
        { _frontendConfig_warpPort = 3911
        , _frontendConfig_registerDeviceForNotifications = Nothing
        }
  runFrontend frontendConfig $ app (Left route)

headTag :: DomBuilder t m => m ()
headTag = do
  mapM_ (\s -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank)
    [ "css/font-awesome.min.css"
    , "semantic-ui/semantic.css"
    , "css/main.css"
    ]
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0, maximum-scale=1.0") blank
  elAttr "meta" ("charset" =: "utf-8") blank

app
  :: Either RouteEnv Text
  -> (() -> Widget () (), () -> Widget () ())
app r = (\_ -> headTag, \_ -> void $ runFocusWidget (mapLeft websocketUrlFromRouteEnv r) appMain)

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

appMain :: forall t m. (MonadFocusFrontendWidget Bake t m, MonadJSM (Performable m)) => m ()
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

summaryTab :: forall t m. (MonadFocusFrontendWidget Bake t m, MonadJSM (Performable m)) => m ()
summaryTab = divClass "card" . divClass "content" $ do
  dlevel <- watchTezosLevel
  clients <- watchClients
  rewards <- watchRewards
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

  divClass "header" $ text "Summary"
  text "These are the totals of various events across all monitored bakers."
  dyn . ffor (foldMap (aggCounts . snd) <$> clients) $ \(counts, e) -> do
    divClass "counts" $ el "ul" $ do
      tooltipPos "right center" "This occurs whenever one of the bakers selects a candidate block" . text $ ("Selected:" <>) . T.pack . show $ _count_selected counts
      tooltipPos "right center" "This occurs whenever a baker finishes baking a block" . text $ ("Injected:" <>) . T.pack . show $ _count_injected counts
      tooltipPos "right center" "This occurs whenever an error is reported in any monitored baker." . text $ ("Errors:" <>) . T.pack . show $ _count_errors counts
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
  return ()

optionsTab :: (MonadFocusFrontendWidget Bake t m) => m ()
optionsTab = do

  clients <- watchClients
  divClass "header" $ text "Monitored Clients"
  elAttr "table" ("class" =: "ui celled striped table") $ do
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
  return ()

clientTab :: (MonadFocusFrontendWidget Bake t m) => Id Client -> Dynamic t (Maybe (ClientAddress, Either Text ClientInfo)) -> m ()
clientTab _ mReportD =
  void . dyn . ffor mReportD $ \case
    Nothing -> text "Waiting for response..."
    Just (_, mReport) -> do
      divClass "ui grid" $ case mReport of
        Left e -> text e
        Right clientInfo -> do
          dparameters <- watchProtoInfo
          let report = unJson (_clientInfo_report clientInfo)
              counts = mkCount report
              baked = _report_baked report
          divClass "six wide column" $ do
            elAttr "div" ("class" =: "delegates") $ do
              text $ "ID: "
              text $ (T.intercalate " " $ fmap unPublicKeyHash $ _clientConfig_delegates $ unJson $ _clientInfo_config clientInfo)
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
              tooltip "This counts the number of times that a candidate block was selected by this baker for baking since it began running." . text $
                "Selected:" <> (T.pack . show $ _count_selected counts)
              tooltip "This counts the number of times that a block was baked and injected into the blockchain by this baker since it began running." . text $
                "Injected:" <> (T.pack . show $ _count_injected counts)
              tooltip "This counts the number of errors that this baker has encountered since it began running." . text $
                "Errors:" <> (T.pack . show $ _count_errors counts)
            case _report_errors report of
              [] -> blank
              es -> divClass "errors" $ do
                divClass "header" $ text "Errors"
                el "ul" . forM_ es $ \e -> do
                  el "li" $ do
                    divClass "timestamp" . text . T.pack . show . _error_time . mkErr $ e
                    divClass "errortext" . el "strong" . text . T.pack . show . _error_text . mkErr $ e
          divClass "ten wide column" $ do
            divClass "header" $ text "Activity"
            elAttr "table" ("class" =: "ui celled striped table") $ do
              el "thead" . el "tr" $ do
                el "th" $ text "Time"
                el "th" $ text "Block Hash"
                el "th" $ text "Level"
                el "th" $ text "Reward"
              forM_ baked $ \b -> el "tr" $ do
                el "td" . el "strong" $ text $ T.pack . formatTime defaultTimeLocale "%Y-%m-%d at %H:%M" . _event_time $ b
                el "td" . text . T.pack . show . blockLevel $ b
                el "td" . text . T.take 14 . unBlockHash . _bakedEvent_hash . _event_detail $ b
                el "td" . dyn . ffor dparameters $ \case
                  Nothing -> blank
                  Just protoInfo -> text . tezzies . _protoInfo_blockReward $ protoInfo

semuiTab :: (DomBuilder t m, PostBuild t m, Eq k) => Text -> k -> Demux t k -> m (Event t k)
semuiTab label k currentTab =
  fmap ((k <$) . domEvent Click . fst) .
    elDynAttr' "a" (ffor (demuxed currentTab k) $ \b -> "class" =: if b then "item active" else "item") $
      text label

watchProtoInfo :: MonadFocusFrontendWidget Bake t m => m (Dynamic t (Maybe ProtoInfo))
watchProtoInfo = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_parameters = Just 1
    }
  return $ fmap (join . fmap (getFirst . fst) . firstOf traverse) (fmap _bakeView_parameters theView)

watchTezosLevel :: (MonadFocusFrontendWidget Bake t m) => m (Dynamic t (Maybe Word64))
watchTezosLevel = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_level = Just 1
    }
  return $ fmap (join . fmap (getFirst . fst) . firstOf traverse) (fmap _bakeView_level theView)

watchClients :: MonadFocusFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Client) (ClientAddress, Either Text ClientInfo)))
watchClients = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_clients = Just 1
    }
  return . ffor theView $ \v' -> flip Map.mapMaybeWithKey (_bakeView_clients v') $ \_ (First r,_) ->
    case r of
      Nothing -> Nothing
      (Just (name, Nothing)) -> Just (name, Left "No response yet.")
      (Just (name, Just ci)) -> Just (name, Right ci)

watchRewards :: MonadFocusFrontendWidget Bake t m => m (Dynamic t (AppendMap (Id Client) (AppendMap Integer Micro)))
watchRewards = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_clients = Just 1
    }
  return . ffor theView $ \v -> Map.mapWithKey (\_ (First r,_) -> Map.mapKeys fromIntegral r) (_bakeView_rewards v)
