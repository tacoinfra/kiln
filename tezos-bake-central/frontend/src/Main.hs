{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Common.Api
import Common.App
import Common.Schema
import Control.Lens (firstOf)
import Control.Monad
import Control.Monad.Trans
import qualified Data.AppendMap as Map
import Data.AppendMap (AppendMap, _unAppendMap)
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

import GHCJS.DOM.Element (setInnerHTML) -- for now

import Tezos.BakeMonitor.Types

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
tezzies (Tezzies n) = T.pack (show n) <> "ꜩ"

buttonWithInfo :: (DomBuilder t m) => Text -> Text -> m (Event t ())
buttonWithInfo label t =
  fmap (domEvent Click . fst) <$> elAttr' "button" ("class" =: "ui button" <> "data-tooltip" =: t) $ do
    text label

tooltip :: (DomBuilder t m) => Text -> m a -> m a
tooltip t = elAttr "div" ("data-tooltip" =: t)

tooltipPos :: (DomBuilder t m) => Text -> Text -> m a -> m a
tooltipPos p t = elAttr "div" ("data-tooltip" =: t <> "data-position" =: p)

appMain :: forall t m. MonadFocusFrontendWidget Bake t m => m ()
appMain = elAttr "div" ("style" =: "width: 80%; margin-left: auto; margin-right: auto;") $ do
  theView <- watchViewSelector . pure $ BakeViewSelector
    { _bakeViewSelector_clients = Just 1
    , _bakeViewSelector_parameters = Just 1
    , _bakeViewSelector_level = Just 1
    }
  let dparameters :: Dynamic t (Maybe ProtoInfo)
      dparameters = fmap (join . fmap (getFirst . fst) . firstOf traverse) (fmap _bakeView_parameters theView)

      dlevel :: Dynamic t (Maybe Word64)
      dlevel = fmap (join . fmap (getFirst . fst) . firstOf traverse) (fmap _bakeView_level theView)

      clients :: Dynamic t (AppendMap (Id Client) (ClientAddress, Either Text Report))
      clients = ffor theView $ \v' -> flip Map.mapMaybeWithKey (_bakeView_clients v') $ \k (First r,_) ->
          case r of
            Nothing -> Nothing
            (Just (name, Nothing)) -> Just (name, Left "No response yet.")
            (Just (name, Just ci)) -> Just (name, Right . unJson $ _clientInfo_report ci)

      rewards :: Dynamic t (AppendMap (Id Client) (AppendMap Integer Micro))
      rewards = ffor theView $ \v -> Map.mapWithKey (\k (First r,_) -> Map.mapKeys fromIntegral r) (_bakeView_rewards v)

      cumulate :: (Ord a, Integral a, Num b) => a -> AppendMap a b -> [(a,b)]
      cumulate l m = (-l,0) : foldr (\(x,y) xs _ s -> let y' = s + y in (x - l, y') : xs x y') (\m s -> []) (Map.toList m) 0 0

      cumulativeRewards :: Dynamic t (Maybe [(Integer, Micro)])
      cumulativeRewards = do
        mLevel :: Maybe Integer <- fmap fromIntegral <$> dlevel
        rs <- rewards
        return . ffor mLevel $ \l -> cumulate l $ foldl' (Map.unionWith (+)) Map.empty rs

  el "h1" $ text "Baker Central"
  addressInput <- textInput def
  addButton <- buttonWithInfo "Add Baker" "Begin monitoring the baker at the address entered."
  let address = value addressInput
      addE = tag (current address) $ leftmost [addButton, keypress Enter addressInput]
  requestingIdentity . ffor addE $ \addr -> public (PublicRequest_AddClient addr)
  divClass "ui cards" $ do
    divClass "card" $ divClass "content" $ do
      let aggCounts :: Either a Report -> (Count, Sum Int)
          aggCounts (Left _) = (mempty, Sum 1)
          aggCounts (Right r) = (_report_counts r, Sum 0)
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
        Just cr -> case drop 2 cr of
          [] -> Nothing
          _ -> Just $ public (PublicRequest_RenderGraph "Cumulative Rewards" cr)
      performEvent_ . ffor graphText $ \theSVG -> do
        setInnerHTML (_element_raw graphEl) theSVG

      divClass "header" $ text "Bakers"
      text "This is the list of all currently monitored bakers."
      divClass "bakerlist" $ el "ul" $ dyn . ffor clients $ \x -> forM_ x $ \x -> do
        el "li" $ text (fst x)

    list (_unAppendMap <$> clients) $ \x -> divClass "card" $ divClass "content" $ do
      dyn . ffor x $ \(name, mReport) -> do
        divClass "header" $ text name
        eRemove <- buttonWithInfo "Remove" "Remove this baker from view. It will continue running."
        requestingIdentity $ (public (PublicRequest_RemoveClient name) <$ eRemove)
        case mReport of
          Left e -> text e
          Right report -> do
            let counts = _report_counts report
                baked = _report_lastBaked report
            forM_ (_report_tezzies report) $ \tz -> do
              elAttr "div" ("class" =: "balance" <> "data-tooltip" =: "This is the current number of tezzies in the account that this baker is using.") $ do
                text "Current Balance: "
                text (tezzies tz)
              dyn . ffor dparameters $ \parameters -> forM_ (parameters) $ \protoInfo -> do
                let bSD = _protoInfo_blockSecurityDeposit protoInfo
                    eSD = _protoInfo_endorsementSecurityDeposit protoInfo
                    failures = ["baking or endorsement" | tz < min bSD eSD] <> ["baking" | tz < bSD] <> ["endorsement" | tz < eSD]
                case failures of
                  (t:ts) -> do
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
                    divClass "timestamp" . text . T.pack . show . _error_time $ e
                    divClass "errortext" . el "strong" . text . T.pack . show . _error_text $ e
            divClass "header" $ text "Baked Blocks"
            el "description" . forM_ baked $ \b -> do
              el "div" . el "strong" $ text $ T.pack . formatTime defaultTimeLocale "%Y-%m-%d at %H:%M" . _baked_time $ b
              el "div" $ do
                text $ ("Level: "<>) . T.pack . show . _baked_level $ b
                text $ (" Hash: " <>) . T.take 14 . unBlockHash . _baked_hash $ b
  return ()
