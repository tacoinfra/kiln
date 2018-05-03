{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import Common.Api
import Common.App
import Common.Schema
import Control.Monad
import Control.Monad.Trans
import qualified Data.AppendMap as Map
import Data.AppendMap (AppendMap, _unAppendMap)
import Data.Either.Combinators
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Monoid hiding (First(..), (<>))
import Data.Semigroup
import Data.Text (Text)
import Focus.Api
import Focus.JS.App
import Focus.JS.Request
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

tezzies :: Micro -> Text
tezzies n = T.pack (show n) <> "ꜩ"

buttonWithInfo :: (DomBuilder t m) => Text -> Text -> m (Event t ())
buttonWithInfo label tooltip =
  fmap (domEvent Click . fst) <$> elAttr' "button" ("class" =: "ui button" <> "data-tooltip" =: tooltip) $ do
    text label

tooltip :: (DomBuilder t m) => Text -> m a -> m a
tooltip t = elAttr "div" ("data-tooltip" =: t)

appMain :: forall t m. MonadFocusFrontendWidget Bake t m => m ()
appMain = divClass "ui" $ do
  v <- fmap _bakeView_clients <$> watchViewSelector (pure $ BakeViewSelector { _bakeViewSelector_clients = Just 1 })
  let clients :: Dynamic t (AppendMap (Id Client) (ClientAddress, Either Text Report))
      clients = ffor v $ \v' -> flip Map.mapMaybeWithKey v' $ \k (r,_) ->
          case r of
            (First Nothing) -> Nothing
            (First (Just (name, Nothing))) -> Just (name, Left "No response yet.")
            (First (Just (name, Just ci))) -> Just (name, Right . unJson $ _clientInfo_report ci)
  el "h1" $ text "Baker Central"
  addressInput <- textInput def
  addButton <- buttonWithInfo "Add Baker" "Begin monitoring the baker at the address entered."
  let address = value addressInput
      addE = tag (current address) $ leftmost [addButton, keypress Enter addressInput]
  requestingIdentity . ffor addE $ \addr -> public (PublicRequest_AddClient addr)
  divClass "ui cards" $ do
    divClass "card" $ divClass "content" $ do
      let aggCounts (Left _) = (mempty, Sum 1)
          aggCounts (Right r) = (_report_counts r, Sum 0)
      divClass "header" $ text "Summary"
      text "These are the totals of various events across all monitored bakers."
      dyn . ffor (foldMap (aggCounts . snd) <$> clients) $ \(counts, e) -> do
        divClass "counts" $ el "ul" $ do
          el "li" . tooltip "This occurs whenever one of the bakers selects a candidate block" . text $ ("Selected:" <>) . T.pack . show $ _count_selected counts
          el "li" . tooltip "This occurs whenever a baker finishes baking a block" . text $ ("Injected:" <>) . T.pack . show $ _count_injected counts
          el "li" . tooltip "This occurs whenever an error is reported in any monitored baker." . text $ ("Errors:" <>) . T.pack . show $ _count_errors counts
          el "li" . tooltip "This is the number of bakers from which we're still awaiting any response." . text $ ("Waiting:" <>) . T.pack . show $ getSum e
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
                baked = _report_last_baked report
            forM_ (_report_tezzies report) $ \tz -> do
              elAttr "div" ("class" =: "balance" <> "data-tooltip" =: "This is the current number of tezzies in the account that this baker is using.") $ do
                text "Current Balance: "
                text (tezzies tz)
              forM_ (_report_protoInfo report) $ \protoInfo -> do
                let bSD = _protoInfo_blockSecurityDeposit protoInfo
                    eSD = _protoInfo_endorsementSecurityDeposit protoInfo
                    failures = ["baking or endorsement" | tz < min bSD eSD] <> ["baking" | tz < bSD] <> ["endorsement" | tz < eSD]
                case failures of
                  (t:ts) -> do
                    text $ "The identity in use by this baker has not enough tezzies to pay the security deposit for " <> t <> "."
                    text $ "The security deposit for baking is currently " <> tezzies bSD <> " and for endorsement is currently " <> tezzies eSD <> "."
                    text $ "You'll need to transfer sufficient tezzies into the account before it can continue."
                  [] | tz < 4 * (bSD + eSD) -> do
                    text $ "The identity in use by this baker is running somewhat low on tezzies."
                    text $ "The security deposit for baking is currently " <> tezzies bSD <> " and for endorsement is currently " <> tezzies eSD <> "."
                    text $ "Be sure to keep enough tezzies in the account to pay the security deposits on blocks you'll be baking or endorsing."

            divClass "counts" $ do
              tooltip "This counts the number of times that a candidate block was selected by this baker for baking since it began running." . text $
                "Selected:" <> (T.pack . show $ _count_selected counts)
              tooltip "This counts the number of times that a block was baked and injected into the blockchain by this baker since it began running." . text $
                "Injected:" <> (T.pack . show $ _count_injected counts)
              tooltip "This counts the number of errors that this baker has encountered since it began running. The most recent errors will be detailed below, if any have occurred." . text $
                "Errors:" <> (T.pack . show $ _count_errors counts)
            case _report_errors report of
              [] -> blank
              es -> divClass "errors" . el "ul" . forM_ es $ \e -> do
                el "li" $ do
                  divClass "timestamp" . text . T.pack . show . _error_time $ e
                  divClass "errortext" . el "strong" . text . T.pack . show . _error_text $ e
            el "description" . el "ul" . forM_ baked $ \b -> do
              el "li" $ do
                el "strong" $ text $ T.pack . show . _baked_time $ b
                el "ul" $ do
                  el "li" $ text $ ("Sequence: "<>) . T.pack . show . _baked_seq $ b
                  el "li" $ text $ ("Hash: " <>) . _baked_hash $ b
  return ()
