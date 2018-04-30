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

appMain :: forall t m. MonadFocusFrontendWidget Bake t m => m ()
appMain = divClass "ui container" $ do
  el "h1" $ text "Baker Central"
  address <- value <$> textInput def
  btn <- button "Add Baker"
  requestingIdentity $ ffor (tag (current address) btn) $ \addr -> public (PublicRequest_AddClient addr)
  divClass "ui cards" $ do
    v <- fmap _bakeView_clients <$> watchViewSelector (pure $ BakeViewSelector { _bakeViewSelector_clients = Just 1 })
    let clients :: Dynamic t (AppendMap (Id Client) (ClientAddress, Either Text Report))
        clients = ffor v $ \v' -> flip Map.mapMaybeWithKey v' $ \k (r,_) ->
          case r of
            (First Nothing) -> Nothing
            (First (Just (name, Nothing))) -> Just (name, Left "No response yet.")
            (First (Just (name, Just ci))) -> Just (name, Right . unJson $ _clientInfo_report ci)
    list (_unAppendMap <$> clients) $ \x -> divClass "card" $ divClass "content" $ do
      dyn . ffor x $ \(name, mReport) -> do
        divClass "header" $ text name
        eRemove <- button "Remove Baker"
        requestingIdentity $ (public (PublicRequest_RemoveClient name) <$ eRemove)
        case mReport of
          Left e -> text e
          Right report -> do
            let counts = _report_counts report
                baked = _report_last_baked report
            divClass "counts" $ do
              text $ T.unwords
                [ "Selected:"
                , T.pack . show $ _count_selected counts
                , "Injected:"
                , T.pack . show $ _count_injected counts
                , "Errors:"
                , T.pack . show $ _count_errors counts
                ]
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