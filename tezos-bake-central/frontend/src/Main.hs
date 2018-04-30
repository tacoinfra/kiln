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
import Data.Monoid
import Data.Text (Text)
import Focus.Api
import Focus.JS.App
import Focus.JS.Request
import Focus.JS.Run
import Focus.Request
import Focus.Route
import Focus.WebSocket
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Obelisk.ExecutableConfig
import Reflex.Dom

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
    v <- fmap (fmap Map.keys . _bakeView_clients) <$> watchViewSelector (pure $ BakeViewSelector { _bakeViewSelector_clients = Just 1 })
    let clients = ffor v $ \v' -> Map.mapWithKey (\k xs ->
          let name = case xs of
                [] -> "Invalid baker address"
                (a:_) -> fst a
              reports = fmap (maybe (Left "Waiting for first report...") parseReport . snd) xs
          in (name, reports)) v'
    list (_unAppendMap <$> clients) $ \x -> divClass "card" $ divClass "content" $ do
      divClass "header" $ dynText $ fst <$> x
      -- TODO, remove client from view
      unbtn <- button "Remove Baker"
      requestingIdentity $ ffor (tag (current x) unbtn) $ \(addr, _) -> public (PublicRequest_RemoveClient addr)
      let baked = reverse . sortOn (_baked_time) . nub . concat . fmap _top_last_baked . catMaybes . fmap rightToMaybe . snd <$> x
      el "description" $ el "ul" $ simpleList baked $ \b -> do
        el "li" $ do
          el "strong" $ dynText $ T.pack . show . _baked_time <$> b
          el "ul" $ do
            el "li" $ dynText $ ("Sequence: "<>) . T.pack . show . _baked_seq <$> b
            el "li" $ dynText $ ("Hash: " <>) . _baked_hash <$> b
  return ()

parseReport :: ClientInfo -> Either Text Top
parseReport ci = case decodeValueFromText $ _clientInfo_report ci of
  Nothing -> Left "Couldn't parse report from baker monitor"
  Just top -> Right top
