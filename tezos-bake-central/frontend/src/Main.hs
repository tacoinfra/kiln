{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import Data.Monoid
import Data.Either.Combinators
import Control.Monad.Trans
import Reflex.Dom
import qualified Obelisk.ExecutableConfig
import Focus.Route
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import Focus.JS.App
import Focus.JS.Run
import Focus.Request
import Common.App
import Focus.WebSocket
import Common.Api ()

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
app r = (\() -> headTag, \() -> void $ runFocusWidget (mapLeft websocketUrlFromRouteEnv r) appMain)

appMain :: MonadFocusFrontendWidget Bake t m => m ()
appMain = do
  text "hi"