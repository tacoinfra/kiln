{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Common.Api
import Common.App
import Control.Monad
import Control.Monad.Trans
import Data.Either.Combinators
import Data.Monoid
import Data.Text (Text)
import Focus.Api
import Focus.JS.App
import Focus.JS.Run
import Focus.Request
import Focus.Route
import Focus.WebSocket
import qualified Data.ByteString.Lazy as LBS
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

appMain :: MonadFocusFrontendWidget Bake t m => m ()
appMain = do
  address <- value <$> textInput def
  btn <- button "Add Address"
  requestingIdentity $ ffor (tag (current address) btn) $ \addr -> public (PublicRequest_AddClient addr)
  v <- watchViewSelector (pure $ BakeViewSelector { _bakeViewSelector_clients = Just 1 })
  display v
  return ()