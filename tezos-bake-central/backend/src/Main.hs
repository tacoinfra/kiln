{-# LANGUAGE OverloadedStrings #-}

import Backend.RequestHandler
import Backend.NotifyHandler
import Backend.ViewSelectorHandler
import Backend.Schema
import Control.Category ((.))
import Control.Lens
import Control.Exception
import Control.Monad.Trans
import Control.Monad.Logger (runNoLoggingT)
import Data.Default
import Database.Groundhog.Generic.Migration (getTableAnalysis)
import Database.Groundhog.Postgresql
import Focus.Backend
import Focus.Backend.Account
import Focus.Backend.App
import Focus.Backend.DB
import Focus.Backend.Snap
import Obelisk.Asset.Serve.Snap
import Obelisk.ExecutableConfig.Inject (inject)
import Prelude hiding (id, (.))
import qualified Web.ClientSession as CS
import Snap

import Common.Api ()

main :: IO ()
main = withFocus $ do
  csk <- liftIO $ CS.getKey "config/clientSessionKey"
  liftIO $ withDb "db" $ \db -> do
    runNoLoggingT . runDb (Identity db) $ do
      tableInfo <- getTableAnalysis
      runMigration $ do
        migrateAccount tableInfo
        migrateSchema tableInfo

    (handleListen, wsFinalizer) <- serveDbOverWebsockets db
      (requestHandler csk db)
      (notifyHandler db)
      (viewSelectorHandler csk db)
      (queryMorphismPipeline $ transposeMonoidMap . monoidMapQueryMorphism)
    liftIO . flip finally wsFinalizer . quickHttpServe $ route
      [ ("", rootHandler)
      , ("/listen", handleListen)
      , ("static", serveAssets "static" "static")
      ]

rootHandler :: MonadSnap m => m ()
rootHandler = do
  cfg <- liftIO $ inject "route"
  serveApp "" $ def
    & appConfig_initialHead .~ Just cfg

