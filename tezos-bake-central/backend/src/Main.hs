{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Backend.RequestHandler
import Backend.NotifyHandler
import Backend.ViewSelectorHandler
import Backend.Schema
import Control.Category ((.))
import Control.Lens
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Monoid
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Database.Groundhog.Generic.Migration (getTableAnalysis)
import Database.Groundhog.Postgresql
import Focus.Backend
import Focus.Backend.Account
import Focus.Backend.App
import Focus.Backend.DB
import Focus.Backend.DB.PsqlSimple
import Focus.Backend.Schema.TH
import Focus.Backend.Snap
import Focus.Concurrent (worker)
import Focus.Schema
import Network.HTTP.Simple
import Obelisk.Asset.Serve.Snap
import Obelisk.ExecutableConfig.Inject (inject)
import Prelude hiding (id, (.))
import qualified Web.ClientSession as CS
import Snap

import Common.Schema
import Common.Api ()

seconds :: Int -> Int
seconds = (* 10^(6 :: Int))

clientWorker :: (MonadIO m)
             => Pool Postgresql
             -> m (IO ())
clientWorker db = do
  worker (seconds 60) $ do
    putStrLn "Update cycle."
    runNoLoggingT . runDb (Identity db) $ do
      now <- getTime
      let maxTime = Just (addUTCTime (-60) now)
      toUpdate <- [queryQ| SELECT id, address
                           FROM "Client"
                           WHERE updated < ?maxTime OR updated IS NULL
                           ORDER BY updated NULLS FIRST |]
      forM_ toUpdate $ \(cid :: Id Client, address :: Text) -> do
        request <- parseRequest ("http://" <> T.unpack address <> "/")
        response <- httpJSON request
        update [Client_updatedField =. Just now] (AutoKeyField ==. fromId cid)
        update [ClientInfo_reportField =. T.decodeUtf8 (BSL.toStrict (encode (getResponseBody response :: Value))) ] (ClientInfo_clientField ==. cid)

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

