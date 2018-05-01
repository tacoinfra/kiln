{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Backend.RequestHandler
import Backend.NotifyHandler
import Backend.ViewSelectorHandler
import Backend.Schema
import Control.Category ((.))
import Control.Concurrent.STM
import Control.Lens
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Function (on)
import Data.IORef
import Data.List
import Data.Monoid
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock
import Database.Groundhog.Generic.Migration (getTableAnalysis)
import Database.Groundhog.Postgresql
import Focus.Backend
import Focus.Backend.Account
import Focus.Backend.App
import Focus.Backend.DB
import Focus.Backend.DB.PsqlSimple
import Focus.Backend.EmailWorker
import Focus.Backend.Listen
import Focus.Backend.Snap
import Focus.Concurrent (worker)
import Focus.Request (decodeValue')
import Focus.Schema
import Network.HTTP.Simple
import Network.Mail.Mime
import Obelisk.Asset.Serve.Snap
import Obelisk.ExecutableConfig.Inject (inject)
import Prelude hiding (id, (.))
import qualified Web.ClientSession as CS
import Snap
import Safe

import Tezos.BakeMonitor.Types

import Common.Schema
import Common.Api ()

seconds :: Int -> Int
seconds = (* 10^(6 :: Int))


mailFor :: Text -> [Error] -> Mail
mailFor toAddr errs =
  let fromA = Address (Just "Tezos Bake Monitor") "noreply@obsidian.systems"
      toA = Address Nothing toAddr
      body = TL.fromStrict . T.unlines $ [T.pack (show t) <> ": " <> e | Error t e <- errs]
  in simpleMail' toA fromA "Error from Tezos bake monitor" body


clientWorker :: (MonadIO m)
             => Email -- email address of user to notify about errors
             -> Int -- delay between checking for updates, in seconds
             -> Pool Postgresql
             -> m (IO ())
clientWorker toAddr delay db = do
  lastErrorRef <- liftIO $ newIORef Nothing
  worker (seconds delay) $ do
    putStrLn "Update cycle."
    runNoLoggingT . runDb (Identity db) $ do
      now <- getTime
      let maxTime = Just (addUTCTime (- fromIntegral delay) now)
      toUpdate <- [queryQ| SELECT id, address
                           FROM "Client"
                           WHERE updated < ?maxTime OR updated IS NULL
                           ORDER BY updated NULLS FIRST |]
      forM_ toUpdate $ \(cid :: Id Client, address :: Text) -> do
        liftIO $ T.putStrLn address
        request <- parseRequest ("http://" <> T.unpack address <> "/")
        response <- httpJSON request
        liftIO $ print response
        let report = getResponseBody response :: Report
            reportJson = Json report
        case maximumByMay (compare `on` _baked_time) $ _report_last_seen report of
          -- Nothing -> liftIO $ mailFor toAddr ("baker " <> address <> " has not seen a block!")
          -- TODO: configurable timeout
          Just b -> when (addUTCTime (fromIntegral 30) (_baked_time b) < now) $ void $ queueEmail (mailFor toAddr $ [Error now ("baker " <> address <> " has not seen a recently!\n" <> T.pack (show b))]) Nothing

        _ <- [executeQ| INSERT INTO "ClientInfo" (client, report)
                        VALUES (?cid, ?reportJson)
                        ON CONFLICT (client) DO UPDATE SET report = ?reportJson |]
        updateAndNotify cid [Client_updatedField =. Just now]
        case sort (_report_errors report) of
          [] -> return ()
          es -> do
            lastError <- liftIO $ readIORef lastErrorRef
            let (new,_) = span ((>= lastError) . Just . _error_time) es
            case new of
              [] -> return ()
              (x:_) -> do
                liftIO $ writeIORef lastErrorRef (Just $ _error_time x)
                _ <- queueEmail (mailFor toAddr new) Nothing
                return ()

main :: IO ()
main = withFocus $ do
  userEmailAddress <- T.readFile "config/userEmailAddress"
  Just email <- decodeValue' <$> LBS.readFile "config/email"
  csk <- liftIO $ CS.getKey "config/clientSessionKey"
  cfg <- liftIO $ inject "route"

  finalizers <- newTVarIO (return ())
  let addFinalizer f = atomically $ modifyTVar finalizers (f >>)

  liftIO $ withDb "db" $ \db -> do
    runNoLoggingT . runDb (Identity db) $ do
      tableInfo <- getTableAnalysis
      runMigration $ do
        migrateAccount tableInfo
        migrateQueuedEmail tableInfo
        migrateSchema tableInfo

    -- Start a thread to send queued emails
    addFinalizer =<< (runNoLoggingT $ emailWorker (seconds 10) (Identity db) email)

    (handleListen, wsFinalizer) <- serveDbOverWebsockets db
      (requestHandler csk db)
      (notifyHandler db)
      (viewSelectorHandler csk db)
      (queryMorphismPipeline $ transposeMonoidMap . monoidMapQueryMorphism)
    addFinalizer wsFinalizer

    addFinalizer =<< clientWorker userEmailAddress 10 db

    liftIO (quickHttpServe $ route
      [ ("", rootHandler cfg)
      , ("/listen", handleListen)
      , ("static", serveAssets "static" "static")
      ]) `finally` join (readTVarIO finalizers)

rootHandler :: MonadSnap m => ByteString -> m ()
rootHandler cfg = do
  serveApp "" $ def
    & appConfig_initialHead .~ Just cfg

