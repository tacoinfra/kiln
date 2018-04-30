{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens.Combinators (views, over, set, makeLenses, _head)
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Aeson (FromJSON, ToJSON, encode, decode, Value)
import Data.Monoid
import Data.Monoid ((<>), mempty)
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status(Status(..))
import Network.HTTP.Types.Header
import Options.Applicative
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString as BS
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Safe (headDef)
import Snap hiding (method)
import System.Environment (getArgs)
import System.Process

import Tezos.BakeMonitor.Types

-- TODO: write a pid file for the spawned baker client

-- Type for common sorts of categories of message that the baker emits while baking.
-- We're mainly interested in counting the blocks that are injected, but some of the rest is potentially useful.
data MessageType =
    MessageType_Selected   -- Select candidate block after BKidogWLoxoM (slot 1) fitness: 00::00000000000000df
  | MessageType_Injected Text -- Injected block BKiNQABfPLcg for my-ident after BKiNpAqXuqEx  (level 222, slot 0, fitness 00::00000000000000de, operations 0+0+0+0)
  | MessageType_NoNonce    -- No nonce to reveal for block BKiNmhYodVSR
  | MessageType_Error      -- Error while endorsing:
  | MessageType_ErrorCont  -- Error, dumping error stack:
                           --   Wrong predecessor BKiQtCxSQRGcr3QPX7RR62VNnM9DXNv4NnJ6HjJso3f9W87FLHT, expected BKiUhCVyeftvggKDw3UjHhXSXdmUnV4epidsyHnS5B2XEA2gcEh
                           -- ^^ note these two spaces.
  | MessageType_Unknown    -- Anything else.

classify :: Text -> MessageType
classify t
  | "Select candidate block" `T.isPrefixOf` t = MessageType_Selected
  | "Injected block " `T.isPrefixOf` t = MessageType_Injected . T.takeWhile (/= ' ')  . T.drop (T.length "Injected block ") $ t
  | "No nonce to reveal" `T.isPrefixOf` t = MessageType_NoNonce
  | "error stack:" `T.isSuffixOf` t = MessageType_ErrorCont
  | "Error" `T.isPrefixOf` t = MessageType_Error
  | "  " `T.isPrefixOf` t = MessageType_ErrorCont
  | otherwise = MessageType_Unknown

currentTop :: TopV -> IO Report
currentTop (TopV counts bakedVs errors) = do
  bakeds <- traverse readMVar bakedVs
  return $ Report counts bakeds errors

data TopV = TopV
  { _topV_counts :: Count
  , _topV_last_baked :: [MVar Baked]
  , _topV_errors :: [Error]
  }

makeLenses 'TopV

baked_horizon = 20
error_horizon = 20

opts :: Parser (IO ())
opts = mainArgs
  <$> option auto
      (  long "port"
      <> short 'p'
      <> help "Port to listen on"
      <> showDefault
      <> value 9800
      <> metavar "PORT"
      )
  <*> strOption
      (  long "rpchost"
      <> short 'r'
      <> help "Node url to listen on"
      <> showDefault
      <> value "http://127.0.0.1:8730"
      <> metavar "NODERPC"
      )
  <*> argument str
      (  metavar "CLIENT"
      )
  <*> many (argument str (metavar "ARGS..."))

fetchBlockFromFragment :: Text -> Manager -> MVar (Baked) -> Text -> IO ()
fetchBlockFromFragment nodeHost mgr bucket fragment = goFragment 20
  where
    rpcBoilerplate req = req
      { method = "POST"
      , requestBody = "{}"
      , requestHeaders =
        [ (hContentType, "application/json")
        , (hUserAgent, "tezos-bake-monitor")
        , (hAccept, "*/*")
        ]
      }
    sulk = liftIO $ T.putStrLn $ "Ran out of retries (tried 20 times) getting block for " <> fragment
    goFragment 0 = sulk
    goFragment gas = do
      let request = rpcBoilerplate $ parseRequest_ $ T.unpack $ T.concat [nodeHost, "/blocks/head/complete/", fragment]
      print request
      result' <- liftIO $ try $ httpLbs request mgr
      case result' of
        Left err -> do
          liftIO $ putStrLn $ ("bad response from node" <> ) $ show @ HttpException $ err
          return ()
        Right result -> case responseStatus result of
          Status 200 _ -> case decode (responseBody result) of
            Just (blockId:_) -> goBlock blockId
            _ -> do
              liftIO $ threadDelay (10*10^6) -- TODO backoff man^H^H^Hexponentially
              goFragment (gas - 1)
          Status code phrase -> do
            liftIO $ putStrLn $ ("bad response from node" <> ) $ show $ Status code phrase

    goBlock blockId = do
      let request = rpcBoilerplate $ parseRequest_ $ T.unpack $ T.concat [nodeHost, "/blocks/", blockId]
      print request
      result' <- liftIO $ try $ httpLbs request mgr
      case result' of
        Left err -> do
          liftIO $ putStrLn $ ("bad response from node" <> ) $ show @ HttpException $ err
          return ()
        Right result -> case responseStatus result of
          Status 200 _ ->
            liftIO $ modifyMVar_ bucket $ return . (set baked_block $ decode $ responseBody result)
          Status code phrase -> do
            liftIO $ putStrLn $ ("bad response from node" <> ) $ show $ Status code phrase

main = join $ do
  customExecParser
    (prefs $ showHelpOnEmpty <> showHelpOnError)
    (info (opts <**> helper) idm)

mainArgs port nodeRPC x xs = do
  (_, Just out, _, ph) <- createProcess (proc x xs)
    { std_out = CreatePipe
    }
  started <- getCurrentTime
  dataRef <- newMVar $ TopV
    (Count 0 0 0)
    []
    []

  let bakerTask = forever $ do
        httpMgr <- liftIO $ newManager tlsManagerSettings
        msg <- T.hGetLine out
        now <- getCurrentTime
        case classify msg of
          MessageType_Selected -> modifyMVar_ dataRef $ return . over (topV_counts . count_selected) (+1)
          MessageType_Injected h -> do
            modifyMVar_ dataRef $ \tops -> do
              bakedV <- newMVar (Baked (views (topV_counts . count_injected) (+1) tops) h now Nothing)
              forkIO $ fetchBlockFromFragment nodeRPC httpMgr bakedV h
              return
                . over (topV_counts . count_injected) (+1)
                . over topV_last_baked (\bs -> take 20 $ bakedV : bs)
                $ tops

          MessageType_Error ->
            modifyMVar_ dataRef $ return
              . over (topV_counts . count_errors) (+1)
              . over topV_errors (take 20 . (Error now msg :))
          MessageType_ErrorCont ->
            modifyMVar_ dataRef $ return
              . over (topV_errors . _head . error_text) (`T.append` msg)
          _ -> return ()
        T.putStrLn (T.pack (show now) <> ": " <> msg)
  countRef <- newMVar (Count 0 0 0)
  let rootHandler = do
        c' <- liftIO $ readMVar dataRef
        c <- liftIO $ currentTop c'
        writeLBS (encode c)

  let httpTask = httpServe (setPort port mempty) rootHandler
  void $ race bakerTask httpTask
