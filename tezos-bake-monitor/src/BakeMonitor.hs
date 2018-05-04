{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens.Combinators (views, over, set, _head)
import Control.Monad
import Control.Monad.Reader
-- import Control.Monad.Trans
import Data.Aeson (encode)
import Data.Char
import Data.Maybe (catMaybes)
import Data.Monoid ((<>), mempty)
import Data.Text (Text)
import Data.Time
import GHC.IO.Exception
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Snap hiding (method)
import System.Process
import Text.Read

import Tezos.BakeMonitor.Types
import Tezos.NodeRPC

-- TODO: write a pid file for the spawned baker client

-- Type for common sorts of categories of message that the baker emits while baking.
-- We're mainly interested in counting the blocks that are injected, but some of the rest is potentially useful.
data MessageType =
    MessageType_Selected BlockPrefix  -- Select candidate block after BKidogWLoxoM (slot 1) fitness: 00::00000000000000df
  | MessageType_Injected BlockPrefix -- Injected block BKiNQABfPLcg for my-ident after BKiNpAqXuqEx  (level 222, slot 0, fitness 00::00000000000000de, operations 0+0+0+0)
  | MessageType_NoNonce BlockPrefix   -- No nonce to reveal for block BKiNmhYodVSR
  | MessageType_Error      -- Error while endorsing:
  | MessageType_ErrorCont  -- Error, dumping error stack:
                           --   Wrong predecessor BKiQtCxSQRGcr3QPX7RR62VNnM9DXNv4NnJ6HjJso3f9W87FLHT, expected BKiUhCVyeftvggKDw3UjHhXSXdmUnV4epidsyHnS5B2XEA2gcEh
                           -- ^^ note these two spaces.
  | MessageType_Unknown    -- Anything else.

snipBlockPrefix :: (BlockPrefix -> MessageType) -> Text -> Text -> Maybe MessageType
snipBlockPrefix ctor pfx line
  | pfx `T.isPrefixOf` line = Just . ctor . BlockPrefix . T.takeWhile (/= ' ')  . T.drop (T.length pfx) $ line
  | otherwise = Nothing

classify :: Text -> MessageType
classify t
  | "error stack:" `T.isSuffixOf` t = MessageType_ErrorCont
  | "Error" `T.isPrefixOf` t = MessageType_Error
  | "  " `T.isPrefixOf` t = MessageType_ErrorCont
  | otherwise = head $ catMaybes
      [ snipBlockPrefix MessageType_Selected "Select candidate block after " t
      , snipBlockPrefix MessageType_Injected "Injected block " t
      , snipBlockPrefix MessageType_NoNonce "No nonce to reveal for block " t
      , Just MessageType_Unknown
      ]

fetchBlockFromFragment :: Text -> Manager -> BlockPrefix -> IO BlockHash
fetchBlockFromFragment nodeAddr httpMgr = flip runReaderT (NodeRPCContext httpMgr nodeAddr) . go
  where
    go pfx = doRPC (Complete pfx) >>= \case
        RpcResponse_HttpException e -> error $ "bad response from node" <> show e <> "for prefix" <> show pfx
        RpcResponse_UnexpectedStatus s -> error $ "bad response from node" <> show s <> "for prefix" <> show pfx
        RpcResponse_NonJSON clue raw -> error $ "Non JSON response from node: " <> show clue <> "\n" <> show raw <> "for prefix" <> show pfx
        RpcResponse_Success v -> case v of
            (blockId:_) -> return blockId
            _ -> error $ "Block Prefix not known to node" <> "for prefix" <> show pfx


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
      <> help "Node url to listen on (without protocol)"
      <> showDefault
      <> value "127.0.0.1:8732"
      <> metavar "NODERPC"
      )
  <*> strOption
      (  long "client"
      <> short 'c'
      <> help "Which tezos-client executable to use"
      <> metavar "FILE"
      )
  <*> strOption
      (  long "identity"
      <> short 'i'
      <> help "Which identity to use for baking"
      <> metavar "TEZOS-IDENTITY"
      )


main :: IO ()
main = join $ do
  customExecParser
    (prefs $ showHelpOnEmpty <> showHelpOnError)
    (info (opts <**> helper) idm)

bumpBlockSeen :: BlockHash -> UTCTime -> Report -> Report
bumpBlockSeen h now = over report_last_seen (\bs -> take 20 $ (Baked 0 h now) : bs)

mainArgs :: Int -> Text -> FilePath -> String -> IO ()
mainArgs monitorPort nodeRPC clientExecutable identity = do
  let [rpcAddr, rpcPort] = T.splitOn ":" nodeRPC
  (_, Just out, Just err, ph) <- createProcess
    (proc clientExecutable ["--addr", T.unpack rpcAddr, "--port", T.unpack rpcPort, "launch", "daemon", identity, "-B", "-E", "-D"])
      { std_out = CreatePipe
      , std_err = CreatePipe
      }
  dataRef <- newMVar $ Report
    (Count 0 0 0)
    []
    []
    []
    []
    Nothing

  let updateData f = modifyMVar_ dataRef $ return . f

  httpMgr <- liftIO $ newManager tlsManagerSettings

  -- TODO: if the baker is on a bad fork, it may erroneously belive it has
  -- more tezzies than it really does on the real fork.  this should be moved
  -- to the central monitor, and checked by several nodes
  void . forkIO $ forever $ do
    balanceLine <- readProcess clientExecutable ["get", "balance", "for", identity] ""
    let balance = readMaybe (filter (\c -> isDigit c || c == '.') balanceLine)
    updateData (set report_tezzies balance)
    threadDelay (60*10^(6 :: Int))

  -- consume from stdout looking for data.
  void . forkIO $ forever $ do
    msg <- T.hGetLine out
    now <- getCurrentTime
    case classify msg of
      MessageType_Selected h -> modifyMVar_ dataRef $ \tops -> do
        blockHash <- fetchBlockFromFragment ("http://" <> nodeRPC) httpMgr h
        return . bumpBlockSeen blockHash now . over (report_counts . count_selected) (+1) $ tops
      MessageType_Injected h -> do
        modifyMVar_ dataRef $ \tops -> do

          blockHash <- fetchBlockFromFragment ("http://" <> nodeRPC) httpMgr h
          let bakedV = (Baked (views (report_counts . count_injected) (+1) tops) blockHash now)
          return
            . over (report_counts . count_injected) (+1)
            . over report_last_baked (\bs -> take 20 $ bakedV : bs)
            . bumpBlockSeen blockHash now
            $ tops

      MessageType_Error ->
        updateData
          $ over (report_counts . count_errors) (+1)
          . over report_errors (take 20 . (Error now msg :))
      MessageType_ErrorCont ->
        updateData $ over (report_errors . _head . error_text) (`T.append` msg)
      MessageType_NoNonce h -> do
        blockHash <- fetchBlockFromFragment ("http://" <> nodeRPC) httpMgr h
        updateData (bumpBlockSeen blockHash now)
      MessageType_Unknown -> return ()

    T.putStrLn (T.pack (show now) <> ": " <> msg)

  let bakerTask = waitForProcess ph >>= \case
        ExitSuccess -> error "this is really not supposed to happen..."
        _ -> modifyMVar_ dataRef $ \tops -> do
          stdErrors <- LT.hGetContents err
          LT.putStr stdErrors
          return $ over report_failedbaker (stdErrors:) tops

  let rootHandler = do
        c <- liftIO $ readMVar dataRef
        writeLBS (encode c)

  let httpTask = httpServe (setPort monitorPort mempty) rootHandler
  void $ race bakerTask httpTask
