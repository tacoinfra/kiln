{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-} -- for the parsers specifically

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens.Combinators (views, over, set, _head)
import Control.Monad
import Control.Monad.Reader
import Data.Aeson (encode)
import Data.Attoparsec.Text hiding (take, option)
import qualified Data.Attoparsec.Text as P
import Data.Char
import Data.Monoid ((<>), mempty)
import Data.Text (Text)
import Data.Time
import Data.Word
import GHC.IO.Exception
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as O
import Snap hiding (method)
import System.Process
import Text.Read
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT

import Tezos.BakeMonitor.Types
import Tezos.NodeRPC

-- TODO: write a pid file for the spawned baker client

-- Type for common sorts of categories of message that the baker emits while baking.
-- We're mainly interested in counting the blocks that are injected, but some of the rest is potentially useful.
data MessageType =
    MessageType_Selected BlockPrefix  -- Select candidate block after BKidogWLoxoM (slot 1) fitness: 00::00000000000000df
  | MessageType_Injected BlockPrefix Word64 {- level -} -- Injected block BKiNQABfPLcg for my-ident after BKiNpAqXuqEx  (level 222, slot 0, fitness 00::00000000000000de, operations 0+0+0+0)
  | MessageType_NoNonce BlockPrefix   -- No nonce to reveal for block BKiNmhYodVSR
  | MessageType_Error      -- Error while endorsing:
  | MessageType_ErrorCont  -- Error, dumping error stack:
                           --   Wrong predecessor BKiQtCxSQRGcr3QPX7RR62VNnM9DXNv4NnJ6HjJso3f9W87FLHT, expected BKiUhCVyeftvggKDw3UjHhXSXdmUnV4epidsyHnS5B2XEA2gcEh
                           -- ^^ note these two spaces.
  | MessageType_Unknown    -- Anything else.
  deriving Show

blockPrefix :: Parser BlockPrefix
blockPrefix = BlockPrefix <$> P.takeWhile isAlphaNum

errorCont :: Parser MessageType
errorCont = do
  string "Error, dumping error stack:" <|> string "  "
  return $ MessageType_ErrorCont

errorLine :: Parser MessageType
errorLine = do
  string "Error"
  return $ MessageType_Error

selectCandidate :: Parser MessageType
selectCandidate = do
  string "Select candidate block after "
  bp <- blockPrefix
  return $ MessageType_Selected bp

injectedBlock :: Parser MessageType
injectedBlock = do
  string "Injected block "
  bp <- blockPrefix
  P.takeWhile (/= '(')
  string "(level "
  n <- decimal
  return $ MessageType_Injected bp n

noNonce :: Parser MessageType
noNonce = do
  string "No nonce to reveal for block "
  bp <- blockPrefix
  return $ MessageType_NoNonce bp

messageType :: Parser MessageType
messageType = errorCont <|> errorLine <|> selectCandidate <|> injectedBlock <|> noNonce

classify :: Text -> MessageType
classify t = case parseOnly messageType t of
  Left _ -> MessageType_Unknown
  Right x -> x

fetchBlockFromFragment :: Text -> Manager -> BlockPrefix -> IO BlockHash
fetchBlockFromFragment nodeAddr httpMgr = runNodeRPCT (NodeRPCContext httpMgr nodeAddr) . go
  where
    go pfx = nodeRPC (Complete pfx) >>= \case
        RpcResponse_HttpException e -> error $ "bad response from node" <> show e <> "for prefix" <> show pfx
        RpcResponse_UnexpectedStatus s -> error $ "bad response from node" <> show s <> "for prefix" <> show pfx
        RpcResponse_NonJSON clue raw -> error $ "Non JSON response from node: " <> show clue <> "\n" <> show raw <> "for prefix" <> show pfx
        RpcResponse_Success v -> case v of
            (blockId:_) -> return blockId
            _ -> error $ "Block Prefix not known to node" <> "for prefix" <> show pfx

opts :: O.Parser (IO ())
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

bumpBlockSeen :: UTCTime -> Report -> Report
bumpBlockSeen now = over report_lastSeen (\old -> max (Just now) old)

mainArgs :: Int -> Text -> FilePath -> String -> IO ()
mainArgs monitorPort nodeRPCLocation clientExecutable identity = do
  let [rpcAddr, rpcPort] = T.splitOn ":" nodeRPCLocation
  (_, Just out, Just err, ph) <- createProcess
    (proc clientExecutable ["--addr", T.unpack rpcAddr, "--port", T.unpack rpcPort, "launch", "daemon", identity, "-B", "-E", "-D"])
      { std_out = CreatePipe
      , std_err = CreatePipe
      }
  dataRef <- newMVar $ Report
    { _report_counts = (Count 0 0 0)
    , _report_lastBaked = []
    , _report_errors = []
    , _report_failedBaker = []
    , _report_lastSeen = Nothing
    , _report_tezzies = Nothing
    }

  let updateData f = modifyMVar_ dataRef $ return . f

  httpMgr <- liftIO $ newManager tlsManagerSettings

  -- TODO: if the baker is on a bad fork, it may erroneously belive it has
  -- more tezzies than it really does on the real fork.  this should be moved
  -- to the central monitor, and checked by several nodes
  void . forkIO $ forever $ do
    balanceLine <- readProcess clientExecutable ["get", "balance", "for", identity] ""
    let balance = readMaybe (filter (\c -> isDigit c || c == '.') balanceLine)
    updateData (set report_tezzies $ fmap Tezzies balance)
    threadDelay (60*10^(6 :: Int))

  -- consume from stdout looking for data.
  void . forkIO $ forever $ do
    msg <- T.hGetLine out
    now <- getCurrentTime
    case classify msg of
      MessageType_Selected _ -> modifyMVar_ dataRef $ \tops -> do
        return . bumpBlockSeen now . over (report_counts . count_selected) (+1) $ tops
      MessageType_Injected h level -> do
        modifyMVar_ dataRef $ \tops -> do
          blockHash <- fetchBlockFromFragment ("http://" <> nodeRPCLocation) httpMgr h
          let bakedV = Baked
                { _baked_seq = views (report_counts . count_injected) (+1) tops
                , _baked_hash = blockHash
                , _baked_time = now
                , _baked_level = level
                }
          return
            . over (report_counts . count_injected) (+1)
            . over report_lastBaked (\bs -> take 20 $ bakedV : bs)
            . bumpBlockSeen now
            $ tops
      MessageType_Error ->
        updateData
          $ over (report_counts . count_errors) (+1)
          . over report_errors (take 20 . (Error now msg :))
      MessageType_ErrorCont ->
        updateData $ over (report_errors . _head . error_text) (`T.append` msg)
      MessageType_NoNonce _ -> do
        updateData (bumpBlockSeen now)
      MessageType_Unknown -> return ()

    T.putStrLn (T.pack (show now) <> ": " <> msg)
    print (classify msg)

  let bakerTask = waitForProcess ph >>= \case
        ExitSuccess -> error "this is really not supposed to happen..."
        _ -> modifyMVar_ dataRef $ \tops -> do
          stdErrors <- LT.hGetContents err
          LT.putStr stdErrors
          return $ over report_failedBaker (stdErrors:) tops

  let rootHandler = do
        c <- liftIO $ readMVar dataRef
        writeLBS (encode c)

  let httpTask = httpServe (setPort monitorPort mempty) rootHandler
  void $ race bakerTask httpTask
