{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.Reader
import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.AppendMap as AMap
import Data.Default
import Data.Function (on)
import Data.IORef
import Data.List
import Data.Monoid
import Data.Maybe (catMaybes)
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock
import Data.Void
import Database.Groundhog.Generic.Migration (getTableAnalysis)
import Database.Groundhog.Postgresql
import Focus.Config
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
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import Network.HTTP.Types.Status(Status(..))
import Network.Mail.Mime
import Obelisk.Asset.Serve.Snap
import Obelisk.ExecutableConfig.Inject (inject)
import Prelude hiding (id, (.))
import qualified Web.ClientSession as CS
import Snap
import Safe

import Tezos.BakeMonitor.Types
import Tezos.NodeRPC

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

-- problem:: many baked blocks do not appear on chain
-- problem:: any parent of seen blocks do not appear on chain
-- problem:: blocks are not seen frequently

-- rough sketch, 
-- %seen <- consider some block (say, the most recent, seen block or the most recent baked block)
-- %lvl, %parent <-  ask the node for %seen level, and the ID of its parent. (level - 1)
-- %1 ask the same node for the head and its level (level')
-- %2 ask the same node for head$(level' - level - 1)
-- if %0 != %2; sulk


data BlockHealthData = BlockHealthData
  { best_head :: BlockInfo
  , seen_block :: Maybe BlockInfo
  , nth_parent :: Maybe BlockInfo
  }

data ForkInfo = ForkInfo Node ForkStatus Baked
data ForkStatus
  = ForkStatus_Good
  | ForkStatus_TooNew
  | ForkStatus_TooOld
  | ForkStatus_Forked
  | ForkStatus_BadNode (RpcResponse Void)

cheekyShow = T.pack . \case
  ForkStatus_Good -> "good"
  ForkStatus_TooNew -> "new"
  ForkStatus_TooOld -> "old"
  ForkStatus_Forked -> "forked"
  ForkStatus_BadNode _ -> "err"

looksCromulent :: [ForkInfo] -> IO ()
looksCromulent xs = print total >> print (fmap length <$> counts)
  where
    sing :: ForkInfo -> AMap.AppendMap Node (AMap.AppendMap Text [Baked])
    sing (ForkInfo n h b) = AMap.singleton n $ AMap.singleton (cheekyShow h) [b]
    total = length xs
    counts = foldMap sing xs


factorResponse :: RpcResponse a -> Either (RpcResponse Void) a
factorResponse (RpcResponse_HttpException bad) = Left $ RpcResponse_HttpException bad
factorResponse (RpcResponse_UnexpectedStatus bad) = Left $ RpcResponse_UnexpectedStatus bad
factorResponse (RpcResponse_NonJSON clue bad) = Left $ RpcResponse_NonJSON clue bad
factorResponse (RpcResponse_Success ok) = Right ok

cromulent1 :: MonadIO m => UTCTime -> Report -> Node -> m [ForkInfo]
cromulent1 now rpt node = do
  httpMgr <- liftIO $ newManager tlsManagerSettings
  let ctx = NodeRPCContext httpMgr $ _node_address node -- "http://127.0.0.1:18731"
  traverse (flip runReaderT ctx . checkChainHealth now 30) $ {- catMaybes $ maximumByMay (compare `on` _baked_time) <$> -} concat [_report_last_baked rpt, _report_last_seen rpt]

checkChainHealth
  :: MonadIO m
  => UTCTime
  -> Int -- ^ max unseen age, in seconds
  -> Baked
  -> NodeRPCT m ForkInfo
checkChainHealth now delay seenBaked = do
    addr <- asks (Node . _nodeRPCContext_node)
    x <- go
    return $ ForkInfo addr x seenBaked
  where
    go = (factorResponse <$> (doRPC $ Block $ BlockHash "head")) >>= \case
      Left bad -> (liftIO $ putStrLn "no head") >> (return $ ForkStatus_BadNode bad)
      Right head -> do
        -- liftIO $ putStrLn ("head:" <> show head)
        (factorResponse <$> (doRPC $ Block $ _baked_hash seenBaked)) >>= \case
          Left (RpcResponse_UnexpectedStatus (Status 404 _)) -> do
            let maxTime = addUTCTime (- fromIntegral delay) now
            if (_baked_time seenBaked >= maxTime)
            then return ForkStatus_TooNew
            else return ForkStatus_TooOld
          Left bad -> (liftIO $ putStrLn "not seen") >> (return $ ForkStatus_BadNode bad)
          Right seen -> do
            -- liftIO $ putStrLn ("seen:" <> show seen)
            let ancestorBlockHash = (BlockHash $ (unBlockHash $ _blockInfo_hash head) <> "~" <> T.pack (show (_blockInfo_level head - _blockInfo_level seen)))
            (factorResponse <$> (doRPC $ Block $ ancestorBlockHash)) >>= \case
              Left bad -> (liftIO $ putStrLn "no ancestor") >> (return $ ForkStatus_BadNode bad)
              Right ancestor -> do
                -- liftIO $ putStrLn ("ancestor:" <> show ancestor)
                if _blockInfo_predecessor seen == _blockInfo_predecessor ancestor
                then return ForkStatus_Good
                else return ForkStatus_Forked


clientWorker :: (MonadIO m)
             => [Node] -- [(Id Node, Text)]
             -> Email -- email address of user to notify about errors
             -> Int -- delay between checking for updates, in seconds
             -> Pool Postgresql
             -> m (IO ())
clientWorker nodes toAddr delay db = do
  lastErrorRef <- liftIO $ newIORef Nothing
  worker (seconds delay) $ do
    putStrLn "Update cycle."
    runNoLoggingT . runDb (Identity db) $ do
      now <- getTime
      let maxTime = Just (addUTCTime (- fromIntegral delay) now)
      -- nodes :: [(Id Node, Text)] <- [queryQ| SELECT id, address FROM "Node" |]

      toUpdate <- [queryQ| SELECT id, address
                           FROM "Client"
                           WHERE updated < ?maxTime OR updated IS NULL
                           ORDER BY updated NULLS FIRST |]
      forM_ toUpdate $ \(cid :: Id Client, address :: Text) -> do
        liftIO $ T.putStrLn address
        request <- parseRequest ("http://" <> T.unpack address <> "/")
        response <- httpJSON request
        -- liftIO $ print response
        let report = getResponseBody response :: Report
            reportJson = Json report
        case maximumByMay (compare `on` _baked_time) $ _report_last_seen report of
          -- Nothing -> liftIO $ mailFor toAddr ("baker " <> address <> " has not seen a block!")
          -- TODO: configurable timeout
          Just b -> when (addUTCTime (fromIntegral 30) (_baked_time b) < now) $ void $ queueEmail (mailFor toAddr $ [Error now ("baker " <> address <> " has not seen a block recently!\n" <> T.pack (show b))]) Nothing

        _ <- [executeQ| INSERT INTO "ClientInfo" (client, report)
                        VALUES (?cid, ?reportJson)
                        ON CONFLICT (client) DO UPDATE SET report = ?reportJson |]
        asdf <- traverse (cromulent1 now report) nodes -- (Node . snd <$> nodes)
        liftIO $looksCromulent $ concat $ asdf
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
  nodes :: [Node] <- getConfig "config/nodes"
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

    addFinalizer =<< clientWorker nodes userEmailAddress 10 db

    liftIO (quickHttpServe $ route
      [ ("", rootHandler cfg)
      , ("/listen", handleListen)
      , ("static", serveAssets "static" "static")
      ]) `finally` join (readTVarIO finalizers)

rootHandler :: MonadSnap m => ByteString -> m ()
rootHandler cfg = do
  serveApp "" $ def
    & appConfig_initialHead .~ Just cfg

