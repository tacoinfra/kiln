{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.AppendMap as AMap
import Data.Default
import Data.Either.Validation
import Data.Function (on)
import Data.Foldable
import Data.IORef
import Data.List hiding (head)
import Data.Monoid
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
import Prelude hiding (head, id, (.))
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


data ForkInfo = ForkInfo
  { _forkInfo_node :: Node
  , _forkInfo_forkStatus :: ForkStatus
  , _forkInfo_baked :: Baked
  }

data ForkStatus
  = ForkStatus_Good
  | ForkStatus_TooNew
  | ForkStatus_TooOld
  | ForkStatus_Forked
  | ForkStatus_BadNode (RpcResponse Void)


showForkStatus :: ForkStatus -> Text
showForkStatus = T.pack . \case
  ForkStatus_Good -> "good"
  ForkStatus_TooNew -> "new"
  ForkStatus_TooOld -> "block not in chain"
  ForkStatus_Forked -> "forked"
  ForkStatus_BadNode _ -> "no response from node"

onBadForkState :: (ForkInfo -> a) -> ForkInfo -> Validation a ()
onBadForkState k fi = case _forkInfo_forkStatus fi of
  ForkStatus_TooOld -> Failure $ k fi
  ForkStatus_Forked -> Failure $ k fi
  _ -> Success ()

showBadFork :: ForkInfo -> [Error]
showBadFork (ForkInfo node status baked) = pure $ Error (_baked_time baked) $ T.concat
          [ "node: ", _node_address node
          , " BAKER STATE:" , showForkStatus status
          , " for block:", unBlockHash $ _baked_hash baked
          , " @ ",  T.pack $ show $ _baked_time baked
          , "\n"
          ]

validateForkyBlocks :: Applicative f => ([Error] -> f ()) -> [ForkInfo] -> f ()
validateForkyBlocks f xs = case traverse (onBadForkState (showBadFork)) xs of
  Success _ -> pure ()
  Failure bad -> f bad


factorResponse :: RpcResponse a -> Either (RpcResponse Void) a
factorResponse (RpcResponse_HttpException bad) = Left $ RpcResponse_HttpException bad
factorResponse (RpcResponse_UnexpectedStatus bad) = Left $ RpcResponse_UnexpectedStatus bad
factorResponse (RpcResponse_NonJSON clue bad) = Left $ RpcResponse_NonJSON clue bad
factorResponse (RpcResponse_Success ok) = Right ok

scanForkInfo :: MonadIO m => UTCTime -> Report -> Node -> m [ForkInfo]
scanForkInfo now rpt node = do
  httpMgr <- liftIO $ newManager tlsManagerSettings
  let ctx = NodeRPCContext httpMgr $ _node_address node -- "http://127.0.0.1:18731"
  -- traverse (flip runReaderT ctx . checkChainHealth now 30) $ concat [_report_last_baked rpt, _report_last_seen rpt]
  traverse (flip runReaderT ctx . checkChainHealth now 30) $ _report_last_baked rpt

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
addSomeNodes
  :: (MonadBaseControl IO m, MonadIO m)
  => [Node]
  -> Pool Postgresql
  -> m ()
addSomeNodes nodes db = void . runNoLoggingT . runDb (Identity db) $ do
  flip traverse nodes $ \(Node addr) -> [queryQ| SELECT id FROM "Node" WHERE address = ?addr |] >>= \case
    (Only (nodeId :: Id Node):_) -> updateAndNotify nodeId [Node_addressField =. addr]
    _ -> insertAndNotify_ $ Node {_node_address = addr}


nodeWorker
  :: (MonadIO m)
  => Int -- delay between checking for updates, in seconds
  -> Pool Postgresql
  -> m (IO ())
nodeWorker delay db = do
  httpMgr <- liftIO $ newManager tlsManagerSettings
  worker (seconds delay) $ do
    putStrLn "Update cycle."
    runNoLoggingT . runDb (Identity db) $ do
      nodes <- [queryQ| SELECT id, address FROM "Node" |]
      flip traverse nodes $ \(nodeId :: Id Node, nodeAddr) -> do
        let ctx = NodeRPCContext httpMgr nodeAddr -- "http://127.0.0.1:18731"
        params <- flip runReaderT ctx $ doRPC ProtoConstants
        flip traverse params $ \protoInfo -> do
          [queryQ| SELECT id FROM "Parameters" WHERE node = ?nodeId |] >>= \case
            (Only (pid :: Id Parameters): _) ->
              updateAndNotify pid [Parameters_protoInfoField =. protoInfo]
            _ ->
              insertAndNotify_ $ Parameters {_parameters_node = nodeId, _parameters_protoInfo = protoInfo}



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
          Nothing -> return () -- liftIO $ mailFor toAddr ("baker " <> address <> " has not seen a block!")
          -- TODO: configurable timeout
          Just b -> when (addUTCTime (fromInteger 30) (_baked_time b) < now) $ void $ queueEmail (mailFor toAddr $ [Error now ("baker " <> address <> " has not seen a block recently!\n" <> T.pack (show b))]) Nothing

        _ <- [executeQ| INSERT INTO "ClientInfo" (client, report)
                        VALUES (?cid, ?reportJson)
                        ON CONFLICT (client) DO UPDATE SET report = ?reportJson |]
        forkInfo <- traverse (scanForkInfo now report) nodes -- (Node . snd <$> nodes)
        liftIO $ validateForkyBlocks (putStrLn . show) $ concat $ forkInfo

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
        -- TODO.  debounce below as above
        flip validateForkyBlocks (concat $ forkInfo) $ \errors -> do
          void $ queueEmail (mailFor toAddr errors) Nothing

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

    -- TODO: in the real thing, users should manage their own list of nodes,
    -- with some bootstrapping by using the well known address
    addSomeNodes nodes db


    (handleListen, wsFinalizer) <- serveDbOverWebsockets db
      (requestHandler csk db)
      (notifyHandler db)
      (viewSelectorHandler csk db)
      (queryMorphismPipeline $ transposeMonoidMap . monoidMapQueryMorphism)
    addFinalizer wsFinalizer

    addFinalizer =<< nodeWorker 30 db
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

