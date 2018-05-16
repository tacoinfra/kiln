{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Category ((.))
import Control.Concurrent.STM
import Control.Lens
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Default
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
import Data.Word
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
import Network.Mail.Mime
import Obelisk.Asset.Serve.Snap
import Obelisk.ExecutableConfig.Inject (inject)
import Prelude hiding (head, id, (.))
import qualified Web.ClientSession as CS
import Snap

import Tezos.BakeMonitor.Types
import Tezos.NodeRPC

import Backend.RequestHandler
import Backend.NotifyHandler
import Backend.ViewSelectorHandler
import Backend.Schema
import Backend.ChainHealth
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

addSomeNodes
  :: (MonadBaseControl IO m, MonadIO m)
  => [Node]
  -> Pool Postgresql
  -> m ()
addSomeNodes nodes db = void . runNoLoggingT . runDb (Identity db) $ do
  forM_ nodes $ \n@(Node addr level) -> [queryQ| SELECT id FROM "Node" WHERE address = ?addr |] >>= \case
    (Only (nodeId :: Id Node):_) -> updateAndNotify nodeId [Node_addressField =. addr, Node_headLevelField =. level]
    _ -> insertAndNotify_ n

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
      forM nodes $ \(nodeId :: Id Node, nodeAddr) -> do
        let ctx = NodeRPCContext httpMgr nodeAddr -- "http://127.0.0.1:18731"
        params <- runNodeRPCT ctx $ nodeRPC ProtoConstants
        forM_ params $ \protoInfo -> do
          [queryQ| SELECT id FROM "Parameters" WHERE node = ?nodeId |] >>= \case
            (Only (pid :: Id Parameters): _) ->
              updateAndNotify pid [Parameters_protoInfoField =. protoInfo]
            _ ->
              insertAndNotify_ $ Parameters {_parameters_node = nodeId, _parameters_protoInfo = protoInfo}
        headBlockRsp <- runNodeRPCT ctx . nodeRPC $ Block (BlockHash "head")
        forM_ headBlockRsp $ \headBlockInfo -> do
          updateAndNotify nodeId [Node_headLevelField =. Just (_blockInfo_level headBlockInfo) ]

-- I'm fairly sure this is not 100% correct, but I'm also not 100% sure what the correct thing is. Which block's protocol constants should be
-- inspected when determining the rewards for a block which is baked? I'm basically assuming that the constants are sufficiently constant for now.
getLatestProtoInfo :: (Monad m, PersistBackend m, PostgresRaw m) => m (Maybe (Word64, ProtoInfo))
getLatestProtoInfo = do
  nodeIds <- [queryQ| SELECT n.id, n."headLevel"
                      FROM "Node" n LEFT JOIN "Parameters" p ON p.node = n.id
                      WHERE n."headLevel" IS NOT NULL
                      ORDER BY n."headLevel" DESC
                      LIMIT 1 |]
  case nodeIds of
    ((nid, headLevel):_) -> do
      rs <- project Parameters_protoInfoField $ (Parameters_nodeField ==. (nid :: Id Node)) `limitTo` 1
      return $ case rs of
        (info:_) -> Just (headLevel, info)
        _ -> Nothing
    [] -> return Nothing

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
      -- TODO: params and blockHeightTimeout were unused! Is this code simply deletable, or did we mean to do something with these values?
      -- params :: [Parameters] <- fmap snd <$> selectAll -- | TODO, take the newest
      -- let blockHeightTimeout :: Int = maybe 600 (max 15 . (5*) . sum . take 3 . toList . _protoInfo_timeBetweenBlocks . _parameters_protoInfo ) $ listToMaybe params
      toUpdate <- [queryQ| SELECT id, address
                           FROM "Client"
                           WHERE updated < ?maxTime OR updated IS NULL
                           ORDER BY updated NULLS FIRST |]
      mLevelAndProto <- getLatestProtoInfo

      forM_ toUpdate $ \(cid :: Id Client, address :: Text) -> do
        liftIO $ T.putStrLn address
        request <- parseRequest ("http://" <> T.unpack address <> "/")
        response <- httpJSON request
        -- liftIO $ print response
        let report = getResponseBody response :: Report
            reportJson = Json report

        case _report_lastSeen report of
          Nothing -> return ()
          -- TODO: configurable timeout
          Just b -> when (addUTCTime (fromInteger 30) b < now) $
            void $ queueEmail (mailFor toAddr $ [Error now ("baker " <> address <> " has not seen a block recently!\nLast block was at " <> T.pack (show b) <> ".")]) Nothing

        forM_ mLevelAndProto $ \(_headLevel, protoInfo) -> do
          let blockReward = _protoInfo_blockReward protoInfo
              rewardDelay l =
                let c = fromIntegral l `div` _protoInfo_blocksPerCycle protoInfo + 1
                    rc = c + _protoInfo_preservedCycles protoInfo
                in rc * _protoInfo_blocksPerCycle protoInfo
              insertValues = Values ["int8", "varchar", "int8", "int8"]
                [(cid, unBlockHash (_baked_hash b), rewardDelay (_baked_level b), blockReward) | b <- _report_lastBaked report]
          when (not . null $ _report_lastBaked report) $ do
            _ <- [executeQ| INSERT INTO "PendingReward" (client, hash, level, amount)
                            ?insertValues
                            ON CONFLICT DO NOTHING |]
            return ()
          return ()

        _ <- [executeQ| INSERT INTO "ClientInfo" (client, report)
                        VALUES (?cid, ?reportJson)
                        ON CONFLICT (client) DO UPDATE SET report = ?reportJson |]
        forkInfo <- mapM (scanForkInfo now report) nodes -- (Node . snd <$> nodes)
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

