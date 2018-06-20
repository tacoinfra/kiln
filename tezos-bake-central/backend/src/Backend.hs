{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Backend where

import Control.Applicative ((<|>))
import Control.Category ((.))
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Exception (finally)
import Control.Lens ((.~), (^.))
import Control.Monad (forM, forM_, join, unless, void, when, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Default (def)
import Data.Foldable (toList)
import Data.Function (on, (&))
import Data.Functor.Identity (Identity (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup (Semigroup, Sum (..), getSum, (<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Data.Word (Word64)
import Database.Groundhog.Generic.Migration (getTableAnalysis)
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Https
import qualified Network.HTTP.Simple as Http
import Network.Mail.Mime (Address (..), Mail, simpleMail')
import Network.URI (URI)
import qualified Network.URI as Uri
import Obelisk.Asset.Serve.Snap (serveAssets)
import Obelisk.ExecutableConfig.Inject (injectPure)
import Prelude hiding ((.))
import Reflex.Dom.Core (renderStatic)
import Rhyolite.Backend (withDb)
import Rhyolite.Backend.Account (migrateAccount)
import qualified Rhyolite.Backend.App as RhyoliteApp
import Rhyolite.Backend.DB (RunDb, getTime, openDb, runDb)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (Only (..), PostgresRaw, Values (..), executeQ, queryQ)
import qualified Rhyolite.Backend.Email as RhyoliteEmail
import Rhyolite.Backend.EmailWorker (clearMailQueue, migrateQueuedEmail, queueEmail)
import Rhyolite.Backend.Listen (insertAndNotify, insertAndNotify_, updateAndNotify)
import Rhyolite.Backend.Snap (appConfig_initialHead, serveApp)
import Rhyolite.Concurrent (worker)
import Rhyolite.Route (RouteEnv)
import Rhyolite.Schema (Id, Json (..))
import Safe (maximumByMay, maximumMay)
import Say (say, sayShow)
import Snap.Core (MonadSnap, route)
import qualified Snap.Http.Server as SnapServer
import Snap.Util.FileServe (serveDirectory)
import System.Console.GetOpt (ArgDescr (ReqArg), OptDescr (Option))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr)
import qualified Web.ClientSession as CS

import Backend.ChainHealth (obtainNode, scanForkInfo, validateForkyBlocks)
import Backend.NodeRPC (NodeRPCContext (..), runNodeRPCT)
import Backend.NotifyHandler (notifyHandler)
import Backend.RequestHandler
import Backend.Schema
import Backend.ViewSelectorHandler (viewSelectorHandler)
import Common.Base16ByteString (unbase16ByteString)
import Common.Json (TezosWord64 (..))
import Common.Operation (sumFees)
import Common.Schema
import Common.TaggedHash (toBase58Text)
import Frontend (frontend)

seconds :: Int -> Int
seconds = (* 10^(6 :: Int))

mailFor :: Text -> [Error] -> Mail
mailFor toAddr errs =
  let fromA = Address (Just "Tezos Bake Monitor") "noreply@obsidian.systems"
      toA = Address Nothing toAddr
      body = TL.fromStrict . T.unlines $ [T.pack (show t) <> ": " <> e | Error t e <- errs]
  in simpleMail' toA fromA "Error from Tezos bake monitor" body

addNode
  :: (PostgresRaw m, Monad m, PersistBackend m)
  => Node
  -> m (Id Node)
addNode node = do
  let addr = _node_address node
  [queryQ| SELECT id FROM "Node" WHERE address = ?addr |] >>= \case
    (Only (nodeId :: Id Node):_) -> do
      updateAndNotify nodeId
        [ Node_addressField =. addr
        , Node_headLevelField =. _node_headLevel node
        , Node_peerCountField =. _node_peerCount node
        , Node_networkStatField =. _node_networkStat node
        ]
      return nodeId
    _ -> insertAndNotify node

nodeWorker
  :: (MonadIO m)
  => Int -- delay between checking for updates, in seconds
  -> Http.Manager
  -> Pool Postgresql
  -> m (IO ())
nodeWorker delay httpMgr db = do
  worker (seconds delay) $ do
    say "Update node cycle."
    runNoLoggingT . runDb (Identity db) $ do
      nodes <- [queryQ| SELECT id, address FROM "Node" |]

      clients :: [(Id ClientInfo, Json ClientConfig)] <- [queryQ| SELECT id, config FROM "ClientInfo" |]
      heads <- forM nodes $ \(nodeId :: Id Node, nodeAddr) -> do
        let ctx = NodeRPCContext httpMgr nodeAddr -- "http://127.0.0.1:18731"
        params <- runNodeRPCT ctx $ nodeRPC RProtoConstants
        forM_ params $ \protoInfo -> do
          [queryQ| SELECT id FROM "Parameters" WHERE node = ?nodeId |] >>= \case
            (Only (pid :: Id Parameters): _) ->
              updateAndNotify pid [Parameters_protoInfoField =. protoInfo]
            _ ->
              insertAndNotify_ $ Parameters {_parameters_node = nodeId, _parameters_protoInfo = protoInfo}
        headBlockRsp <- runNodeRPCT ctx . nodeRPC $ RBlock headId
        forM_ headBlockRsp $ \headBlockInfo -> do
          updateAndNotify nodeId [Node_headLevelField =. Just (unTezosWord64 $ headBlockInfo ^. blockInfo_header . blockInfoHeader_level) ]
        return (nodeAddr, headBlockRsp)
      let heads' = toList =<< fmap (\(x, ys) -> fmap ((,) x) ys) heads
          headMaybe = maximumByMay (on compare $ _blockInfoHeader_fitness . _blockInfo_header . snd) heads'
      case headMaybe of
        Nothing -> say "no visible nodes"
        Just (nodeAddr, blockInfo) -> forM_ clients $ \(clientInfoId, Json ci) -> do
          let ctx = NodeRPCContext httpMgr nodeAddr -- "http://127.0.0.1:18731"
          let headHash = _blockInfo_hash blockInfo
          runNodeRPCT ctx $ forM_ (_clientConfig_delegates ci) $ \delegate -> do
            accountResp <- nodeRPC (RContract (blockHashId headHash) delegate)
            forM_ accountResp $ \account -> do
              let balance = _account_balance account
              void $ [executeQ| UPDATE "ClientInfo"
                                SET balance = ?balance
                                WHERE id = ?clientInfoId
                              |]

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

queueAllEmails :: (PersistBackend m, PostgresLargeObject m, MonadIO m) => [Error] -> m ()
queueAllEmails message = do
  ns <- selectAll
  forM_ ns $ \(_, n) ->
    queueEmail (mailFor (_notificatee_email n) message) Nothing

clientWorker :: (MonadIO m)
             => Int -- delay between checking for updates, in seconds
             -> Http.Manager
             -> Pool Postgresql
             -> m (IO ())
clientWorker delay httpMgr db = do
  lastErrorRef <- liftIO $ newIORef Nothing
  worker (seconds delay) $ do
    say "Update client cycle."
    runNoLoggingT $ runDb (Identity db) $ do
      now <- getTime
      let maxTime = Just (addUTCTime (- fromIntegral delay) now)
      params :: [Parameters] <- fmap snd <$> selectAll -- TODO, take the newest
      let blockHeightTimeout :: NominalDiffTime = fromIntegral
            $ maybe 600 (max 15 . (5*) . sum . take 3 . toList . _protoInfo_timeBetweenBlocks . _parameters_protoInfo )
            $ listToMaybe params
      toUpdate <- [queryQ| SELECT id, address
                           FROM "Client"
                           WHERE updated < ?maxTime OR updated IS NULL
                           ORDER BY updated NULLS FIRST |]
      mLevelAndProto <- getLatestProtoInfo

      forM_ toUpdate $ \(cid :: Id Client, address :: Text) -> do
        say address
        -- TODO: abstract this into a ClientRPC like the way there's a NodeRPC
        configRequest <- Http.parseRequest ("http://" <> T.unpack address <> "/config")
        configResponse <- Http.httpJSON configRequest
        let clientConfig = Http.getResponseBody configResponse :: ClientConfig
            clientConfigJson = Json clientConfig
            clientNodeRPCContext = NodeRPCContext httpMgr (_clientConfig_nodeUri clientConfig)

        (_, node) <- runNodeRPCT clientNodeRPCContext obtainNode
        nodeId <- addNode node

        request <- Http.parseRequest ("http://" <> T.unpack address <> "/events")
        response <- Http.httpJSON request
        let report = Http.getResponseBody response :: Report
            reportJson = Json report

        case maximumMay $ fmap _event_time $ _report_seen report of
          Nothing -> return ()
          Just b -> when (addUTCTime blockHeightTimeout b < now) $
            void $ queueAllEmails [Error now ("baker " <> address <> " has not seen a block recently!\nLast block was at " <> T.pack (show b) <> ".")]

        forM_ mLevelAndProto $ \(_headLevel, protoInfo) -> do
          let bakingReward blk = _protoInfo_blockReward protoInfo + getSum ((foldMap . foldMap) (Sum . sumFees . unbase16ByteString . _bakedEventOperation_data) (_bakedEvent_operations $ _event_detail blk))
              rewardDelay l =
                let c = fromIntegral l `div` _protoInfo_blocksPerCycle protoInfo + 1
                    rc = c + _protoInfo_preservedCycles protoInfo

                in rc * _protoInfo_blocksPerCycle protoInfo
              insertValues = Values ["int8", "varchar", "int8", "int8"]
                [(cid, toBase58Text (_bakedEvent_hash $ _event_detail b), rewardDelay (blockLevel b) , bakingReward b) | b <- _report_baked report]
          unless (null $ _report_baked report) $ do
            void $ [executeQ| INSERT INTO "PendingReward" (client, hash, level, amount)
                            ?insertValues
                            ON CONFLICT DO NOTHING |]

        _ <- [executeQ| INSERT INTO "ClientInfo" (client, report, config, node)
                        VALUES (?cid, ?reportJson, ?clientConfigJson, ?nodeId)
                        ON CONFLICT (client) DO UPDATE SET
                          report = ?reportJson
                        , config = ?clientConfigJson
                        , node = ?nodeId |]
        forkInfo <- mapM (scanForkInfo httpMgr now report) [node]
        liftIO $ validateForkyBlocks sayShow $ concat forkInfo

        updateAndNotify cid [Client_updatedField =. Just now]
        case sortBy (compare `on` _event_time) (_report_errors report) of
          [] -> return ()
          es -> do
            lastError <- liftIO $ readIORef lastErrorRef
            let (new,_) = span ((>= lastError) . Just . _error_time) (mkErr <$> es)
            case new of
              [] -> return ()
              (x:_) -> do
                liftIO $ writeIORef lastErrorRef (Just $ _error_time x)
                _ <- queueAllEmails new
                return ()
        -- TODO.  debounce below as above
        flip validateForkyBlocks (concat forkInfo) $ \errors ->
          void $ queueAllEmails errors

backend :: IO ()
backend = do
  hSetBuffering stderr LineBuffering -- Decrease likelihood of output from multiple threads being interleaved
  csk <- CS.getKey "config/clientSessionKey"

  let cfg0 = SnapServer.defaultConfig & SnapServer.setOther mempty
  cfg <- SnapServer.extendedCommandLineConfig (SnapServer.optDescrs cfg0 <> optsArgDescr) (<>) cfg0

  routeEnv <- maybe (getConfigFromFile "config/route") (pure . uriToRouteEnv) $ _opts_rootUrl =<< SnapServer.getOther cfg
  routeHead <- snd <$> renderStatic (injectPure "route" $ decodeUtf8 $ LBS.toStrict $ Aeson.encode routeEnv)
  frontendHead <- snd <$> renderStatic (fst frontend)

  let pgConnStr = _opts_pgConnectionString =<< SnapServer.getOther cfg
  withGargoyleOrConnStr (maybe (Left "db") Right pgConnStr) $ \db -> do
    runNoLoggingT $ runDb (Identity db) $ do
      tableInfo <- getTableAnalysis
      runMigration $ do
        migrateAccount tableInfo
        migrateQueuedEmail tableInfo
        migrateSchema tableInfo

    finalizers <- newTVarIO (return ())
    let addFinalizer f = atomically $ modifyTVar finalizers (f >>)

    -- Start a thread to send queued emails
    addFinalizer <=< worker (seconds 10) $ runNoLoggingT (clearMailQueueWithDynamicEmailEnv $ Identity db)

    httpMgr <- Http.newManager Https.tlsManagerSettings

    (handleListen, wsFinalizer) <- RhyoliteApp.serveDbOverWebsockets db
      (requestHandler csk httpMgr db)
      (notifyHandler db)
      (viewSelectorHandler csk db)
      (RhyoliteApp.queryMorphismPipeline $ RhyoliteApp.transposeMonoidMap . RhyoliteApp.monoidMapQueryMorphism)
    addFinalizer wsFinalizer

    addFinalizer =<< nodeWorker 30 httpMgr db
    addFinalizer =<< clientWorker 10 httpMgr db

    SnapServer.httpServe cfg (route
      [ ("", rootHandler $ routeHead <> frontendHead)
      , ("/listen", handleListen)
      , ("static", serveAssets "static" "static")
      , ("", serveDirectory "frontend.jsexe")
      ]) `finally` join (readTVarIO finalizers)

rootHandler :: MonadSnap m => ByteString -> m ()
rootHandler pageHead =
  serveApp "" $ def
    & appConfig_initialHead .~ Just pageHead


clearMailQueueWithDynamicEmailEnv
  :: forall m f.
  ( RunDb f
  , MonadIO m
  , MonadBaseControl IO m
  , MonadLogger m
  )
  => f (Pool Postgresql)
  -> m ()
clearMailQueueWithDynamicEmailEnv db = do
  emailEnv <- runDb db $ do
    defaultMailServer <- getDefaultMailServer
    pure $ case defaultMailServer of
      Nothing -> error "No mail server configuration found"
      Just (_, c) ->
        ( T.unpack $ _mailServerConfig_hostName c
        , case _mailServerConfig_smtpProtocol c of
          SmtpProtocol_Plain -> RhyoliteEmail.SMTPProtocol_Plain
          SmtpProtocol_Ssl -> RhyoliteEmail.SMTPProtocol_SSL
          SmtpProtocol_Starttls -> RhyoliteEmail.SMTPProtocol_STARTTLS
        , fromIntegral (_mailServerConfig_portNumber c)
        , T.unpack $ _mailServerConfig_userName c
        , T.unpack $ _mailServerConfig_password c
        )

  clearMailQueue db emailEnv


getConfigFromFile :: (FromJSON a, MonadIO m) => FilePath -> m a
getConfigFromFile f = either error id . Aeson.eitherDecode <$> liftIO (LBS.readFile f)


withGargoyleOrConnStr :: Either FilePath Text -> (Pool Postgresql -> IO a) -> IO a
withGargoyleOrConnStr cfg f = case cfg of
  Left dbPath -> withDb dbPath f
  Right connStr -> f =<< openDb (encodeUtf8 connStr)


uriToRouteEnv :: URI -> RouteEnv
uriToRouteEnv uri =
  ( Uri.uriScheme uri
  , Uri.uriRegName authority
  , Uri.uriPort authority <> Uri.uriPath uri
    <> mustBeNull "query" (Uri.uriQuery uri)
    <> mustBeNull "fragment" (Uri.uriFragment uri)
  )
  where
    authority = fromMaybe (error "URI must have a host") $ Uri.uriAuthority uri
    mustBeNull thing x = if null x then "" else error ("URL " <> thing <> " must be empty")

data Opts = Opts
  { _opts_pgConnectionString :: Maybe Text
  , _opts_rootUrl :: Maybe URI
  }

instance Semigroup Opts where
  a <> b = Opts -- Right biased
    { _opts_pgConnectionString = _opts_pgConnectionString b <|> _opts_pgConnectionString a
    , _opts_rootUrl = _opts_rootUrl b <|> _opts_rootUrl a
    }

instance Monoid Opts where
  mempty = Opts Nothing Nothing
  mappend = (<>)

optsArgDescr :: MonadSnap m => [OptDescr (Maybe (SnapServer.Config m Opts))]
optsArgDescr =
  [ Option [] ["pg-connection"] (mkReqArg "CONNSTRING" $ \x -> mempty { _opts_pgConnectionString = Just $ T.pack x })
      "Connection string or URI to PostgreSQL database. If blank, use connection string in 'db' file or create a database there if empty."
  , Option [] ["root-url"] (mkReqArg "URL" $ \x -> mempty { _opts_rootUrl = Just $ parseUrlOpt x })
      "Root URL for this service as seen by external users. If blank, use contents of 'config/route'."
  ]
  where
    mkReqArg var f = ReqArg (\x -> Just $ SnapServer.setOther (f x) mempty) var
    parseUrlOpt x = fromMaybe (error $ x <> " is not a valid URL") $ Uri.parseURI x
