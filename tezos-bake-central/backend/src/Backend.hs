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
import Control.Exception (catch, finally, throwIO)
import Control.Lens ((.~), (<&>), (^.))
import Control.Monad (join, unless, void, when, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Default (def)
import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (for_, toList)
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
import Data.Traversable (for)
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
import System.IO.Error (isDoesNotExistError)
import qualified Web.ClientSession as CS

import Backend.ChainHealth (scanForkInfo, validateForkyBlocks)
import Backend.NodeRPC (NodeRPCContext (..), runNodeRPCT)
import Backend.NotifyHandler (notifyHandler)
import Backend.RequestHandler
import Backend.Schema
import Backend.ViewSelectorHandler (viewSelectorHandler)
import Common (tshow)
import Common.Base16ByteString (unbase16ByteString)
import Common.Json (TezosWord64 (..))
import Common.Operation (sumFees)
import Common.Schema
import Common.TaggedHash (toBase58Text)
import Frontend (frontend)

seconds :: Int -> Int
seconds = (* 10^(6 :: Int))

mailFor :: Address -> Text -> [Error] -> Mail
mailFor fromAddr toAddr errs =
  let
    toA = Address Nothing toAddr
    body = TL.fromStrict . T.unlines $ [T.pack (show t) <> ": " <> e | Error t e <- errs]
  in simpleMail' toA fromAddr "Error from Tezos bake monitor" body

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
      heads <- for nodes $ \(nodeId :: Id Node, nodeAddr) -> do
        say $ "Updating node at " <> nodeAddr
        let ctx = NodeRPCContext httpMgr nodeAddr -- "http://127.0.0.1:18731"
        params <- runNodeRPCT ctx $ nodeRPC RProtoConstants
        for_ params $ \protoInfo -> do
          [queryQ| SELECT id FROM "Parameters" WHERE node = ?nodeId |] >>= \case
            (Only (pid :: Id Parameters): _) ->
              updateAndNotify pid [Parameters_protoInfoField =. protoInfo]
            _ ->
              insertAndNotify_ $ Parameters {_parameters_node = nodeId, _parameters_protoInfo = protoInfo}
        headBlockRsp <- runNodeRPCT ctx . nodeRPC $ RBlock headId
        for_ headBlockRsp $ \headBlockInfo -> do
          updateAndNotify nodeId
            [ Node_headLevelField =. Just (unTezosWord64 $ headBlockInfo ^. blockInfo_header . blockInfoHeader_level)
            , Node_fitnessField =. Just (headBlockInfo ^. blockInfo_header . blockInfoHeader_fitness)
            ]
        return (nodeAddr, headBlockRsp)
      let heads' = toList =<< fmap (\(x, ys) -> fmap ((,) x) ys) heads
          headMaybe = maximumByMay (on compare $ _blockInfoHeader_fitness . _blockInfo_header . snd) heads'
      case headMaybe of
        Nothing -> say "no visible nodes"
        Just (nodeAddr, blockInfo) -> for_ clients $ \(clientInfoId, Json ci) -> do
          let ctx = NodeRPCContext httpMgr nodeAddr -- "http://127.0.0.1:18731"
          let headHash = _blockInfo_hash blockInfo
          runNodeRPCT ctx $ for_ (_clientConfig_delegates ci) $ \delegate -> do
            accountResp <- nodeRPC (RContract (blockHashId headHash) delegate)
            for_ accountResp $ \account -> do
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

queueAllEmails :: (PersistBackend m, PostgresLargeObject m, MonadIO m) => Address -> [Error] -> m ()
queueAllEmails fromAddr message = do
  ns <- select CondEmpty
  for_ ns $ \n ->
    queueEmail (mailFor fromAddr (_notificatee_email n) message) Nothing

clientWorker :: (MonadIO m)
             => Int -- delay between checking for updates, in seconds
             -> Address
             -> Http.Manager
             -> Pool Postgresql
             -> m (IO ())
clientWorker delay emailFromAddress httpMgr db = do
  lastErrorRef <- liftIO $ newIORef Nothing
  worker (seconds delay) $ do
    say "Update client cycle."
    runNoLoggingT $ runDb (Identity db) $ do
      now <- getTime
      let maxTime = Just (addUTCTime (- fromIntegral delay) now)
      params :: Maybe Parameters <- listToMaybe <$> select (CondEmpty `limitTo` 1) -- TODO, take the newest
      allNodes  <- select $ Not $ isFieldNothing Node_fitnessField
      for_ ( maximumByMay (on compare _node_fitness) allNodes ) $ \bestNode -> do
        let blockHeightTimeout :: NominalDiffTime = fromIntegral
              $ maybe 600 (max 15 . (5*) . sum . take 3 . toList . _protoInfo_timeBetweenBlocks . _parameters_protoInfo) params

        toUpdate <- [queryQ| SELECT id, address
                             FROM "Client"
                             WHERE updated < ?maxTime OR updated IS NULL
                             ORDER BY updated NULLS FIRST |]
        mLevelAndProto <- getLatestProtoInfo

        for_ toUpdate $ \(cid :: Id Client, address :: Text) -> do
          say $ "Updating client at " <> address
          -- TODO: abstract this into a ClientRPC like the way there's a NodeRPC
          clientConfig :: ClientConfig <- fmap Http.getResponseBody $ Http.httpJSON =<< Http.parseRequest (T.unpack address <> "/config")
          let clientConfigJson = Json clientConfig

          report :: Report <- fmap Http.getResponseBody $ Http.httpJSON =<< Http.parseRequest (T.unpack address <> "/events")
          let reportJson = Json report

          case maximumMay $ fmap _event_time $ _report_seen report of
            Nothing -> return ()
            Just b -> when (addUTCTime blockHeightTimeout b < now) $
              void $ queueAllEmails emailFromAddress
                [Error now ("baker " <> address <> " has not seen a block recently!\nLast block was at " <> T.pack (show b) <> ".")]

          for_ mLevelAndProto $ \(_headLevel, protoInfo) -> do
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

          _ <- [executeQ| INSERT INTO "ClientInfo" (client, report, config)
                          VALUES (?cid, ?reportJson, ?clientConfigJson)
                          ON CONFLICT (client) DO UPDATE SET
                            report = ?reportJson
                          , config = ?clientConfigJson
                          |]
          forkInfo <- scanForkInfo httpMgr now report bestNode
          validateForkyBlocks sayShow forkInfo

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
                  queueAllEmails emailFromAddress new
          -- TODO.  debounce below as above
          flip validateForkyBlocks forkInfo $ \errors ->
            queueAllEmails emailFromAddress errors


backend :: IO ()
backend = do
  hSetBuffering stderr LineBuffering -- Decrease likelihood of output from multiple threads being interleaved
  csk <- CS.getKey "config/clientSessionKey"

  let cfg0 = SnapServer.defaultConfig & SnapServer.setOther mempty
  cfg <- SnapServer.extendedCommandLineConfig (SnapServer.optDescrs cfg0 <> optsArgDescr) (<>) cfg0

  let emailFromAddress = Address (Just "Tezos Bake Monitor") . fromMaybe "noreply@obsidian.systems" $
        _opts_emailFromAddress =<< SnapServer.getOther cfg

  routeEnv :: Maybe RouteEnv <- case _opts_route =<< SnapServer.getOther cfg of
    Nothing -> getConfigFromFile "config/route"
    Just env -> pure $ Just $ uriToRouteEnv env

  routeHead <- for routeEnv $ \env ->
    snd <$> renderStatic (injectPure "route" $ decodeUtf8 $ LBS.toStrict $ Aeson.encode env)
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
      (requestHandler csk emailFromAddress httpMgr db)
      (notifyHandler db)
      (viewSelectorHandler csk db)
      (RhyoliteApp.queryMorphismPipeline $ RhyoliteApp.transposeMonoidMap . RhyoliteApp.monoidMapQueryMorphism)
    addFinalizer wsFinalizer

    addFinalizer =<< nodeWorker 30 httpMgr db
    addFinalizer =<< clientWorker 10 emailFromAddress httpMgr db

    SnapServer.httpServe cfg (route
      [ ("", rootHandler $ fromMaybe mempty routeHead <> frontendHead)
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
    getDefaultMailServer <&> \case
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


getConfigFromFile :: (FromJSON a) => FilePath -> IO (Maybe a)
getConfigFromFile f = (rightToMaybe . Aeson.eitherDecode <$> LBS.readFile f)
  `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e


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
  , _opts_route :: Maybe URI
  , _opts_emailFromAddress :: Maybe Text
  }

instance Semigroup Opts where
  a <> b = Opts -- Right biased
    { _opts_pgConnectionString = _opts_pgConnectionString b <|> _opts_pgConnectionString a
    , _opts_route = _opts_route b <|> _opts_route a
    , _opts_emailFromAddress = _opts_emailFromAddress b <|> _opts_emailFromAddress a
    }

instance Monoid Opts where
  mempty = Opts Nothing Nothing Nothing
  mappend = (<>)

optsArgDescr :: MonadSnap m => [OptDescr (Maybe (SnapServer.Config m Opts))]
optsArgDescr =
  [ Option [] ["pg-connection"] (mkReqArg "CONNSTRING" $ \x -> mempty { _opts_pgConnectionString = Just $ T.pack x })
      "Connection string or URI to PostgreSQL database. If blank, use connection string in 'db' file or create a database there if empty."
  , Option [] ["route"] (mkReqArg "URL" $ \x -> mempty { _opts_route = Just $ parseUrlOpt x })
      "Root URL for this service as seen by external users. If blank, use contents of 'config/route'."
  , Option [] ["email-from"] (mkReqArg "EMAIL" $ \x -> mempty { _opts_emailFromAddress = Just $ T.pack x })
      "Email address to use for 'From' field in email notifications."
  ]
  where
    mkReqArg var f = ReqArg (\x -> Just $ SnapServer.setOther (f x) mempty) var
    parseUrlOpt x = fromMaybe (error $ x <> " is not a valid URL") $ Uri.parseURI x
