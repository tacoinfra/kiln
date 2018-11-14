{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Backend where

import Control.Exception.Safe (catch, throwIO, throwString)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Logger (LoggingT (..), MonadLogger, logInfo, runStderrLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Dependent.Map (DSum (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Pool (Pool)
import qualified Data.Random as Random
import qualified Data.Random.Extras as Random
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Client as Http (newManager)
import qualified Network.HTTP.Client.TLS as Https
import Network.Mail.Mime (Address (..))
import Obelisk.Backend (Backend (..))
import Obelisk.ExecutableConfig.Inject (injectPure)
import Obelisk.Frontend
import Obelisk.Route (R)
import Reflex.Dom.Core (DomBuilder)
import qualified Rhyolite.Backend.App as RhyoliteApp
import Rhyolite.Backend.DB (RunDb, runDb)
import qualified Rhyolite.Backend.Email as RhyoliteEmail
import Rhyolite.Backend.EmailWorker (clearMailQueue)
import Rhyolite.Backend.Logging (LoggingConfig (..), LoggingEnv (..), RhyoliteLogAppender,
                                 RhyoliteLogLevel (..), runLoggingEnv, withLogging)
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as SnapServer
import qualified System.Console.GetOpt as GetOpt
import System.Environment (getArgs, getProgName, withArgs)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr)
import System.IO.Error (isDoesNotExistError)
import Text.URI (URI)
import qualified Text.URI as URI

import Backend.Db (gargoyleSupported, withDb)
import Tezos.Chain (mainnetChainId)
import Tezos.NodeRPC
import Tezos.NodeRPC.Sources (PublicNode (..), getPublicNodeUri)
import Tezos.Types

import Backend.CachedNodeRPC (blankNodeDataSource)
import Backend.Common (workerWithDelay)
import Backend.Config (AppConfig (..))
import Backend.Http (runHttpT)
import Backend.Migrations (migrateKiln)
import Backend.NotifyHandler (notifyHandler)
import Backend.RequestHandler (getDefaultMailServer, requestHandler)
import Backend.Schema
import Backend.Supervisor (withTermination)
import qualified Backend.Telegram as Telegram
import Backend.Upgrade (upgradeCheckWorker)
import Backend.Version (version)
import Backend.ViewSelectorHandler (viewSelectorHandler)
import Backend.WebApi (v1PublicApi)
import Backend.Workers.Cache (cacheWorker)
import Backend.Workers.Client (clientWorker)
import Backend.Workers.Delegate (delegateWorker)
import Backend.Workers.Node (DataSource, nodeAlertWorker, nodeWorker, publicNodesWorker)
import qualified Common.Config as Config
import Common.HeadTag (headTag)
import Common.Route (AppRoute, BackendRoute (..), backendRouteEncoder)
import Common.Schema
import Common.URI (mkRootUri)
import ExtraPrelude
import Frontend (frontend)

onRpcError :: (MonadError Text m, Show a) => Either a b -> m b
onRpcError = either (throwError . tshow) pure

askLogger :: Monad m => LoggingT m LoggingEnv
askLogger = LoggingT $ return . LoggingEnv

backendImpl :: Opts -> ((R BackendRoute -> Snap.Snap ()) -> IO ()) -> IO ()
backendImpl cfg serve = do
  hSetBuffering stderr LineBuffering -- Decrease likelihood of output from multiple threads being interleaved

  let defaultLoggingConfig = [LoggingConfig
        { _loggingConfig_logger = def @ RhyoliteLogAppender
        , _loggingConfig_filters = Just $ Map.fromList
          [ ("SQL", RhyoliteLogLevel_Error)
          ]
        }]

  !loggingConfig <- fromMaybe defaultLoggingConfig <$> getJSONConfigFromFile (configPath "loggers")

  !emailFromAddress <- Address (Just "Tezos Bake Monitor") . fromMaybe "noreply@obsidian.systems" <$>
    liftA2 (<|>)
      (pure $ _opts_emailFromAddress cfg)
      (getConfigFromFile Just $ configPath Config.emailFromAddress)

  !(chain :: Either NamedChain ChainId) <- fmap (fromMaybe Config.defaultChain) $ liftA2 (<|>)
    (pure $ _opts_chain cfg)
    (getConfigFromFile (Just . parseChainOrError) $ configPath Config.chain)

  !(serveNodeCache :: Bool) <- fmap (fromMaybe False) $ liftA2 (<|>)
    (pure $ _opts_serveNodeCache cfg)
    (getConfigFromFile (Just . Config.parseBool) $ configPath Config.serveNodeCache)

  !(checkForUpgrade :: Bool) <- fmap (fromMaybe Config.checkForUpgradeDefault) $ liftA2 (<|>)
    (pure $ _opts_checkForUpgrade cfg)
    (getConfigFromFile (Just . Config.parseBool) $ configPath Config.checkForUpgrade)

  !(upgradeBranch :: Text) <- fmap (fromMaybe Config.upgradeBranchDefault) $ liftA2 (<|>)
    (pure $ _opts_upgradeBranch cfg)
    (getConfigFromFile Just $ configPath Config.upgradeBranch)

  !(pgConnString :: Maybe Text) <- liftA2 (<|>)
    (pure $ _opts_pgConnectionString cfg)
    (getConfigFromFile Just $ configPath Config.db)

  let
    maybeNamedChain = either Just (const Nothing) chain

    firstOption :: [IO (Maybe a)] -> IO (Maybe a)
    firstOption = (fmap.fmap) getFirst . fmap getOption . fold . (fmap.fmap) Option . (fmap.fmap.fmap) First

  !(tzscanApi :: Maybe (NonEmpty URI)) <- firstOption
    [ pure $ getOption $  _opts_tzscanApiUri cfg
    , getConfigFromFile' (Aeson.eitherDecodeStrict . T.encodeUtf8) $ configPath Config.tzscanApiUri
    , pure $ getPublicNodeUri PublicNode_TzScan <$> maybeNamedChain
    ]
  !(blockscaleApi :: Maybe (NonEmpty URI)) <- firstOption
    [ pure $ getOption $ _opts_blockscaleApiUri cfg
    , getConfigFromFile' (Aeson.eitherDecodeStrict . T.encodeUtf8) $ configPath Config.blockscaleApiUri
    , pure $ getPublicNodeUri PublicNode_Blockscale <$> maybeNamedChain
    ]
  !(obsidianApi :: Maybe (NonEmpty URI)) <- firstOption
    [ pure $ getOption $ _opts_obsidianApiUri cfg
    , getConfigFromFile' (Aeson.eitherDecodeStrict . T.encodeUtf8) $ configPath Config.obsidianApiUri
    , pure $ getPublicNodeUri PublicNode_Obsidian <$> maybeNamedChain
    ]

  !(nodes :: Maybe (Set URI)) <- liftA2 (<|>)
    (pure $ getOption $ _opts_nodes cfg)
    (getConfigFromFile (Just . Config.parseNodes) $ configPath Config.nodes)

  let
    publicDataSources' :: [(PublicNode, Either NamedChain ChainId, NonEmpty URI)]
    publicDataSources' = catMaybes
      [ (,,) <$> pure PublicNode_TzScan <*> pure chain <*> tzscanApi
      , (,,) <$> pure PublicNode_Blockscale <*> pure chain <*> blockscaleApi
      , (,,) <$> pure PublicNode_Obsidian <*> pure chain <*> obsidianApi
      ]

  publicDataSources :: [DataSource] <- (traverse . _3) (flip Random.runRVar Random.StdRandom . Random.choice . toList) publicDataSources'

  let
    defaultDbSpec = if gargoyleSupported
      then Left Config.db
      else error "Please specify a PostgreSQL connection string"
  !dbSpec <- pure $ maybe defaultDbSpec Right pgConnString

  httpMgr <- Http.newManager Https.tlsManagerSettings

  chainId <- runHttpT httpMgr $ case chain of
    Right chainId -> pure chainId
    Left NamedChain_Mainnet -> pure mainnetChainId

    -- We're doing some RPC here, which needs logging, but we haven't really
    -- started yet so where it does log, we log to stderr instead of normally.
    -- if there's issues, we exit immediately anyhow.
    Left chainName -> runStderrLoggingT $ runExceptT (runReaderT (nodeRPC rChain) (NodeRPCContext httpMgr (URI.render $ NonEmpty.head $ getPublicNodeUri PublicNode_Blockscale chainName))) >>= \case
      Left (e :: RpcError) -> throwString $
        "Unable to connect to foundation node for chain " <> T.unpack (showChain chain) <> ": " <> show e
      Right chainId -> pure chainId

  withDb dbSpec $ \db -> withLogging loggingConfig $ do
    logger <- askLogger
    $(logInfo) $ "Monitoring network " <> toBase58Text chainId

    runDb (Identity db) $ do
      migrateKiln

      -- Set nodes overrides based on configuration
      for_ nodes $ \ns -> do
        update [Node_deletedField =. True] CondEmpty
        update [Node_deletedField =. False] (Node_addressField `in_` toList ns)
        enabled <- project Node_addressField (Node_deletedField ==. False)

        let needToAdd = ns `Set.difference` Set.fromList enabled
        for_ needToAdd $ \newAddress ->
          insert $ mkNode newAddress Nothing

    params <- runLoggingEnv logger $ runDb (Identity db) $
      listToMaybe <$> project Parameters_protoInfoField (Parameters_chainField ==. chainId)
    dataSrc <- liftIO $ blankNodeDataSource db chainId params httpMgr logger

    withTermination $ \addFinalizer -> do
      -- Start a thread to send queued emails
      addFinalizer <=< workerWithDelay (pure 10) $ const $
        runLoggingEnv logger $ clearMailQueueWithDynamicEmailEnv $ Identity db

      let
        appConfig = AppConfig emailFromAddress
        frontendConfig = Config.FrontendConfig
          { Config._frontendConfig_chain = chain
          , Config._frontendConfig_chainId = chainId
          , Config._frontendConfig_upgradeBranch = if checkForUpgrade then Just upgradeBranch else Nothing
          , Config._frontendConfig_appVersion = version
          }

      _ <- Telegram.initState addFinalizer httpMgr logger db

      (handleListen, wsFinalizer) <- RhyoliteApp.serveDbOverWebsockets db
        (requestHandler upgradeBranch emailFromAddress dataSrc publicDataSources)
        (notifyHandler dataSrc)
        (viewSelectorHandler frontendConfig (preview _Left chain) dataSrc db)
        (RhyoliteApp.queryMorphismPipeline $ RhyoliteApp.transposeMonoidMap <<< RhyoliteApp.monoidMapQueryMorphism)
      addFinalizer wsFinalizer

      addFinalizer =<< cacheWorker 30 dataSrc
      addFinalizer =<< nodeWorker 10 dataSrc appConfig db
      addFinalizer =<< publicNodesWorker dataSrc publicDataSources
      addFinalizer =<< nodeAlertWorker dataSrc appConfig db
      addFinalizer =<< clientWorker appConfig dataSrc
      addFinalizer =<< delegateWorker dataSrc

      when checkForUpgrade $
        addFinalizer =<< upgradeCheckWorker upgradeBranch (60 * 60) logger httpMgr db

      liftIO $ serve $ \case
        BackendRoute_Missing :=> _ -> pure ()
        BackendRoute_Listen :=> _ -> handleListen
        BackendRoute_PublicCacheApi :=> _
          | serveNodeCache -> v1PublicApi dataSrc
          | otherwise -> return ()

backend :: Backend BackendRoute AppRoute
backend = backend' mempty

backend' :: Opts -> Backend BackendRoute AppRoute
backend' cfg = Backend
  { _backend_run = backendImpl cfg
  , _backend_routeEncoder = backendRouteEncoder
  }

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
  emailEnv <- runDb db $
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

getJSONConfigFromFile :: Aeson.FromJSON a => FilePath -> IO (Maybe a)
getJSONConfigFromFile f = (either (error . (("JSON decode error while reading file " <> f <> ":") <>)) Just . Aeson.eitherDecode' <$> LBS.readFile f)
  `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e

getConfigFromFile :: (Text -> Maybe a) -> FilePath -> IO (Maybe a)
getConfigFromFile parser f = (parser . T.strip <$> T.readFile f)
  `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e

getConfigFromFile' :: (Text -> Either String a) -> FilePath -> IO (Maybe a)
getConfigFromFile' parser f = (either error Just . parser . T.strip <$> T.readFile f)
  `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e

data Opts = Opts
  { _opts_pgConnectionString :: !(Maybe Text)
  , _opts_route :: !(Maybe URI)
  , _opts_emailFromAddress :: !(Maybe Text)
  , _opts_chain :: !(Maybe (Either NamedChain ChainId))
  , _opts_checkForUpgrade :: !(Maybe Bool)
  , _opts_upgradeBranch :: !(Maybe Text)
  , _opts_serveNodeCache :: !(Maybe Bool)
  , _opts_tzscanApiUri     :: !(Option (NonEmpty URI))
  , _opts_blockscaleApiUri :: !(Option (NonEmpty URI))
  , _opts_obsidianApiUri   :: !(Option (NonEmpty URI))
  , _opts_nodes :: !(Option (Set URI))
  }

instance Semigroup Opts where
  a <> b = Opts -- Right biased
    { _opts_pgConnectionString = _opts_pgConnectionString b <|> _opts_pgConnectionString a
    , _opts_route = _opts_route b <|> _opts_route a
    , _opts_emailFromAddress = _opts_emailFromAddress b <|> _opts_emailFromAddress a
    , _opts_chain = _opts_chain b <|> _opts_chain a
    , _opts_checkForUpgrade = _opts_checkForUpgrade b <|> _opts_checkForUpgrade a
    , _opts_upgradeBranch = _opts_upgradeBranch b <|> _opts_upgradeBranch a
    , _opts_serveNodeCache = _opts_serveNodeCache b <|> _opts_serveNodeCache a
    , _opts_tzscanApiUri = _opts_tzscanApiUri b <|> _opts_tzscanApiUri a
    , _opts_blockscaleApiUri = _opts_blockscaleApiUri b <|> _opts_blockscaleApiUri a
    , _opts_obsidianApiUri = _opts_obsidianApiUri b <|> _opts_obsidianApiUri a
    , _opts_nodes = _opts_nodes b <> _opts_nodes a -- Union the sets if there are multiple
    }

instance Monoid Opts where
  mempty = Opts Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty mempty mempty mempty
  mappend = (<>)

optsArgDescr :: [GetOpt.OptDescr Opts]
optsArgDescr =
  [ mkReqArg Config.pgConnectionString "CONNSTRING" (\x -> mempty { _opts_pgConnectionString = Just $ T.pack x }) $
      "Connection string or URI to PostgreSQL database. If blank, use connection string in '" <> Config.db <> "' file or create a database there if empty."
  , mkReqArg Config.route "URL" (\x -> mempty { _opts_route = Just $ mkRootUriOrError $ T.pack x }) $
      "Root URL for this service as seen by external users. If blank, use contents of '" <> configPath Config.route <> "'."
  , mkReqArg Config.emailFromAddress "EMAIL" (\x -> mempty { _opts_emailFromAddress = Just $ T.pack x }) $
      "Email address to use for 'From' field in email notifications. If blank, use contents of '" <> configPath Config.emailFromAddress <> "'."
  , mkReqArg Config.checkForUpgrade "BOOL" (\x -> mempty { _opts_checkForUpgrade = Just $ Config.parseBool $ T.pack x }) $
      "Enable/disable upgrade checks. If blank, use contents of '" <> configPath Config.checkForUpgrade <>
      "'. If that is blank, default to " <> (if Config.checkForUpgradeDefault then "enabled" else "disabled") <> "."

  , mkReqArg Config.upgradeBranch "BRANCH" (\x -> mempty { _opts_upgradeBranch = Just $ T.pack x }) $
      "Upstream Git branch to use for checking upgrades. If blank, use contents of '" <> configPath Config.upgradeBranch <>
      "'. If that is blank, default to '" <> T.unpack Config.upgradeBranchDefault <> "'."
  , mkReqArg Config.chain "NETWORK" (\x -> mempty { _opts_chain = Just $ parseChainOrError $ T.pack x }) $
      "Name of a network (mainnet, alphanet, zeronet) or a network ID to monitor. If blank, use contents of '" <> configPath Config.chain <>
      "'. If also blank, default to '" <> T.unpack (showChain Config.defaultChain) <> "'."
  , mkReqArg Config.serveNodeCache "BOOL" (\x -> mempty { _opts_serveNodeCache = Just $ Config.parseBool $ T.pack x })
      "Serve Node Cache.  Default disabled."

  , mkReqArg Config.tzscanApiUri "URL" (\x -> mempty { _opts_tzscanApiUri = pure $ pure $ Config.parseURIUnsafe $ T.pack x })
      "Custom tzscan API URL.  Default none."
  , mkReqArg Config.blockscaleApiUri "URL" (\x -> mempty { _opts_blockscaleApiUri = pure $ pure $ Config.parseURIUnsafe $ T.pack x })
      "Custom Blockscale API URL.  Default none."
  , mkReqArg Config.obsidianApiUri "URL" (\x -> mempty { _opts_obsidianApiUri = pure $ pure $ Config.parseURIUnsafe $ T.pack x })
      "Custom Obsidian API URL.  Default none."

  , mkReqArg Config.nodes "URIS" (\x -> mempty { _opts_nodes = Option $ Just $ Config.parseNodes $ T.pack x })
      "Force the set of monitored nodes to be exactly the given set of (comma-separated) list of nodes. If given multiple times, the sets will be unioned. Defaults to off."
  ]
  where
    mkReqArg opt var f = GetOpt.Option [] [opt] (GetOpt.ReqArg f var)

configPath :: FilePath -> FilePath
configPath = ("config" </>)

mkRootUriOrError :: Text -> URI
mkRootUriOrError x = either (\e -> error $ T.unpack $ e <> ": " <> x) id $ mkRootUri x

encodeViaJson :: Aeson.ToJSON a => a -> Text
encodeViaJson = T.decodeUtf8 . LBS.toStrict . Aeson.encode


-- | This does *not* run in @ob run@.
backendMain :: (Backend BackendRoute AppRoute -> Frontend (R AppRoute) -> IO ()) -> IO ()
backendMain k = do
  myArgs <- getArgs

  let (opts', rest, errs) = GetOpt.getOpt GetOpt.RequireOrder optsArgDescr myArgs
  case errs of
    _:_ -> do
      prog <- getProgName
      let header = "Usage: " <> prog <> " [OPTION...] files..."
      let msg = concat errs
            ++ GetOpt.usageInfo header optsArgDescr
            ++ GetOpt.usageInfo "\n\nadditional options for snap can be provided after a --\n" (SnapServer.optDescrs @Snap.Snap SnapServer.defaultConfig)

      ioError $ userError msg
    [] -> do
      print errs
      let cfg = fold opts'

      !(route :: Maybe URI) <- liftA2 (<|>)
        (pure $ _opts_route cfg)
        (getConfigFromFile' (Aeson.eitherDecodeStrict . T.encodeUtf8) $ configPath Config.route)

      let
        staticHead :: DomBuilder t m => m ()
        !staticHead = do
          let injectIt config = injectPure (T.pack $ "config/" <> config)
          headTag
          for_ route $ injectIt Config.route . URI.render

      for_ route $ \r -> putStrLn $ "Using route " <> T.unpack (URI.render r)
      withArgs rest $ k (backend' cfg) (frontend { _frontend_head = staticHead })
