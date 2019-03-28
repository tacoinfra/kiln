{-# LANGUAGE BangPatterns #-}
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

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend where

import Control.Concurrent.STM (atomically, readTQueue)
import Control.Exception.Safe (catch, throwIO, throwString)
import Control.Lens (set)
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Logger (LoggingT (..), MonadLogger, logInfo, logWarn, runStderrLoggingT)
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
import Database.Groundhog.Core (Field, SubField)
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
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (RunDb, runDb, selectSingle)
import qualified Rhyolite.Backend.Email as RhyoliteEmail
import Rhyolite.Backend.EmailWorker (clearMailQueue)
import Rhyolite.Backend.Logging (LoggingConfig (..), LoggingEnv (..), RhyoliteLogAppender,
                                 RhyoliteLogLevel (..), runLoggingEnv, withLogging)
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as SnapServer
import qualified System.Console.GetOpt as GetOpt
import System.Directory (doesDirectoryExist, renameDirectory)
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

import Backend.CachedNodeRPC (blankNodeDataSource, _nodeDataSource_ioQueue)
import Backend.Common (workerWithDelay, worker')
import Backend.Config (AppConfig (..), defaultNodeConfigFile, nodeDataDir)
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
import Backend.Workers.Accusation (accusationWorker)
import Backend.Workers.Block (blockWorker)
import Backend.Workers.Cache (cacheWorker)
import Backend.Workers.Client (clientWorker)
import Backend.Workers.Baker (bakerRightsWorker, bakerWorker)
import Backend.Workers.Node (DataSource, nodeAlertWorker, nodeWorker, publicNodesWorker)
import Backend.Workers.TezosClient
import qualified Common.Config as Config
import Common.HeadTag (headTag)
import Common.Route (AppRoute, BackendRoute (..), backendRouteEncoder)
import Common.Schema
import Common.URI (Port)
import ExtraPrelude
import Frontend (frontend)
import Backend.NodeCmd

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

  !(networkGitLabProjectId :: Text) <- fmap (fromMaybe Config.networkGitLabProjectIdDefault) $ liftA2 (<|>)
    (pure $ _opts_networkGitLabProjectId cfg)
    (getConfigFromFile Just $ configPath Config.networkGitLabProjectId)

  !(kilnNodePort :: Port) <- fmap (fromMaybe Config.defaultKilnNodePort) $ liftA2 (<|>)
    (pure $ _opts_kilnNodePort cfg)
    (getConfigFromFile (Just . Config.parsePortUnsafe) $ configPath Config.kilnNodePort)

  !(kilnDataDir :: FilePath) <- fmap (fromMaybe Config.defaultKilnDataDir) $ liftA2 (<|>)
    (pure $ _opts_kilnDataDir cfg)
    (getConfigFromFile (Just . T.unpack) $ configPath Config.kilnDataDir)

  let
    maybeNamedChain = either Just (const Nothing) chain

    firstOption :: [IO (Maybe a)] -> IO (Maybe a)
    firstOption = (fmap.fmap) getFirst . fmap getOption . fold . (fmap.fmap) Option . (fmap.fmap.fmap) First

  !(tzscanApi :: Maybe (NonEmpty URI)) <- firstOption
    [ pure $ getOption $  _opts_tzscanApiUri cfg
    , getConfigFromFile' (Aeson.eitherDecodeStrict' . T.encodeUtf8) $ configPath Config.tzscanApiUri
    , pure $ getPublicNodeUri PublicNode_TzScan <$> maybeNamedChain
    ]
  !(blockscaleApi :: Maybe (NonEmpty URI)) <- firstOption
    [ pure $ getOption $ _opts_blockscaleApiUri cfg
    , getConfigFromFile' (Aeson.eitherDecodeStrict' . T.encodeUtf8) $ configPath Config.blockscaleApiUri
    , pure $ getPublicNodeUri PublicNode_Blockscale <$> maybeNamedChain
    ]
  !(obsidianApi :: Maybe (NonEmpty URI)) <- firstOption
    [ pure $ getOption $ _opts_obsidianApiUri cfg
    , getConfigFromFile' (Aeson.eitherDecodeStrict' . T.encodeUtf8) $ configPath Config.obsidianApiUri
    , pure $ getPublicNodeUri PublicNode_Obsidian <$> maybeNamedChain
    ]

  !(nodes :: Maybe (Map.Map URI (Maybe Text))) <- liftA2 (<|>)
    (pure $ getOption $ _opts_nodes cfg)
    (getConfigFromFile (Just . Config.parseNodesUnsafe) $ configPath Config.nodes)

  !(bakers :: Maybe (Map.Map PublicKeyHash (Maybe Text))) <- liftA2 (<|>)
    (pure $ getOption $ _opts_bakers cfg)
    (getConfigFromFile (Just . Config.parseBakersUnsafe) $ configPath Config.bakers)

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
      let
        mkDeletable data_ = DeletableRow
          { _deletableRow_data = data_
          , _deletableRow_deleted = False
          }

        -- These look the same, but I can't get groundhog to unify 'Field' & 'SubField'
        updateNodesAndAlias
          :: Map.Map URI (Maybe Text)
          -> Field NodeExternal NodeExternalConstructor (DeletableRow NodeExternalData)
          -> SubField Postgresql NodeExternal NodeExternalConstructor URI
          -> SubField Postgresql NodeExternal NodeExternalConstructor (Maybe Text)
          -> DbPersist Postgresql (LoggingT IO) (Map.Map URI (Maybe Text))
        updateNodesAndAlias names deletable nameSelector aliasSelector = do
          update [deletable ~> DeletableRow_deletedSelector =. True] CondEmpty
          update [deletable ~> DeletableRow_deletedSelector =. False] $ nameSelector `in_` Map.keys names
          kept <- fmap Set.fromList $ project nameSelector $ (deletable ~> DeletableRow_deletedSelector) ==. False
          ifor_ (Map.restrictKeys names kept) $ \address alias -> do
            update [aliasSelector =. alias] $ nameSelector ==. address
          pure $ Map.withoutKeys names kept

        updateBakersAndAlias
          :: Map.Map PublicKeyHash (Maybe Text)
          -> Field Baker BakerConstructor (DeletableRow BakerData)
          -> Field Baker BakerConstructor PublicKeyHash
          -> SubField Postgresql Baker BakerConstructor (Maybe Text)
          -> DbPersist Postgresql (LoggingT IO) (Map.Map PublicKeyHash (Maybe Text))
        updateBakersAndAlias names deletable nameSelector aliasSelector = do
          update [deletable ~> DeletableRow_deletedSelector =. True] CondEmpty
          update [deletable ~> DeletableRow_deletedSelector =. False] $ nameSelector `in_` Map.keys names
          kept <- fmap Set.fromList $ project nameSelector $ (deletable ~> DeletableRow_deletedSelector) ==. False
          ifor_ (Map.restrictKeys names kept) $ \address alias -> do
            update [aliasSelector =. alias] $ nameSelector ==. address
          pure $ Map.withoutKeys names kept

      for_ nodes $ \ns -> do
        new <- updateNodesAndAlias ns
          NodeExternal_dataField
          (NodeExternal_dataField ~> DeletableRow_dataSelector ~> NodeExternalData_addressSelector)
          (NodeExternal_dataField ~> DeletableRow_dataSelector ~> NodeExternalData_aliasSelector)
        ifor_ new $ \newAddress alias -> do
          nid <- insert' Node
          insert $ NodeExternal
            { _nodeExternal_id = nid
            , _nodeExternal_data = mkDeletable $ NodeExternalData
              { _nodeExternalData_address = newAddress
              , _nodeExternalData_alias = alias
              , _nodeExternalData_minPeerConnections = Nothing
              }
            }

      for_ bakers $ \bs -> do
        new <- updateBakersAndAlias bs
          Baker_dataField
          Baker_publicKeyHashField
          (Baker_dataField ~> DeletableRow_dataSelector ~> BakerData_aliasSelector)
        ifor new $ \newPkh alias -> do
          insert $ Baker
            { _baker_publicKeyHash = newPkh
            , _baker_data = mkDeletable $ BakerData
              { _bakerData_alias = alias
              }
            }

    params <- runLoggingEnv logger $ runDb (Identity db) $
      listToMaybe <$> project Parameters_protoInfoField (Parameters_chainField ==. chainId)
    dataSrc <- liftIO $ blankNodeDataSource db chainId params httpMgr logger

    withTermination $ \addFinalizer -> do
      -- Start a thread to send queued emails
      addFinalizer <=< workerWithDelay (pure 10) $ const $
        runLoggingEnv logger $ clearMailQueueWithDynamicEmailEnv $ Identity db

      addFinalizer <=< worker' $ join $ atomically $ readTQueue $ _nodeDataSource_ioQueue dataSrc

      let
        appConfig = AppConfig emailFromAddress kilnNodePort kilnDataDir defaultNodeConfigFile chainId
        frontendConfig = Config.FrontendConfig
          { Config._frontendConfig_chain = chain
          , Config._frontendConfig_chainId = chainId
          , Config._frontendConfig_upgradeBranch = if checkForUpgrade then Just upgradeBranch else Nothing
          , Config._frontendConfig_appVersion = version
          }

      -- migrate old kiln storage
      runLoggingEnv logger $ do
        let oldDir = "./.tezos-node"
            newDir = nodeDataDir appConfig
        mNode <- runDb (Identity db) $ selectSingle $ NodeInternal_dataField ~> DeletableRow_deletedSelector ==. False
        for_ mNode $ \_ni -> liftIO (doesDirectoryExist newDir) >>= \case
          True -> $(logInfo) $ "Node data already exists at " <> T.pack newDir
          False -> do
            liftIO (doesDirectoryExist oldDir) >>= \case
              False -> $(logInfo) $ "No node data to migrate..."
              True -> do
                $(logWarn) $ "Migrating node data from " <> T.pack oldDir <> " to " <> T.pack newDir
                liftIO $ renameDirectory oldDir newDir

      _ <- Telegram.initState addFinalizer httpMgr logger db

      (handleListen, wsFinalizer) <- RhyoliteApp.serveDbOverWebsockets db
        (requestHandler upgradeBranch emailFromAddress dataSrc publicDataSources)
        (notifyHandler dataSrc)
        (viewSelectorHandler frontendConfig (preview _Left chain) dataSrc db)
        (RhyoliteApp.queryMorphismPipeline $ RhyoliteApp.transposeMonoidMap <<< RhyoliteApp.monoidMapQueryMorphism)
      addFinalizer wsFinalizer

      addFinalizer =<< cacheWorker 90 dataSrc
      addFinalizer =<< nodeWorker 10 dataSrc appConfig db
      addFinalizer =<< publicNodesWorker dataSrc publicDataSources
      addFinalizer =<< nodeAlertWorker dataSrc appConfig db
      addFinalizer =<< clientWorker appConfig dataSrc
      addFinalizer =<< bakerRightsWorker dataSrc
      addFinalizer =<< bakerWorker appConfig dataSrc
      addFinalizer =<< blockWorker 0.3 dataSrc appConfig db
      addFinalizer =<< accusationWorker (realToFrac (15*sqrt 5 :: Double)) dataSrc appConfig db
        -- TODO: also make all the other workers have irrational ratios with each other to avoid resonance.
        -- Square roots of rationals are the most effective for this because number theory.

      when checkForUpgrade $
        addFinalizer =<< upgradeCheckWorker maybeNamedChain networkGitLabProjectId upgradeBranch (60 * 60) logger httpMgr db appConfig

      for_ maybeNamedChain $ \namedChain -> do
        addFinalizer =<< internalNodeWorker appConfig logger db namedChain
        addFinalizer =<< protocolMonitorWorker dataSrc db
        (\(a,b) -> addFinalizer a >> addFinalizer b) =<< bakerDaemonProcess appConfig logger db namedChain
        addFinalizer =<< tezosClientWorker 1.3 logger appConfig db namedChain

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
  , MonadBaseNoPureAborts IO m
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

configPath :: FilePath -> FilePath
configPath = ("config" </>)

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
  , _opts_nodes :: !(Option (Map.Map URI (Maybe Text)))
  , _opts_bakers :: !(Option (Map.Map PublicKeyHash (Maybe Text)))
  , _opts_networkGitLabProjectId :: !(Maybe Text)
  , _opts_kilnNodePort :: !(Maybe Port)
  , _opts_kilnDataDir :: !(Maybe FilePath)
  }
makeLenses ''Opts

instance Semigroup Opts where
  a <> b = Opts -- Right biased
    { _opts_pgConnectionString = rightBiased (<|>) _opts_pgConnectionString
    , _opts_route = rightBiased (<|>) _opts_route
    , _opts_emailFromAddress = rightBiased (<|>) _opts_emailFromAddress
    , _opts_chain = rightBiased (<|>) _opts_chain
    , _opts_checkForUpgrade = rightBiased (<|>) _opts_checkForUpgrade
    , _opts_upgradeBranch = rightBiased (<|>) _opts_upgradeBranch
    , _opts_serveNodeCache = rightBiased (<|>) _opts_serveNodeCache
    , _opts_tzscanApiUri = rightBiased (<|>) _opts_tzscanApiUri
    , _opts_blockscaleApiUri = rightBiased (<|>) _opts_blockscaleApiUri
    , _opts_obsidianApiUri = rightBiased (<|>) _opts_obsidianApiUri
    , _opts_nodes = rightBiased (<>) _opts_nodes -- Last alias (or lack of) wins
    , _opts_bakers = rightBiased (<>) _opts_bakers -- Last alias (or lack of) wins
    , _opts_networkGitLabProjectId = rightBiased (<|>) _opts_networkGitLabProjectId
    , _opts_kilnNodePort = rightBiased (<|>) _opts_kilnNodePort
    , _opts_kilnDataDir = rightBiased (<|>) _opts_kilnDataDir
    }
    where
      rightBiased :: (b -> b -> c) -> (Opts -> b) -> c
      rightBiased binOp f = (binOp `on` f) b a

instance Monoid Opts where
  mempty = Opts Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty mempty mempty mempty mempty Nothing Nothing Nothing
  mappend = (<>)

optsArgDescr :: [GetOpt.OptDescr Opts]
optsArgDescr =
  [ mkReqArg Config.pgConnectionString "CONNSTRING" (set opts_pgConnectionString . Just) $
      "Connection string or URI to PostgreSQL database. If blank, use connection string in '" <> Config.db <> "' file or create a database there if empty."

  , mkReqArg Config.route "URL" (set opts_route . Just . Config.parseRootURIUnsafe) $
      "Root URL for this service as seen by external users. If blank, use contents of '" <> configPath Config.route <> "'."

  , mkReqArg Config.emailFromAddress "EMAIL" (set opts_emailFromAddress . Just) $
      "Email address to use for 'From' field in email notifications. If blank, use contents of '" <> configPath Config.emailFromAddress <> "'."
  , mkReqArg Config.checkForUpgrade "BOOL" (set opts_checkForUpgrade . Just . Config.parseBool) $
      "Enable/disable upgrade checks. If blank, use contents of '" <> configPath Config.checkForUpgrade <>
      "'. If that is blank, default to " <> (if Config.checkForUpgradeDefault then "enabled" else "disabled") <> "."

  , mkReqArg Config.upgradeBranch "BRANCH" (set opts_upgradeBranch . Just) $
      "Upstream Git branch to use for checking upgrades. If blank, use contents of '" <> configPath Config.upgradeBranch <>
      "'. If that is blank, default to '" <> T.unpack Config.upgradeBranchDefault <> "'."

  , mkReqArg Config.chain "NETWORK" (set opts_chain . Just . parseChainOrError) $
      "Name of a network (mainnet, alphanet, zeronet) or a network ID to monitor. If blank, use contents of '" <> configPath Config.chain <>
      "'. If also blank, default to '" <> T.unpack (showChain Config.defaultChain) <> "'."

  , mkReqArg Config.serveNodeCache "BOOL" (set opts_serveNodeCache . Just . Config.parseBool)
      "Serve Node Cache.  Default disabled."

  , mkReqArg Config.tzscanApiUri "URL" (set opts_tzscanApiUri . pure . pure . Config.parseRootURIUnsafe)
      "Custom tzscan API URL.  Default none."

  , mkReqArg Config.blockscaleApiUri "URL" (set opts_blockscaleApiUri . pure . pure . Config.parseRootURIUnsafe)
      "Custom Blockscale API URL.  Default none."

  , mkReqArg Config.obsidianApiUri "URL" (set opts_obsidianApiUri . pure . pure . Config.parseRootURIUnsafe)
      "Custom Obsidian API URL.  Default none."

  , mkReqArg Config.nodes "URIS" (set opts_nodes . Option . Just . Config.parseNodesUnsafe)
      "Force the set of monitored nodes to be exactly the given set of (comma-separated) list of nodes. If given multiple times, the sets will be unioned. Defaults to off."

  , mkReqArg Config.bakers "PUBLICKEYHASHES" (set opts_bakers . Option . Just . Config.parseBakersUnsafe)
      "Force the set of monitored bakers to be exactly the given set of (comma-separated) list of bakers. If given multiple times, the sets will be unioned. Defaults to off."

  , mkReqArg Config.networkGitLabProjectId "PROJECTID" (set opts_networkGitLabProjectId . Just)
      "The GitLab project id to query for network updates. Defaults to off." -- TODO default

  , mkReqArg Config.kilnNodePort "PORT" (set opts_kilnNodePort . Just . Config.parsePortUnsafe)
      ("The port to use for the kiln node. Defaults to " <> show Config.defaultKilnNodePort <> ".")

  , mkReqArg Config.kilnDataDir "DIRECTORY" (set opts_kilnDataDir . Just . T.unpack)
      ("The data directory used by the kiln node and tezos-client. Defaults to " <> show Config.defaultKilnDataDir <> ".")
  ]
  where
    mkReqArg opt var f = GetOpt.Option [] [opt] (GetOpt.ReqArg (\x -> f (T.pack x) mempty) var)

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
        (getConfigFromFile (Just . Config.parseRootURIUnsafe) $ configPath Config.route)

      let
        staticHead :: DomBuilder t m => m ()
        !staticHead = do
          let injectIt config = injectPure (T.pack $ "config/" <> config)
          headTag
          for_ route $ injectIt Config.route . URI.render

      for_ route $ \r -> putStrLn $ "Using route " <> T.unpack (URI.render r)
      withArgs rest $ k (backend' cfg) (frontend { _frontend_head = staticHead })
