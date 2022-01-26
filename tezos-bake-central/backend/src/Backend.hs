{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Backend where

import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Concurrent.STM (atomically, newTQueueIO, newTVarIO, readTQueue)
import Control.Exception.Safe (catch, throwIO, throwString)
import Control.Lens (set)
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Logger (NoLoggingT(..), LoggingT (..), MonadLoggerIO, MonadLogger, logError, logInfo, logWarn)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Dependent.Map (DSum (..))
import qualified Data.Map as Map
import Data.Pool (Pool)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time (NominalDiffTime)
import Data.Time.Clock (nominalDay)
import Data.Validation
import Database.Groundhog.Core (Field, SubField)
import Database.Groundhog.Postgresql
import Gargoyle.PostgreSQL.Connect (withDb)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
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
import Rhyolite.Backend.DB.Serializable
import qualified Rhyolite.Backend.Email as RhyoliteEmail
import Rhyolite.Backend.EmailWorker (clearMailQueue)
import Rhyolite.Backend.Logging (LoggingConfig (..), LoggingEnv (..), RhyoliteLogAppender (..),
                                 RhyoliteLogLevel (..), runLoggingEnv, withLoggingMinLevel)
#if defined(SUPPORT_SYSTEMD_JOURNAL)
import Rhyolite.Backend.Logging (RhyoliteLogAppenderJournald (..))
#endif

import qualified Rhyolite.Backend.WebSocket as RhyoliteWs
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as SnapServer
import qualified System.Console.GetOpt as GetOpt
import System.Directory (doesDirectoryExist, renameDirectory)
import System.Environment (getArgs, getProgName, lookupEnv, withArgs)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (writeSystemTempFile)
import Text.URI (URI)
import qualified Text.URI as URI

import Tezos.Common.Chain (identifyChain)
import Tezos.Types

import Backend.Common (worker', workerWithDelay)
import Backend.Config (AppConfig (..), BinaryPaths (..), defaultNodeConfigFile, kilnNodeRpcURI, nodeDataDir
                      , _nodeConfigFile_network, validateNodeConfigFile)
import Backend.Http (runHttpT)
import Backend.Migrations (migrateKiln)
import Backend.NodeCmd (bakerDaemonProcess, handleExportLogs, internalNodeWorker)
import Backend.NodeRPC (NodeDataSource (..))
import Backend.NotifyHandler (notifyHandler)
import Backend.RequestHandler (getDefaultMailServer, requestHandler)
import Backend.Schema
import Backend.Snapshot
import Backend.Supervisor (withTermination)
import qualified Backend.Telegram as Telegram
import Backend.Upgrade (upgradeCheckWorker)
import Backend.Version (version)
import Backend.ViewSelectorHandler (viewSelectorHandler)
import Backend.Workers.Accusation (accusationWorker)
import Backend.Workers.Baker (bakerRightsWorker, bakerWorker)
import Backend.Workers.Block (blockWorker)
import Backend.Workers.Node (amendmentProcessWorker, nodeWorker, protocolMonitorWorker)
import Backend.Workers.TezosClient (resetLedgerQueue, tezosClientWorker, computeChainId)
import Backend.Workers.TezosRelease
import qualified Common.Config as Config
import Common.Distribution (Distribution (..), distributionMethod)
import Common.HeadTag (headTag)
import Common.Route (AppRoute, BackendRoute (..), fullRouteEncoder)
import Common.Schema
import Common.URI (Port)
import ExtraPrelude
import Frontend (frontend)
import Orphans.Instances ()

onRpcError :: (MonadError Text m, Show a) => Either a b -> m b
onRpcError = either (throwError . tshow) pure

askLogger :: Monad m => LoggingT m LoggingEnv
askLogger = LoggingT $ return . LoggingEnv

resolveKnownChains :: Either NamedChain ChainId -> Either NamedChain ChainId
resolveKnownChains = \case
  Right chainId
    | Just chain <- identifyChain chainId -> Left chain
  x -> x

backendImpl :: Opts -> ((R BackendRoute -> Snap.Snap ()) -> IO ()) -> IO ()
backendImpl cfg serve = do
  hSetBuffering stderr LineBuffering -- Decrease likelihood of output from multiple threads being interleaved

  -- Just in case, text encoding/decoding is not set to UTF-8
  setLocaleEncoding utf8

  let
    loggingConfigForDistro = case distributionMethod of
      Distribution_FromSource -> defaultLoggingConfig
      Distribution_Docker -> defaultLoggingConfig
#if defined(SUPPORT_SYSTEMD_JOURNAL)
      Distribution_LinuxPackage -> map (\(t, p, l) -> LoggingConfig
        { _loggingConfig_logger = RhyoliteLogAppender_Journald (RhyoliteLogAppenderJournald t)
        , _loggingConfig_filters = Just $ Map.fromList [(p, l)]
        })
        [ ("kiln", "", RhyoliteLogLevel_Warn)
        , ("kiln", "SQL", RhyoliteLogLevel_Error)
        , ("kiln-node", "kiln-node", RhyoliteLogLevel_Info)
        , ("kiln-baker", "kiln-baker", RhyoliteLogLevel_Info)
        , ("kiln-endorser", "kiln-endorser", RhyoliteLogLevel_Info)
        ]
#else
      Distribution_LinuxPackage -> defaultLoggingConfig
#endif

    defaultLoggingConfig = [LoggingConfig
      { _loggingConfig_logger = def @ RhyoliteLogAppender
      , _loggingConfig_filters = Just $ Map.fromList
        [ ("SQL", RhyoliteLogLevel_Error)
        , ("", RhyoliteLogLevel_Warn)
        ]
      }]

  !loggingConfig <- fromMaybe loggingConfigForDistro <$> getJSONConfigFromFile (configPath "loggers")
  let logExportAvailable = distributionMethod == Distribution_LinuxPackage && loggingConfig == loggingConfigForDistro

  !emailFromAddress <- Address (Just "Tezos Bake Monitor") . fromMaybe "noreply@obsidian.systems" <$>
    liftA2 (<|>)
      (pure $ _opts_emailFromAddress cfg)
      (getConfigFromFile Just $ configPath Config.emailFromAddress)

  !(nodeConfigFile :: Maybe Aeson.Value) <- liftA2 (<|>)
    (maybe (pure Nothing) (getConfigFromFile' (Aeson.eitherDecodeStrict' . T.encodeUtf8)) $ _opts_nodeConfigFile cfg)
    (getConfigFromFile' (Aeson.eitherDecodeStrict' . T.encodeUtf8) $ configPath Config.nodeConfigFile)

  !(configChain :: Either NamedChain ChainId) <- fmap (resolveKnownChains . fromMaybe Config.defaultChain) $ liftA2 (<|>)
    (pure $ _opts_chain cfg)
    (getConfigFromFile (Just . parseChainOrError) $ configPath Config.chain)

  !(checkForUpgrade :: Bool) <- fmap (fromMaybe Config.checkForUpgradeDefault) $ liftA2 (<|>)
    (pure $ _opts_checkForUpgrade cfg)
    (getConfigFromFile (Just . Config.parseBool) $ configPath Config.checkForUpgrade)

  !(pgConnStringFile :: Maybe FilePath) <- do
    let fileName = configPath Config.pgConnectionString
    inFile <- getConfigFromFile Just fileName
    case (inFile, _opts_pgConnectionString cfg) of
      (_, Just str) -> Just <$> writeSystemTempFile "pg-connection" (T.unpack str)
      (Just _, Nothing) -> pure $ Just fileName
      _ -> pure Nothing

  !(networkGitLabProjectId :: Text) <- fmap (fromMaybe Config.networkGitLabProjectIdDefault) $ liftA2 (<|>)
    (pure $ _opts_networkGitLabProjectId cfg)
    (getConfigFromFile Just $ configPath Config.networkGitLabProjectId)

  !(tezosReleaseTag :: Maybe Text) <- liftA2 (<|>) (pure $ _opts_tezosReleaseTag cfg)
    (getConfigFromFile Just $ configPath Config.tezosReleaseTag)

  !(kilnNodeRpcPort :: Port) <- fmap (fromMaybe Config.defaultKilnNodeRpcPort) $ liftA2 (<|>)
    (pure $ _opts_kilnNodeRpcPort cfg)
    (getConfigFromFile (Just . Config.parsePortUnsafe) $ configPath Config.kilnNodeRpcPort)

  !(kilnNodeNetPort :: Port) <- fmap (fromMaybe Config.defaultKilnNodeNetPort) $ liftA2 (<|>)
    (pure $ _opts_kilnNodeNetPort cfg)
    (getConfigFromFile (Just . Config.parsePortUnsafe) $ configPath Config.kilnNodeNetPort)

  !(kilnDataDir :: FilePath) <- fmap (fromMaybe Config.defaultKilnDataDir) $ liftA2 (<|>)
    (pure $ _opts_kilnDataDir cfg)
    (getConfigFromFile (Just . T.unpack) $ configPath Config.kilnDataDir)

  !(kilnNodeCustomArgs :: Maybe Text) <- liftA2 (<|>)
    (pure $ _opts_kilnNodeCustomArgs cfg)
    (getConfigFromFile Just $ configPath Config.kilnNodeCustomArgs)

  !(kilnBakerCustomArgs :: Maybe Text) <- liftA2 (<|>)
    (pure $ _opts_kilnBakerCustomArgs cfg)
    (getConfigFromFile Just $ configPath Config.kilnBakerCustomArgs)

  !(binaryPaths :: Maybe BinaryPaths) <- liftA2 (<|>)
    (pure $ (Aeson.decodeStrict' . T.encodeUtf8) =<< _opts_binaryPaths cfg)
    (getJSONConfigFromFile $ configPath Config.binaryPaths)

  (rightsHistoryWindow :: Int) <- fmap (fromMaybe Config.defaultRightsHistoryWindow) $ liftA2 (<|>)
    (pure $ _opts_rightsHistoryWindow cfg)
    (getConfigFromFile (Just . read . T.unpack) $ configPath Config.rightsHistoryWindow)

  !(nodes :: Maybe (Map.Map URI (Maybe Text))) <- liftA2 (<|>)
    (pure $ getOption $ _opts_nodes cfg)
    (getConfigFromFile (Just . Config.parseNodesUnsafe) $ configPath Config.nodes)

  !(bakers :: Maybe (Map.Map PublicKeyHash (Maybe Text))) <- liftA2 (<|>)
    (pure $ getOption $ _opts_bakers cfg)
    (getConfigFromFile (Just . Config.parseBakersUnsafe) $ configPath Config.bakers)

  !(ledgerCheckDelay :: NominalDiffTime) <- fmap (fromMaybe Config.defaultLedgerCheckDelay) $ liftA2 (<|>)
    (pure $ _opts_ledgerCheckDelaySeconds cfg)
    (getConfigFromFile (Just . Config.parseSecondsUnsafe) $ configPath Config.ledgerCheckDelay)

  let computeChainId' bins json =
        runNoLoggingT
        $ computeChainId Config.defaultKilnNodeRpcPort Config.defaultKilnDataDir bins json
        <&> \e ->
          toEither
          $ let f = first toList
              in f $ validateNodeConfigFile (Left json) *> validationNel e

  -- error out if chain id is invalaid
  customChainId <- for nodeConfigFile (computeChainId' binaryPaths) >>= \case
      Nothing -> pure Nothing
      Just e -> either (throwString . T.unpack . T.intercalate ":") (pure . Just . Right) e
  let
    chain = fromMaybe configChain customChainId

    maybeNamedChain = either Just (const Nothing) chain

    !dbSpec = fromMaybe Config.db pgConnStringFile

  httpMgr <- Http.newManager Https.tlsManagerSettings

  chainId <- runHttpT httpMgr $ case chain of
    Right chainId -> pure chainId
    Left chainName -> case getNamedChainId chainName of
      Just chainId -> pure chainId
      Nothing -> throwString $" Unable to fetch chain ID chain " <> T.unpack (showChain chain)

  withDb dbSpec $ \(coerce -> db) -> withLoggingMinLevel Nothing loggingConfig $ do
    logger <- askLogger
    $(logInfo) $ "Monitoring network " <> toBase58Text chainId

    runDb (Identity db) $ do
      migrateKiln chainId

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
          -> Serializable (Map.Map URI (Maybe Text))
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
          -> Serializable (Map.Map PublicKeyHash (Maybe Text))
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

      -- Clean error log for all processes to restart them automatically after restarting Kiln
      update [ ProcessData_errorLogField =. (Nothing :: Maybe Text)] CondEmpty

    resetLedgerQueue logger db

    tezosNodeEnvVar <- liftIO $ lookupEnv "TEZOS_NODE_DIR"

    let

      networkName :: Maybe Text
      networkName = case chain of
        Left namedChain -> pure $ showNamedChain namedChain
        Right chainId' -> fmap showNamedChain $ identifyChain chainId'

      appConfig = AppConfig
        { _appConfig_emailFromAddress = emailFromAddress
        , _appConfig_kilnNodeRpcPort = kilnNodeRpcPort
        , _appConfig_kilnNodeNetPort = kilnNodeNetPort
        , _appConfig_kilnDataDir = kilnDataDir
        , _appConfig_kilnNodeConfig =
          maybe (Right $ defaultNodeConfigFile {_nodeConfigFile_network = networkName}) Left nodeConfigFile

        , _appConfig_chainId = chainId
        , _appConfig_kilnNodeCustomArgs = kilnNodeCustomArgs
        , _appConfig_kilnBakerCustomArgs = kilnBakerCustomArgs
        , _appConfig_binaryPaths = binaryPaths
        , _appConfig_tezosNodeEnvVar = tezosNodeEnvVar
        }


    dataSrc <- liftIO $ do
      latestHead <- newTVarIO Nothing
      ioQueue <- newTQueueIO
      return NodeDataSource
        { _nodeDataSource_chain = chainId
        , _nodeDataSource_httpMgr = httpMgr
        , _nodeDataSource_pool = db
        , _nodeDataSource_latestHead = latestHead
        , _nodeDataSource_logger = logger
        , _nodeDataSource_ioQueue = ioQueue
        , _nodeDataSource_kilnNodeUri = kilnNodeRpcURI appConfig
        , _nodeDataSource_nodeForQuery = Nothing
        }

    withTermination $ \addFinalizer -> do
      -- Start a thread to send queued emails
      addFinalizer <=< workerWithDelay "clearMailQueueWithDynamicEmail" (pure 10) $ const $
        runLoggingEnv logger $ clearMailQueueWithDynamicEmailEnv $ Identity db

      addFinalizer <=< worker' "readNodeDataSourceIOQueue" $ join $ atomically $ readTQueue $ _nodeDataSource_ioQueue dataSrc

      let
        frontendConfig = Config.FrontendConfig
          { Config._frontendConfig_chain = fromMaybe chain customChainId
          , Config._frontendConfig_chainId = maybe chainId (either (const $ error "impossible") id) customChainId
          , Config._frontendConfig_checkForUpgrade = checkForUpgrade
          , Config._frontendConfig_appVersion = version
          , Config._frontendConfig_usingNodeOption = join $ (Config.UsingCustomNode <$> nodeConfigFile) <$ customChainId
          , Config._frontendConfig_logExportAvailable = logExportAvailable
          , Config._frontendConfig_ledgerConnectedChecks = True
          , Config._frontendConfig_tezosGitlabProjectId = networkGitLabProjectId
          , Config._frontendConfig_tezosRelease = tezosReleaseTag
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
              False -> $(logInfo) "No node data to migrate..."
              True -> do
                $(logWarn) $ "Migrating node data from " <> T.pack oldDir <> " to " <> T.pack newDir
                liftIO $ renameDirectory oldDir newDir

      _ <- Telegram.initState addFinalizer httpMgr logger db

      let withWs = RhyoliteWs.withWebsocketsConnectionLogging @Snap.Snap (\str e -> runLoggingEnv logger $ $logError $ T.pack $ "Websocket error: " <> str <> " " <> show e)
      (handleListen, wsFinalizer) <- RhyoliteApp.serveDbOverWebsocketsRaw withWs "v3" RhyoliteApp.functorFromWire db
        (requestHandler appConfig emailFromAddress dataSrc)
        (notifyHandler dataSrc)
        (viewSelectorHandler frontendConfig dataSrc db)
        (RhyoliteApp.queryMorphismPipeline $ RhyoliteApp.transposeMonoidMap <<< RhyoliteApp.monoidMapQueryMorphism)
      addFinalizer wsFinalizer

      addFinalizer =<< nodeWorker 10 dataSrc appConfig db
      addFinalizer =<< bakerRightsWorker dataSrc rightsHistoryWindow
      addFinalizer =<< bakerWorker appConfig dataSrc
      addFinalizer =<< blockWorker 0.3 dataSrc appConfig db
      addFinalizer =<< accusationWorker (realToFrac (15*sqrt 5 :: Double)) dataSrc appConfig
      addFinalizer =<< amendmentProcessWorker appConfig dataSrc db
      addFinalizer =<< latestTezosReleaseWorker nominalDay networkGitLabProjectId tezosReleaseTag dataSrc db
        -- TODO: also make all the other workers have irrational ratios with each other to avoid resonance.
        -- Square roots of rationals are the most effective for this because number theory.

      when checkForUpgrade $ for_ maybeNamedChain $ \namedChain -> do
        addFinalizer =<< upgradeCheckWorker namedChain tezosReleaseTag networkGitLabProjectId (60 * 60) logger httpMgr db appConfig

      addFinalizer =<< internalNodeWorker appConfig logger db binaryPaths
      addFinalizer =<< protocolMonitorWorker dataSrc db
      addFinalizer =<< bakerDaemonProcess appConfig logger db binaryPaths
      addFinalizer =<< tezosClientWorker 1.3 ledgerCheckDelay logger dataSrc appConfig db binaryPaths

      snapshotUploadLock :: MVar () <- liftIO newEmptyMVar
      liftIO $ serve $ \case
        BackendRoute_Missing :=> _ -> pure ()
        BackendRoute_Listen :=> _ -> handleListen
        BackendRoute_SnapshotUpload :=> _ -> handleSnapshotUpload appConfig dataSrc snapshotUploadLock
        BackendRoute_PublicCacheApi :=> _ -> pure ()
        BackendRoute_ExportLogs :=> Identity lType -> when logExportAvailable $ handleExportLogs dataSrc lType

backend :: Backend BackendRoute AppRoute
backend = backend' mempty

backend' :: Opts -> Backend BackendRoute AppRoute
backend' cfg = Backend
  { _backend_run = backendImpl cfg
  , _backend_routeEncoder = fullRouteEncoder
  }

clearMailQueueWithDynamicEmailEnv
  :: forall m f.
  ( RunDb f
  , MonadIO m
  , MonadBaseNoPureAborts IO m
  , MonadLogger m
  , MonadLoggerIO m
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
  { _opts_pgConnectionString :: Maybe Text
  , _opts_route :: Maybe URI
  , _opts_emailFromAddress :: Maybe Text
  , _opts_chain :: Maybe (Either NamedChain ChainId)
  , _opts_checkForUpgrade :: Maybe Bool
  , _opts_tzscanApiUri     :: Option (NonEmpty URI)
  , _opts_blockscaleApiUri :: Option (NonEmpty URI)
  , _opts_nodes :: Option (Map.Map URI (Maybe Text))
  , _opts_bakers :: Option (Map.Map PublicKeyHash (Maybe Text))
  , _opts_networkGitLabProjectId :: Maybe Text
  , _opts_tezosReleaseTag :: Maybe Text
  , _opts_kilnNodeRpcPort :: Maybe Port
  , _opts_kilnNodeNetPort :: Maybe Port
  , _opts_kilnNodeCustomArgs :: Maybe Text
  , _opts_kilnBakerCustomArgs :: Maybe Text
  , _opts_kilnDataDir :: Maybe FilePath
  , _opts_binaryPaths :: Maybe Text
  , _opts_ledgerCheckDelaySeconds :: Maybe NominalDiffTime
  , _opts_nodeConfigFile :: Maybe FilePath
  , _opts_rightsHistoryWindow :: Maybe Int
  }
makeLenses ''Opts

instance Semigroup Opts where
  a <> b = Opts -- Right biased
    { _opts_pgConnectionString = rightBiased (<|>) _opts_pgConnectionString
    , _opts_route = rightBiased (<|>) _opts_route
    , _opts_emailFromAddress = rightBiased (<|>) _opts_emailFromAddress
    , _opts_chain = rightBiased (<|>) _opts_chain
    , _opts_checkForUpgrade = rightBiased (<|>) _opts_checkForUpgrade
    , _opts_tzscanApiUri = rightBiased (<|>) _opts_tzscanApiUri
    , _opts_blockscaleApiUri = rightBiased (<|>) _opts_blockscaleApiUri
    , _opts_nodes = rightBiased (<>) _opts_nodes -- Last alias (or lack of) wins
    , _opts_bakers = rightBiased (<>) _opts_bakers -- Last alias (or lack of) wins
    , _opts_networkGitLabProjectId = rightBiased (<|>) _opts_networkGitLabProjectId
    , _opts_tezosReleaseTag = rightBiased (<|>) _opts_tezosReleaseTag
    , _opts_kilnNodeRpcPort = rightBiased (<|>) _opts_kilnNodeRpcPort
    , _opts_kilnNodeNetPort = rightBiased (<|>) _opts_kilnNodeNetPort
    , _opts_kilnNodeCustomArgs = rightBiased (<|>) _opts_kilnNodeCustomArgs
    , _opts_kilnBakerCustomArgs = rightBiased (<|>) _opts_kilnBakerCustomArgs
    , _opts_kilnDataDir = rightBiased (<|>) _opts_kilnDataDir
    , _opts_binaryPaths = rightBiased (<|>) _opts_binaryPaths
    , _opts_ledgerCheckDelaySeconds = rightBiased (<|>) _opts_ledgerCheckDelaySeconds
    , _opts_nodeConfigFile = rightBiased (<|>) _opts_nodeConfigFile
    , _opts_rightsHistoryWindow = rightBiased (<|>) _opts_rightsHistoryWindow
    }
    where
      rightBiased :: (b -> b -> c) -> (Opts -> b) -> c
      rightBiased binOp f = (binOp `on` f) b a

instance Monoid Opts where
  mempty = Opts
    { _opts_pgConnectionString = Nothing
      , _opts_route = Nothing
      , _opts_emailFromAddress = Nothing
      , _opts_chain = Nothing
      , _opts_checkForUpgrade = Nothing
      , _opts_tzscanApiUri     = mempty
      , _opts_blockscaleApiUri = mempty
      , _opts_nodes = mempty
      , _opts_bakers = mempty
      , _opts_networkGitLabProjectId = Nothing
      , _opts_tezosReleaseTag = Nothing
      , _opts_kilnNodeRpcPort = Nothing
      , _opts_kilnNodeNetPort = Nothing
      , _opts_kilnNodeCustomArgs = Nothing
      , _opts_kilnBakerCustomArgs = Nothing
      , _opts_kilnDataDir = Nothing
      , _opts_binaryPaths = Nothing
      , _opts_ledgerCheckDelaySeconds = Nothing
      , _opts_nodeConfigFile = Nothing
      , _opts_rightsHistoryWindow = Nothing
      }

optsArgDescr :: [GetOpt.OptDescr Opts]
optsArgDescr =
  [ mkReqArg Config.pgConnectionString "CONNSTRING" (set opts_pgConnectionString . Just) $
      "Connection string or URI to PostgreSQL database. If blank, use connection string in '" <> configPath Config.pgConnectionString <> "' file or create a database in '" <> Config.db <> "' if empty."

  , mkReqArg Config.route "URL" (set opts_route . Just . Config.parseRootURIUnsafe) $
      "Root URL for this service as seen by external users. If blank, use contents of '" <> configPath Config.route <> "'."

  , mkReqArg Config.emailFromAddress "EMAIL" (set opts_emailFromAddress . Just) $
      "Email address to use for 'From' field in email notifications. If blank, use contents of '" <> configPath Config.emailFromAddress <> "'."
  , mkReqArg Config.checkForUpgrade "BOOL" (set opts_checkForUpgrade . Just . Config.parseBool) $
      "Enable/disable upgrade checks. If blank, use contents of '" <> configPath Config.checkForUpgrade <>
      "'. If that is blank, default to " <> (if Config.checkForUpgradeDefault then "enabled" else "disabled") <> "."

  , mkReqArg Config.chain "NETWORK" (set opts_chain . Just . parseChainOrError) $
      "Name of a network (mainnet, babylonnet, carthagenet, zeronet) or a network ID to monitor. If blank, use contents of '" <> configPath Config.chain <>
      "'. If also blank, default to '" <> T.unpack (showChain Config.defaultChain) <> "'."

  , mkReqArg Config.tzscanApiUri "URL" (set opts_tzscanApiUri . pure . pure . Config.parseRootURIUnsafe)
      "Custom tzscan API URL.  Default none."

  , mkReqArg Config.blockscaleApiUri "URL" (set opts_blockscaleApiUri . pure . pure . Config.parseRootURIUnsafe)
      "Custom Blockscale API URL.  Default none."

  , mkReqArg Config.nodes "URIS" (set opts_nodes . Option . Just . Config.parseNodesUnsafe)
      "Force the set of monitored nodes to be exactly the given set of (comma-separated) list of nodes. If given multiple times, the sets will be unioned. Defaults to off."

  , mkReqArg Config.bakers "PUBLICKEYHASHES" (set opts_bakers . Option . Just . Config.parseBakersUnsafe)
      "Force the set of monitored bakers to be exactly the given set of (comma-separated) list of bakers. If given multiple times, the sets will be unioned. Defaults to off."

  , mkReqArg Config.networkGitLabProjectId "PROJECTID" (set opts_networkGitLabProjectId . Just)
      "The GitLab project id to query for network updates. Defaults to off." -- TODO default

  , mkReqArg Config.kilnNodeRpcPort "PORT" (set opts_kilnNodeRpcPort . Just . Config.parsePortUnsafe)
      ("The RPC port to use for the kiln node. Defaults to " <> show Config.defaultKilnNodeRpcPort <> ".")

  , mkReqArg Config.kilnNodeNetPort "PORT" (set opts_kilnNodeNetPort . Just . Config.parsePortUnsafe)
      ("The net-addr port to use for the kiln node. Defaults to " <> show Config.defaultKilnNodeNetPort <> ".")

  , mkReqArg Config.kilnDataDir "DIRECTORY" (set opts_kilnDataDir . Just . T.unpack)
      ("The data directory used by the kiln node and tezos-client. Defaults to " <> show Config.defaultKilnDataDir <> ".")

  , mkReqArg Config.kilnNodeCustomArgs "ARGS" (set opts_kilnNodeCustomArgs . Just)
      "Custom arguments for the Kiln Node."

  , mkReqArg Config.kilnBakerCustomArgs "ARGS" (set opts_kilnBakerCustomArgs . Just)
      "Custom arguments for the Kiln Baker."

  , mkReqArg Config.binaryPaths "BINPATHS" (set opts_binaryPaths . Just)
      "Custom paths to tezos binaries."

  , mkReqArg Config.ledgerCheckDelay "SECONDS" (set opts_ledgerCheckDelaySeconds . Just . Config.parseSecondsUnsafe)
      "Check ledger connectivity every X seconds (off by default)"

  , mkReqArg Config.nodeConfigFile "FILEPATH" (set opts_nodeConfigFile . Just . T.unpack)
      "The file containing the custom tezos-node configuration (for running custom networks)"

  , mkReqArg Config.rightsHistoryWindow "INT" (set opts_rightsHistoryWindow . Just . read . T.unpack) $
      "How much baking and endorsing rights will be gathered from the past in blocks. Defaults to " <>
        show Config.defaultRightsHistoryWindow <> " blocks."
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
          for_ route $ injectIt Config.route . T.encodeUtf8 . URI.render

      for_ route $ \r -> putStrLn $ "Using route " <> T.unpack (URI.render r)
      withArgs rest $ k (backend' cfg) (frontend { _frontend_head = staticHead })
