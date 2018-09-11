{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend where

import Control.Applicative (ZipList (..), liftA2, (<|>))
import Control.Category ((.))
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Exception.Safe (Handler (..), catch, catches, finally, throwIO, throwString)
import Control.Lens (ifor, ifor_, ix, to, (.~), (<&>), (^.), (^?), _Just, _Right)
import Control.Monad (join, unless, void, when, (<=<))
import Control.Monad.Except (ExceptT (..), MonadError, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as Aeson
import qualified Data.AppendMap as AppendMap
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy as LBS
import Data.Default (def)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Either.Combinators (leftToMaybe)
import Data.Foldable (fold, foldl', for_, toList, traverse_)
import Data.Function (on, (&))
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortBy)
import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Pool (Pool)
import Data.Semigroup (Semigroup, Sum (..), getSum, (<>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Traversable (for)
import Data.Word (Word64)
import Database.Groundhog.Generic.Migration (getTableAnalysis)
import Database.Groundhog.Postgresql
import qualified Network.HTTP.Client as Http (Manager, newManager)
import qualified Network.HTTP.Client.TLS as Https
import qualified Network.HTTP.Simple as Http
import Network.Mail.Mime (Address (..), Mail, simpleMail')
import Obelisk.Asset.Serve.Snap (serveAssets)
import Obelisk.ExecutableConfig.Inject (injectPure)
import Prelude hiding ((.))
import Reflex.Dom.Core (renderStatic)
import Rhyolite.Backend.Account (migrateAccount)
import qualified Rhyolite.Backend.App as RhyoliteApp
import Rhyolite.Backend.DB (RunDb, getTime, runDb, selectMap)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (In (..), Only (..), PostgresRaw, Values (..), executeQ, queryQ)
import qualified Rhyolite.Backend.Email as RhyoliteEmail
import Rhyolite.Backend.EmailWorker (clearMailQueue, migrateQueuedEmail)
import Rhyolite.Backend.Listen (NotificationType (..), insertAndNotify, insertAndNotify_, notifyEntityId,
                                updateAndNotify)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Backend.Snap (appConfig_initialHead, serveApp)
import Rhyolite.Concurrent (worker)
import Rhyolite.Route (RouteEnv)
import Rhyolite.Schema (Id (..), Json (..))
import Safe (maximumByMay, maximumMay)
import Say (say, sayErr, sayShow)
import Snap.Core (MonadSnap, route)
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as SnapServer
import Snap.Util.FileServe (serveDirectory)
import System.Console.GetOpt (ArgDescr (ReqArg), OptDescr (Option))
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr)
import System.IO.Error (isDoesNotExistError)
import Text.URI (URI)
import qualified Text.URI as URI
import qualified Text.URI.Lens as Uri

import Backend.Db (gargoyleSupported, withDb)
import Tezos.Base58Check (HashedValue (..), fromBase58)
import Tezos.Chain (betanetChainId)
import Tezos.Lenses
import Tezos.NodeRPC
import Tezos.NodeRPC.Sources (PublicNode (..), getPublicNodeUri)
import Tezos.Types

import Backend.Alerts (clearUpgradeNotice)
import Backend.CachedNodeRPC
import Backend.ChainHealth (scanForkInfo)
import Backend.Common (workerWithDelay)
import Backend.Config (AppConfig (..), HasAppConfig, getAppConfig)
import Backend.NotifyHandler (notifyHandler)
import Backend.RequestHandler (getDefaultMailServer, requestHandler)
import Backend.Schema
import Backend.Supervisor
import Backend.Upgrade (upgradeCheckWorker)
import Backend.ViewSelectorHandler (viewSelectorHandler)
import Backend.Workers.Cache (cacheWorker)
import Backend.Workers.Client
import Backend.Workers.Delegate
import Backend.Workers.Node
import Common (tshow)
import qualified Common.Config as Config
import Common.HeadTag (headTag)
import Common.Schema
import Common.URI (mkRootUri)
import Common.Verification (ForkInfo (..), ForkStatus (..), validateForkyBlocks)

import Backend.WebApi (v1PublicApi)

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


timeit :: MonadIO m => Text -> (e -> m a) -> ExceptT e m a -> m a
timeit note errback action = do
  !now <- liftIO getCurrentTime
  !result <- either errback return =<< runExceptT action
  !later <- liftIO getCurrentTime
  sayShow (note, diffUTCTime later now)
  return result

onRpcError :: (MonadError Text m, Show a) => Either a b -> m b
onRpcError = either (throwError . tshow) pure

backend :: IO ()
backend = do
  hSetBuffering stderr LineBuffering -- Decrease likelihood of output from multiple threads being interleaved

  let cfg0 = SnapServer.defaultConfig & SnapServer.setOther mempty
  cfg <- SnapServer.extendedCommandLineConfig (SnapServer.optDescrs cfg0 <> optsArgDescr) (<>) cfg0

  !emailFromAddress <- Address (Just "Tezos Bake Monitor") . fromMaybe "noreply@obsidian.systems" <$>
    liftA2 (<|>)
      (pure $ _opts_emailFromAddress =<< SnapServer.getOther cfg)
      (getConfigFromFile Just $ configPath Config.emailFromAddress)

  !(routeEnv :: Maybe RouteEnv) <- liftA2 (<|>)
    (pure $
      fromMaybe (error "invalid URL") . uriToRouteEnv <$>
        (_opts_route =<< SnapServer.getOther cfg))
    (getConfigFromFile (Aeson.decodeStrict . T.encodeUtf8) $ configPath Config.route)

  !(chain :: Either NamedChain ChainId) <- fmap (fromMaybe Config.defaultChain) $ liftA2 (<|>)
    (pure $ _opts_chain =<< SnapServer.getOther cfg)
    (getConfigFromFile (Just . parseChainOrError) $ configPath Config.chain)

  !(serveNodeCache :: Bool) <- fmap (fromMaybe False) $ liftA2 (<|>)
    (pure $ _opts_serveNodeCache =<< SnapServer.getOther cfg)
    (getConfigFromFile (Just . Config.parseBool) $ configPath Config.serveNodeCache)

  !(checkForUpgrade :: Bool) <- fmap (fromMaybe Config.checkForUpgradeDefault) $ liftA2 (<|>)
    (pure $ _opts_checkForUpgrade =<< SnapServer.getOther cfg)
    (getConfigFromFile (Just . Config.parseBool) $ configPath Config.checkForUpgrade)

  !(upgradeBranch :: Text) <- fmap (fromMaybe Config.upgradeBranchDefault) $ liftA2 (<|>)
    (pure $ _opts_upgradeBranch =<< SnapServer.getOther cfg)
    (getConfigFromFile Just $ configPath Config.upgradeBranch)

  !(pgConnString :: Maybe Text) <- liftA2 (<|>)
    (pure $ _opts_pgConnectionString =<< SnapServer.getOther cfg)
    (getConfigFromFile Just $ configPath Config.db)

  let
    defaultDbSpec = if gargoyleSupported
      then Left Config.db
      else error "Please specify a PostgreSQL connection string"
  !dbSpec <- pure $ maybe defaultDbSpec Right pgConnString

  httpMgr <- Http.newManager Https.tlsManagerSettings

  chainId <- case chain of
    Right chainId -> pure chainId
    Left NamedChain_Betanet -> pure betanetChainId

    Left chainName -> runExceptT (runReaderT (nodeRPC rChain) (NodeRPCContext httpMgr (URI.render $ getPublicNodeUri PublicNode_Blockscale chainName))) >>= \case
      Left (e :: RpcError) -> throwString $
        "Unable to connect to foundation node for chain " <> T.unpack (showChain chain) <> ": " <> show e
      Right chainId -> pure chainId

  say $ "Monitoring network " <> toBase58Text chainId

  let encodeViaJson = T.decodeUtf8 . LBS.toStrict . Aeson.encode
  !staticHead <- fmap mconcat $ traverse (fmap snd . renderStatic) $ catMaybes
    [ Just headTag
    , injectPure Config.route . encodeViaJson <$> routeEnv
    , Just $ injectPure Config.checkForUpgrade (tshow checkForUpgrade)
    , Just $ injectPure Config.chain $ showChain chain
    ]

  withDb dbSpec $ \db -> do
    runNoLoggingT $ runDb (Identity db) $ do
      tableInfo <- getTableAnalysis
      runMigration $ do
        migrateAccount tableInfo
        migrateQueuedEmail tableInfo
        migrateSchema tableInfo

    dataSrc <- blankNodeDataSource db chainId httpMgr

    -- If tracking a named chain, use foundation nodes to initialize the chain parameters.
    for_ (leftToMaybe chain) $ \namedChain ->
      initParams dataSrc [getPublicNodeUri PublicNode_Blockscale namedChain]

    withTermination $ \addFinalizer -> do
      -- Start a thread to send queued emails
      addFinalizer <=< workerWithDelay (pure 10) $ const $
        runNoLoggingT (clearMailQueueWithDynamicEmailEnv $ Identity db)

      let appConfig = AppConfig emailFromAddress

      (handleListen, wsFinalizer) <- RhyoliteApp.serveDbOverWebsockets db
        (requestHandler upgradeBranch emailFromAddress httpMgr db appConfig)
        (notifyHandler dataSrc)
        (viewSelectorHandler (leftToMaybe chain) dataSrc db)
        (RhyoliteApp.queryMorphismPipeline $ RhyoliteApp.transposeMonoidMap . RhyoliteApp.monoidMapQueryMorphism)
      addFinalizer wsFinalizer

      addFinalizer =<< cacheWorker 30 dataSrc
      addFinalizer =<< nodeWorker 10 dataSrc appConfig db
      addFinalizer =<< nodeAlertWorker dataSrc appConfig db

      case chain of
        Left chainName ->
          addFinalizer =<< publicNodesWorker dataSrc chainName appConfig db
        _ -> pure ()

      addFinalizer =<< clientWorker appConfig dataSrc
      addFinalizer =<< delegateWorker dataSrc

      if checkForUpgrade then
        addFinalizer =<< upgradeCheckWorker upgradeBranch (60 * 60) appConfig httpMgr db
      else
        runNoLoggingT $ runDb (Identity db) clearUpgradeNotice


      SnapServer.httpServe cfg (route $
        [ ("", rootHandler staticHead)
        , ("/listen", handleListen)
        , ("static", serveAssets "static" "static")
        , ("", serveDirectory "frontend.jsexe")
        ] ++ [x | x <- [("/api/v1", v1PublicApi dataSrc)] , serveNodeCache ]
        )

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


getConfigFromFile :: (Text -> Maybe a) -> FilePath -> IO (Maybe a)
getConfigFromFile parser f = (parser . T.strip <$> T.readFile f)
  `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e


uriToRouteEnv :: URI -> Maybe RouteEnv
uriToRouteEnv uri = (,,)
  <$> (uri ^? Uri.uriScheme . _Just . Uri.unRText . to (<> ":") . to T.unpack)
  <*> (uri ^? Uri.uriAuthority . _Right . to renderBeforePort . to T.unpack)
  <*> Just (T.unpack renderPortAndAfter)
  where
    renderBeforePort a = maybe "" ((<> "@") . renderUserInfo) (a ^. Uri.authUserInfo)
      <> (a ^. Uri.authHost . Uri.unRText)
    renderUserInfo u = (u ^. Uri.uiUsername . Uri.unRText) <> maybe "" (":" <>) (u ^? Uri.uiPassword . _Just . Uri.unRText)
    renderPortAndAfter =
      fromMaybe "" (uri ^? Uri.uriAuthority . _Right . Uri.authPort . _Just . to tshow . to (":" <>))
      <>
      (if null $ uri ^. Uri.uriPath then "" else renderPieces $ uri ^. Uri.uriPath)
    renderPieces pieces = "/" <> T.intercalate "/" (map (^. Uri.unRText) pieces)

data Opts = Opts
  { _opts_pgConnectionString :: !(Maybe Text)
  , _opts_route :: !(Maybe URI)
  , _opts_emailFromAddress :: !(Maybe Text)
  , _opts_chain :: !(Maybe (Either NamedChain ChainId))
  , _opts_checkForUpgrade :: !(Maybe Bool)
  , _opts_upgradeBranch :: !(Maybe Text)
  , _opts_serveNodeCache :: !(Maybe Bool)
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
    }

instance Monoid Opts where
  mempty = Opts Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  mappend = (<>)

optsArgDescr :: MonadSnap m => [OptDescr (Maybe (SnapServer.Config m Opts))]
optsArgDescr =
  [ Option [] ["pg-connection"] (mkReqArg "CONNSTRING" $ \x -> mempty { _opts_pgConnectionString = Just $ T.pack x }) $
      "Connection string or URI to PostgreSQL database. If blank, use connection string in '" <> Config.db <> "' file or create a database there if empty."
  , Option [] [Config.route] (mkReqArg "URL" $ \x -> mempty { _opts_route = Just $ mkRootUriOrError $ T.pack x }) $
      "Root URL for this service as seen by external users. If blank, use contents of '" <> configPath Config.route <> "'."
  , Option [] [Config.emailFromAddress] (mkReqArg "EMAIL" $ \x -> mempty { _opts_emailFromAddress = Just $ T.pack x }) $
      "Email address to use for 'From' field in email notifications. If blank, use contents of '" <> configPath Config.emailFromAddress <> "'."
  , Option [] [Config.checkForUpgrade] (mkReqArg "BOOL" $ \x -> mempty { _opts_checkForUpgrade = Just $ Config.parseBool $ T.pack x }) $
      "Enable/disable upgrade checks. If blank, use contents of '" <> configPath Config.checkForUpgrade <>
      "'. If that is blank, default to " <> (if Config.checkForUpgradeDefault then "enabled" else "disabled") <> "."

  , Option [] [Config.upgradeBranch] (mkReqArg "BRANCH" $ \x -> mempty { _opts_upgradeBranch = Just $ T.pack x }) $
      "Upstream Git branch to use for checking upgrades. If blank, use contents of '" <> configPath Config.upgradeBranch <>
      "'. If that is blank, default to '" <> T.unpack Config.upgradeBranchDefault <> "'."
  , Option [] [Config.chain] (mkReqArg "NETWORK" $ \x -> mempty { _opts_chain = Just $ parseChainOrError $ T.pack x }) $
      "Name of a network (betanet, alphanet, zeronet) or a network ID to monitor. If blank, use contents of '" <> configPath Config.chain <>
      "'. If also blank, default to '" <> T.unpack (showChain Config.defaultChain) <> "'."
  , Option [] [Config.serveNodeCache] (mkReqArg "BOOL" $ \x -> mempty { _opts_serveNodeCache = Just $ Config.parseBool $ T.pack x })
      "Serve Node Cache.  Default enabled"
  ]
  where
    mkReqArg var f = ReqArg (\x -> Just $ SnapServer.setOther (f x) mempty) var

configPath :: FilePath -> FilePath
configPath = ("config" </>)

mkRootUriOrError :: Text -> URI
mkRootUriOrError x = either (\e -> error $ T.unpack $ e <> ": " <> x) id $ mkRootUri x
