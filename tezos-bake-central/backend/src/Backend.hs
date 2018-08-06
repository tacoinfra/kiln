{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Exception.Safe (Handler (..), catch, catches, finally, throwIO)
import Control.Lens (ifor, ifor_, ix, to, (.~), (<&>), (^.), (^?), _Just, _Right)
import Control.Monad (join, unless, void, when, (<=<))
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT, throwError, catchError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Control.Monad.Reader (MonadReader, runReaderT)
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
import qualified Data.Text.Encoding as T -- (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Traversable (for)
import Data.Word (Word64)
import Database.Groundhog.Generic.Migration (getTableAnalysis)
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple as Pg
import qualified Network.HTTP.Client as Http (Manager, newManager)
import qualified Network.HTTP.Client.TLS as Https
import qualified Network.HTTP.Simple as Http
import Network.Mail.Mime (Address (..), Mail, simpleMail')
import Obelisk.Asset.Serve.Snap (serveAssets)
import Obelisk.ExecutableConfig.Inject (injectPure)
import Prelude hiding ((.))
import Reflex.Dom.Core (renderStatic)
import Rhyolite.Backend (withDb)
import Rhyolite.Backend.Account (migrateAccount)
import qualified Rhyolite.Backend.App as RhyoliteApp
import Rhyolite.Backend.DB (RunDb, getTime, openDb, runDb, selectMap)
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
import qualified Snap.Http.Server as SnapServer
import Snap.Util.FileServe (serveDirectory)
import System.Console.GetOpt (ArgDescr (ReqArg), OptDescr (Option))
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr)
import System.IO.Error (isDoesNotExistError)
import Text.URI (URI)
import qualified Text.URI.Lens as Uri

import Tezos.Base58Check (HashedValue(..), fromBase58)
import Tezos.Types
import Tezos.Lenses
import Tezos.NodeRPC -- (HasNodeRPC, NodeRPCContext (..), nodeRPC, RpcError)

import Backend.Supervisor
import Backend.Workers.Node
import Backend.Workers.Client
import Backend.Workers.Delegate

import Backend.ChainHealth (scanForkInfo)
import Backend.Config (AppConfig (..), HasAppConfig, getAppConfig)
import Backend.Errors
import Backend.NotifyHandler (notifyHandler)
import Backend.RequestHandler
import Backend.Schema
import Backend.ViewSelectorHandler (viewSelectorHandler)
import Common (tshow)
import qualified Common.Config as Config
import Common.Schema
import Common.URI (mkRootUri)
import Common.Verification (ForkInfo (..), ForkStatus (..), validateForkyBlocks)
import Frontend (frontend)

import Backend.CachedNodeRPC


seconds :: Int -> Int
seconds = (* 10^(6 :: Int))

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





-- doThing
--   :: ChainId
--   -> Pool Postgresql
--   -> Http.Manager
--   -> Text
--   -> MonitorBlock
--   -> IO ()
-- doThing chain db mgr nodeUrl newBlock = timeit "doThing" say $ runNoLoggingT $ runDb (Identity db) $ do
--   let ctx = NodeRPCContext mgr nodeUrl
--   let blockHash = _monitorBlock_hash newBlock
--   let blockPredecessor = _monitorBlock_predecessor newBlock
--   -- if non empty, we're done!
--   sayShow (T.pack "NEW BLOCK", nodeUrl, blockHash)
--   fmap listToMaybe (select (CachedBlock_hashField ==. blockHash)) >>= \case
--     Just _ -> return () -- sayShow (T.pack "have block, DONE", blockHash)
--     Nothing -> do
--       fmap listToMaybe [queryQ|
--           SELECT cb.chain, cb."cyclePosition"
--           FROM "CachedBlock" cb
--           JOIN "CachedChainCycle" ccc
--             ON cb.chain = ccc.id
--           JOIN "CachedProtocolConstants" cpc
--             ON ccc.constants = cpc.id
--           WHERE cb.hash = ?blockPredecessor
--             AND cb."cyclePosition" < (cpc."blocksPerCycle" - 1)
--         |] >>= \case
--         Just (chainCycleId, predCyclePosition) -> do
--           -- sayShow (T.pack "Have predecessor in chain", blockPredecessor)
--           cacheOneBlock ctx chain chainCycleId (predCyclePosition + 1) blockHash
--           return ()
--           -- insert_ $ CachedBlock chainCycleId Nothing (Json mempty) (predCyclePosition + 1) (_monitorBlock_hash newBlock) (_monitorBlock_predecessor newBlock)
--         -- we're not caught up yet :(
--         Nothing -> do
--           -- sayShow (T.pack "need chain history", blockHash)
--           block <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RBlock $ blockHashId' chain blockHash) ctx
--           let protoHash = _blockMetadata_protocol $ _block_metadata block
--           let cycle = _level_cycle $ _blockMetadata_level $ _block_metadata block
--           let cyclePosition = _level_cyclePosition $ _blockMetadata_level $ _block_metadata block
--           -- if we're at position n we need a result of length n + 1 to include the first block of the current cycle.
--           cycleInitBlock <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RBlock $ blockHashIdPred' chain blockHash $ fromIntegral cyclePosition) ctx
--           let cycleInitHash :: BlockHash = _block_hash cycleInitBlock
--           --sayShow cycleInitBlock
--           -- we now have enough information to get our metadata in sync
-- 
--           (chainCycleId, preservedCycles) :: (Id CachedChainCycle, Cycle) <- listToMaybe <$> [queryQ|
--               SELECT ccc.id, cpc."preservedCycles"
--               FROM "CachedChainCycle" ccc
--               JOIN "CachedProtocolConstants" cpc
--                 ON ccc.constants = cpc.id
--               WHERE hash = ?cycleInitHash
--             |] >>= \case
--             Nothing -> do
--               -- sayShow (T.pack "need chain metadata")
--               (protoId, proto) :: (Id CachedProtocolConstants, CachedProtocolConstants) <- listToMaybe <$> [queryQ|
--                   SELECT "id", "protocol", "blocksPerCycle", "preservedCycles"
--                   FROM "CachedProtocolConstants"
--                   WHERE "protocol" = ?protoHash
--                 |] >>= \case
--                 Nothing -> do
--                   proto <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RProtoConstants $ blockHashId' chain blockHash) ctx
--                   let
--                     proto' = CachedProtocolConstants
--                       { _cachedProtocolConstants_protocol = protoHash
--                       , _cachedProtocolConstants_blocksPerCycle = _protoInfo_blocksPerCycle proto
--                       , _cachedProtocolConstants_preservedCycles = _protoInfo_preservedCycles proto
--                       }
--                   protoId' <- insert proto'
--                   return (toId protoId', proto')
--                 Just (protoId', p, bpc, pc) -> return (protoId', CachedProtocolConstants p bpc pc)
-- 
--               previousCycleHash <- fmap _block_hash . onRpcError <=< flip runReaderT ctx $ runExceptT $ nodeRPC $
--                 RBlock (blockHashIdPred' chain cycleInitHash $ fromIntegral $ _cachedProtocolConstants_blocksPerCycle proto)
--               -- protocol version data is now in sync
--               cid' <- toId <$> insert CachedChainCycle
--                 { _cachedChainCycle_chainId = chain
--                 , _cachedChainCycle_constants = protoId
--                 , _cachedChainCycle_cycle = cycle
--                 , _cachedChainCycle_hash = cycleInitHash
--                 , _cachedChainCycle_predecessor = previousCycleHash
--                 }
--               return (cid', _cachedProtocolConstants_preservedCycles proto)
--             Just (cid', pc) -> do
--               -- sayShow ("What actually happened?")
--               return (cid', pc)
--           -- chain/cycle is now in sync
--           -- which blocks are still missing?
--           ancestorMap <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RBlocks (DynamicParamChainId_ChainId chain) (1 + cyclePosition) $ Set.singleton blockHash) ctx
--           ancestors <- maybe (throwError $ "bad heads response from node, missing hash:" <> toBase58Text blockHash) return $ Map.lookup blockHash ancestorMap
--           -- sayShow ("foundAncestors:", Seq.length ancestors, Seq.take 3 ancestors)
--           let inAncestors = In $ toList ancestors
--           maxGoodAncestor :: RawLevel <- head . stripOnly <$> [queryQ|
--               SELECT COALESCE(MAX("cyclePosition"), -1)
--               FROM "CachedBlock"
--               WHERE hash in ?inAncestors
--             |]
--           -- sayShow ("haveAncestors:", maxGoodAncestor)
--           -- let needAncestors = Seq.take (Seq.length ancestors - maxGoodAncestor) ancestors
--           -- sayShow ("needAncestors:", Seq.length needAncestors, Seq.take 3 needAncestors)
-- 
--           let blocks = cacheOneBlock ctx chain chainCycleId
--                 <$> ZipList [cyclePosition,cyclePosition-1..maxGoodAncestor+1]
--                 <*> ZipList (blockHash : toList ancestors)
--           sequence_ blocks
--           -- TODO: it makes sense to insertAndNotify if we actually inserted the MonitorBlock we just recieved...
--           -- we now have our history... all that's left is rights.
--           -- in context $cycleInitBlock, we can find rights for cycles in range [$cycle .. ($cycle - $preservedCycles - 1))]
--           -- but really, the cycle rights that are *determined* by $cycle is just $cycle + $preservedCycles
--           -- except for cycles [0 .. $preservedCycles], which are all determined by the genesis block and not too important anyway.
--           knownRights :: Maybe (Id CachedBlockRights) <- listToMaybe . stripOnly <$> [queryQ|
--               SELECT id
--               FROM "CachedBlockRights"
--               WHERE cycle = ?chainCycleId
--               LIMIT 1
--               |]
--           case knownRights of
--             Just _ -> return ()
--             Nothing -> do
--               -- TODO:  if cycle == 0, request [0..preservedCycles]
--               bRights <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ RBakingRights (blockHashId' chain blockHash) $ Set.singleton $ Right . Cycle . fromIntegral $ cycle + preservedCycles) ctx
--               eRrights <- onRpcError =<< runReaderT (runExceptT $ nodeRPC $ REndorsingRights (blockHashId' chain blockHash) $ Set.singleton $ Right . Cycle . fromIntegral $ cycle + preservedCycles) ctx
--               let cachedRights = CachedBlockRights chainCycleId (fromIntegral $ cycle + preservedCycles) (Json bRights) (Json eRrights)
--               insertAndNotify_ cachedRights

backend :: IO ()
backend = do
  hSetBuffering stderr LineBuffering -- Decrease likelihood of output from multiple threads being interleaved

  let cfg0 = SnapServer.defaultConfig & SnapServer.setOther mempty
  cfg <- SnapServer.extendedCommandLineConfig (SnapServer.optDescrs cfg0 <> optsArgDescr) (<>) cfg0

  emailFromAddress <- Address (Just "Tezos Bake Monitor") . fromMaybe "noreply@obsidian.systems" <$>
    liftA2 (<|>)
      (pure $ _opts_emailFromAddress =<< SnapServer.getOther cfg)
      (getConfigFromFile Just $ configPath Config.emailFromAddress)

  routeEnv :: Maybe RouteEnv <- liftA2 (<|>)
    (pure $
      fromMaybe (error "invalid URL") . uriToRouteEnv <$>
        (_opts_route =<< SnapServer.getOther cfg))
    (getConfigFromFile (Aeson.decodeStrict . T.encodeUtf8) $ configPath Config.route)

  blockExplorer :: Maybe URI <- liftA2 (<|>)
    (pure $ _opts_blockExplorer =<< SnapServer.getOther cfg)
    (getConfigFromFile (Just . mkRootUriOrError) $ configPath Config.blockExplorer)

  chainId :: ChainId <- fmap (fromMaybe betanetChain) $ liftA2 (<|>)
    (pure $ _opts_chain =<< SnapServer.getOther cfg)
    (getConfigFromFile (either (const Nothing) Just  . fromBase58 . T.encodeUtf8) $ configPath Config.chain)


  staticHead <- fmap mconcat $ traverse (fmap snd . renderStatic) $ catMaybes
    [ Just $ fst frontend
    , injectPure Config.route . T.decodeUtf8 . LBS.toStrict . Aeson.encode <$> routeEnv
    , injectPure Config.blockExplorer . tshow <$> blockExplorer
    ]

  let pgConnStr = _opts_pgConnectionString =<< SnapServer.getOther cfg
  withGargoyleOrConnStr (maybe (Left Config.db) Right pgConnStr) $ \db -> do
    runNoLoggingT $ runDb (Identity db) $ do
      tableInfo <- getTableAnalysis
      runMigration $ do
        migrateAccount tableInfo
        migrateQueuedEmail tableInfo
        migrateSchema tableInfo

    supervise $ \addFinalizer -> do
      -- finalizers <- newTVarIO (return ())
      -- let addFinalizer f = atomically $ modifyTVar finalizers (f *>)

      -- Start a thread to send queued emails
      addFinalizer <=< worker (seconds 10) $ runNoLoggingT (clearMailQueueWithDynamicEmailEnv $ Identity db)

      httpMgr <- Http.newManager Https.tlsManagerSettings
      dataSrc <- blankNodeDataSource chainId httpMgr

      (handleListen, wsFinalizer) <- RhyoliteApp.serveDbOverWebsockets db
        (requestHandler emailFromAddress httpMgr db)
        (notifyHandler db)
        (viewSelectorHandler dataSrc db)
        (RhyoliteApp.queryMorphismPipeline $ RhyoliteApp.transposeMonoidMap . RhyoliteApp.monoidMapQueryMorphism)
      addFinalizer wsFinalizer

      -- TODO: move this to nodeWorker
      let nodeCtx = NodeRPCContext httpMgr "http://127.0.0.1:18731"

      let appConfig = AppConfig emailFromAddress
      addFinalizer =<< nodeWorker (seconds 30) dataSrc appConfig httpMgr db
      addFinalizer =<< clientWorker (seconds 10) appConfig dataSrc db
      addFinalizer =<< delegateWorker (seconds 10) httpMgr db

      SnapServer.httpServe cfg (route
        [ ("", rootHandler staticHead)
        , ("/listen", handleListen)
        , ("static", serveAssets "static" "static")
        , ("", serveDirectory "frontend.jsexe")
        ]) --  `finally` join (readTVarIO finalizers)

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


getConfigFromFile :: (Text -> Maybe a) -> FilePath -> IO (Maybe a)
getConfigFromFile parser f = (parser . T.strip <$> T.readFile f)
  `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e


withGargoyleOrConnStr :: Either FilePath Text -> (Pool Postgresql -> IO a) -> IO a
withGargoyleOrConnStr cfg f = case cfg of
  Left dbPath -> withDb dbPath f
  Right connStr -> f =<< openDb (T.encodeUtf8 connStr)


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
  , _opts_blockExplorer :: !(Maybe URI)
  , _opts_chain :: !(Maybe ChainId)
  }

instance Semigroup Opts where
  a <> b = Opts -- Right biased
    { _opts_pgConnectionString = _opts_pgConnectionString b <|> _opts_pgConnectionString a
    , _opts_route = _opts_route b <|> _opts_route a
    , _opts_emailFromAddress = _opts_emailFromAddress b <|> _opts_emailFromAddress a
    , _opts_blockExplorer = _opts_blockExplorer b <|> _opts_blockExplorer a
    , _opts_chain = _opts_chain b <|> _opts_chain a
    }

instance Monoid Opts where
  mempty = Opts Nothing Nothing Nothing Nothing Nothing
  mappend = (<>)

optsArgDescr :: MonadSnap m => [OptDescr (Maybe (SnapServer.Config m Opts))]
optsArgDescr =
  [ Option [] ["pg-connection"] (mkReqArg "CONNSTRING" $ \x -> mempty { _opts_pgConnectionString = Just $ T.pack x }) $
      "Connection string or URI to PostgreSQL database. If blank, use connection string in '" <> Config.db <> "' file or create a database there if empty."
  , Option [] [Config.route] (mkReqArg "URL" $ \x -> mempty { _opts_route = Just $ mkRootUriOrError $ T.pack x }) $
      "Root URL for this service as seen by external users. If blank, use contents of '" <> configPath Config.route <> "'."
  , Option [] [Config.emailFromAddress] (mkReqArg "EMAIL" $ \x -> mempty { _opts_emailFromAddress = Just $ T.pack x }) $
      "Email address to use for 'From' field in email notifications. If blank, use contents of '" <> configPath Config.emailFromAddress <> "'."
  , Option [] [Config.blockExplorer] (mkReqArg "URL" $ \x -> mempty { _opts_blockExplorer = Just $ mkRootUriOrError $ T.pack x }) $
      "URL of the block explorer to use for links. If blank, use contents of '" <> configPath Config.blockExplorer <> "'."
  , Option [] [Config.chain] (mkReqArg "ChainId" $ \x -> mempty { _opts_chain = Just $ fromString x }) $
      "Chain Id.  default:" <> T.unpack (toBase58Text betanetChain) <> " " <> configPath Config.blockExplorer <> "'."
  ]
  where
    mkReqArg var f = ReqArg (\x -> Just $ SnapServer.setOther (f x) mempty) var

configPath :: FilePath -> FilePath
configPath = ("config" </>)

mkRootUriOrError :: Text -> URI
mkRootUriOrError x = either (\e -> error $ T.unpack $ e <> ": " <> x) id $ mkRootUri x

-- $(error "stop")
