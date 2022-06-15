{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Workers.TezosClient where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (catchJust)
import Control.Monad.Except
import Control.Monad.Logger
import Data.Aeson.Lens
import Data.Either (fromLeft)
import Data.List (sortOn)
import Data.Pool (Pool)
import Data.Time (NominalDiffTime, diffUTCTime)
import Database.Groundhog
import Database.Groundhog.Postgresql (Postgresql(..), SqlDb, in_)
import Database.Id.Class
import Database.Id.Groundhog
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.PsqlSimple (executeQ, queryQ)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Safe
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.IO (hIsEOF)
import System.IO.Error (isEOFError)
import System.Which
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.URI (render, URI)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified System.Process as Process

import Tezos.Types

import Backend.Alerts
import Backend.Common (AppSerializable, addBakerImpl, workerWithDelay, readCreateProcessWithExitCodeWithLogging, timeout', withDbAndConfig)
import Backend.Config (AppConfig (..), tezosClientDataDir, kilnNodeRpcURI', kilnNodeRpcURI, BinaryPaths(..))
import Backend.NodeRPC
import Backend.Schema
import Common.App (ImportSecretKeyStep(..), SetupLedgerToBakeStep(..), RegisterStep(..), SetupState(..), SetHWMStep(..), VoteState(..), VoteStep(..))
import Common.Schema
import Common.URI (Port)
import ExtraPrelude

startBaking :: (PersistBackend m, SqlDb (PhantomDb m)) => PublicKeyHash -> m ()
startBaking pkh = do
  addBakerImpl pkh (Just "Kiln Baker")
  bdis :: [BakerDaemonInternal] <- fmap snd <$> selectAll
  let processes = fmap fromId $ flip concatMap bdis $ \bdi ->
        [ _bakerDaemonInternalData_bakerProcessData $ _deletableRow_data $ _bakerDaemonInternal_data bdi
        , _bakerDaemonInternalData_endorserProcessData $ _deletableRow_data $ _bakerDaemonInternal_data bdi
        ]
  update [ BakerDaemonInternal_dataField ~> DeletableRow_dataSelector ~> BakerDaemonInternalData_publicKeyHashSelector =. Just pkh
        , BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector =. False] $ CondEmpty
  update [ ProcessData_controlField =. ProcessControl_Run
         , ProcessData_errorLogField =. (Nothing :: Maybe Text)] $ AutoKeyField `in_` processes

ledgerConnectivityCheckWorker
  :: NominalDiffTime
  -> NominalDiffTime
  -> LoggingEnv
  -> NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
ledgerConnectivityCheckWorker delay !ledgerCheckDelay logger nds appConfig db = runLoggingEnv logger $ do
  workerWithDelay "tezosClientWorker" (pure delay) $ const $ runLoggingEnv logger $ do
    liftIO $ createDirectoryIfMissing True (tezosClientDataDir appConfig)

    mConnectedLedger :: Maybe ConnectedLedger <- inDb $ selectSingle CondEmpty
    currentTime <- inDb getTime
    case mConnectedLedger of
      Just cl -> do
        -- We might want to do the connectivity check because some time has passed
        case (ledgerCheckDelay, _connectedLedger_updated cl) of
          -- Check if it wasn't updated previously
          (_, Nothing) -> liftIO $ atomically $ writeTQueue ledgerIOQueue $ runLoggingEnv logger $
            updateConnectedLedgerViaGetConnectedLedger appConfig db
          (ledgerBackgroundUpdateInterval, Just upd) ->
            -- Attempt to check if sufficient amout of time has passed
            when (currentTime `diffUTCTime` upd > ledgerBackgroundUpdateInterval) $ do
              doSensibleLedgerCheck (isJust $ _connectedLedger_ledgerIdentifier cl)

      _ -> do
        -- If there is no row in the DB, this is our first time running and we should check it
        doSensibleLedgerCheck False
        pure ()

    where
      inDb :: AppSerializable a -> LoggingT IO a
      inDb = runDb (Identity db) . flip runReaderT appConfig

      ledgerIOQueue = _nodeDataSource_ledgerIOQueue nds

      -- Regardless of updated time, we ought not to check the ledger if we are two levels around
      -- a baking right and we shouldn't bother checking if we don't have an internal baker running
      -- either
      doSensibleLedgerCheck wasConnected = do
        -- Here we use latest head instead of latest final head to check whether we have baking/endorsement
        -- opportunities in upcoming blocks
        dsh <- liftIO $ atomically $ dataSourceHead nds
        doCheck <- for dsh $ \blk -> checkKilnBakerAndNextRights appConfig nds blk >>= \case
          -- If we don't have an internal baker, don't bother checking
          (Nothing, _, _) -> pure False
          -- If we do and the ledger was previously disconnected, we need to check again
          (_, _, _) | not wasConnected -> pure True
          -- Avoid sending commands to the ledger within two blocks of baking rights
          (_, Just (_, lvl), progressMay) -> do
            let doC = blk ^. level < lvl - 2 || blk ^. level > lvl + 2
            -- This is pretty spammy. We probably don't want this without updating the updated flag...
            -- unless (doC || wasConnected) $ $(logWarn) ("Baking rights approaching at level " <> tshow lvl <> ". Kiln last saw that the ledger was disconnected!")
            case progressMay of
              -- If there are no rights we check that we actually seen all rights up to current block
              -- since there is a possibility that there are rights that we haven't seen yet
              Just progressLvl -> pure $ doC && progressLvl >= blk ^. level
              -- This case shouldn't actually be possible
              Nothing -> pure False
          -- If we have no rights but a baker, we may as well check because the rights are coming
          (_, Nothing, progressMay) -> case progressMay of
              -- If we have no rights, we still check that we've seen all rights up to current block
              Just progressLvl -> pure $ progressLvl >= blk ^. level
              Nothing -> pure False
        when (doCheck == Just True) $ liftIO $ atomically $ writeTQueue ledgerIOQueue $ runLoggingEnv logger $
          updateConnectedLedgerViaGetConnectedLedger appConfig db

updateConnectedLedgerViaGetConnectedLedger :: AppConfig -> Pool Postgresql -> LoggingT IO ()
updateConnectedLedgerViaGetConnectedLedger appConfig db = do
  getConnectedLedger appConfig >>= \case
    Left err -> do
      $(logError) (tshow err)
      reportLedgerDisconnection db appConfig False
      updateConnectedLedger Nothing
    Right mliv -> do
      case mliv of
        Nothing -> do
          reportLedgerDisconnection db appConfig False
          $(logDebug) "The connectedledger is Nothing"

        Just (_, LedgerApp_Baking, _) -> do
          clearLedgerDisconnection db appConfig

        Just (_, LedgerApp_Wallet, _) -> do
          reportLedgerDisconnection db appConfig True

      updateConnectedLedger mliv

  where
    -- TODO: Because this deletes and re-adds, we will only have the walletAppVersion or the bakerAppVersion
    -- is this what we want?
    updateConnectedLedger mliv = do
      withDbAndConfig db appConfig $ do
        $(logDebug) ("Updating connectedledger: " <> tshow mliv)
        now <- getTime
        let connectedLedger = ConnectedLedger
              { _connectedLedger_ledgerIdentifier = fmap (view _1) mliv
              , _connectedLedger_bakingAppVersion = mliv >>= \(_, app, version) -> version <$ guard (app == LedgerApp_Baking)
              , _connectedLedger_walletAppVersion = mliv >>= \(_, app, version) -> version <$ guard (app == LedgerApp_Wallet)
              , _connectedLedger_forceConnectivityCheck = False
              , _connectedLedger_updated = Just now
              }
        deleteAll' @ConnectedLedger Proxy
        insert connectedLedger
        notify NotifyTag_ConnectedLedger $ Just connectedLedger

reportLedgerDisconnection :: Pool Postgresql -> AppConfig -> Bool -> LoggingT IO ()
reportLedgerDisconnection db appConfig isWrongApp = withDbAndConfig db appConfig $ do
  bdis :: [BakerDaemonInternal] <- select (BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector ==. False)
  for_ bdis $ \bdi -> do
    for_ (_bakerDaemonInternalData_publicKeyHash $ _deletableRow_data $ _bakerDaemonInternal_data $ bdi) $ \pkh ->
      reportBakerLedgerDisconnected pkh isWrongApp

clearLedgerDisconnection :: Pool Postgresql -> AppConfig -> LoggingT IO ()
clearLedgerDisconnection db appConfig = withDbAndConfig db appConfig $ do
  bdis :: [BakerDaemonInternal] <- select (BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector ==. False)
  for_ bdis $ \bdi -> do
    for_ (_bakerDaemonInternalData_publicKeyHash $ _deletableRow_data $ _bakerDaemonInternal_data $ bdi) $ \pkh ->
      clearBakerLedgerDisconnected pkh

-- THIS IS SOUND! Either the binary is present in the nix closure
-- or the user provides them (via BinaryPaths).
clientPath :: Maybe BinaryPaths -> FilePath
clientPath = \case
  Just (BinaryPaths _ c _) -> c
  Nothing -> $(staticWhich "tezos-client")

{- Example output from `list connected ledgers`
Found a Tezos Baking 1.5.0 (commit v1.4.3-19-g55cc026d) application running on Ledger Nano S at [0003:0007:00].

To use keys at BIP32 path m/44'/1729'/0'/0' (default Tezos key path), use one of
 tezos-client import secret key ledger_tom "ledger://odd-himalayan-lustrous-falcon/ed25519/0'/0'"
 tezos-client import secret key ledger_tom "ledger://odd-himalayan-lustrous-falcon/secp256k1/0'/0'"
 tezos-client import secret key ledger_tom "ledger://odd-himalayan-lustrous-falcon/P-256/0'/0'"
-}

data LedgerApp
  = LedgerApp_Baking
  | LedgerApp_Wallet
  deriving (Show, Eq)

defaultTimeout :: Maybe (NominalDiffTime, ClientError)
defaultTimeout = Just (5, ClientError_Timeout)

noTimeout :: Maybe (NominalDiffTime, e)
noTimeout = Nothing

getConnectedLedger :: (MonadLoggerIO m) => AppConfig -> m (Either ClientError (Maybe (LedgerIdentifier, LedgerApp, Text)))
getConnectedLedger appConfig = runExceptT $ do
  stdout <- runClientCommand appConfig defaultTimeout ["list", "connected", "ledgers"] $ \_warnings errors -> if
    | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
    | otherwise -> Left $ ClientError_Other $ T.unlines errors
  getKungFuName (T.lines stdout)
  -- case maybePaths of
    -- Left NamedChain_Zeronet -> getKungFuNameZeronet (T.lines stdout)
    -- _ -> getKungFuName (T.lines stdout)
  where
    getVersion t = do
      appAndVersion <- T.stripPrefix "Found a Tezos " t
      let checkApp x app = (,) app . T.takeWhile (/= ' ') <$> T.stripPrefix x appAndVersion
      checkApp "Baking " LedgerApp_Baking <|> checkApp "Wallet " LedgerApp_Wallet
    getKungFuName = \case
      foundApp : _blank : useKeys : keyExample : _
        | Just (app, version) <- getVersion foundApp
        , "To use keys at BIP32 path" `T.isPrefixOf` useKeys -- sanity check
        , Just ledger' <- T.stripPrefix "\"ledger://" (T.dropWhile (/= '"') keyExample)
        , ledger <- T.takeWhile (/= '/') ledger'
        , [_1, _2, _3, _4] <- T.splitOn "-" ledger -- sanity check formatting of ledger
        -> pure $ Just (LedgerIdentifier ledger, app, version)
      xs -> getKungFuNameZeronet xs
    getLedgerZeronet = fmap (T.takeWhile (/= '`')) . T.stripPrefix "## Ledger `"
    getKungFuNameZeronet = \case
      ledgerName : foundApp : _blank : _ : _ : _
        | Just ledger <- getLedgerZeronet ledgerName
        , Just (app, version) <- getVersion foundApp
        , [_1, _2, _3, _4] <- T.splitOn "-" ledger -- sanity check formatting of ledger
        -> pure $ Just (LedgerIdentifier ledger, app, version)
      xs -> do
        $(logWarn) $ "getConnectedLedger: failed to find kung fu name of ledger from: " <> T.unlines xs
        pure Nothing

{- Example output for `show ledger`
Found a Tezos Baking 1.5.0 application running on a Ledger Nano S at [0003:0007:00].
Tezos address at this path/curve: tz1NXDWqwMv1Zi7Jo9za7YN9orap94XQmFSv
Corresponding full public key: edpkuSWMVjedhmQHarHMxvzdLV69cRWERM9yk4H8FAAfuexz3L9bCM
-}

showLedger :: (MonadLoggerIO m, MonadIO m) => AppConfig -> Pool Postgresql -> SecretKey -> m ()
showLedger appConfig db sk = do
  la <- withDbAndConfig db appConfig $ do
    existing <- selectSingle $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
    case existing of
      Just la -> do
        update [LedgerAccount_balanceField =. (Nothing :: Maybe Tez)] $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
        pure $ la { _ledgerAccount_balance = Nothing }
      Nothing -> do
        let la = LedgerAccount
              { _ledgerAccount_secretKey = sk
              , _ledgerAccount_publicKeyHash = Nothing
              , _ledgerAccount_balance = Nothing
              , _ledgerAccount_shouldImport = False
              , _ledgerAccount_imported = False
              , _ledgerAccount_shouldSetupToBake = False
              , _ledgerAccount_shouldRegister = False
              , _ledgerAccount_shouldSetHWM = Nothing
              , _ledgerAccount_shouldDoVoteProtocol = Nothing
              , _ledgerAccount_shouldDoVoteBallot = Nothing
              }
        insert la
        pure la
  pkhOrErr <- case _ledgerAccount_publicKeyHash la of
    Nothing -> runExceptT $ do
      stdout <- runClientCommand appConfig defaultTimeout ["show", "ledger", T.unpack $ toSecretKeyText sk] $ \_warnings errors -> if
        | e : _ <- errors, Just _sk' <- T.stripPrefix "No ledger found for " e -> Left ClientError_LedgerDisconnected
        | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
        | "(Invalid_argument int32_of_path_element_exn)" : _ <- errors -> Right ""
        | otherwise -> Left $ ClientError_Other $ T.unlines errors
      let pkh = getPublicKeyHash (T.lines stdout)
      when (isNothing pkh) $ $(logWarn) $ "showLedger: failed to find public key hash from: " <> stdout
      pure pkh
    Just pkh -> pure $ Right $ Just pkh
  case pkhOrErr of
    Left err -> do
      withDbAndConfig db appConfig $ do
        delete $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
        notify NotifyTag_ShowLedger (sk, Left (T.pack $ show err))
      $(logError) (T.pack (show err))
    Right Nothing -> withDbAndConfig db appConfig $
      notify NotifyTag_ShowLedger (sk, Left "tezosClientWorker:showLedger: public key hash unavailable")
    Right (Just pkh) -> do
      withDbAndConfig db appConfig $
        update [LedgerAccount_publicKeyHashField =. Just pkh] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
  where
    getPublicKeyHashZeronet = \case
      foundApp : _manufacturer: _product: _application: _curve: _path: _pk : pkh' : _
        | T.isPrefixOf "Found ledger corresponding to " foundApp
        , Just pkht <- T.stripPrefix "* Public Key Hash: " pkh'
        , Right pkh <- tryReadPublicKeyHashText pkht
        -> Just pkh
      _ -> Nothing
    getPublicKeyHash = \case
      foundApp : pkh' : _
        | T.isPrefixOf "Found a Tezos Baking " foundApp
        , Just pkht <- T.stripPrefix "Tezos address at this path/curve: " pkh'
        , Right pkh <- tryReadPublicKeyHashText pkht
        -> Just pkh
      xs -> getPublicKeyHashZeronet xs

-- Fetch balances for given @SecretKey@s. At this point public keys for them are expected to be known and stored in the DB.
fetchBalances :: (MonadLoggerIO m, MonadIO m) => AppConfig -> Pool Postgresql -> NodeDataSource -> [SecretKey] -> m ()
fetchBalances appConfig db nds sks = withDbAndConfig db appConfig $ for_ sks $ \sk -> do
  mla <- selectSingle $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
  for_ mla $ \la ->
    case _ledgerAccount_publicKeyHash la of
      Nothing -> pure ()
      Just pkh -> do
        balanceOrErr <- flip runReaderT nds . runExceptT @KilnRpcError $ do
          mbHeadBlock <- ask >>= liftIO . atomically . dataSourceFinalHead
          case mbHeadBlock of
            Nothing -> ExceptT $ pure $ Left KilnRpcError_NoKnownHeads
            Just headBlock -> nodeQueryDataSource $ NodeQuery_Balance (headBlock ^. hash) (headBlock ^. level) pkh
        case balanceOrErr of
          Left err -> $(logError) $ "Failed to get balance of account " <> toPublicKeyHashText pkh <> " due to: " <> prettyKilnRpcError err
          Right balance -> do
            update [LedgerAccount_balanceField =. Just balance] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
            notify NotifyTag_ShowLedger (sk, Right (pkh, balance))

importSecretKey :: MonadLoggerIO m => AppConfig -> Pool Postgresql -> SecretKey -> m ()
importSecretKey appConfig db sk = ledgerSetupStep appConfig db sk (mempty { _setupState_import = Just $ First ImportSecretKeyStep_Prompting })
  (\res -> mempty { _setupState_import = Just $ First res }) $ do
    e <- runExceptT $ runClientCommand appConfig noTimeout ["import", "secret", "key", T.unpack kilnLedgerAlias, T.unpack $ toSecretKeyText sk, "--force"] $ \_warnings errors -> if
      | "Ledger Application level error (get_public_key): Conditions of use not satisfied" : _ <- errors -> Left ImportSecretKeyStep_Declined
      | "Ledger Transport level error:" : _ <- errors -> Left ImportSecretKeyStep_Disconnected
      | otherwise -> Left $ ImportSecretKeyStep_Failed $ T.unlines errors
    withDbAndConfig db appConfig $ update [LedgerAccount_importedField =. False] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
    pure $ fromLeft ImportSecretKeyStep_Done e

runClientCommand'
  :: (MonadLoggerIO m, Show e)
  => URI
  -> FilePath
  -> Maybe BinaryPaths
  -> Maybe (NominalDiffTime, e)
  -> [String]
  -> ([Text] -> [Text] -> Either e Text)
  -> ExceptT e m Text
runClientCommand' nodeRpcURI clientDataDir maybePaths mTimeout args handleError = do
  liftIO $ createDirectoryIfMissing True clientDataDir
  le <- askLoggerIO
  let procSpec = Process.proc (clientPath maybePaths) (["--endpoint", T.unpack $ render nodeRpcURI, "--base-dir", clientDataDir] ++ args)
      runProc = runLoggingEnv (LoggingEnv le) $ readCreateProcessWithExitCodeWithLogging procSpec ""
      withTimeout run handle = flip (maybe ((liftIO run) >>= handle)) mTimeout $ \(t, err) -> (liftIO $ timeout' t run) >>= \case
        Just v -> handle v
        Nothing -> do
          $(logInfo) "runClientCommand Timedout"
          throwError err
  withTimeout runProc $ \(exitCode, stdout, stderr) -> case exitCode of
    ExitSuccess -> pure $ T.strip stdout
    ExitFailure _ -> do
      $(logInfo) $ "runClientCommand failed: " <> stderr
      let strippedLines = fmap T.strip $ T.lines stderr
          warnings = takeWhile (/= "Error:") $ drop 1 $ dropWhile (/= "Warning:") strippedLines
          errors = filter (/= "Error:") $ dropWhile (/= "Error:") strippedLines
          fatal = drop 1 $ dropWhile (/= "Fatal error:") $ fmap T.strip $ T.lines stdout -- yes, fatal errors go to stdout
      case handleError warnings (fatal ++ errors) of
        Right t -> pure t
        Left e -> do
          $(logInfo) $ T.pack $ show e
          throwError e


runClientCommand
  :: (MonadLoggerIO m, Show e)
  => AppConfig
  -> Maybe (NominalDiffTime, e)
  -> [String]
  -> ([Text] -> [Text] -> Either e Text)
  -> ExceptT e m Text
runClientCommand appConfig = runClientCommand' (kilnNodeRpcURI appConfig) (tezosClientDataDir appConfig) (_appConfig_binaryPaths appConfig)

computeChainId :: (MonadLoggerIO m) => Port -> FilePath -> Maybe BinaryPaths -> Aeson.Value -> m (Either Text ChainId)
computeChainId port kilnDataDir maybePaths json = do
    e <- runExceptT $ ExceptT (pure eCommand) >>= \command -> runClientCommand' (kilnNodeRpcURI' port) kilnDataDir maybePaths noTimeout command $ \_warnings errors -> if
      | "Wrong value for command line option --protocol" : _ <- errors -> Left "Wrong Protocol"
      | otherwise -> Left $ "'tezos-client compute chain id' failed with the following error: " <> unwords (map T.unpack errors)
    pure $ first T.pack e >>= first tshow . fromBase58 . TE.encodeUtf8
  where
    note key' = maybe (Left $ printf "key %s not available" key') (Right . T.unpack)
    protocol = note ("protocol" :: String) $ json ^? key "network" . key "genesis" . key "protocol" . _String
    genesisBlock = note ("block" :: String) $ json ^? key "network" . key "genesis" . key "block" . _String
    eCommand :: Either String [String]
    eCommand =
      liftA2 (\p gb -> words $ printf "--protocol %s compute chain id from block hash %s" p gb) protocol genesisBlock

setupLedgerToBake :: (MonadLoggerIO m) => AppConfig -> Pool Postgresql -> NodeDataSource -> SecretKey -> m ()
setupLedgerToBake appConfig db nds sk = do
  mla <- withDbAndConfig db appConfig $ selectSingle $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
  for_ mla $ \la -> ledgerSetupStep appConfig db sk (mempty { _setupState_setup = Just $ First SetupLedgerToBakeStep_Prompting })
    (\(isReg, res) -> mempty { _setupState_setup = Just $ First $ bool res SetupLedgerToBakeStep_DoneAndRegistered isReg }) $ do
      e <- runExceptT $ runClientCommand appConfig noTimeout ["setup", "ledger", "to", "bake", "for", T.unpack kilnLedgerAlias] $ \_warnings errors -> if
        | "Ledger Application level error (setup): Conditions of use not satisfied" : _ <- errors -> Left SetupLedgerToBakeStep_Declined
        | "Ledger Transport level error:" : _ <- errors -> Left SetupLedgerToBakeStep_Disconnected
        | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left SetupLedgerToBakeStep_Disconnected
        | "This command (`setup ledger ...`) is not compatible with this version" : version'' : _ <- errors
        , Just version' <- T.stripPrefix "of the Ledger Baking app (Tezos Baking " version''
        , version <- T.takeWhile (/= ' ') version'
        -> Left $ SetupLedgerToBakeStep_OutdatedVersion version
        | otherwise -> Left SetupLedgerToBakeStep_Failed
      let res = fromLeft SetupLedgerToBakeStep_Done e
      isReg <- if res == SetupLedgerToBakeStep_Done
        then (fromMaybe False <$>) $ traverse (checkIfRegistered db nds) $ _ledgerAccount_publicKeyHash la
        else pure False
      when isReg $ withDbAndConfig db appConfig $ traverse_ startBaking $ _ledgerAccount_publicKeyHash la
      pure (isReg, res)

checkIfRegistered :: MonadIO m => Pool Postgresql -> NodeDataSource -> PublicKeyHash -> m Bool
checkIfRegistered db nds pkh = do
  delegateInfoOrErr <- flip runReaderT nds . runExceptT @KilnRpcError $ do
    mbHeadBlock <- ask >>= liftIO . atomically . dataSourceFinalHead
    case mbHeadBlock of
      Nothing -> ExceptT $ pure $ Left KilnRpcError_NoKnownHeads
      Just headBlock -> nodeQueryDataSource $ NodeQuery_DelegateInfo (headBlock ^. hash) (headBlock ^. level) pkh
  let isReg = case delegateInfoOrErr of
        Right delegateInfo -> not (_cacheDelegateInfo_deactivated delegateInfo)
        _ -> False
  liftIO $ runLoggingEnv (_nodeDataSource_logger nds) $ runDb (Identity db) $
    notify NotifyTag_BakerRegistered (pkh, isReg)
  pure isReg

-- If node isn't synced, this command will block while it waits for the node to
-- get up-to-date. We detect that case and just return an error.
registerKeyAsDelegate
  :: (MonadLoggerIO m)
  => Pool Postgresql -> NodeDataSource -> SecretKey -> AppConfig -> m ()
registerKeyAsDelegate db nds sk appConfig = do
  mla <- withDbAndConfig db appConfig $ selectSingle $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
  for_ mla $ \la -> do
    let mbPkh = _ledgerAccount_publicKeyHash la
    isReg <- (fromMaybe False <$>) $ traverse (checkIfRegistered db nds) $ mbPkh
    result <- case isReg of
      True -> pure RegisterStep_AlreadyRegistered
      False -> do
        withDbAndConfig db appConfig $
          notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_register = Just $ First RegisterStep_Prompting })
        -- withCreateProcess will close these automatically
        (readPipe, writePipe) <- liftIO Process.createPipe
        let p = (Process.proc (clientPath $ _appConfig_binaryPaths appConfig) ["--endpoint", T.unpack $ render $  kilnNodeRpcURI appConfig, "--base-dir", tezosClientDataDir appConfig, "register", "key", T.unpack kilnLedgerAlias, "as", "delegate"])
              { Process.std_err = Process.UseHandle writePipe
              , Process.std_out = Process.UseHandle writePipe
              }
        $(logInfoSH) $ ("registerKeyAsDelegate: process: " :: Text, p)
        result <- liftIO $ Process.withCreateProcess p $ \_ _ _ ph -> runLoggingEnv (_nodeDataSource_logger nds) $ do
          let notifyStep rs = runDb (Identity db) $ notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_register = Just $ First rs })
              go mrs' = liftIO (hIsEOF readPipe) >>= \case
                True -> liftIO (Process.waitForProcess ph) >>= \case
                  ExitFailure _ -> pure $ fromMaybe RegisterStep_Failed mrs'
                  ExitSuccess -> pure RegisterStep_Registered -- Succeeds if already registered too
                False -> do
                  t <- liftIO $ catchJust (guard . isEOFError) (T.hGetLine readPipe) (\() -> pure "")
                  let mrs = parseRegisterStep t
                  $(logInfo) $ "registerKeyAsDelegate: " <> t <> " -> " <> T.pack (show mrs)
                  traverse_ notifyStep mrs
                  case mrs of
                    Just RegisterStep_NodeNotReady -> pure RegisterStep_NodeNotReady
                    _ -> go $ mrs' <|> mrs
          go Nothing
        $(logWarn) $ T.pack $ show result
        pure result
    withDbAndConfig db appConfig $ do
      notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_register = Just $ First result })
      when (result == RegisterStep_Registered) $ traverse_ startBaking mbPkh


-- Most of the steps that require interaction with ledger are similar. At first, they prompt user
-- that interaction with the ledger is required, then they call 'tezos-client' and update UI
-- accordingly to the 'tezos-client' call result
ledgerSetupStep :: MonadLoggerIO m => AppConfig -> Pool Postgresql -> SecretKey -> SetupState -> (a -> SetupState) -> m a -> m ()
ledgerSetupStep appConfig db sk initialSetupState endingSetupState setupStep = do
  withDbAndConfig db appConfig $
    notify NotifyTag_Prompting (sk, Just initialSetupState)
  setupRes <- setupStep
  withDbAndConfig db appConfig $
    notify NotifyTag_Prompting (sk, Just $ endingSetupState setupRes)

parseRegisterStep :: Text -> Maybe RegisterStep
parseRegisterStep (T.strip -> err)
  | T.isInfixOf "Ledger Application level error (sign): Conditions of use not satisfied" err
  = Just RegisterStep_Declined
  | T.isInfixOf "Ledger Transport level error:" err
  = Just RegisterStep_Disconnected
  | T.isInfixOf "Empty implicit contract " err
  = Just $ RegisterStep_NotEnoughFunds 0
  -- Balance of contract tz1VeX1Wso2LRGW2rpgKHoyFkHUxJpvSxLWP too low (860) to spend 1000
  | Just pkhBalance <- T.stripPrefix "Balance of contract " err
  , Just bal <- readMaybe (T.unpack $ T.takeWhile (/= ')') $ T.drop 1 $ T.dropWhile (/= '(') pkhBalance)
  = Just $ RegisterStep_NotEnoughFunds $ Tez bal
  | err == "Waiting for the operation to be included..."
  = Just RegisterStep_WaitingForInclusion
  | err == "Waiting for the node to be bootstrapped before injection..."
  = Just RegisterStep_NodeNotReady
  | otherwise = Nothing

setHighWaterMark :: (MonadLoggerIO m) => AppConfig -> Pool Postgresql -> SecretKey -> RawLevel -> m ()
setHighWaterMark appConfig db sk bl =
  ledgerSetupStep appConfig db sk (mempty { _setupState_setHWM = Just $ First SetHWMStep_Prompting }) (\res -> mempty { _setupState_setHWM = Just $ First res }) $ do
    e <- runExceptT $ runClientCommand appConfig noTimeout ["set", "ledger", "high", "watermark", "for", T.unpack (toSecretKeyText sk), "to", show (unRawLevel bl)] $ \_warnings errors -> if
      | "Ledger Application level error (set_high_watermark): Conditions of use not satisfied" : _ <- errors -> Left SetHWMStep_Declined
      | "Ledger Transport level error:" : _ <- errors -> Left SetHWMStep_Disconnected
      | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left SetHWMStep_Disconnected
      | otherwise -> Left $ SetHWMStep_Failed $ T.unlines errors
    pure $ fromLeft SetHWMStep_Done e

submitVote :: (MonadLoggerIO m) => AppConfig -> Pool Postgresql -> NodeDataSource -> SecretKey -> Id PeriodProposal -> Maybe Ballot -> m ()
submitVote appConfig db nds sk p b = do
  withDbAndConfig db appConfig $
    notify NotifyTag_VotePrompting (sk, Just $ mempty { _voteState_step = Just $ First VoteStep_Prompting })
  dsh <- liftIO $ atomically $ dataSourceFinalHead nds
  let attempted = view hash <$> dsh
  (mbProposal :: Maybe PeriodProposal) <- withDbAndConfig db appConfig $ selectSingle (AutoKeyField ==. fromId p)
  mla <-
    withDbAndConfig db appConfig $ selectSingle $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
  for_ mla $ \la -> for_ (_ledgerAccount_publicKeyHash la) $ \pkh -> for_ mbProposal $ \proposal -> do
    let proposalHash = proposal ^. periodProposal_hash
    vs <- case b of
      Nothing -> do
        vs <- submitProposals appConfig [proposalHash]
        when (vs == VoteStep_Done) $ withDbAndConfig db appConfig $ do
          _ <- [executeQ|
            INSERT INTO "BakerProposal" (pkh, proposal, included, attempted)
            VALUES (?pkh, ?p, null, ?attempted)
            ON CONFLICT DO NOTHING
          |]
          notify NotifyTag_Proposals (p, Just (proposal, Just False))
        pure vs
      Just ballot -> do
        vs <- submitBallot appConfig proposalHash ballot
        when (vs == VoteStep_Done) $ withDbAndConfig db appConfig $ do
          let bv = BakerVote
                { _bakerVote_pkh = pkh
                , _bakerVote_proposal = p
                , _bakerVote_ballot = ballot
                , _bakerVote_included = Nothing
                , _bakerVote_attempted = attempted
                }
          insert_ bv
          notify NotifyTag_BakerVote $ Just bv
        pure vs
    withDbAndConfig db appConfig $
      notify NotifyTag_VotePrompting (sk, Just $ mempty { _voteState_step = Just $ First vs })

submitProposals :: (MonadLoggerIO m) => AppConfig -> [ProtocolHash] -> m VoteStep
submitProposals appConfig proposals = do
  e <- runExceptT $ runClientCommand appConfig noTimeout (["submit", "proposals", "for", T.unpack kilnLedgerAlias] ++ map (T.unpack . toBase58Text) proposals) $ \_warnings errors -> if
    | "Submission failed because of invalid proposals." : _ <- errors -> Left $ VoteStep_Failed "Invalid proposals"
    | "Ledger Application level error (sign): Unregistered status message" : _ <- errors -> Left $ VoteStep_Failed "Not in wallet app"
    | "Ledger Application level error (sign): Conditions of use not satisfied" : _ <- errors -> Left VoteStep_Declined
    | "Unauthorized ballot" : _ <- errors -> Left $ VoteStep_Failed "Unauthorized ballot"
    | "Not in a proposal period" : _ <- errors -> Left VoteStep_WrongPeriod
    | "Ledger Transport level error:" : _ <- errors -> Left VoteStep_Disconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left VoteStep_Disconnected
    | otherwise -> Left $ VoteStep_Failed $ T.unlines errors
  pure $ either id (const VoteStep_Done) e

submitBallot :: (MonadLoggerIO m) => AppConfig -> ProtocolHash -> Ballot -> m VoteStep
submitBallot appConfig proposal ballot = do
  e <- runExceptT $ runClientCommand appConfig noTimeout ["submit", "ballot", "for", T.unpack kilnLedgerAlias, T.unpack (toBase58Text proposal), ballotText ballot] $ \_warnings errors -> if
    | "Ledger Application level error (sign): Unregistered status message" : _ <- errors -> Left $ VoteStep_Failed "Not in wallet app"
    | "Ledger Application level error (sign): Conditions of use not satisfied" : _ <- errors -> Left VoteStep_Declined
    | "Unauthorized ballot" : _ <- errors -> Left $ VoteStep_Failed "Unauthorized ballot"
    | "Not in a Testing_vote or Promotion_vote period" : _ <- errors -> Left VoteStep_WrongPeriod
    | "Ledger Transport level error:" : _ <- errors -> Left VoteStep_Disconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left VoteStep_Disconnected
    | otherwise -> Left $ VoteStep_Failed $ T.unlines errors
  pure $ either id (const VoteStep_Done) e
  where
    ballotText = \case
      Ballot_Yay -> "yay"
      Ballot_Nay -> "nay"
      Ballot_Pass -> "pass"

-- Logic mostly copied from viewselector' next rights code
checkKilnBakerAndNextRights :: (BlockLike blk) => AppConfig -> NodeDataSource -> blk -> LoggingT IO (Maybe PublicKeyHash, Maybe (RightKind, RawLevel), Maybe RawLevel)
checkKilnBakerAndNextRights appConfig nds blk = withDbAndConfig (_nodeDataSource_pool nds) appConfig $ do
  v <- flip runReaderT nds $ runExceptT @KilnRpcError $ tryNodeQueryT $ do
    let headLevel = blk ^. level
        chainId = _appConfig_chainId appConfig
    bakerInt :: Maybe PublicKeyHash <- join . listToMaybe <$> project (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector ~> BakerDaemonInternalData_publicKeyHashSelector)
      (BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector ==. False)

    progressMay :: Maybe RawLevel <- flip (maybe (pure Nothing)) bakerInt $ \pkh -> do
      fmap (headMay . fmap fromOnly) [queryQ|
        SELECT brp."progress"
        FROM "BakerRightsProgress" brp
        WHERE brp."chainId" = ?chainId
          AND brp."publicKeyHash" = ?pkh
      |]

    rightsMay :: Maybe (RightKind, RawLevel) <- flip (maybe (pure Nothing)) bakerInt $ \pkh -> do
      fmap (headMay . sortOn snd) [queryQ|
          SELECT br."right", MIN(br.level)
          FROM "BakerRightsProgress" brp
          JOIN "BakerRight" br
            ON br.branch = brp.id
            AND br.level > ?headLevel + CASE WHEN br."right" = 'RightKind_Endorsing' THEN -1 ELSE 0 END -- if the endorsement is of the current block, you haven't missed it yet.
          WHERE brp."chainId" = ?chainId
            AND brp."publicKeyHash" = ?pkh
          GROUP BY brp."publicKeyHash", br."right"
        |]

    pure (bakerInt, rightsMay, progressMay)
  pure (v^?_Right._Just._1._Just, v^?_Right._Just._2._Just, v^?_Right._Just._3._Just)
