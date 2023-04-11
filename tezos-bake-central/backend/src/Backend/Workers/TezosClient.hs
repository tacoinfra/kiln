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
{-# LANGUAGE DoAndIfThenElse #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Workers.TezosClient where

import Control.Concurrent.STM (atomically, flushTQueue)
import Control.Exception (catchJust)
import Control.Monad.Except
import Control.Monad.Logger
import Data.Aeson.Lens
import Data.Either (fromLeft)
import Data.Either.Combinators (whenLeft)
import Data.Int (Int32)
import Data.Pool (Pool)
import Data.Time (NominalDiffTime)
import Database.Groundhog
import Database.Groundhog.Postgresql (Postgresql(..), SqlDb)
import Database.Id.Class
import Database.Id.Groundhog
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.PsqlSimple (executeQ)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Rhyolite.Backend.DB.Serializable (Serializable)
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
import Backend.Common
  (LedgerQuery(..), LedgerQueryType(..), addBakerImpl,
  readCreateProcessWithExitCodeWithLogging, timeout', withDbAndConfig)
import Backend.Config (AppConfig (..), tezosClientDataDir, kilnNodeRpcURI', kilnNodeRpcURI, BinaryPaths(..))
import Backend.NodeRPC
import Backend.Schema
import Common.App
import Common.Schema
import Common.URI (Port)
import ExtraPrelude
import Tezos.Common.PublicKeyHash (PublicKeyHash)

startBaking :: (PersistBackend m, SqlDb (PhantomDb m), MonadIO m) => NodeDataSource -> PublicKeyHash -> m ()
startBaking nds pkh = do
  -- At the moment when baker starts all possible remaining ledger queries are connectivity checks. We're
  -- dropping them in order to avoid clashing baker daemon with 'tezos-client list connected ledgers'
  void $ liftIO $ atomically $ flushTQueue $ _nodeDataSource_ledgerIOQueue nds
  addBakerImpl pkh (Just "Kiln Baker")
  mbBdi <- fmap listToMaybe $ fmap snd <$> selectAll
  for_ mbBdi $ \bdi -> do
    let
      bakerProcess = bdi
        & _bakerDaemonInternal_data
        & _deletableRow_data
        & _bakerDaemonInternalData_bakerProcessData
        & fromId
    update [ BakerDaemonInternal_dataField ~> DeletableRow_dataSelector ~> BakerDaemonInternalData_publicKeyHashSelector =. Just pkh
           , BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector =. False] CondEmpty
    update [ ProcessData_controlField =. ProcessControl_Run
           , ProcessData_errorLogField =. (Nothing :: Maybe Text)] $ AutoKeyField ==. bakerProcess

updateConnectedLedgerViaGetConnectedLedger :: AppConfig -> Pool Postgresql -> LedgerQuery (LoggingT IO)
updateConnectedLedgerViaGetConnectedLedger appConfig db = LedgerQuery LedgerQueryType_PollLedger $ do
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

defaultTimeout :: Maybe (NominalDiffTime, ClientError)
defaultTimeout = Just (20, ClientError_Timeout)

noTimeout :: Maybe (NominalDiffTime, e)
noTimeout = Nothing

checkLedgerHighWatermark :: (Monad m, MonadLoggerIO m) => AppConfig -> NodeDataSource -> LedgerQuery m
checkLedgerHighWatermark appConfig nds = LedgerQuery LedgerQueryType_CheckHWM $ do
  let db = _nodeDataSource_pool nds
  eiHwm <- getLedgerHighWatermark appConfig nds
  case eiHwm of
    Right (Just hwm) -> do
      mbLatestHeadLevel <- liftIO $ atomically $ dataSourceHeadLevel nds
      reportLedgerNeedToResetHWM db appConfig mbLatestHeadLevel hwm
    -- If we couldn't get high-witermark, then most likely ledger has been disconnected
    -- and it will be reported in another function.
    _ -> pure ()

getLedgerHighWatermark :: (MonadLoggerIO m) => AppConfig -> NodeDataSource -> m (Either ClientError (Maybe RawLevel))
getLedgerHighWatermark appConfig nds = runExceptT $ do
  let
    db = _nodeDataSource_pool nds
    logger = _nodeDataSource_logger nds
    inDb :: (MonadIO m) => Serializable a -> m a
    inDb = runLoggingEnv logger . runDb (Identity db)
  mbIntBakerPkh <- fmap join $ inDb $ project1
    (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector ~> BakerDaemonInternalData_publicKeyHashSelector)
    (BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector ==. False)
  fmap join $ for mbIntBakerPkh $ \intBakerPkh ->  do
    cachedHWM <- fmap join $ inDb $
      project1 LedgerAccount_highWatermarkField $ LedgerAccount_publicKeyHashField ==. Just intBakerPkh
    case cachedHWM of
      Nothing -> do
        mbSecretKey <- inDb $ project1 LedgerAccount_secretKeyField (LedgerAccount_publicKeyHashField ==. Just intBakerPkh)
        case mbSecretKey of
          Nothing -> error "Ledger secret key not found in db while fetching ledger high-watermark."
          Just sk -> do
            let
              args =
                [ "get"
                , "ledger"
                , "high"
                , "watermark"
                , "for"
                , T.unpack (toSecretKeyText sk)
                ]
            stdout <- runClientCommand appConfig defaultTimeout args $ \_warnings errors -> if
              | e : _ <- errors, Just _sk' <- T.stripPrefix "Found no ledger corresponding to " e -> Left ClientError_LedgerDisconnected
              | otherwise -> Left $ ClientError_Other $ T.unlines errors
            let hwm = getHWMFromStdout stdout (_appConfig_chainId appConfig)
            inDb $ update [LedgerAccount_highWatermarkField =. hwm] (LedgerAccount_publicKeyHashField ==. Just intBakerPkh)
            pure hwm
      _ -> pure cachedHWM
  where
    -- The example 'tezos-client get ledger high watermark for ...' output:
    {-
      The high water mark values for
      <ledger_url> are
      380000 for the main-chain (NetXLH1uAxK7CCh) and
      380000 for the test-chain.
    -}
    getHWMFromStdout :: Text -> ChainId -> Maybe RawLevel
    getHWMFromStdout stdout chainId =
      let
        prettyChainId = toBase58Text chainId
        line = find (T.isInfixOf prettyChainId) (T.lines stdout)
      in case T.words <$> line of
        Just (hwmStr : _ )-> fmap fromIntegral $ readMaybe @Int32 $ T.unpack hwmStr
        _ -> Nothing

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

-- | Checks whether @PublicKeyHash@ of a given @SecretKey@ is already known or was previously already requested
-- and expected to be fetched in future
isKnownLedgerPkh :: (MonadLoggerIO m, MonadIO m) => AppConfig -> Pool Postgresql -> SecretKey -> m Bool
isKnownLedgerPkh appConfig db sk = do
  withDbAndConfig db appConfig $ do
    existing <- selectSingle $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
    case existing of
      Just la -> do
        update [LedgerAccount_balanceField =. (Nothing :: Maybe Tez)] $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
        pure $ isJust (_ledgerAccount_publicKeyHash la) || _ledgerAccount_requested la
      Nothing -> do
        let la = LedgerAccount
              { _ledgerAccount_secretKey = sk
              , _ledgerAccount_publicKeyHash = Nothing
              , _ledgerAccount_balance = Nothing
              , _ledgerAccount_imported = False
              , _ledgerAccount_requested = True
              , _ledgerAccount_highWatermark = Nothing
              }
        insert la
        pure False

fetchBalances :: (MonadLoggerIO m, MonadIO m) => AppConfig -> Pool Postgresql -> NodeDataSource -> [SecretKey] -> m ()
fetchBalances appConfig db nds sks = withDbAndConfig db appConfig $ for_ sks $ \sk -> do
  mla <- selectSingle $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
  for_ mla $ \la -> case _ledgerAccount_publicKeyHash la of
    -- pkh is not yet known, balance will be fetched later
    Nothing -> pure ()
    Just pkh -> do
      balanceOrErr <- flip runReaderT nds . runExceptT @KilnRpcError $ do
        mbHeadBlock <- ask >>= liftIO . atomically . dataSourceFinalHead
        case mbHeadBlock of
          Nothing -> ExceptT $ pure $ Left KilnRpcError_NoKnownHeads
          Just headBlock -> nodeQueryDataSource $ nodeQuery_Balance (headBlock ^. hash) (headBlock ^. level) pkh
      case balanceOrErr of
        Left err -> $(logError) $ "Failed to get balance of account " <> toPublicKeyHashText pkh <> " due to: " <> prettyKilnRpcError err
        Right balance -> do
          update [LedgerAccount_balanceField =. Just balance] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
          notify NotifyTag_ShowLedger (sk, Right (pkh, Just balance))

showLedger
  :: ( MonadLoggerIO m
     , MonadIO m
     )
  => AppConfig
  -> Pool Postgresql
  -> NodeDataSource
  -> SecretKey
  -> LedgerQuery m
showLedger appConfig db nds sk = LedgerQuery LedgerQueryType_ShowLedger $
  showLedgerWithRetry appConfig db nds sk 3

showLedgerWithRetry
  :: ( MonadLoggerIO m
     , MonadIO m
     )
  => AppConfig
  -> Pool Postgresql
  -> NodeDataSource
  -> SecretKey
  -> Int
  -> m ()
showLedgerWithRetry appConfig db nds sk numRetries = do
  resOrErr <- runExceptT $ showLedgerImpl appConfig db nds sk
  whenLeft resOrErr $ \err -> do
    if numRetries <= 0
    then handleClientError err
    else showLedgerWithRetry appConfig db nds sk (numRetries - 1)
  where
    handleClientError = \case
      err@ClientError_PublicKeyHashNotFound -> withDbAndConfig db appConfig $ do
        -- If we failed to fetch pkh, we'll attempt once again on the next iteration
        update [LedgerAccount_requestedField =. False] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
        notify NotifyTag_ShowLedger (sk, Left $ prettyClientError err)
      err -> do
        withDbAndConfig db appConfig $ do
          delete $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
          notify NotifyTag_ShowLedger (sk, Left $ prettyClientError err)
        $(logError) $ "showLedgerWithRetry: " <> prettyClientError err

showLedgerImpl
  :: ( MonadIO m
     , MonadLoggerIO m
     )
  => AppConfig
  -> Pool Postgresql
  -> NodeDataSource
  -> SecretKey
  -> ExceptT ClientError m ()
showLedgerImpl appConfig db nds sk = do
  let logger = _nodeDataSource_logger nds
  mbPkh <- do
    stdout <- runClientCommand appConfig defaultTimeout ["show", "ledger", T.unpack $ toSecretKeyText sk] $ \_warnings errors -> if
      | e : _ <- errors, Just _sk' <- T.stripPrefix "No ledger found for " e -> Left ClientError_LedgerDisconnected
      | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
      | "(Invalid_argument int32_of_path_element_exn)" : _ <- errors -> Right ""
      | otherwise -> Left $ ClientError_Other $ T.unlines errors
    let pkh = getPublicKeyHash (T.lines stdout)
    when (isNothing pkh) $ $(logWarn) $ "showLedger: failed to find public key hash from: " <> stdout
    pure pkh
  case mbPkh of
    Nothing -> throwError ClientError_PublicKeyHashNotFound
    Just pkh -> withDbAndConfig db appConfig $ do
      runLoggingEnv logger $ runDb (Identity db) $ flip runReaderT appConfig $ do
        update [LedgerAccount_publicKeyHashField =. Just pkh] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
        notify NotifyTag_ShowLedger (sk, Right (pkh, Nothing))
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

importSecretKey :: MonadLoggerIO m => AppConfig -> Pool Postgresql -> SecretKey -> LedgerQuery m
importSecretKey appConfig db sk = LedgerQuery LedgerQueryType_ImportKey $
  ledgerSetupStep appConfig db sk (mempty { _setupState_import = Just $ First ImportSecretKeyStep_Prompting })
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
  -> ExceptT (e, [Text]) m Text
runClientCommand' nodeRpcURI clientDataDir maybePaths mTimeout args handleError = do
  liftIO $ createDirectoryIfMissing True clientDataDir
  le <- askLoggerIO
  let procSpec = Process.proc (clientPath maybePaths) (["--endpoint", T.unpack $ render nodeRpcURI, "--base-dir", clientDataDir] ++ args)
      runProc = runLoggingEnv (LoggingEnv le) $ readCreateProcessWithExitCodeWithLogging procSpec ""
      withTimeout run handle = flip (maybe (liftIO run >>= handle)) mTimeout $ \(t, err) -> liftIO (timeout' t run) >>= \case
        Just v -> handle v
        Nothing -> do
          $(logInfo) "runClientCommand Timedout"
          throwError (err, [])
  withTimeout runProc $ \(exitCode, stdout, stderr) -> case exitCode of
    ExitSuccess -> pure $ T.strip stdout
    ExitFailure _ -> do
      $(logInfo) $ "runClientCommand failed: " <> stderr
      let strippedLines = fmap T.strip $ T.lines stderr
          warnings = takeWhile (/= "Error:") $ drop 1 $ dropWhile (/= "Warning:") strippedLines
          errors = filter (/= "Error:") $ dropWhile (/= "Error:") strippedLines
          fatal = drop 1 $ dropWhile (/= "Fatal error:") $ fmap T.strip $ T.lines stdout -- yes, fatal errors go to stdout
          allErrors = fatal ++ errors
      case handleError warnings allErrors of
        Right t -> pure t
        Left e -> do
          $(logInfo) $ T.pack $ show e
          throwError (e, allErrors)

-- | Specialized version of @runClientCommand'@ that also
-- returns command's stderr in addition to the error itself.
runClientCommandReturnsStderr
  :: (MonadLoggerIO m, Show e)
  => AppConfig
  -> Maybe (NominalDiffTime, e)
  -> [String]
  -> ([Text] -> [Text] -> Either e Text)
  -> ExceptT (e, [Text]) m Text
runClientCommandReturnsStderr appConfig = runClientCommand'
  (kilnNodeRpcURI appConfig)
  (tezosClientDataDir appConfig)
  (_appConfig_binaryPaths appConfig)

runClientCommand
  :: (MonadLoggerIO m, Show e)
  => AppConfig
  -> Maybe (NominalDiffTime, e)
  -> [String]
  -> ([Text] -> [Text] -> Either e Text)
  -> ExceptT e m Text
runClientCommand appConfig mTimeout args handler = withExceptT fst $ runClientCommand'
  (kilnNodeRpcURI appConfig)
  (tezosClientDataDir appConfig)
  (_appConfig_binaryPaths appConfig)
  mTimeout
  args
  handler

computeChainId
  :: (MonadLoggerIO m)
  => Port
  -> FilePath
  -> Maybe BinaryPaths
  -> Aeson.Value
  -> m (Either Text ChainId)
computeChainId port kilnDataDir maybePaths json = do
    e <- runExceptT $ ExceptT (pure eCommand) >>= \command -> runClientCommand' (kilnNodeRpcURI' port) kilnDataDir maybePaths noTimeout command $ \_warnings errors -> if
      | "Wrong value for command line option --protocol" : _ <- errors -> Left "Wrong Protocol"
      | otherwise -> Left $ "'tezos-client compute chain id' failed with the following error: " <> unwords (map T.unpack errors)
    pure $ first (T.pack . fst) e >>= first tshow . fromBase58 . TE.encodeUtf8
  where
    note key' = maybe (Left (printf "key %s not available" key', [])) (Right . T.unpack)
    protocol = note ("protocol" :: String) $ json ^? key "network" . key "genesis" . key "protocol" . _String
    genesisBlock = note ("block" :: String) $ json ^? key "network" . key "genesis" . key "block" . _String
    eCommand :: Either (String, [Text]) [String]
    eCommand =
      liftA2 (\p gb -> words $ printf "--protocol %s compute chain id from block hash %s" p gb) protocol genesisBlock

setupLedgerToBake :: (MonadLoggerIO m) => AppConfig -> Pool Postgresql -> NodeDataSource -> SecretKey -> LedgerQuery m
setupLedgerToBake appConfig db nds sk = LedgerQuery LedgerQueryType_SetupToBake $ do
  mla <- withDbAndConfig db appConfig $ selectSingle $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
  mbLatestHeadLevel <- liftIO $ atomically $ dataSourceHeadLevel nds
  let
    RawLevel latestHeadLevel = mbLatestHeadLevel ?: error "Latest head is 'Nothing' during setting up ledger to bake."
    args =
      [ "setup"
      , "ledger"
      , "to"
      , "bake"
      , "for"
      , T.unpack kilnLedgerAlias
      , "--main-hwm"
      , show latestHeadLevel
      ]
  for_ mla $ \la -> ledgerSetupStep appConfig db sk (mempty { _setupState_setup = Just $ First SetupLedgerToBakeStep_Prompting })
    (\(isReg, res) -> mempty { _setupState_setup = Just $ First $ bool res SetupLedgerToBakeStep_DoneAndRegistered isReg }) $ do
      e <- runExceptT $ runClientCommand appConfig noTimeout args $ \_warnings errors -> if
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
      when isReg $ withDbAndConfig db appConfig $ traverse_ (startBaking nds) $ _ledgerAccount_publicKeyHash la
      pure (isReg, res)

checkIfRegistered :: MonadIO m => Pool Postgresql -> NodeDataSource -> PublicKeyHash -> m Bool
checkIfRegistered db nds pkh = do
  delegateInfoOrErr <- flip runReaderT nds . runExceptT @KilnRpcError $ do
    mbHeadBlock <- ask >>= liftIO . atomically . dataSourceFinalHead
    case mbHeadBlock of
      Nothing -> ExceptT $ pure $ Left KilnRpcError_NoKnownHeads
      Just headBlock -> nodeQueryDataSource $ nodeQuery_DelegateInfo (headBlock ^. hash) (headBlock ^. level) pkh
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
  => Pool Postgresql -> NodeDataSource -> SecretKey -> AppConfig -> LedgerQuery m
registerKeyAsDelegate db nds sk appConfig = LedgerQuery LedgerQueryType_ImportKey $ do
  mla <- withDbAndConfig db appConfig $ selectSingle $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
  for_ mla $ \la -> do
    let mbPkh = _ledgerAccount_publicKeyHash la
    isReg <- (fromMaybe False <$>) $ traverse (checkIfRegistered db nds) mbPkh
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
        $(logInfoSH) ("registerKeyAsDelegate: process: " :: Text, p)
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
      when (result == RegisterStep_Registered) $ traverse_ (startBaking nds) mbPkh


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

setHighWaterMark :: (MonadLoggerIO m) => AppConfig -> Pool Postgresql -> SecretKey -> RawLevel -> LedgerQuery m
setHighWaterMark appConfig db sk bl = LedgerQuery LedgerQueryType_SetHWM $
  ledgerSetupStep appConfig db sk (mempty { _setupState_setHWM = Just $ First SetHWMStep_Prompting }) (\res -> mempty { _setupState_setHWM = Just $ First res }) $ do
    e <- runExceptT $ runClientCommand appConfig noTimeout ["set", "ledger", "high", "watermark", "for", T.unpack (toSecretKeyText sk), "to", show (unRawLevel bl)] $ \_warnings errors -> if
      | "Ledger Application level error (set_high_watermark): Conditions of use not satisfied" : _ <- errors -> Left SetHWMStep_Declined
      | "Ledger Transport level error:" : _ <- errors -> Left SetHWMStep_Disconnected
      | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left SetHWMStep_Disconnected
      | otherwise -> Left $ SetHWMStep_Failed $ T.unlines errors
    clearLedgerNeedToResetHWM db appConfig
    pure $ fromLeft SetHWMStep_Done e

submitVote :: (MonadLoggerIO m) => AppConfig -> Pool Postgresql -> NodeDataSource -> SecretKey -> Id PeriodProposal -> Maybe Ballot -> LedgerQuery m
submitVote appConfig db nds sk p b = LedgerQuery LedgerQueryType_Vote $ do
  withDbAndConfig db appConfig $
    notify NotifyTag_VotePrompting (sk, Just $ VoteState (Just VoteStep_Prompting) mempty)
  dsh <- liftIO $ atomically $ dataSourceFinalHead nds
  let attempted = view hash <$> dsh
  (mbProposal :: Maybe PeriodProposal) <- withDbAndConfig db appConfig $ selectSingle (AutoKeyField ==. fromId p)
  mla <-
    withDbAndConfig db appConfig $ selectSingle $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
  for_ mla $ \la -> for_ (_ledgerAccount_publicKeyHash la) $ \pkh -> for_ mbProposal $ \proposal -> do
    let proposalHash = proposal ^. periodProposal_hash
    (vs, errLog) <- case b of
      Nothing -> do
        (vs, stderr) <- submitProposals appConfig [proposalHash]
        when (vs == VoteStep_Done) $ withDbAndConfig db appConfig $ do
          _ <- [executeQ|
            INSERT INTO "BakerProposal" (pkh, proposal, included, attempted)
            VALUES (?pkh, ?p, null, ?attempted)
            ON CONFLICT DO NOTHING
          |]
          notify NotifyTag_Proposals (p, Just (proposal, Just False))
        pure (vs, T.unlines stderr)
      Just ballot -> do
        (vs, stderr) <- submitBallot appConfig proposalHash ballot
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
        pure (vs, T.unlines stderr)
    withDbAndConfig db appConfig $
      notify NotifyTag_VotePrompting (sk, Just $ VoteState (Just vs) errLog)

submitProposals :: (MonadLoggerIO m) => AppConfig -> [ProtocolHash] -> m (VoteStep, [Text])
submitProposals appConfig proposals = do
  e <- runExceptT $ runClientCommandReturnsStderr appConfig noTimeout (["submit", "proposals", "for", T.unpack kilnLedgerAlias] ++ map (T.unpack . toBase58Text) proposals) $ \_warnings errors -> if
    | "Submission failed because of invalid proposals." : _ <- errors -> Left $ VoteStep_Failed "Invalid proposals"
    | "Ledger Application level error (sign): Unregistered status message" : _ <- errors -> Left $ VoteStep_Failed "Not in wallet app"
    | "Ledger Application level error (sign): Conditions of use not satisfied" : _ <- errors -> Left VoteStep_Declined
    | "Unauthorized ballot" : _ <- errors -> Left $ VoteStep_Failed "Unauthorized ballot"
    | "Not in a proposal period" : _ <- errors -> Left VoteStep_WrongPeriod
    | "Ledger Transport level error:" : _ <- errors -> Left VoteStep_Disconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left VoteStep_Disconnected
    | otherwise -> Left $ VoteStep_Failed $ T.unlines errors
  pure $ fromLeft (VoteStep_Done, []) e

submitBallot :: (MonadLoggerIO m) => AppConfig -> ProtocolHash -> Ballot -> m (VoteStep, [Text])
submitBallot appConfig proposal ballot = do
  e <- runExceptT $ runClientCommandReturnsStderr appConfig noTimeout ["submit", "ballot", "for", T.unpack kilnLedgerAlias, T.unpack (toBase58Text proposal), ballotText ballot] $ \_warnings errors -> if
    | "Ledger Application level error (sign): Unregistered status message" : _ <- errors -> Left $ VoteStep_Failed "Not in wallet app"
    | "Ledger Application level error (sign): Conditions of use not satisfied" : _ <- errors -> Left VoteStep_Declined
    | "Unauthorized ballot" : _ <- errors -> Left $ VoteStep_Failed "Unauthorized ballot"
    | "Not in a Testing_vote or Promotion_vote period" : _ <- errors -> Left VoteStep_WrongPeriod
    | "Ledger Transport level error:" : _ <- errors -> Left VoteStep_Disconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left VoteStep_Disconnected
    | otherwise -> Left $ VoteStep_Failed $ T.unlines errors
  pure $ fromLeft (VoteStep_Done, []) e
  where
    ballotText = \case
      Ballot_Yay -> "yay"
      Ballot_Nay -> "nay"
      Ballot_Pass -> "pass"

prettyClientError :: ClientError -> Text
prettyClientError = \case
  ClientError_NodeNotReady -> "Kiln node should be synced to run octez-client command"
  ClientError_RequestDeclinedByLedger -> "Octez-client command has been declined by ledger"
  ClientError_LedgerDisconnected -> "Ledger device is disconnected"
  ClientError_Timeout -> "Timeout while executing octez-client command"
  ClientError_PublicKeyHashNotFound -> "Public key hash is unavailable"
  ClientError_Other desc -> "Octez-client command failed: " <> desc
