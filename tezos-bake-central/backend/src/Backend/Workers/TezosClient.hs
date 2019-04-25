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

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Workers.TezosClient where

import Control.Exception (catchJust)
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT)
import Data.Maybe (mapMaybe)
import Data.List.NonEmpty (nonEmpty)
import Data.Pool (Pool)
import Data.Time (NominalDiffTime)
import Database.Groundhog
import Database.Groundhog.Postgresql (Postgresql, in_)
import Rhyolite.Backend.DB
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Schema (Id (..))
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.IO.Error (isEOFError)
import System.Timeout (timeout)
import System.Which
import Text.Read (readMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified System.Process as Process

import Tezos.Types

import Backend.Common (workerWithDelay)
import Backend.Config (AppConfig (..), tezosClientDataDir, BinaryPaths(..))
import Backend.Schema
import Common.App (ImportSecretKeyStep(..), SetupLedgerToBakeStep(..), RegisterStep(..), SetupState(..), SetHWMStep(..))
import Common.Schema
import ExtraPrelude

tezosClientWorker
  :: NominalDiffTime
  -> LoggingEnv
  -> AppConfig
  -> Pool Postgresql
  -> Either NamedChain BinaryPaths
  -> IO (IO ())
tezosClientWorker delay logger appConfig db chain = runLoggingEnv logger $ do
  workerWithDelay (pure delay) $ const $ runLoggingEnv logger $ do
    $(logDebug) "Tezos client worker"
    liftIO $ createDirectoryIfMissing True (tezosClientDataDir appConfig)
    mConnectedLedger :: Maybe ConnectedLedger <- inDb $ selectSingle CondEmpty
    case mConnectedLedger of
      Just cl
        -- If we think the ledger is connected
        | isJust (_connectedLedger_ledgerIdentifier cl) && isJust (_connectedLedger_updated cl)
        -> do
          -- import secret keys
          inDb (selectSingle $ LedgerAccount_shouldImportField ==. True) >>= \mla -> for_ mla $ \la -> do
            let sk = _ledgerAccount_secretKey la
            inDb $ notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_import = Just $ First ImportSecretKeyStep_Prompting })
            importSecretKey appConfig chain sk >>= \i -> inDb $ do
              update [LedgerAccount_importedField =. False] (LedgerAccount_importedField ==. True)
              update
                [LedgerAccount_importedField =. (i == ImportSecretKeyStep_Done), LedgerAccount_shouldImportField =. False]
                (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
              notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_import = Just $ First i })

          -- setup to bake
          inDb (selectSingle $ LedgerAccount_shouldSetupToBakeField ==. True &&. LedgerAccount_importedField ==. True) >>= \mla -> for_ mla $ \la -> do
            let sk = _ledgerAccount_secretKey la
            inDb $ notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_setup = Just $ First SetupLedgerToBakeStep_Prompting })
            setupLedgerToBake appConfig chain >>= \i -> inDb $ do
              update
                [LedgerAccount_shouldSetupToBakeField =. False]
                (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
              notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_setup = Just $ First i })

          -- register
          inDb (selectSingle $
                LedgerAccount_publicKeyHashField /=. (Nothing :: Maybe PublicKeyHash)
            &&. LedgerAccount_shouldRegisterFeeField /=. (Nothing :: Maybe Tez)
            &&. LedgerAccount_importedField ==. True) >>= \mla -> for_ mla $ \la -> case liftA2 (,) (_ledgerAccount_shouldRegisterFee la) (_ledgerAccount_publicKeyHash la) of
            Nothing -> pure () -- shouldn't happen due to WHERE clause
            Just (fee, pkh) -> do
              let sk = _ledgerAccount_secretKey la
              inDb $ notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_register = Just $ First RegisterStep_Prompting })
              registerKeyAsDelegate appConfig chain fee >>= \result -> inDb $ do
                update
                  [LedgerAccount_shouldRegisterFeeField =. (Nothing :: Maybe Tez)]
                  (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
                notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_register = Just $ First result })
                when (result == RegisterStep_Registered) $ do
                  addBakerImpl pkh (Just "Kiln Baker")
                  bdis :: [BakerDaemonInternal] <- fmap snd <$> selectAll
                  let processes = fmap fromId $ flip concatMap bdis $ \bdi ->
                        [ _bakerDaemonInternalData_bakerProcessData $ _deletableRow_data $ _bakerDaemonInternal_data bdi
                        , _bakerDaemonInternalData_endorserProcessData $ _deletableRow_data $ _bakerDaemonInternal_data bdi
                        ]
                  update [ BakerDaemonInternal_dataField ~> DeletableRow_dataSelector ~> BakerDaemonInternalData_publicKeyHashSelector =. Just pkh
                        , BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector =. False] $ CondEmpty
                  update [ProcessData_controlField =. ProcessControl_Run] $ AutoKeyField `in_` processes

          -- run appropriate 'show ledger' commands, but only do one at a time
          -- to allow other commands to take precedence
          inDb (project1 LedgerAccount_secretKeyField (isFieldNothing LedgerAccount_publicKeyHashField)) >>= \msk -> for_ msk $ \sk -> do
            runClientT (showLedger appConfig chain sk) >>= \case
              Left ClientError_LedgerDisconnected -> inDb $ do
                now <- getTime
                -- Mark ledger as disconnected
                update
                  [ ConnectedLedger_ledgerIdentifierField =. (Nothing :: Maybe LedgerIdentifier)
                  , ConnectedLedger_bakingAppVersionField =. (Nothing :: Maybe Text)
                  , ConnectedLedger_updatedField =. Just now
                  ] CondEmpty
              Left err -> do
                inDb $ do
                  delete $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
                  notify NotifyTag_ShowLedger (sk, Nothing)
                $(logError) (T.pack (show err))
              Right mPkh -> do
                case mPkh of
                  Nothing -> inDb $ notify NotifyTag_ShowLedger (sk, Nothing)
                  Just pkh -> runClientT (getBalanceFor appConfig chain pkh) >>= \case
                    Left err -> $(logError) (T.pack (show err))
                    Right Nothing -> $(logError) $ "Failed to get balance of account " <> toPublicKeyHashText pkh
                    Right (Just tez) -> inDb $ do
                      update [LedgerAccount_balanceField =. Just tez] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
                      notify NotifyTag_ShowLedger (sk, Just (pkh, tez))
                inDb $ (maybe delete (\pkh -> update [LedgerAccount_publicKeyHashField =. Just pkh]) mPkh)
                  (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)

          -- get any missing balances
          las <- inDb $ select $ LedgerAccount_publicKeyHashField /=. (Nothing :: Maybe PublicKeyHash) &&. isFieldNothing LedgerAccount_balanceField
          let las' = mapMaybe (\la -> (,) (_ledgerAccount_secretKey la) <$> _ledgerAccount_publicKeyHash la) las
          for_ las' $ \(sk, pkh) -> runClientT (getBalanceFor appConfig chain pkh) >>= \case
            Left err -> $(logError) (T.pack (show err))
            Right Nothing -> $(logError) $ "Failed to get balance of account " <> toPublicKeyHashText pkh
            Right (Just tez) -> inDb $ do
              update [LedgerAccount_balanceField =. Just tez] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
              notify NotifyTag_ShowLedger (sk, Just (pkh, tez))

          -- set high water mark
          inDb (selectSingle $ LedgerAccount_shouldSetHWMField /=. (Nothing :: Maybe RawLevel)) >>= \mla ->
            for_ mla $ \la -> case _ledgerAccount_shouldSetHWM la of
              Nothing -> pure () -- shouldn't happen
              Just hwm -> do
                let sk = _ledgerAccount_secretKey la
                inDb $ notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_setHWM = Just $ First SetHWMStep_Prompting })
                setHighWaterMark appConfig chain sk hwm >>= \i -> inDb $ do
                  update [LedgerAccount_shouldSetHWMField =. (Nothing :: Maybe RawLevel)] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
                  notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_setHWM = Just $ First i })

        -- If there is a ConnectedLedger row but the updated field is null
        -- (marked for update)
        | isNothing (_connectedLedger_updated cl) -> runClientT (getConnectedLedger appConfig chain) >>= \case
        Left err -> $(logError) (T.pack (show err))
        Right mliv -> do
          liftIO $ print mliv
          inDb $ do
            $(logWarn) "updating connectedledger"
            now <- getTime
            let connectedLedger = ConnectedLedger
                  { _connectedLedger_ledgerIdentifier = fmap fst mliv
                  , _connectedLedger_bakingAppVersion = fmap snd mliv
                  , _connectedLedger_updated = Just now
                  }
            deleteAll connectedLedger
            insert connectedLedger
            notify NotifyTag_ConnectedLedger $ Just connectedLedger
      _ -> pure ()
  where
    inDb :: ReaderT AppConfig (DbPersist Postgresql (LoggingT IO)) a -> LoggingT IO a
    inDb = runDb (Identity db) . flip runReaderT appConfig

-- TODO XXX OBVIOUSLY BAD
clientPath :: Either NamedChain BinaryPaths -> FilePath
clientPath = \case
  Right (BinaryPaths _ c _) -> c
  Left NamedChain_Mainnet -> $(staticWhich "mainnet-tezos-client")
  Left NamedChain_Alphanet -> $(staticWhich "alphanet-tezos-client")
  Left NamedChain_Zeronet -> $(staticWhich "zeronet-tezos-client")


{- Example output from `list connected ledgers`
Found a Tezos Baking 1.5.0 (commit v1.4.3-19-g55cc026d) application running on Ledger Nano S at [0003:0007:00].

To use keys at BIP32 path m/44'/1729'/0'/0' (default Tezos key path), use one of
 tezos-client import secret key ledger_tom "ledger://odd-himalayan-lustrous-falcon/ed25519/0'/0'"
 tezos-client import secret key ledger_tom "ledger://odd-himalayan-lustrous-falcon/secp256k1/0'/0'"
 tezos-client import secret key ledger_tom "ledger://odd-himalayan-lustrous-falcon/P-256/0'/0'"
-}

getConnectedLedger :: (MonadIO m, MonadLogger m) => AppConfig -> Either NamedChain BinaryPaths -> ExceptT ClientError m (Maybe (LedgerIdentifier, Text))
getConnectedLedger appConfig chain = do
  stdout <- runClientCommand appConfig chain ["list", "connected", "ledgers"] $ \_warnings errors -> if
    | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
    | otherwise -> Left $ ClientError_Other $ T.unlines errors
  getKungFuName (T.lines stdout)
  where
    getVersion = fmap (T.takeWhile (/= ' ')) . T.stripPrefix "Found a Tezos Baking "
    getKungFuName = \case
      foundApp : _blank : useKeys : keyExample : _
        | Just version <- getVersion foundApp
        , "To use keys at BIP32 path" `T.isPrefixOf` useKeys -- sanity check
        , Just ledger' <- T.stripPrefix "\"ledger://" (T.dropWhile (/= '"') keyExample)
        , ledger <- T.takeWhile (/= '/') ledger'
        , [_1, _2, _3, _4] <- T.splitOn "-" ledger -- sanity check formatting of ledger
        -> pure $ Just (LedgerIdentifier ledger, version)
      xs -> do
        $(logWarn) $ "getConnectedLedger: failed to find kung fu name of ledger from: " <> T.unlines xs
        pure $ Nothing

getBalanceFor :: (MonadIO m, MonadLogger m) => AppConfig -> Either NamedChain BinaryPaths -> PublicKeyHash -> ExceptT ClientError m (Maybe Tez)
getBalanceFor appConfig chain pkh = do
  stdout <- runClientCommand appConfig chain ["get", "balance", "for", T.unpack $ toPublicKeyHashText pkh] $ \warnings errors -> if
    | "Failed to acquire the protocol version from the node" : _ <- warnings
    , "Unrecognized command." : _ <- errors -> Left ClientError_NodeNotReady
    | otherwise -> Left $ ClientError_Other $ T.unlines errors
  pure $ case T.stripSuffix " ꜩ" stdout of
    Just x | Just micro <- Aeson.decodeStrict (TE.encodeUtf8 x) -> Just $ Tez micro
    _ -> Nothing

{- Example output for `show ledger`
Found a Tezos Baking 1.5.0 application running on a Ledger Nano S at [0003:0007:00].
Tezos address at this path/curve: tz1NXDWqwMv1Zi7Jo9za7YN9orap94XQmFSv
Corresponding full public key: edpkuSWMVjedhmQHarHMxvzdLV69cRWERM9yk4H8FAAfuexz3L9bCM
-}

showLedger :: (MonadIO m, MonadLogger m) => AppConfig -> Either NamedChain BinaryPaths -> SecretKey -> ExceptT ClientError m (Maybe PublicKeyHash)
showLedger appConfig chain sk = do
  stdout <- runClientCommand appConfig chain ["show", "ledger", T.unpack $ toSecretKeyText sk] $ \_warnings errors -> if
    | e : _ <- errors, Just _sk' <- T.stripPrefix "No ledger found for " e -> Left ClientError_LedgerDisconnected
    | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
    | "(Invalid_argument int32_of_path_element_exn)" : _ <- errors -> Right ""
    | otherwise -> Left $ ClientError_Other $ T.unlines errors
  let pkh = getPublicKeyHash (T.lines stdout)
  when (isNothing pkh) $ $(logWarn) $ "showLedger: failed to find public key hash from: " <> stdout
  pure pkh
  where
    getPublicKeyHash = \case
      foundApp : pkh' : _
        | T.isPrefixOf "Found a Tezos Baking " foundApp
        , Just pkht <- T.stripPrefix "Tezos address at this path/curve: " pkh'
        , Right pkh <- tryReadPublicKeyHashText pkht
        -> Just pkh
      _ -> Nothing

importSecretKey :: (MonadIO m, MonadLogger m) => AppConfig -> Either NamedChain BinaryPaths -> SecretKey -> m ImportSecretKeyStep
importSecretKey appConfig chain sk = do
  e <- runExceptT $ runClientCommand appConfig chain ["import", "secret", "key", T.unpack kilnLedgerAlias, T.unpack $ toSecretKeyText sk, "--force"] $ \_warnings errors -> if
    | "Ledger Application level error (get_public_key): Conditions of use not satisfied" : _ <- errors -> Left ImportSecretKeyStep_Declined
    | "Ledger Transport level error:" : _ <- errors -> Left ImportSecretKeyStep_Disconnected
    | otherwise -> Left $ ImportSecretKeyStep_Failed $ T.unlines errors
  pure $ either id (const ImportSecretKeyStep_Done) e

runClientCommand
  :: (MonadLogger m, MonadIO m, Show e)
  => AppConfig -> Either NamedChain BinaryPaths -> [String] -> ([Text] -> [Text] -> Either e Text) -> ExceptT e m Text
runClientCommand appConfig chain args handleError = do
  $(logWarn) $ "runClientCommand: " <> T.pack (unwords args)
  (exitCode, stdout, stderr) <- liftIO $ Process.readProcessWithExitCode (clientPath chain) (["--port", show (_appConfig_kilnNodePort appConfig), "--base-dir", tezosClientDataDir appConfig] ++ args) ""
  case exitCode of
    ExitSuccess -> pure $ T.strip $ T.pack stdout
    ExitFailure _ -> do
      $(logWarn) $ "runClientCommand failed: " <> T.pack stderr
      let strippedLines = fmap T.strip $ T.lines $ T.pack stderr
          warnings = takeWhile (/= "Error:") $ drop 1 $ dropWhile (/= "Warning:") strippedLines
          errors = filter (/= "Error:") $ dropWhile (/= "Error:") strippedLines
          fatal = drop 1 $ dropWhile (/= "Fatal error:") $ fmap T.strip $ T.lines $ T.pack stdout -- yes, fatal errors go to stdout
      case handleError warnings (fatal ++ errors) of
        Right t -> pure t
        Left e -> do
          $(logWarn) $ T.pack $ show e
          throwError e

runClientT :: (MonadIO m, MonadLoggerIO m) => ExceptT ClientError (LoggingT IO) a -> m (Either ClientError a)
runClientT m = do
  le <- askLoggerIO
  liftIO $ timeout (45000000) (runLoggingEnv (LoggingEnv le) (runExceptT m)) >>= \case
    Nothing -> pure $ Left $ ClientError_Other "Timeout"
    Just a -> pure a

setupLedgerToBake :: (MonadIO m, MonadLogger m) => AppConfig -> Either NamedChain BinaryPaths -> m SetupLedgerToBakeStep
setupLedgerToBake appConfig chain = do
  e <- runExceptT $ runClientCommand appConfig chain ["setup", "ledger", "to", "bake", "for", T.unpack kilnLedgerAlias] $ \_warnings errors -> if
    | "Ledger Application level error (setup): Conditions of use not satisfied" : _ <- errors -> Left SetupLedgerToBakeStep_Declined
    | "Ledger Transport level error:" : _ <- errors -> Left SetupLedgerToBakeStep_Disconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left SetupLedgerToBakeStep_Disconnected
    | "This command (`setup ledger ...`) is not compatible with this version" : version'' : _ <- errors
    , Just version' <- T.stripPrefix "of the Ledger Baking app (Tezos Baking " version''
    , version <- T.takeWhile (/= ' ') version'
    -> Left $ SetupLedgerToBakeStep_OutdatedVersion version
    | otherwise -> Left SetupLedgerToBakeStep_Failed
  pure $ either id (const SetupLedgerToBakeStep_Done) e

-- If node isn't synced, this command will block while it waits for the node to
-- get up-to-date. We detect that case and just return an error.
-- Also, if we are already registered as a delegate, the tezos-client command
-- succeeds without re-registering.
registerKeyAsDelegate :: (MonadIO m, MonadLogger m) => AppConfig -> Either NamedChain BinaryPaths -> Tez -> m RegisterStep
registerKeyAsDelegate appConfig chain fee
  | fee > Tez 1 = pure $ RegisterStep_FeeTooHigh fee
  | otherwise = do
  let p = (Process.proc (clientPath chain) ["--port", show (_appConfig_kilnNodePort appConfig), "--base-dir", tezosClientDataDir appConfig, "register", "key", T.unpack kilnLedgerAlias, "as", "delegate", "--fee", show (getTez fee)])
        { Process.std_err = Process.CreatePipe
        , Process.std_out = Process.CreatePipe
        }
  result <- liftIO $ Process.withCreateProcess p $ \_mstdin mstdout mstderr ph -> case liftA2 (,) mstdout mstderr of
    Nothing -> pure RegisterStep_Failed
    Just (stdout, stderr) -> do
      line <- catchJust (guard . isEOFError) (T.hGetLine stdout) $ \() -> pure ""
      case T.strip line of
        "Waiting for the node to be bootstrapped before injection..." -> pure RegisterStep_NodeNotReady
        _ -> Process.waitForProcess ph >>= \case
          ExitFailure _ -> do
            err <- T.hGetContents stderr
            pure $ if
              | T.isInfixOf "Ledger Application level error (sign): Unregistered status message" err
              -> RegisterStep_FeeTooHigh fee
              | T.isInfixOf "Ledger Application level error (sign): Conditions of use not satisfied" err
              -> RegisterStep_Declined
              | T.isInfixOf "Ledger Transport level error:" err
              -> RegisterStep_Disconnected
              | fatal : _ <- drop 1 $ dropWhile (/= "Fatal error:") $ T.lines err
              , Just fee' <- T.stripPrefix "The proposed fee " (T.strip fatal)
              , T.isPrefixOf " are lower than the fee that baker expect by default " (T.dropWhile (/= ' ') fee')
              -> RegisterStep_FeeTooLow fee
              | T.isInfixOf "Empty implicit contract " err
              -> RegisterStep_NotEnoughFunds 0
              -- Balance of contract tz1VeX1Wso2LRGW2rpgKHoyFkHUxJpvSxLWP too low (860) to spend 1000
              | Just pkhBalance <- T.stripPrefix "Balance of contract " err
              , Just bal <- readMaybe (T.unpack $ T.takeWhile (/= ')') $ T.drop 1 $ T.dropWhile (/= '(') pkhBalance)
              -> RegisterStep_NotEnoughFunds $ Tez bal
              | otherwise -> RegisterStep_Failed -- err
          ExitSuccess -> pure RegisterStep_Registered -- Succeeds if already registered too
  $(logWarn) $ T.pack $ show result
  pure result

setHighWaterMark :: MonadLoggerIO m => AppConfig -> Either NamedChain BinaryPaths -> SecretKey -> RawLevel -> m SetHWMStep
setHighWaterMark appConfig chain sk bl = do
  e <- runExceptT $ runClientCommand appConfig chain ["set", "ledger", "high", "watermark", "for", T.unpack (toSecretKeyText sk), "to", show (unRawLevel bl)] $ \_warnings errors -> if
    | "Ledger Application level error (set_high_watermark): Conditions of use not satisfied" : _ <- errors -> Left SetHWMStep_Declined
    | "Ledger Transport level error:" : _ <- errors -> Left SetHWMStep_Disconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left SetHWMStep_Disconnected
    | otherwise -> Left $ SetHWMStep_Failed $ T.unlines errors
  pure $ either id (const SetHWMStep_Done) e

-- This is moved from RequestHandler to here. But perhaps this should belong to a common module
addBakerImpl :: (Monad m, PersistBackend m) => PublicKeyHash -> Maybe Text -> m ()
addBakerImpl pkh alias = do
  existingIds :: [Id Baker] <- fmap toId <$> project BakerKey (Baker_publicKeyHashField ==. pkh)
  let newVal = BakerData
        { _bakerData_alias = alias
        }
  case nonEmpty existingIds of
    Nothing -> void $ insert $ Baker
      { _baker_publicKeyHash = pkh
      , _baker_data = DeletableRow
        { _deletableRow_data = newVal
        , _deletableRow_deleted = False
        }
      }
    Just bIds -> for_ bIds $ \bId ->
      update [ Baker_dataField ~> DeletableRow_deletedSelector =. False
             , Baker_dataField ~> DeletableRow_dataSelector ~> BakerData_aliasSelector =. alias
             ]
             (BakerKey ==. fromId bId)
  notify NotifyTag_Baker (Id pkh, Just newVal)
