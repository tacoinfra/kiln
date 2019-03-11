{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoDoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.ClientCmd where

-- import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw)
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class ()
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, logWarn, LoggingT, askLoggerIO)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified System.Process as Process

import Common.Schema
import ExtraPrelude
import Rhyolite.Backend.Logging (runLoggingEnv, LoggingEnv(..))
import System.Which
import Tezos.Chain (NamedChain(..))
import Tezos.Ledger
import Tezos.PublicKeyHash
import Tezos.Types (RawLevel(..), Tez(..))

-- TODO XXX OBVIOUSLY BAD
clientPath :: Maybe NamedChain -> FilePath
clientPath Nothing = $(staticWhich "mainnet-tezos-client") -- TODO what should we actually do here?
clientPath (Just NamedChain_Mainnet) = $(staticWhich "mainnet-tezos-client")
clientPath (Just NamedChain_Alphanet) = $(staticWhich "alphanet-tezos-client")
clientPath (Just NamedChain_Zeronet) = $(staticWhich "zeronet-tezos-client")

{- Example output from `list connected ledgers`
Found a Tezos Baking 1.5.0 (commit v1.4.3-19-g55cc026d) application running on Ledger Nano S at [0003:0007:00].

To use keys at BIP32 path m/44'/1729'/0'/0' (default Tezos key path), use one of
 tezos-client import secret key ledger_tom "ledger://odd-himalayan-lustrous-falcon/ed25519/0'/0'"
 tezos-client import secret key ledger_tom "ledger://odd-himalayan-lustrous-falcon/secp256k1/0'/0'"
 tezos-client import secret key ledger_tom "ledger://odd-himalayan-lustrous-falcon/P-256/0'/0'"
-}

getConnectedLedger :: (MonadIO m, MonadLogger m) => Maybe NamedChain -> ExceptT ClientError m (Maybe LedgerIdentifier)
getConnectedLedger chain = do
  stdout <- runClientCommand chain ["list", "connected", "ledgers"] $ \_{-warnings-} errors -> if
    | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
    | otherwise -> Left $ ClientError_Other $ T.unlines errors
  let kfn = getKungFuName (T.lines stdout)
  when (isNothing kfn) $ $(logWarn) $ "getConnectedLedger: failed to find kung fu name of ledger from: " <> stdout
  pure kfn
  where
    getKungFuName = \case
      foundApp : _blank : useKeys : keyExample : _
        | Just version' <- T.stripPrefix "Found a Tezos Baking " foundApp
        , _{-version-} <- T.takeWhile (/= ' ') version'
        , "To use keys at BIP32 path" `T.isPrefixOf` useKeys -- sanity check
        , Just ledger' <- T.stripPrefix "\"ledger://" (T.dropWhile (/= '"') keyExample)
        , ledger <- T.takeWhile (/= '/') ledger'
        , [_1, _2, _3, _4] <- T.splitOn "-" ledger -- sanity check formatting of ledger
        -> Just $ LedgerIdentifier ledger
      _ -> Nothing

getBalanceFor :: (MonadIO m, MonadLogger m) => Maybe NamedChain -> PublicKeyHash -> ExceptT ClientError m (Maybe Tez)
getBalanceFor chain pkh = do
  stdout <- runClientCommand chain ["get", "balance", "for", T.unpack $ toPublicKeyHashText pkh] $ \warnings errors -> if
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

showLedger :: (MonadIO m, MonadLogger m) => Maybe NamedChain -> SecretKey -> ExceptT ClientError m (Maybe PublicKeyHash)
showLedger chain sk = do
  stdout <- runClientCommand chain ["show", "ledger", T.unpack $ toSecretKeyText sk] $ \_{-warnings-} errors -> if
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

importSecretKey :: (MonadIO m, MonadLogger m) => Maybe NamedChain -> SecretKey -> ExceptT ClientError m ()
importSecretKey chain sk = do
  void $ runClientCommand chain ["import", "secret", "key", T.unpack kilnLedgerAlias, T.unpack $ toSecretKeyText sk, "--force"] $ \_{-warnings-} errors -> if
    | "Ledger Application level error (get_public_key): Conditions of use not satisfied" : _ <- errors -> Left ClientError_RequestDeclinedByLedger
    | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
    -- This check is never used because of --force, but may be useful to keep around for reference
    | e1 : e2 : _ <- errors
    , e1 == "The secret_key alias " <> kilnLedgerAlias <> " already exists."
    , Just skdot <- T.stripPrefix "The current value is " e2
    , Just sk' <- T.stripSuffix "." skdot
      -> if toSecretKeyText sk == sk' then Right "" else Left ClientError_AliasAlreadyUsed
    | otherwise -> Left $ ClientError_Other $ T.unlines errors

runClientCommand :: (MonadLogger m, MonadIO m) => Maybe NamedChain -> [String] -> ([Text] -> [Text] -> Either ClientError Text) -> ExceptT ClientError m Text
runClientCommand chain args handleError = do
  $(logWarn) $ "runClientCommand: " <> T.pack (unwords args)
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode (clientPath chain) args ""
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

setupLedgerToBake :: (MonadIO m, MonadLogger m) => Maybe NamedChain -> ExceptT ClientError m ()
setupLedgerToBake chain = do
  void $ runClientCommand chain ["setup", "ledger", "to", "bake", "for", T.unpack kilnLedgerAlias] $ \_{-warnings-} errors -> if
    | "Ledger Application level error (get_public_key): Conditions of use not satisfied" : _ <- errors -> Left ClientError_RequestDeclinedByLedger
    | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left ClientError_LedgerDisconnected
    | "This command (`setup ledger ...`) is not compatible with this version" : version'' : _ <- errors
    , Just version' <- T.stripPrefix "of the Ledger Baking app (Tezos Baking " version''
    , version <- T.takeWhile (/= ' ') version'
    -> Left $ ClientError_OutdatedLedgerBakingVersion version
    | otherwise -> Left $ ClientError_Other $ T.unlines errors

-- If node isn't synced, this command will block while it waits for the node to
-- get up-to-date. We detect that case and just return an error.
-- Also, if we are already registered as a delegate, the tezos-client command
-- succeeds without re-registering.
registerKeyAsDelegate :: (MonadIO m, MonadLogger m) => Maybe NamedChain -> m (Either ClientError ())
registerKeyAsDelegate chain = do
  $(logWarn) "registerKeyAsDelegate requested"
  let p = (Process.proc (clientPath chain) ["register", "key", T.unpack kilnLedgerAlias, "as", "delegate"])
        { Process.std_err = Process.CreatePipe
        , Process.std_out = Process.CreatePipe
        }
  result <- liftIO $ Process.withCreateProcess p $ \_mstdin mstdout mstderr ph -> case liftA2 (,) mstdout mstderr of
    Nothing -> pure $ Left ClientError_ProcessError
    Just (stdout, stderr) -> do
      T.hGetLine stdout >>= \case -- TODO handle isEOFError
        "Waiting for the node to be bootstrapped before injection..." -> pure $ Left ClientError_NodeNotReady
        _ -> Process.waitForProcess ph >>= \case
          ExitFailure _ -> do
            err <- T.hGetContents stderr
            pure $ Left $ ClientError_Other err
          ExitSuccess -> pure $ Right () -- Succeeds if already registered too
  case result of
    Right () -> pure ()
    Left err -> $(logWarn) $ T.pack $ show err
  pure result

setHighWaterMark :: MonadLoggerIO m => Maybe NamedChain -> SecretKey -> RawLevel -> ExceptT ClientError m ()
setHighWaterMark chain sk bl = do
  void $ runClientCommand chain ["set", "ledger", "high", "watermark", "for", T.unpack (toSecretKeyText sk), "to", show (unRawLevel bl)] $ \_{-warnings-} errors -> if
    | "Ledger Application level error (set_high_watermark): Conditions of use not satisfied" : _ <- errors -> Left ClientError_RequestDeclinedByLedger
    | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left ClientError_LedgerDisconnected
    | otherwise -> Left $ ClientError_Other $ T.unlines errors
