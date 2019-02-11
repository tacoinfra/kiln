{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoDoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Backend.ClientCmd where

-- import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw)
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class ()
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, logWarn, logError, LoggingT, askLoggerIO)
import Data.Maybe (mapMaybe)
import System.Exit (ExitCode(..))
import System.Process (readProcess, readProcessWithExitCode)
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
import Tezos.Ledger
import Tezos.PublicKeyHash
import Tezos.Tez

-- TODO fix this
clientPath :: FilePath
clientPath = $(staticWhich "zeronet-tezos-client")

getConnectedLedger :: (MonadIO m, MonadLogger m) => m (Maybe LedgerIdentifier)
getConnectedLedger = do
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode clientPath ["list", "connected", "ledgers", "--for-script", "TSV"] ""
  case exitCode of
    ExitFailure _ -> do
      $(logError) $ T.pack $ "getConnectedLedger: " <> stderr
      pure Nothing
    ExitSuccess -> do
      case T.lines $ T.pack stdout of
        r : _ -> do
          let r' = getKungFuName r
          when (isNothing r') $ $(logWarn) $ "getConnectedLedger: failed to find kung fu name of ledger from: " <> r
          pure r'
        [] -> pure Nothing
      where
        getKungFuName t = case T.splitOn "\t" t of
          [_baker, _version, _usb, kungFu] -> Just $ LedgerIdentifier kungFu
          _ -> Nothing

getBalanceFor :: (MonadIO m, MonadLogger m) => PublicKeyHash -> ExceptT ClientError m (Maybe Tez)
getBalanceFor pkh = do
  stdout <- runClientCommand ["get", "balance", "for", T.unpack $ toPublicKeyHashText pkh] $ \warnings errors -> if
    | "Failed to acquire the protocol version from the node" : _ <- warnings
    , "Unrecognized command." : _ <- errors -> Left ClientError_NodeNotReady
    | otherwise -> Left $ ClientError_Other $ T.unlines errors
  pure $ case T.stripSuffix " ꜩ" stdout of
    Just x | Just micro <- Aeson.decodeStrict (TE.encodeUtf8 x) -> Just $ Tez micro
    _ -> Nothing

showLedger :: (MonadIO m, MonadLogger m) => SecretKey -> ExceptT ClientError m (Maybe PublicKeyHash)
showLedger sk = do
  stdout <- runClientCommand ["show", "ledger", T.unpack $ toSecretKeyText sk, "--for-script", "TSV"] $ \_warnings errors -> if
    | e : _ <- errors, Just _sk' <- T.stripPrefix "No ledger found for " e -> Left ClientError_LedgerDisconnected
    | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
    | "(Invalid_argument int32_of_path_element_exn)" : _ <- errors -> Right ""
    | otherwise -> Left $ ClientError_Other $ T.unlines errors
  pure $ case T.splitOn "\t" stdout of
    -- TODO should "Ledger Nano S" have a tab char in the middle?
    [_baker, _ledger, _nanoS, _usb, pkht, _publicKey] | Right pkh <- tryReadPublicKeyHashText pkht -> pure pkh
    _ -> Nothing

importSecretKey :: (MonadIO m, MonadLogger m) => Text -> SecretKey -> ExceptT ClientError m ()
importSecretKey alias sk = do
  void $ runClientCommand ["import", "secret", "key", T.unpack alias, T.unpack $ toSecretKeyText sk, "--force"] $ \warnings errors -> if
    | "Ledger Application level error (get_public_key): Conditions of use not satisfied" : _ <- errors -> Left ClientError_RequestDeclinedByLedger
    | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
    -- This check is never used because of --force, but may be useful to keep around for reference
    | e1 : e2 : _ <- errors
    , e1 == "The secret_key alias " <> alias <> " already exists."
    , Just skdot <- T.stripPrefix "The current value is " e2
    , Just sk' <- T.stripSuffix "." skdot
      -> if toSecretKeyText sk == sk' then Right "" else Left ClientError_AliasAlreadyUsed
    | otherwise -> Left $ ClientError_Other $ T.unlines errors

runClientCommand :: (MonadLogger m, MonadIO m) => [String] -> ([Text] -> [Text] -> Either ClientError Text) -> ExceptT ClientError m Text
runClientCommand args handleError = do
  $(logWarn) $ "runClientCommand: " <> T.pack (unwords args)
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode clientPath args ""
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

authorizeLedgerToBake :: (MonadIO m, MonadLogger m) => Text -> ExceptT ClientError m ()
authorizeLedgerToBake alias = do
  void $ runClientCommand ["authorize", "ledger", "to", "bake", "for", T.unpack alias] $ \warnings errors -> if
    | "Ledger Application level error (get_public_key): Conditions of use not satisfied" : _ <- errors -> Left ClientError_RequestDeclinedByLedger
    | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left ClientError_LedgerDisconnected
    | otherwise -> Left $ ClientError_Other $ T.unlines errors

-- If node isn't synced, this command will block while it waits for the node to
-- get up-to-date. We detect that case and just return an error.
-- Also, if we are already registered as a delegate, the tezos-client command
-- succeeds without re-registering.
registerKeyAsDelegate :: (MonadIO m, MonadLogger m) => Text -> m (Either ClientError ())
registerKeyAsDelegate alias = do
  $(logWarn) "registerKeyAsDelegate requested"
  let p = (Process.proc clientPath ["register", "key", T.unpack alias, "as", "delegate"])
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

