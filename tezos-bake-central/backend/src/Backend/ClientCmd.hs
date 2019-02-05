{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoDoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Backend.ClientCmd where

-- import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw)
import Control.Monad (when)
import Control.Monad.IO.Class ()
import Control.Monad.Logger (MonadLogger, logWarn, logError)
import Data.Maybe (mapMaybe)
import System.Exit (ExitCode(..))
import System.Process (readProcess, readProcessWithExitCode)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import ExtraPrelude
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

getBalanceFor :: MonadIO m => PublicKeyHash -> m (Maybe Tez)
getBalanceFor pkh = do
  stdout <- liftIO $ readProcess clientPath ["get", "balance", "for", T.unpack $ toPublicKeyHashText pkh] ""
  let parseTez t = case T.stripSuffix " ꜩ" t of
        Just x | Just micro <- Aeson.decodeStrict (TE.encodeUtf8 x) -> Just $ Tez micro
        _ -> Nothing
  pure $ listToMaybe $ mapMaybe parseTez $ T.lines (T.pack stdout)

showLedger :: MonadIO m => SecretKey -> m (Maybe PublicKeyHash)
showLedger sk = do
  stdout <- liftIO $ readProcess clientPath ["show", "ledger", T.unpack $ toSecretKeyText sk, "--for-script", "TSV"] ""
  liftIO $ putStrLn stdout
  let getAddress x = case T.splitOn "\t" x of
        -- TODO should "Ledger Nano S" have a tab char in the middle?
        [_baker, _ledger, _nanoS, _usb, pkht, _publicKey] | Right pkh <- tryReadPublicKeyHashText pkht -> Just pkh
        _ -> Nothing
  pure $ listToMaybe $ mapMaybe getAddress $ T.lines (T.pack stdout)

