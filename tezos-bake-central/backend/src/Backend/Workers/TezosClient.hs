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
import Control.Exception (catchJust)
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Maybe (mapMaybe)
import Data.List.NonEmpty (nonEmpty)
import Data.Pool (Pool)
import Data.Time (NominalDiffTime, diffUTCTime)
import Database.Groundhog
import Database.Groundhog.Postgresql (Postgresql, SqlDb, in_)
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.PsqlSimple (executeQ, queryQ)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Schema (Id (..))
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.IO (hIsEOF)
import System.IO.Error (isEOFError)
import System.Which
import Text.Read (readMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified System.Process as Process

import Tezos.Operation (Ballot(..))
import Tezos.Types

import Backend.Alerts
import Backend.CachedNodeRPC
import Backend.Common
import Backend.Config (AppConfig (..), tezosClientDataDir, BinaryPaths(..))
import Backend.Schema
import Common.App (ImportSecretKeyStep(..), SetupLedgerToBakeStep(..), RegisterStep(..), SetupState(..), SetHWMStep(..), VoteState(..), VoteStep(..))
import Common.Schema
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
  update [ProcessData_controlField =. ProcessControl_Run] $ AutoKeyField `in_` processes

tezosClientWorker
  :: NominalDiffTime
  -> LoggingEnv
  -> NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> Either NamedChain BinaryPaths
  -> IO (IO ())
tezosClientWorker delay logger nds appConfig db chain = runLoggingEnv logger $ do
  workerWithDelay (pure delay) $ const $ runLoggingEnv logger $ do
    liftIO $ createDirectoryIfMissing True (tezosClientDataDir appConfig)

    inDb (selectSingle $ ConnectedLedger_forceConnectivityCheckField ==. True) >>= \mcl -> for_ mcl $ \(_cl :: ConnectedLedger) -> do
      updateConnectedLedgerViaGetConnectedLedger appConfig db chain

    mConnectedLedger :: Maybe ConnectedLedger <- inDb $ selectSingle CondEmpty
    currentTime <- inDb getTime
    case mConnectedLedger of
      Just cl -> do
        -- If we think the ledger is connected
        when (isJust (_connectedLedger_ledgerIdentifier cl) && isJust (_connectedLedger_updated cl)) $ do
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
            setupLedgerToBake appConfig chain >>= \i -> do
              isReg <- if i == SetupLedgerToBakeStep_Done
                then (fromMaybe False <$>) $ traverse (checkIfRegistered logger db nds) $ _ledgerAccount_publicKeyHash la
                else pure False
              inDb $ do
                when isReg $ void $ traverse startBaking $ _ledgerAccount_publicKeyHash la
                update
                  [LedgerAccount_shouldSetupToBakeField =. False]
                  (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
                notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_setup = Just $ First $ bool i SetupLedgerToBakeStep_DoneAndRegistered isReg })

          -- register
          inDb (selectSingle $
                LedgerAccount_publicKeyHashField /=. (Nothing :: Maybe PublicKeyHash)
            &&. LedgerAccount_shouldRegisterFeeField /=. (Nothing :: Maybe Tez)
            &&. LedgerAccount_importedField ==. True) >>= \mla -> for_ mla $ \la -> case liftA2 (,) (_ledgerAccount_shouldRegisterFee la) (_ledgerAccount_publicKeyHash la) of
            Nothing -> pure () -- shouldn't happen due to WHERE clause
            Just (fee, pkh) -> do
              let sk = _ledgerAccount_secretKey la
              inDb $ notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_register = Just $ First RegisterStep_Prompting })
              registerKeyAsDelegate logger db nds sk pkh appConfig chain fee >>= \result -> inDb $ do
                update
                  [LedgerAccount_shouldRegisterFeeField =. (Nothing :: Maybe Tez)]
                  (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
                notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_register = Just $ First result })
                when (result == RegisterStep_Registered) $ startBaking pkh

          -- run appropriate 'show ledger' commands, but only do one at a time
          -- to allow other commands to take precedence
          inDb (project1 LedgerAccount_secretKeyField (isFieldNothing LedgerAccount_publicKeyHashField)) >>= \msk -> for_ msk $ \sk -> do
            showLedger appConfig chain sk >>= \case
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
                  Just pkh -> getBalanceFor appConfig chain pkh >>= \case
                    -- In case of error, give another try in the code further down
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
          for_ las' $ \(sk, pkh) -> getBalanceFor appConfig chain pkh >>= \case
            Left (ClientError_Timeout)-> do
              $(logError) ("Client Timout: getBalanceFor: " <> toPublicKeyHashText pkh)
              inDb $ do
                delete $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
                notify NotifyTag_ShowLedger (sk, Nothing)
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

          -- do any voting
          let selectProposal = [queryQ|
                SELECT la."publicKeyHash", la."secretKey#ledgerIdentifier", la."secretKey#signingCurve", la."secretKey#derivationPath", la."shouldDoVoteBallot", pp.id, pp.hash
                FROM "PeriodProposal" pp
                JOIN "LedgerAccount" la ON la."shouldDoVoteProtocol" = pp.id
              |]
          inDb selectProposal >>= \case
            [(pkh :: PublicKeyHash, ledgerIdentifier, signingCurve, derivationPath, shouldDoVoteBallot, proposalId :: Id PeriodProposal, proposalHash)] -> do
              dsh <- liftIO $ atomically $ dataSourceHead nds
              let attempted = view hash <$> dsh
                  sk = SecretKey ledgerIdentifier signingCurve derivationPath
              inDb $ notify NotifyTag_VotePrompting (sk, Just $ mempty { _voteState_step = Just $ First VoteStep_Prompting })
              vs <- case shouldDoVoteBallot of
                Nothing -> do
                  vs <- submitProposals appConfig chain [proposalHash]
                  when (vs == VoteStep_Done) $ inDb $ do
                    _ <- [executeQ|
                      INSERT INTO "BakerProposal" (pkh, proposal, included, attempted)
                      VALUES (?pkh, ?proposalId, null, ?attempted)
                      ON CONFLICT DO NOTHING
                    |]
                    mpp <- selectSingle (AutoKeyField ==. fromId proposalId)
                    for_ mpp $ \pp -> notify NotifyTag_Proposals (proposalId, Just (pp, Just False))
                  pure vs
                Just ballot -> do
                  vs <- submitBallot appConfig chain proposalHash ballot
                  when (vs == VoteStep_Done) $ inDb $ do
                    let bv = BakerVote
                          { _bakerVote_pkh = pkh
                          , _bakerVote_proposal = proposalId
                          , _bakerVote_ballot = ballot
                          , _bakerVote_included = Nothing
                          , _bakerVote_attempted = attempted
                          }
                    insert_ bv
                    notify NotifyTag_BakerVote $ Just bv
                  pure vs
              inDb $ do
                update
                  [ LedgerAccount_shouldDoVoteProtocolField =. (Nothing :: Maybe (Id PeriodProposal))
                  , LedgerAccount_shouldDoVoteBallotField =. (Nothing :: (Maybe Ballot))
                  ] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
                notify NotifyTag_VotePrompting (sk, Just $ mempty { _voteState_step = Just $ First vs })
            _ -> pure () -- shouldn't happen

        -- If we want to immediately do the connectivity check
        when (_connectedLedger_forceConnectivityCheck cl) $ do
          updateConnectedLedgerViaGetConnectedLedger appConfig db chain

        -- Otherwise, we might want to do the connectivity check because some time has passed
        case _connectedLedger_updated cl of
          Nothing -> updateConnectedLedgerViaGetConnectedLedger appConfig db chain
          Just upd ->
            if (currentTime `diffUTCTime` upd > 5)
            then updateConnectedLedgerViaGetConnectedLedger appConfig db chain
            else pure ()

      _ -> pure ()
    where
      inDb :: ReaderT AppConfig (DbPersist Postgresql (LoggingT IO)) a -> LoggingT IO a
      inDb = runDb (Identity db) . flip runReaderT appConfig

withDbAndConfig :: Pool Postgresql -> AppConfig -> ReaderT AppConfig (DbPersist Postgresql (LoggingT IO)) a -> LoggingT IO a
withDbAndConfig db appConfig = runDb (Identity db) . flip runReaderT appConfig

updateConnectedLedgerViaGetConnectedLedger :: AppConfig -> Pool Postgresql -> Either NamedChain BinaryPaths -> LoggingT IO ()
updateConnectedLedgerViaGetConnectedLedger appConfig db chain = do
  getConnectedLedger appConfig chain >>= \case
    Left err -> do
      $(logError) (tshow err)
      reportLedgerDisconnection db appConfig
    Right mliv -> do
      case mliv of
        Nothing -> do
          reportLedgerDisconnection db appConfig
          $(logDebug) "The connectedledger is Nothing"
        Just _ -> do
          clearLedgerDisconnection db appConfig

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

reportLedgerDisconnection :: Pool Postgresql -> AppConfig -> LoggingT IO ()
reportLedgerDisconnection db appConfig = withDbAndConfig db appConfig $ do
  bdis :: [BakerDaemonInternal] <- select (BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector ==. False)
  for_ bdis $ \bdi -> do
    for_ (_bakerDaemonInternalData_publicKeyHash $ _deletableRow_data $ _bakerDaemonInternal_data $ bdi) $ \pkh ->
      reportBakerLedgerDisconnected pkh

clearLedgerDisconnection :: Pool Postgresql -> AppConfig -> LoggingT IO ()
clearLedgerDisconnection db appConfig = withDbAndConfig db appConfig $ do
  bdis :: [BakerDaemonInternal] <- select (BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector ==. False)
  for_ bdis $ \bdi -> do
    for_ (_bakerDaemonInternalData_publicKeyHash $ _deletableRow_data $ _bakerDaemonInternal_data $ bdi) $ \pkh ->
      clearBakerLedgerDisconnected pkh

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

data LedgerApp
  = LedgerApp_Baking
  | LedgerApp_Wallet
  deriving (Show, Eq)

defaultTimeout :: Maybe (NominalDiffTime, ClientError)
defaultTimeout = Just (5, ClientError_Timeout)

noTimeout :: Maybe (NominalDiffTime, e)
noTimeout = Nothing

getConnectedLedger :: (MonadIO m, MonadLoggerIO m) => AppConfig -> Either NamedChain BinaryPaths -> m (Either ClientError (Maybe (LedgerIdentifier, LedgerApp, Text)))
getConnectedLedger appConfig chain = runExceptT $ do
  stdout <- runClientCommand appConfig chain defaultTimeout ["list", "connected", "ledgers"] $ \_warnings errors -> if
    | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
    | otherwise -> Left $ ClientError_Other $ T.unlines errors
  case chain of
    Left NamedChain_Zeronet -> getKungFuNameZeronet (T.lines stdout)
    _ -> getKungFuName (T.lines stdout)
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
        pure $ Nothing

getBalanceFor :: (MonadIO m, MonadLoggerIO m) => AppConfig -> Either NamedChain BinaryPaths -> PublicKeyHash -> m (Either ClientError (Maybe Tez))
getBalanceFor appConfig chain pkh = runExceptT $ do
  stdout <- runClientCommand appConfig chain defaultTimeout ["get", "balance", "for", T.unpack $ toPublicKeyHashText pkh] $ \warnings errors -> if
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

showLedger :: (MonadIO m, MonadLoggerIO m) => AppConfig -> Either NamedChain BinaryPaths -> SecretKey -> m (Either ClientError (Maybe PublicKeyHash))
showLedger appConfig chain sk = runExceptT $ do
  stdout <- runClientCommand appConfig chain defaultTimeout ["show", "ledger", T.unpack $ toSecretKeyText sk] $ \_warnings errors -> if
    | e : _ <- errors, Just _sk' <- T.stripPrefix "No ledger found for " e -> Left ClientError_LedgerDisconnected
    | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
    | "(Invalid_argument int32_of_path_element_exn)" : _ <- errors -> Right ""
    | otherwise -> Left $ ClientError_Other $ T.unlines errors
  let pkh = case chain of
        Left NamedChain_Zeronet -> getPublicKeyHashZeronet (T.lines stdout)
        _ -> getPublicKeyHash (T.lines stdout)
  when (isNothing pkh) $ $(logWarn) $ "showLedger: failed to find public key hash from: " <> stdout
  pure pkh
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

importSecretKey :: (MonadIO m, MonadLoggerIO m) => AppConfig -> Either NamedChain BinaryPaths -> SecretKey -> m ImportSecretKeyStep
importSecretKey appConfig chain sk = do
  e <- runExceptT $ runClientCommand appConfig chain noTimeout ["import", "secret", "key", T.unpack kilnLedgerAlias, T.unpack $ toSecretKeyText sk, "--force"] $ \_warnings errors -> if
    | "Ledger Application level error (get_public_key): Conditions of use not satisfied" : _ <- errors -> Left ImportSecretKeyStep_Declined
    | "Ledger Transport level error:" : _ <- errors -> Left ImportSecretKeyStep_Disconnected
    | otherwise -> Left $ ImportSecretKeyStep_Failed $ T.unlines errors
  pure $ either id (const ImportSecretKeyStep_Done) e

runClientCommand
  :: (MonadLoggerIO m, Show e)
  => AppConfig
  -> Either NamedChain BinaryPaths
  -> Maybe (NominalDiffTime, e)
  -> [String]
  -> ([Text] -> [Text] -> Either e Text)
  -> ExceptT e m Text
runClientCommand appConfig chain mTimeout args handleError = do
  le <- askLoggerIO
  let
    procSpec = Process.proc (clientPath chain) (["--port", show (_appConfig_kilnNodeRpcPort appConfig), "--base-dir", tezosClientDataDir appConfig] ++ args)
    runProc = runLoggingEnv (LoggingEnv le) $ readCreateProcessWithExitCodeWithLogging procSpec ""
    withTimeout run handle = flip (maybe ((liftIO run) >>= handle)) mTimeout $ \(t, err) -> (liftIO $ timeout' t run) >>= \case
      Just v -> handle v
      Nothing -> do
        $(logWarn) $ "runClientCommand Timedout"
        throwError err
  withTimeout runProc $ \(exitCode, stdout, stderr) -> case exitCode of
    ExitSuccess -> pure $ T.strip stdout
    ExitFailure _ -> do
      $(logWarn) $ "runClientCommand failed: " <> stderr
      let strippedLines = fmap T.strip $ T.lines stderr
          warnings = takeWhile (/= "Error:") $ drop 1 $ dropWhile (/= "Warning:") strippedLines
          errors = filter (/= "Error:") $ dropWhile (/= "Error:") strippedLines
          fatal = drop 1 $ dropWhile (/= "Fatal error:") $ fmap T.strip $ T.lines stdout -- yes, fatal errors go to stdout
      case handleError warnings (fatal ++ errors) of
        Right t -> pure t
        Left e -> do
          $(logWarn) $ T.pack $ show e
          throwError e

setupLedgerToBake :: (MonadIO m, MonadLoggerIO m) => AppConfig -> Either NamedChain BinaryPaths -> m SetupLedgerToBakeStep
setupLedgerToBake appConfig chain = do
  e <- runExceptT $ runClientCommand appConfig chain noTimeout ["setup", "ledger", "to", "bake", "for", T.unpack kilnLedgerAlias] $ \_warnings errors -> if
    | "Ledger Application level error (setup): Conditions of use not satisfied" : _ <- errors -> Left SetupLedgerToBakeStep_Declined
    | "Ledger Transport level error:" : _ <- errors -> Left SetupLedgerToBakeStep_Disconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left SetupLedgerToBakeStep_Disconnected
    | "This command (`setup ledger ...`) is not compatible with this version" : version'' : _ <- errors
    , Just version' <- T.stripPrefix "of the Ledger Baking app (Tezos Baking " version''
    , version <- T.takeWhile (/= ' ') version'
    -> Left $ SetupLedgerToBakeStep_OutdatedVersion version
    | otherwise -> Left SetupLedgerToBakeStep_Failed
  pure $ either id (const SetupLedgerToBakeStep_Done) e

checkIfRegistered :: MonadIO m => LoggingEnv -> Pool Postgresql -> NodeDataSource -> PublicKeyHash -> m Bool
checkIfRegistered logger db nds pkh = do
  mDelegateInfo <- runMaybeT $ do
    headBlock <- MaybeT $ liftIO $ atomically $ dataSourceHead nds
    MaybeT $ runMaybe $ nodeQueryDataSource $ NodeQuery_DelegateInfo (headBlock ^. hash) (headBlock ^. level) pkh
  let isReg = case mDelegateInfo of
        Just delegateInfo | not (_cacheDelegateInfo_deactivated delegateInfo) -> True
        _ -> False
  liftIO $ runLoggingEnv logger $ runDb (Identity db) $
    notify NotifyTag_BakerRegistered (pkh, isReg)
  pure isReg
  where
    runMaybe :: Functor m => ExceptT CacheError (ReaderT NodeDataSource m) a -> m (Maybe a)
    runMaybe = fmap (either (const Nothing) Just) . flip runReaderT nds . runExceptT

-- If node isn't synced, this command will block while it waits for the node to
-- get up-to-date. We detect that case and just return an error.
registerKeyAsDelegate
  :: (MonadIO m, MonadLogger m)
  => LoggingEnv -> Pool Postgresql -> NodeDataSource -> SecretKey -> PublicKeyHash -> AppConfig -> Either NamedChain BinaryPaths -> Tez -> m RegisterStep
registerKeyAsDelegate logger db nds sk pkh appConfig chain fee
  | fee > Tez 1 = pure $ RegisterStep_FeeTooHigh fee
  | otherwise = checkIfRegistered logger db nds pkh >>= \case
    True -> pure RegisterStep_AlreadyRegistered
    False -> do
      -- withCreateProcess will close these automatically
      (readPipe, writePipe) <- liftIO Process.createPipe
      let p = (Process.proc (clientPath chain) ["--port", show (_appConfig_kilnNodeRpcPort appConfig), "--base-dir", tezosClientDataDir appConfig, "register", "key", T.unpack kilnLedgerAlias, "as", "delegate", "--fee", show (getTez fee)])
            { Process.std_err = Process.UseHandle writePipe
            , Process.std_out = Process.UseHandle writePipe
            }
      $(logInfoSH) $ ("registerKeyAsDelegate: process: " :: Text, p)
      result <- liftIO $ Process.withCreateProcess p $ \_ _ _ ph -> runLoggingEnv logger $ do
        let notifyStep rs = runDb (Identity db) $ notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_register = Just $ First rs })
            go mrs' = liftIO (hIsEOF readPipe) >>= \case
              True -> liftIO (Process.waitForProcess ph) >>= \case
                ExitFailure _ -> pure $ fromMaybe RegisterStep_Failed mrs'
                ExitSuccess -> pure RegisterStep_Registered -- Succeeds if already registered too
              False -> do
                t <- liftIO $ catchJust (guard . isEOFError) (T.hGetLine readPipe) (\() -> pure "")
                let mrs = parseRegisterStep fee t
                $(logInfo) $ "registerKeyAsDelegate: " <> t <> " -> " <> T.pack (show mrs)
                traverse_ notifyStep mrs
                case mrs of
                  Just RegisterStep_NodeNotReady -> pure RegisterStep_NodeNotReady
                  _ -> go $ mrs' <|> mrs
        go Nothing
      $(logWarn) $ T.pack $ show result
      pure result

parseRegisterStep :: Tez -> Text -> Maybe RegisterStep
parseRegisterStep fee (T.strip -> err)
  | T.isInfixOf "Ledger Application level error (sign): Unregistered status message" err
  = Just $ RegisterStep_FeeTooHigh fee
  | T.isInfixOf "Ledger Application level error (sign): Conditions of use not satisfied" err
  = Just RegisterStep_Declined
  | T.isInfixOf "Ledger Transport level error:" err
  = Just RegisterStep_Disconnected
  | fatal : _ <- drop 1 $ dropWhile (/= "Fatal error:") $ T.lines err
  , Just fee' <- T.stripPrefix "The proposed fee " (T.strip fatal)
  , T.isPrefixOf " are lower than the fee that baker expect by default " (T.dropWhile (/= ' ') fee')
  = Just $ RegisterStep_FeeTooLow fee
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

setHighWaterMark :: MonadLoggerIO m => AppConfig -> Either NamedChain BinaryPaths -> SecretKey -> RawLevel -> m SetHWMStep
setHighWaterMark appConfig chain sk bl = do
  e <- runExceptT $ runClientCommand appConfig chain noTimeout ["set", "ledger", "high", "watermark", "for", T.unpack (toSecretKeyText sk), "to", show (unRawLevel bl)] $ \_warnings errors -> if
    | "Ledger Application level error (set_high_watermark): Conditions of use not satisfied" : _ <- errors -> Left SetHWMStep_Declined
    | "Ledger Transport level error:" : _ <- errors -> Left SetHWMStep_Disconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left SetHWMStep_Disconnected
    | otherwise -> Left $ SetHWMStep_Failed $ T.unlines errors
  pure $ either id (const SetHWMStep_Done) e

submitProposals :: MonadLoggerIO m => AppConfig -> Either NamedChain BinaryPaths -> [ProtocolHash] -> m VoteStep
submitProposals appConfig chain proposals = do
  e <- runExceptT $ runClientCommand appConfig chain noTimeout (["submit", "proposals", "for", "ledger_kiln"] ++ map (T.unpack . toBase58Text) proposals) $ \_warnings errors -> if
    | "Submission failed because of invalid proposals." : _ <- errors -> Left $ VoteStep_Failed "Invalid proposals"
    | "Ledger Application level error (sign): Unregistered status message" : _ <- errors -> Left $ VoteStep_Failed "Not in wallet app"
    | "Ledger Application level error (sign): Conditions of use not satisfied" : _ <- errors -> Left VoteStep_Declined
    | "Unauthorized ballot" : _ <- errors -> Left $ VoteStep_Failed "Unauthorized ballot"
    | "Not in a proposal period" : _ <- errors -> Left VoteStep_WrongPeriod
    | "Ledger Transport level error:" : _ <- errors -> Left VoteStep_Disconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left VoteStep_Disconnected
    | otherwise -> Left $ VoteStep_Failed $ T.unlines errors
  pure $ either id (const VoteStep_Done) e

submitBallot :: MonadLoggerIO m => AppConfig -> Either NamedChain BinaryPaths -> ProtocolHash -> Ballot -> m VoteStep
submitBallot appConfig chain proposal ballot = do
  e <- runExceptT $ runClientCommand appConfig chain noTimeout ["submit", "ballot", "for", "ledger_kiln", T.unpack (toBase58Text proposal), ballotText ballot] $ \_warnings errors -> if
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
