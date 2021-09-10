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
import Control.Exception (catchJust)
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Aeson.Lens
import Data.Maybe (mapMaybe)
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
import Backend.CachedNodeRPC
import Backend.Common (AppSerializable, addBakerImpl, workerWithDelay, readCreateProcessWithExitCodeWithLogging, timeout')
import Backend.Config (AppConfig (..), tezosClientDataDir, kilnNodeRpcURI', kilnNodeRpcURI, BinaryPaths(..))
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
  update [ProcessData_controlField =. ProcessControl_Run] $ AutoKeyField `in_` processes

tezosClientWorker
  :: NominalDiffTime
  -> Maybe NominalDiffTime
  -> LoggingEnv
  -> NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> Maybe BinaryPaths
  -> IO (IO ())
tezosClientWorker delay !mLedgerCheckDelay logger nds appConfig db maybePaths = runLoggingEnv logger $ do
  workerWithDelay "tezosClientWorker" (pure delay) $ const $ runLoggingEnv logger $ do
    liftIO $ createDirectoryIfMissing True (tezosClientDataDir appConfig)

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
            importSecretKey appConfig maybePaths sk >>= \i -> inDb $ do
              update [LedgerAccount_importedField =. False] (LedgerAccount_importedField ==. True)
              update
                [LedgerAccount_importedField =. (i == ImportSecretKeyStep_Done), LedgerAccount_shouldImportField =. False]
                (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
              notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_import = Just $ First i })

          -- setup to bake
          inDb (selectSingle $ LedgerAccount_shouldSetupToBakeField ==. True &&. LedgerAccount_importedField ==. True) >>= \mla -> for_ mla $ \la -> do
            let sk = _ledgerAccount_secretKey la
            inDb $ notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_setup = Just $ First SetupLedgerToBakeStep_Prompting })
            setupLedgerToBake appConfig maybePaths >>= \i -> do
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
            &&. LedgerAccount_shouldRegisterField ==. True
            &&. LedgerAccount_importedField ==. True) >>= \mla -> for_ mla $ \la -> case (_ledgerAccount_publicKeyHash la) of
            Nothing -> pure () -- shouldn't happen due to WHERE clause
            Just pkh -> do
              let sk = _ledgerAccount_secretKey la
              inDb $ notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_register = Just $ First RegisterStep_Prompting })
              registerKeyAsDelegate logger db nds sk pkh appConfig maybePaths >>= \result -> inDb $ do
                update
                  [LedgerAccount_shouldRegisterField =. False]
                  (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
                notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_register = Just $ First result })
                when (result == RegisterStep_Registered) $ startBaking pkh

          -- run appropriate 'show ledger' commands, but only do one at a time
          -- to allow other commands to take precedence
          inDb (project1 LedgerAccount_secretKeyField (isFieldNothing LedgerAccount_publicKeyHashField)) >>= \msk -> for_ msk $ \sk -> do
            showLedger appConfig maybePaths sk >>= \case
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
                  notify NotifyTag_ShowLedger (sk, Left (T.pack $ show err))
                $(logError) (T.pack (show err))
              Right mPkh -> do
                case mPkh of
                  Nothing -> inDb $ notify NotifyTag_ShowLedger (sk, Left $ "tezosClientWorker:showLedger: public key hash unavailable")
                  Just pkh -> getBalanceFor appConfig maybePaths pkh >>= \case
                    -- In case of error, give another try in the code further down
                    Left err -> $(logError) (T.pack (show err))
                    Right Nothing -> $(logError) $ "Failed to get balance of account " <> toPublicKeyHashText pkh
                    Right (Just tez) -> inDb $ do
                      update [LedgerAccount_balanceField =. Just tez] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
                      notify NotifyTag_ShowLedger (sk, Right (pkh, tez))
                inDb $ (maybe delete (\pkh -> update [LedgerAccount_publicKeyHashField =. Just pkh]) mPkh)
                  (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)

          -- get any missing balances
          las <- inDb $ select $ LedgerAccount_publicKeyHashField /=. (Nothing :: Maybe PublicKeyHash) &&. isFieldNothing LedgerAccount_balanceField
          let las' = mapMaybe (\la -> (,) (_ledgerAccount_secretKey la) <$> _ledgerAccount_publicKeyHash la) las
          for_ las' $ \(sk, pkh) -> getBalanceFor appConfig maybePaths pkh >>= \case
            Left (ClientError_Timeout)-> do
              $(logError) ("Client Timout: getBalanceFor: " <> toPublicKeyHashText pkh)
              inDb $ do
                delete $ embeddedSecretKeyEquals LedgerAccount_secretKeyField sk
                notify NotifyTag_ShowLedger (sk, Left $ T.pack $ show ClientError_Timeout)
            Left err -> $(logError) (T.pack (show err))
            Right Nothing -> $(logError) $ "Failed to get balance of account " <> toPublicKeyHashText pkh
            Right (Just tez) -> inDb $ do
              update [LedgerAccount_balanceField =. Just tez] (embeddedSecretKeyEquals LedgerAccount_secretKeyField sk)
              notify NotifyTag_ShowLedger (sk, Right (pkh, tez))

          -- set high water mark
          inDb (selectSingle $ LedgerAccount_shouldSetHWMField /=. (Nothing :: Maybe RawLevel)) >>= \mla ->
            for_ mla $ \la -> case _ledgerAccount_shouldSetHWM la of
              Nothing -> pure () -- shouldn't happen
              Just hwm -> do
                let sk = _ledgerAccount_secretKey la
                inDb $ notify NotifyTag_Prompting (sk, Just $ mempty { _setupState_setHWM = Just $ First SetHWMStep_Prompting })
                setHighWaterMark appConfig maybePaths sk hwm >>= \i -> inDb $ do
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
                  vs <- submitProposals appConfig maybePaths [proposalHash]
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
                  vs <- submitBallot appConfig maybePaths proposalHash ballot
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

        if _connectedLedger_forceConnectivityCheck cl
          -- If we want to immediately do the connectivity check
          then updateConnectedLedgerViaGetConnectedLedger appConfig db maybePaths
          -- Otherwise, we might want to do the connectivity check because some time has passed
          else case (mLedgerCheckDelay, _connectedLedger_updated cl) of
            (Nothing, _) -> pure ()
            (_,Nothing) -> updateConnectedLedgerViaGetConnectedLedger appConfig db maybePaths
            (Just ledgerBackgroundUpdateInterval, Just upd) ->
              when (currentTime `diffUTCTime` upd > ledgerBackgroundUpdateInterval) $ do
                doSensibleLedgerCheck (isJust $ _connectedLedger_ledgerIdentifier cl)

      _ -> do
        -- If there is no row in the DB, this is our first time running and we should check it
        doSensibleLedgerCheck False
        pure ()

    where
      inDb :: AppSerializable a -> LoggingT IO a
      inDb = runDb (Identity db) . flip runReaderT appConfig

      -- Regardless of updated time, we ought not to check the ledger if we are two levels around
      -- a baking right and we shouldn't bother checking if we don't have an internal baker running
      -- either
      doSensibleLedgerCheck _wasConnected = do
        dsh <- liftIO $ atomically $ dataSourceHead nds
        doCheck <- for dsh $ \blk -> checkKilnBakerAndNextRights appConfig nds blk >>= \case
          -- If we don't have an internal baker, don't bother checking
          (Nothing, _) -> pure False
          -- Avoid sending commands to the ledger within two blocks of baking rights
          (_, Just (_, lvl)) -> do
            let doC = (blk ^. level < lvl - 2 || blk ^. level > lvl + 2)
            -- This is pretty spammy. We probably don't want this without updating the updated flag...
            -- unless (doC || wasConnected) $ $(logWarn) ("Baking rights approaching at level " <> tshow lvl <> ". Kiln last saw that the ledger was disconnected!")
            pure doC
          -- If we have no rights but a baker, we may as well check because the rights are coming
          _ -> pure True
        when (doCheck == Just True) $ updateConnectedLedgerViaGetConnectedLedger appConfig db maybePaths

withDbAndConfig :: Pool Postgresql -> AppConfig -> AppSerializable a -> LoggingT IO a
withDbAndConfig db appConfig = runDb (Identity db) . flip runReaderT appConfig

updateConnectedLedgerViaGetConnectedLedger :: AppConfig -> Pool Postgresql -> Maybe BinaryPaths -> LoggingT IO ()
updateConnectedLedgerViaGetConnectedLedger appConfig db maybePaths = do
  getConnectedLedger appConfig maybePaths >>= \case
    Left err -> do
      $(logError) (tshow err)
      reportLedgerDisconnection db appConfig
      updateConnectedLedger Nothing
    Right mliv -> do
      case mliv of
        Nothing -> do
          reportLedgerDisconnection db appConfig
          $(logDebug) "The connectedledger is Nothing"

        Just _ -> do
          clearLedgerDisconnection db appConfig

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

-- Clears any notion of us waiting for us to do an action on this ledger. Call this when kiln boots
-- just in case it was in any kind of ledger action prior to the restart
resetLedgerQueue :: (MonadIO m)  => LoggingEnv -> Pool Postgresql ->  m ()
resetLedgerQueue logger db =  liftIO $ runLoggingEnv logger $ runDb (Identity db) $ update
  [ LedgerAccount_shouldImportField =. False
  , LedgerAccount_shouldSetHWMField =. (Nothing :: Maybe RawLevel)
  , LedgerAccount_shouldDoVoteBallotField =. (Nothing :: Maybe Ballot)
  , LedgerAccount_shouldDoVoteProtocolField =. (Nothing :: Maybe (Id PeriodProposal))
  , LedgerAccount_shouldRegisterField =. False
  , LedgerAccount_shouldSetupToBakeField =. False
  ]
  CondEmpty

getConnectedLedger :: (MonadLoggerIO m) => AppConfig -> Maybe BinaryPaths -> m (Either ClientError (Maybe (LedgerIdentifier, LedgerApp, Text)))
getConnectedLedger appConfig maybePaths = runExceptT $ do
  stdout <- runClientCommand appConfig maybePaths defaultTimeout ["list", "connected", "ledgers"] $ \_warnings errors -> if
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
        pure $ Nothing

getBalanceFor :: (MonadLoggerIO m) => AppConfig -> Maybe BinaryPaths -> PublicKeyHash -> m (Either ClientError (Maybe Tez))
getBalanceFor appConfig maybePaths pkh = runExceptT $ do
  stdout <- runClientCommand appConfig maybePaths defaultTimeout ["get", "balance", "for", T.unpack $ toPublicKeyHashText pkh] $ \warnings errors -> if
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

showLedger :: (MonadLoggerIO m) => AppConfig -> Maybe BinaryPaths -> SecretKey -> m (Either ClientError (Maybe PublicKeyHash))
showLedger appConfig maybePaths sk = runExceptT $ do
  stdout <- runClientCommand appConfig maybePaths defaultTimeout ["show", "ledger", T.unpack $ toSecretKeyText sk] $ \_warnings errors -> if
    | e : _ <- errors, Just _sk' <- T.stripPrefix "No ledger found for " e -> Left ClientError_LedgerDisconnected
    | "Ledger Transport level error:" : _ <- errors -> Left ClientError_LedgerDisconnected
    | "(Invalid_argument int32_of_path_element_exn)" : _ <- errors -> Right ""
    | otherwise -> Left $ ClientError_Other $ T.unlines errors
  let pkh = getPublicKeyHash (T.lines stdout)
  {- TODO: probably will need to circle back to this commented out code -}
  -- let pkh = case maybePaths of
        -- Left NamedChain_Zeronet -> getPublicKeyHashZeronet (T.lines stdout)
        -- _ -> getPublicKeyHash (T.lines stdout)
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

importSecretKey :: (MonadLoggerIO m) => AppConfig -> Maybe BinaryPaths -> SecretKey -> m ImportSecretKeyStep
importSecretKey appConfig maybePaths sk = do
  e <- runExceptT $ runClientCommand appConfig maybePaths noTimeout ["import", "secret", "key", T.unpack kilnLedgerAlias, T.unpack $ toSecretKeyText sk, "--force"] $ \_warnings errors -> if
    | "Ledger Application level error (get_public_key): Conditions of use not satisfied" : _ <- errors -> Left ImportSecretKeyStep_Declined
    | "Ledger Transport level error:" : _ <- errors -> Left ImportSecretKeyStep_Disconnected
    | otherwise -> Left $ ImportSecretKeyStep_Failed $ T.unlines errors
  pure $ either id (const ImportSecretKeyStep_Done) e

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
  -> Maybe BinaryPaths
  -> Maybe (NominalDiffTime, e)
  -> [String]
  -> ([Text] -> [Text] -> Either e Text)
  -> ExceptT e m Text
runClientCommand appConfig = runClientCommand' (kilnNodeRpcURI appConfig) (tezosClientDataDir appConfig)

computeChainId :: (MonadLoggerIO m) => Port -> FilePath -> Maybe BinaryPaths -> Aeson.Value -> m (Either Text ChainId)
computeChainId port kilnDataDir maybePaths json = do
    e <- runExceptT $ ExceptT (pure eCommand) >>= \command -> runClientCommand' (kilnNodeRpcURI' port) kilnDataDir maybePaths noTimeout command $ \_warnings errors -> if
      | "Wrong value for command line option --protocol" : _ <- errors -> Left "Wrong Protocol"
      | otherwise -> Left "Something else happened."
    pure $ first T.pack e >>= first tshow . fromBase58 . TE.encodeUtf8
  where
    note key' = maybe (Left $ printf "key %s not available" key') (Right . T.unpack)
    protocol = note ("protocol" :: String) $ json ^? key "network" . key "genesis" . key "protocol" . _String
    genesisBlock = note ("block" :: String) $ json ^? key "network" . key "genesis" . key "block" . _String
    eCommand :: Either String [String]
    eCommand =
      liftA2 (\p gb -> words $ printf "--protocol %s compute chain id from block hash %s" p gb) protocol genesisBlock

setupLedgerToBake :: (MonadLoggerIO m) => AppConfig -> Maybe BinaryPaths -> m SetupLedgerToBakeStep
setupLedgerToBake appConfig maybePaths = do
  e <- runExceptT $ runClientCommand appConfig maybePaths noTimeout ["setup", "ledger", "to", "bake", "for", T.unpack kilnLedgerAlias] $ \_warnings errors -> if
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
  => LoggingEnv -> Pool Postgresql -> NodeDataSource -> SecretKey -> PublicKeyHash -> AppConfig -> Maybe BinaryPaths -> m RegisterStep
registerKeyAsDelegate logger db nds sk pkh appConfig maybePaths = checkIfRegistered logger db nds pkh >>= \case
    True -> pure RegisterStep_AlreadyRegistered
    False -> do
      -- withCreateProcess will close these automatically
      (readPipe, writePipe) <- liftIO Process.createPipe
      let p = (Process.proc (clientPath maybePaths) ["--endpoint", T.unpack $ render $  kilnNodeRpcURI appConfig, "--base-dir", tezosClientDataDir appConfig, "register", "key", T.unpack kilnLedgerAlias, "as", "delegate"])
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
                let mrs = parseRegisterStep t
                $(logInfo) $ "registerKeyAsDelegate: " <> t <> " -> " <> T.pack (show mrs)
                traverse_ notifyStep mrs
                case mrs of
                  Just RegisterStep_NodeNotReady -> pure RegisterStep_NodeNotReady
                  _ -> go $ mrs' <|> mrs
        go Nothing
      $(logWarn) $ T.pack $ show result
      pure result

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

setHighWaterMark :: (MonadLoggerIO m) => AppConfig -> Maybe BinaryPaths -> SecretKey -> RawLevel -> m SetHWMStep
setHighWaterMark appConfig maybePaths sk bl = do
  e <- runExceptT $ runClientCommand appConfig maybePaths noTimeout ["set", "ledger", "high", "watermark", "for", T.unpack (toSecretKeyText sk), "to", show (unRawLevel bl)] $ \_warnings errors -> if
    | "Ledger Application level error (set_high_watermark): Conditions of use not satisfied" : _ <- errors -> Left SetHWMStep_Declined
    | "Ledger Transport level error:" : _ <- errors -> Left SetHWMStep_Disconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left SetHWMStep_Disconnected
    | otherwise -> Left $ SetHWMStep_Failed $ T.unlines errors
  pure $ either id (const SetHWMStep_Done) e

submitProposals :: (MonadLoggerIO m) => AppConfig -> Maybe BinaryPaths -> [ProtocolHash] -> m VoteStep
submitProposals appConfig maybePaths proposals = do
  e <- runExceptT $ runClientCommand appConfig maybePaths noTimeout (["submit", "proposals", "for", "ledger_kiln"] ++ map (T.unpack . toBase58Text) proposals) $ \_warnings errors -> if
    | "Submission failed because of invalid proposals." : _ <- errors -> Left $ VoteStep_Failed "Invalid proposals"
    | "Ledger Application level error (sign): Unregistered status message" : _ <- errors -> Left $ VoteStep_Failed "Not in wallet app"
    | "Ledger Application level error (sign): Conditions of use not satisfied" : _ <- errors -> Left VoteStep_Declined
    | "Unauthorized ballot" : _ <- errors -> Left $ VoteStep_Failed "Unauthorized ballot"
    | "Not in a proposal period" : _ <- errors -> Left VoteStep_WrongPeriod
    | "Ledger Transport level error:" : _ <- errors -> Left VoteStep_Disconnected
    | t : _ <- errors, Just _secretKey <- T.stripPrefix "No Ledger found for " t -> Left VoteStep_Disconnected
    | otherwise -> Left $ VoteStep_Failed $ T.unlines errors
  pure $ either id (const VoteStep_Done) e

submitBallot :: (MonadLoggerIO m) => AppConfig -> Maybe BinaryPaths -> ProtocolHash -> Ballot -> m VoteStep
submitBallot appConfig maybePaths proposal ballot = do
  e <- runExceptT $ runClientCommand appConfig maybePaths noTimeout ["submit", "ballot", "for", "ledger_kiln", T.unpack (toBase58Text proposal), ballotText ballot] $ \_warnings errors -> if
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
checkKilnBakerAndNextRights :: (BlockLike blk) => AppConfig -> NodeDataSource -> blk -> LoggingT IO (Maybe PublicKeyHash, Maybe (RightKind, RawLevel))
checkKilnBakerAndNextRights appConfig nds blk = withDbAndConfig (_nodeDataSource_pool nds) appConfig $ do
  v <- flip runReaderT nds $ runExceptT @CacheError $ tryNodeQueryT $ do
    bakerInt :: Maybe PublicKeyHash <- join . listToMaybe <$> project (BakerDaemonInternal_dataField ~> DeletableRow_dataSelector ~> BakerDaemonInternalData_publicKeyHashSelector)
      (BakerDaemonInternal_dataField ~> DeletableRow_deletedSelector ==. False)

    rightsMay :: Maybe [(RightKind, RawLevel)] <- for bakerInt $ \pkh -> do
      let headLevel = blk ^. level
          chainId = _appConfig_chainId appConfig

      [queryQ|
          SELECT br."right", MIN(br.level)
          FROM "BakerRightsCycleProgress" brcp
          JOIN "BakerRight" br
            ON br.branch = brcp.id
            AND br.level > ?headLevel + CASE WHEN br."right" = 'RightKind_Endorsing' THEN -1 ELSE 0 END -- if the endorsement is of the current block, you haven't missed it yet.
          WHERE brcp."chainId" = ?chainId
            AND brcp."publicKeyHash" = ?pkh
          GROUP BY brcp."publicKeyHash", br."right"
        |]

    pure (bakerInt, (rightsMay >>= headMay))
  pure $ (v^?_Right._Just._1._Just, v^?_Right._Just._2._Just)
