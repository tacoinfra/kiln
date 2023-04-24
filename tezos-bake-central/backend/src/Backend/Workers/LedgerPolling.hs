{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}


module Backend.Workers.LedgerPolling where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLoggerIO)
import Data.List (sortOn)
import Data.Time (NominalDiffTime)
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB (project1, selectSingle)
import Rhyolite.Backend.DB.PsqlSimple (fromOnly, queryQ)
import Safe (headMay)
import UnliftIO.Directory (createDirectoryIfMissing)
import UnliftIO.STM (atomically, writeTQueue)

import Backend.Common.Worker (workerWithDelay)
import Backend.Config (AppConfig (..), HasAppConfig (..), tezosClientDataDir)
import Backend.Env
import Backend.NodeRPC (HasNodeDataSource (..), NodeDataSource (..), dataSourceHeadLevel)
import Backend.Schema
import Backend.Workers.TezosClient (updateConnectedLedgerViaGetConnectedLedger)
import Common.Schema
import Tezos.Types

import ExtraPrelude

-- | This worker polls the ledger device every @delay@ seconds
-- only if ledger polling state is equal to 'LedgerPollingState_Enabled'.
ledgerConnectivityCheckWorker
  :: ( MonadUnliftIO m
     , MonadUnliftIO w
     )
  => NominalDiffTime
  -> NodeDataSource
  -> AppConfig
  -> Bool
  -> m (w ())
ledgerConnectivityCheckWorker delay nds appConfig pollingEnabled = do
  createDirectoryIfMissing True (tezosClientDataDir appConfig)
  mkWorker $ runLogger $ do
    mbLedgerPollingState <- runTransaction $
      project1 ConnectedLedger_ledgerPollingStateField CondEmpty
    for_ mbLedgerPollingState $ \ledgerPollingState ->
      when (ledgerPollingState == LedgerPollingState_Enabled) $ do
        let ledgerIOQueue = _nodeDataSource_ledgerIOQueue nds
        db <- askPool
        atomically $ writeTQueue ledgerIOQueue $
          updateConnectedLedgerViaGetConnectedLedger appConfig db
  where
    mkWorker act | pollingEnabled =
      workerWithDelay "ledgerPollingWorker" (pure delay) $ \_ ->
        runReaderT act (KilnEnv appConfig nds)
    mkWorker _ = pure $ pure ()

-- | Gets the info about Kiln Baker's presence, its upcoming opportunities
-- and progress of their gathering from the database.
getKilnBakerAndNextRights
  :: ( MonadLoggerIO m
     , MonadReader e m
     , HasAppConfig e
     , HasNodeDataSource e
     )
  => RawLevel
  -> m ( Maybe PublicKeyHash
       , Maybe (RightKind, RawLevel)
       , Maybe RawLevel
       )
getKilnBakerAndNextRights headLevel = do
  chainId <- askChainId
  runTransaction $ do
    bakerInt <- join . listToMaybe <$> project
      (  BakerDaemonInternal_dataField
      ~> DeletableRow_dataSelector
      ~> BakerDaemonInternalData_publicKeyHashSelector
      )
      (  BakerDaemonInternal_dataField
      ~> DeletableRow_deletedSelector ==. False
      )

    progressMay <- flip (maybe (pure Nothing)) bakerInt $ \pkh -> do
      fmap (headMay . fmap fromOnly) [queryQ|
        SELECT brp."progress"
        FROM "BakerRightsProgress" brp
        WHERE brp."chainId" = ?chainId
          AND brp."publicKeyHash" = ?pkh
      |]

    rightsMay <- flip (maybe (pure Nothing)) bakerInt $ \pkh -> do
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

-- | This worker is responsible for setting the correct value for
-- 'ledgerPollingState' field in 'ConnectedLedger' table based on
-- the Kiln Baker's upcoming opportunities.
--
-- The @ledgerPollingState@ value is used by 'ledgerConnectivityCheckWorker'
-- to determine if it should poll the ledger device now or not.
ledgerPollingStateWorker
  :: ( MonadUnliftIO m
     , MonadUnliftIO w
     )
  => NominalDiffTime
  -> AppConfig
  -> NodeDataSource
  -> m (w ())
ledgerPollingStateWorker delay appConfig nds = mkWorker $ runLogger $ do
  mbLatestHeadLevel <- atomically $ dataSourceHeadLevel nds
  cl <- runTransaction $ selectSingle CondEmpty
  let mbLedgerPollingState = _connectedLedger_ledgerPollingState <$> cl
  for_ mbLatestHeadLevel $ \latestHeadLevel -> for_ mbLedgerPollingState $ \ledgerPollingState -> do
    (mbIntBakerPkh, mbNextRight, mbProgress) <- getKilnBakerAndNextRights latestHeadLevel
    newState <- case ledgerPollingState of
      LedgerPollingState_Unknown -> case (mbIntBakerPkh, mbNextRight, mbProgress) of
        -- If we don't have an internal baker, the state is still unknown
        (Nothing, _, _) -> pure LedgerPollingState_Unknown
        (_, Just (_, nextRightLvl), _) -> pure $
          -- If baker does have rights and the next right is farther than 10 blocks
          -- then we can safely poll the ledger. Otherwise we can't do it since the
          -- next right is close enough.
          if latestHeadLevel + 10 < nextRightLvl
          then LedgerPollingState_Enabled
          else LedgerPollingState_Disabled
        (_, Nothing, progressMay) -> case progressMay of
            -- If we have no rights, we still check that we've seen all rights up to current block
            Just progressLvl -> pure $
              -- Next baking right can be @Nothing@ not only because baker doesn't have next right,
              -- but also because we still don't have the information about it.
              --
              -- We can be sure that 'nextRight == Nothing' indicates the absence of the rights only
              -- if the progress is above than 'latestHeadLvl + 10'.
              if progressLvl > latestHeadLevel + 10
              then LedgerPollingState_Enabled
              else LedgerPollingState_Unknown
            -- If we have no info about the progress, then the state
            -- remains unknown.
            Nothing -> pure LedgerPollingState_Unknown

      LedgerPollingState_Enabled -> case (mbIntBakerPkh, mbNextRight) of
        -- Kiln Baker has been deleted, so we set the state to 'unknown'.
        (Nothing, _) -> pure LedgerPollingState_Unknown
        (_, Just (_, nextRightLvl)) -> pure $
          -- If baker does have rights and the next right is farther than 10 blocks
          -- then we can safely poll the ledger. Otherwise we can't do it since the
          -- next right is close enough.
          if latestHeadLevel + 10 < nextRightLvl
          then LedgerPollingState_Enabled
          else LedgerPollingState_Disabled
        -- If the state is already 'enabled', we don't care about the progress, and
        -- 'nextRight == Nothing' indicates only that the baker doesn't have baking
        -- rights, and we can safely continue the polling
        (_, Nothing) -> pure LedgerPollingState_Enabled

      LedgerPollingState_Disabled -> case (mbIntBakerPkh, mbNextRight) of
        -- Kiln Baker has been deleted, so we set the state to 'unknown'.
        (Nothing, _) -> pure LedgerPollingState_Unknown
        -- Kiln Baker lost its rights, so we set the state to 'unknown'
        -- and start the whole procedure again
        (_, Nothing) -> pure LedgerPollingState_Unknown
        _ -> pure LedgerPollingState_Disabled
    when (ledgerPollingState /= newState) $ runTransaction $ do
      update [ConnectedLedger_ledgerPollingStateField =. newState] CondEmpty
      let newCl = fmap (\cl' -> cl' { _connectedLedger_ledgerPollingState = newState }) cl
      notify NotifyTag_ConnectedLedger newCl
  where
    mkWorker act = workerWithDelay "ledgerPollingStateWorker" (pure delay) $ \_ ->
      runReaderT act (KilnEnv appConfig nds)
