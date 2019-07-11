{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Workers.Accusation where

import Control.Concurrent.STM (atomically)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebug)
import Control.Monad.Reader (ReaderT)
import Data.Pool (Pool)
import Data.Time (NominalDiffTime)
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql)
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.DB.PsqlSimple (queryQ)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)

import Tezos.Types hiding (hash, level)

import Backend.Alerts (reportAccusation)
import Backend.CachedNodeRPC
import Backend.Common (workerWithDelay)
import Backend.Config (AppConfig (..))
import Common.Schema hiding (blockLevel)
import ExtraPrelude

accusationWorker
  :: NominalDiffTime -- delay between checking for updates, in microseconds
  -> NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
accusationWorker delay nds appConfig db = runLoggingEnv (_nodeDataSource_logger nds) $ do
  let chainId = _nodeDataSource_chain nds
  workerWithDelay (pure delay) $ const $ (runLoggingEnv :: LoggingEnv -> LoggingT IO () -> IO ()) (_nodeDataSource_logger nds) $ do
    $(logDebug) "Check accusations cycle."

    params <- liftIO $ atomically $ waitForParams nds

    inDb $ do
      alertsNeeded <-
        [queryQ|
          select a.hash, a."blockHash", a."isBake", a.baker, a."occurredLevel", a.level
          from "Baker" b join "Accusation" a on b."publicKeyHash" = a.baker
          where a.chain = ?chainId
          order by a.level asc
          |] <&> fmap (\(hash, blockHash, isBake, baker, occurredLevel, level)
                       -> reportAccusation
                          hash
                          blockHash
                          (bool RightKind_Endorsing RightKind_Baking isBake)
                          baker
                          occurredLevel
                          (levelToCycle params occurredLevel)
                          level
                          (levelToCycle params level))

      flip runReaderT appConfig $ sequence_ alertsNeeded

  where
    inDb :: (MonadIO m, MonadBaseNoPureAborts IO m, MonadLogger m) => ReaderT AppConfig (DbPersist Postgresql m) a -> m a
    inDb = runDb (Identity db) . flip runReaderT appConfig
