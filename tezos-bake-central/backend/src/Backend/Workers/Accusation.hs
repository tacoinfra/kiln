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

import Control.Monad.Logger (LoggingT, logDebug)
import Control.Monad.Except (runExceptT)
import Data.Time (NominalDiffTime)
import Rhyolite.Backend.DB.PsqlSimple (queryQ)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)

import Backend.Alerts (reportAccusation)
import Backend.Common (workerWithDelay)
import Backend.Config (AppConfig (..))
import Backend.IndexQueries (levelToCycle)
import Backend.NodeRPC
import Common.Schema
import ExtraPrelude

accusationWorker
  :: NominalDiffTime -- delay between checking for updates, in microseconds
  -> NodeDataSource
  -> AppConfig
  -> IO (IO ())
accusationWorker delay nds appConfig = runLoggingEnv (_nodeDataSource_logger nds) $ do
  let chainId = _nodeDataSource_chain nds
  workerWithDelay "accusationWorker" (pure delay) $ const $ (runLoggingEnv :: LoggingEnv -> LoggingT IO () -> IO ()) (_nodeDataSource_logger nds) $ do
    $(logDebug) "Check accusations cycle."

    either (logKilnRpcError "Accusation Worker") pure <=< flip runReaderT nds $ runExceptT @KilnRpcError $ runNodeQueryT $ do
      alertData <- [queryQ|
        select a.hash, a."blockHash", a."accusationType", a.baker, a."occurredLevel", a.level
        from "Baker" b join "Accusation" a on b."publicKeyHash" = a.baker
        where a.chain = ?chainId
        order by a.level asc
      |]

      for_ alertData $ \(aHash, aBlockHash, aType, aBaker, aOccurredLevel, aLevel) -> do
        (aOccurredCycle, aCycle) <- liftA2 (,) (levelToCycle (aBlockHash, aLevel) aOccurredLevel) (levelToCycle (aBlockHash, aLevel) aLevel)
        flip runReaderT appConfig $ reportAccusation
          aHash
          aBlockHash
          aType
          aBaker
          aOccurredLevel
          aOccurredCycle
          aLevel
          aCycle
