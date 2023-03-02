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
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend.Workers.Block where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, throwM, try)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Either.Combinators (whenLeft, whenRight)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Sequence as Seq
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Database.Groundhog.Core (PhantomDb, PersistBackend)
import Database.Groundhog.Postgresql (SqlDb)
import Rhyolite.Backend.DB (MonadBaseNoPureAborts)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject)
import Rhyolite.Backend.DB.PsqlSimple (executeQ, fromOnly, queryQ)
import Rhyolite.Backend.DB.Serializable (Serializable)
import Safe (headMay)

import Tezos.CrossCompat.Block (BlockCrossCompat(..))
import Tezos.NodeRPC
import Tezos.Types

import Backend.Alerts (reportAccusation)
import Backend.Common (workerWithDelay)
import Backend.Config (AppConfig (..))
import Backend.Env
import Backend.IndexQueries (getLatestProtocolConstants, levelToCycle)
import Backend.NodeRPC
import Common.Schema
import ExtraPrelude

blockWorker
  :: ( MonadUnliftIO m
     , MonadUnliftIO w
     , MonadBaseNoPureAborts IO m
     , MonadCatch m
     )
  => NominalDiffTime -- delay between checking for updates
  -> NodeDataSource
  -> AppConfig
  -> m (w ())
blockWorker delay nds appConfig = mkWorker $ do
  headBlockOrErr <- runExceptT @KilnRpcError $ runNodeQueryT $ fmap fst getLatestProtocolConstants
  whenRight headBlockOrErr $ \headBlock -> do
    now <- liftIO getCurrentTime
    let headBlockTime = headBlock ^. timestamp
        maxTimeDiff = 600 -- 10 minutes, picked somewhat arbitrarily
        isRecentHeadBlock = now `diffUTCTime` headBlockTime < maxTimeDiff

        historyLength = 720
        headBlockLevel = headBlock ^. level
        headBlockHash = headBlock ^. hash
        cutoffLevel = headBlockLevel - historyLength

        chainId = _nodeDataSource_chain nds

    -- we check the timestamp of the head block to avoid unnecessary operations
    -- the idea is that if a node has a lot of catching up to do then:
    -- 1. most of the updates of this worker will be thrown away by the time
    --    the node has finished bootstrapping
    -- 2. if we were to do that we'd be also slowing down the node as well
    -- 3. in particularly bad cases (or, apparently, also when starting from
    --    a fresh snapshot) there are also a lot of missing 'metadata' fields in
    --    the blocks when the history mode is full/rolling (as they are recollected)
    -- IOW during normal operations the node should never fall back more than
    -- 'maxTimeDiff' and if it does we give it time to get back in shape.
    -- Note that this could also be achieved by checking the @is_bootstrapped@
    -- node endpoint, but that would be too strict and much more time consuming.
    when isRecentHeadBlock $ do
      (mbLargestParsedLvl :: Maybe RawLevel) <- fmap (headMay . fmap fromOnly) $ runTransaction [queryQ|
        select "level" from "AccusationBlock" where "chain" = ?chainId order by "level" desc limit 1
      |]
      let blockQueryLength = min historyLength $ headBlockLevel - fromMaybe 0 mbLargestParsedLvl
      blocksOrErr <- if blockQueryLength > 0
        then runExceptT @KilnRpcError $ runNodeQueryT $ nodeQueryDataSourceSafe $ NodeQuery_Blocks headBlockHash blockQueryLength
        else return $ Right mempty

      whenRight blocksOrErr $ \blocks -> do
        -- note: we want to clear old entries first because the loop just below
        -- may be interrupted before finishing
        void $ runTransaction [executeQ|
          delete from "AccusationBlock" where "level" < ?cutoffLevel and "chain" = ?chainId;
        |]

        -- Parse blocks from oldest to newest, so that we always keep the
        -- invariance that 'mbLargestParsedLvl' is always older than any block
        -- that still needs to be handled.
        -- Note that the loop runs in 'ExceptT' so no computation will follow
        -- the first one throwing an error/'Left'.
        loopResult <- try $
          for_ (Seq.reverse blocks) $ \blockHash -> do
            blockOrErr <- runExceptT @KilnRpcError $ runNodeQueryT $ do
              block <- nodeQueryDataSourceSafe $ nodeQuery_Block blockHash
              parseAndReportAccusations appConfig blockHash block
              return block
            case blockOrErr of
              Right block -> do
                let blockLevel = block ^. level
                void $ runTransaction [executeQ|
                  insert into "AccusationBlock" ("hash", "level", "chain")
                  values (?blockHash, ?blockLevel, ?chainId)
                |]
              Left e -> case e of
                -- If the block isn't known within all existing nodes, then we cannot effectively handle it
                KilnRpcError_NoSuitableNode _ _ -> pure ()
                _ -> throwM e
        -- Not all errors are thrown equal...
        -- AFAIU the ones below are both more common and less disruptive than
        -- the real unexpected errors, the latter being the only ones that we
        -- rethrow in the 'LoggingT'/'IO' monad.
        --
        -- TODO: if possible we should avoid this special treatment.
        whenLeft loopResult $ \e -> case e of
          KilnRpcError_RpcError (RpcError_RestrictedEndpoint _) -> pure ()
          KilnRpcError_RpcError (RpcError_UnexpectedStatus _ 404 _) ->
            logKilnRpcError "blockWorker" e
          KilnRpcError_NoSuitableNode _ _ ->
            logKilnRpcError "blockWorker" e
          KilnRpcError_NoKnownHeads ->
            logKilnRpcError "blockWorker" e
          _ -> do
            logKilnRpcError "blockWorker" e
            throwM e
  where
    mkWorker act = workerWithDelay "blockWorker" (pure delay) $ \_ ->
      flip runReaderT (KilnEnv appConfig nds) $ runLogger act

parseAndReportAccusations
  :: ( MonadIO m, MonadReader s m, HasNodeDataSource s, MonadError e m, AsKilnRpcError e
     , MonadMask m, PersistBackend m, PostgresLargeObject m, HasPgConn m
     , SqlDb (PhantomDb m), MonadBase Serializable m
     )
  => AppConfig -> BlockHash -> BlockCrossCompat -> NodeQueryT m ()
parseAndReportAccusations appConfig blockHash block = do
  let
    blockLevel = block ^. level
    blockCycle = case block of
      -- Genesis block doesn't have 'level_info' in metadata
      BlockGenesis _ -> 0
      BlockLima b -> b ^. blockMetadata . blockMetadata_levelInfo . levelInfo_cycle
    accusations = getAccusations block
  for_ accusations $ \(AccusationInfo aType aLevel aHash aBalanceUpdates) -> do
    let accusedBaker = getAccusedBaker aBalanceUpdates
    aCycle <- levelToCycle (blockHash, blockLevel) aLevel
    flip runReaderT appConfig $ reportAccusation
      aHash
      blockHash
      aType
      accusedBaker
      blockLevel
      blockCycle
      aLevel
      aCycle

-- | Withdrawing money from accused baker has 'freezer' kind and
-- 'deposits' category. It's expected that there is only one such
-- balance update in 'double_*_evidence' metadata.
getAccusedBaker :: [BalanceUpdate] -> PublicKeyHash
getAccusedBaker updates =
  let
    pkhsLostMoney = flip mapMaybe updates $ \case
      BalanceUpdate_Freezer (FreezerUpdate pkh change BalanceUpdateCategory_Deposits) | change < 0 -> Just pkh
      _ -> Nothing
  in case nonEmpty pkhsLostMoney of
    Nothing -> error "Negative freezer balance update not found"
    Just (pkh :| []) -> pkh
    Just _ -> error "Found more than one negative freezer balance update"
