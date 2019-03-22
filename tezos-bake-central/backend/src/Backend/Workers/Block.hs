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

module Backend.Workers.Block where

import Control.Concurrent.STM (atomically)
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebug, logErrorSH)
import Control.Monad.Logger (logWarnSH)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Sequence as Seq
import Data.Time (NominalDiffTime)
import Database.Groundhog.Core
import Database.Groundhog.Postgresql (Postgresql)
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.DB.PsqlSimple (executeQ, queryQ)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)

import qualified Tezos.Binary as TBin
import Tezos.Envelope (Envelope (Envelope_Endorsement))
import Tezos.NodeRPC.Types (RpcError(RpcError_UnexpectedStatus))
import Tezos.Operation
import qualified Tezos.Signature.Verify as Sig
import Tezos.Types

import Backend.CachedNodeRPC
import Backend.Common (workerWithDelay)
import Backend.Config (AppConfig (..))
import Common.Schema hiding (blockLevel)
import ExtraPrelude

blockWorker
  :: NominalDiffTime -- delay between checking for updates
  -> NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
blockWorker delay nds appConfig db = runLoggingEnv (_nodeDataSource_logger nds) $ do
  let chainId = _nodeDataSource_chain nds
  let claimTimeout = "15 seconds" :: Text
  workerWithDelay (pure delay) $ const $ (runLoggingEnv :: LoggingEnv -> LoggingT IO () -> IO ()) (_nodeDataSource_logger nds) $ do
    (params, dsh) <- liftIO $ atomically $
      (,) <$> waitForParams nds <*> dataSourceHead nds
    let headLevelMay = (^. level) <$> dsh
    let cutoffLevel = maybe 0 (rightsContextLevel params) headLevelMay

    queuedBlockOrNot <- inDb $ do
      [queryQ|
        update "BlockTodo"
        set "claimedBy" = 1
          , "claimedAt" = now()
        where hash = (
          select hash
          from "BlockTodo"
          where (not "parsedParent" or not "parsedAccusations") and chain = ?chainId and ("claimedBy" is null or "claimedAt" < now() - interval ?claimTimeout) and level >= ?cutoffLevel
          order by level desc
          limit 1
          for update skip locked
          )
        returning hash, level, chain, "claimedBy", "claimedAt" at time zone 'UTC', "parsedParent", "parsedAccusations"
        |] <&> fmap (\(a,b,c,d,e,f,g) -> BlockTodo a b c d e f g)
    -- c.f. https://blog.2ndquadrant.com/what-is-select-skip-locked-for-in-postgresql-9-5/
    -- Note that we don't actually do a very long-running transaction here,
    -- since we're doing something idempotent and it's okay for backends to
    -- step on each other as long as it's rare, so it's better to let
    -- leases on work items time out and let other backends just steal them,
    -- rather than making postgres the central arbiter of locking.

    for_ queuedBlockOrNot $ \queuedBlock -> (either ($(logErrorSH) . \e -> ("blockWorker" :: Text,queuedBlock,e)) pure =<<) $ flip runReaderT nds $ runExceptT @CacheError $ runNodeQueryT $ do
      $(logDebug) $ "Scrape block " <> toBase58Text (_blockTodo_hash queuedBlock) <> "."
      couldBeBlock <- unliftEither $ nodeQueryDataSourceSafe $ NodeQuery_Block $ _blockTodo_hash queuedBlock
      case couldBeBlock of
        Left (CacheError_RpcError (RpcError_UnexpectedStatus 404 _)) ->
          $(logWarnSH) ("blockWorker"::Text,"block cannot be retrieved from available nodes"::Text,toBase58Text (_blockTodo_hash queuedBlock))
        Left (CacheError_NoSuitableNode) ->
          $(logWarnSH) ("blockWorker"::Text,"block cannot be retrieved from available nodes"::Text,toBase58Text (_blockTodo_hash queuedBlock))
        Left e -> nqThrowError e
        Right block -> do
          let blockHash = _block_hash block

          let parentHash = block ^. predecessor
              parentLevel = block ^. level - 1
           in void $ [executeQ|
                insert into "BlockTodo" (hash, level, chain, "claimedBy", "claimedAt", "parsedParent", "parsedAccusations")
                values (?parentHash, ?parentLevel, ?chainId, null, null, false, false)
                on conflict do nothing
                |]
          -- Operations into a block are divided into 4 subsections.  Accusations
          -- are always in the third of these sections.
          let mightBeAccusations = foldMap id $ Seq.lookup 2 $ _block_operations block
          for_ mightBeAccusations $ \op -> do
            let
              opHash = _operation_hash op
              blockLevel = _blockHeader_level (_block_header block)
            for_ (_operation_contents op) $ \case
              OperationContents_DoubleBakingEvidence ev -> do
                let
                  accusedLevel = ev ^. operationContentsDoubleBakingEvidence_bh1 . blockHeader_level
                  accusedPriority = ev ^. operationContentsDoubleBakingEvidence_bh1 . blockHeader_priority
                baker <- fmap _bakingRights_delegate $ nodeQueryDataSourceSafe $ NodeQuery_BakingRights1 blockHash accusedLevel accusedPriority
                void $ [executeQ|
                  insert into "Accusation" (hash, "blockHash", level, chain, baker, "occurredLevel", "isBake")
                  values (?opHash, ?blockHash, ?blockLevel, ?chainId, ?baker, ?accusedLevel, true)
                  on conflict do nothing
                  |]
              OperationContents_DoubleEndorsementEvidence ev -> do
                let
                  accusedLevel = ev ^. operationContentsDoubleEndorsementEvidence_op1 . inlinedEndorsement_operations . inlinedEndorsementContents_level
                possibles <- (fmap.fmap) _endorsingRights_delegate $ nodeQueryDataSourceSafe $ NodeQuery_EndorsingRights blockHash accusedLevel
                possiblesKeys <- traverse (nodeQueryDataSourceSafe . NodeQuery_PublicKey . Implicit) possibles
                let
                  encodedOp1 = TBin.encode $ Envelope_Endorsement chainId $ outlineEndorsement $ ev ^. operationContentsDoubleEndorsementEvidence_op1
                  sig = fromMaybe (error "inlined endorsements in double endorsement evidence are always signed") $ ev ^. operationContentsDoubleEndorsementEvidence_op1 . inlinedEndorsement_signature
                  actuals = Seq.filter (\(_,key) -> Sig.check key sig encodedOp1) $ Seq.zip possibles possiblesKeys
                for_ actuals $ \(baker,_) -> [executeQ|
                  insert into "Accusation" (hash, "blockHash", level, chain, baker, "occurredLevel", "isBake")
                  values (?opHash, ?blockHash, ?blockLevel, ?chainId, ?baker, ?accusedLevel, false)
                  on conflict do nothing
                  |]
              _ -> return ()

          void $ [executeQ|
            update "BlockTodo"
            set "claimedBy" = null,
                "claimedAt" = null,
                "parsedParent" = true,
                "parsedAccusations" = true
            where chain = ?chainId and hash = ?blockHash
            |]
    return ()

  where
    inDb :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => ReaderT AppConfig (DbPersist Postgresql m) a -> m a
    inDb = runDb (Identity db) . flip runReaderT appConfig
