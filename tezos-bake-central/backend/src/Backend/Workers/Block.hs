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

import Control.Lens ((^..))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger (LoggingT, logDebug, logErrorSH, logDebugSH)
import Data.ByteString as BS
import Database.Groundhog.Core (PersistBackend)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Sequence as Seq
import Data.Time (NominalDiffTime)
import Database.Groundhog.Postgresql (Postgresql)
import Rhyolite.Backend.DB.PsqlSimple (executeQ, queryQ, PostgresRaw)
import Rhyolite.Backend.Logging (LoggingEnv (..), runLoggingEnv)

import Tezos.Common.Binary as TBin
import Tezos.NodeRPC
import Tezos.Types
import qualified Tezos.V004.Types as V004
import qualified Tezos.V005.Types as V005
import Tezos.Signature.Verify as Sig

import Backend.CachedNodeRPC
import Backend.Common (workerWithDelay)
import Backend.Config (AppConfig (..))
import Backend.IndexQueries (rightsContextLevel, getLatestProtocolConstants)
import Common.Schema
import ExtraPrelude

blockWorker
  :: NominalDiffTime -- delay between checking for updates
  -> NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
blockWorker delay nds _appConfig _db = runLoggingEnv (_nodeDataSource_logger nds) $ do
  let chainId = _nodeDataSource_chain nds
  let claimTimeout = "15 seconds" :: Text
  workerWithDelay "blockWorker" (pure delay) $ const $ (runLoggingEnv :: LoggingEnv -> LoggingT IO () -> IO ()) (_nodeDataSource_logger nds) $ do
    queuedBlockOrNot :: Either CacheError [BlockTodo] <- flip runReaderT nds $ runExceptT $ runNodeQueryT $ do
      (headBlock, _) <- getLatestProtocolConstants
      cutoffLevel <- rightsContextLevel (headBlock ^. hash) (headBlock ^. level)
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

    for_ (queuedBlockOrNot ^.. _Right . traverse) $ \queuedBlock -> (either ($(logErrorSH) . cacheErrorLogMessage "blockWorker") pure =<<) $ flip runReaderT nds $ runExceptT @CacheError $ runNodeQueryT $ do
      $(logDebug) $ "Scrape block " <> toBase58Text (_blockTodo_hash queuedBlock) <> "."
      couldBeBlock <- unliftEither $ nodeQueryDataSourceSafe $ NodeQuery_Block (_blockTodo_hash queuedBlock)
      case couldBeBlock of
        Left (CacheError_RpcError (RpcError_UnexpectedStatus _url 404 _)) ->
          $(logDebugSH) ("blockWorker"::Text,"Error (404) in retrieving block from available nodes"::Text,toBase58Text (_blockTodo_hash queuedBlock))
        Left (CacheError_NoSuitableNode q reasons) ->
          $(logDebugSH) ("blockWorker"::Text, noSuitableNodeLogMessage q reasons)
        Left e -> nqThrowError e
        Right block -> do
          let blockHash = block ^. hash

          let parentHash = block ^. predecessor
              parentLevel = block ^. level - 1
           in void [executeQ|
                insert into "BlockTodo" (hash, level, chain, "claimedBy", "claimedAt", "parsedParent", "parsedAccusations")
                values (?parentHash, ?parentLevel, ?chainId, null, null, false, false)
                on conflict do nothing
                |]

          blockCrossCata (insertAccusationsV4 blockHash chainId) (insertAccusationsV5 blockHash chainId) block

          void [executeQ|
            update "BlockTodo"
            set "claimedBy" = null,
                "claimedAt" = null,
                "parsedParent" = true,
                "parsedAccusations" = true
            where chain = ?chainId and hash = ?blockHash
            |]

-- TODO: This could use a better abstraction here.
insertAccusationsV4
  :: ( MonadIO m, MonadReader s m, HasNodeDataSource s, MonadError e m, AsCacheError e
     , PostgresRaw m, MonadMask m, PersistBackend m
     )
  => BlockHash -> ChainId -> V004.Block -> NodeQueryT m ()
insertAccusationsV4 blockHash chainId block = do
  -- Operations into a block are divided into 4 subsections.  Accusations
  -- are always in the third of these sections.
  let mightBeAccusations = fold $ Seq.lookup 2 $ V004._block_operations block
  for_ mightBeAccusations $ \op -> do
    let
      opHash = V004._operation_hash op
      blockLevel = block ^. level
    for_ (V004._operation_contents op) $ \case
      V004.OperationContents_DoubleBakingEvidence ev -> do
        let
          accusedLevel = ev ^. V004.operationContentsDoubleBakingEvidence_bh1 . V004.blockHeaderFull_level
          accusedPriority = ev ^. V004.operationContentsDoubleBakingEvidence_bh1 . V004.blockHeaderFull_priority
        insertDoubleBakingEvidence blockHash chainId opHash blockLevel accusedLevel accusedPriority
      V004.OperationContents_DoubleEndorsementEvidence ev -> do
        let
          accusedLevel = ev ^. V004.operationContentsDoubleEndorsementEvidence_op1 . V004.inlinedEndorsement_operations . V004.inlinedEndorsementContents_level
        (possibles,possiblesKeys) <- loadPossibles blockHash accusedLevel
        let
          encodedOp1 = TBin.encode $ V004.Envelope_Endorsement chainId $ V004.outlineEndorsement $ ev ^. V004.operationContentsDoubleEndorsementEvidence_op1
          sig = fromMaybe (error "inlined endorsements in double endorsement evidence are always signed") $ ev ^. V004.operationContentsDoubleEndorsementEvidence_op1 . V004.inlinedEndorsement_signature
        insertDoubleEndorsementEvidence blockHash chainId opHash blockLevel accusedLevel sig encodedOp1 possibles possiblesKeys
      _ -> return ()

insertAccusationsV5
  :: ( MonadIO m, MonadReader s m, HasNodeDataSource s, MonadError e m, AsCacheError e
     , PostgresRaw m, MonadMask m, PersistBackend m
     )
  => BlockHash -> ChainId -> V005.Block -> NodeQueryT m ()
insertAccusationsV5 blockHash chainId block = do
  -- Operations into a block are divided into 4 subsections.  Accusations
  -- are always in the third of these sections.
  let mightBeAccusations = fold $ Seq.lookup 2 $ V005._block_operations block
  for_ mightBeAccusations $ \op -> do
    let
      opHash = V005._operation_hash op
      blockLevel = block ^. level
    for_ (V005._operation_contents op) $ \case
      V005.OperationContents_DoubleBakingEvidence ev -> do
        let
          accusedLevel = ev ^. V005.operationContentsDoubleBakingEvidence_bh1 . V005.blockHeaderFull_level
          accusedPriority = ev ^. V005.operationContentsDoubleBakingEvidence_bh1 . V005.blockHeaderFull_priority
        insertDoubleBakingEvidence blockHash chainId opHash blockLevel accusedLevel accusedPriority
      V005.OperationContents_DoubleEndorsementEvidence ev -> do
        let
          accusedLevel = ev ^. V005.operationContentsDoubleEndorsementEvidence_op1 . V005.inlinedEndorsement_operations . V005.inlinedEndorsementContents_level
        (possibles,possiblesKeys) <- loadPossibles blockHash accusedLevel
        let
          encodedOp1 = TBin.encode $ V005.Envelope_Endorsement chainId $ V005.outlineEndorsement $ ev ^. V005.operationContentsDoubleEndorsementEvidence_op1
          sig = fromMaybe (error "inlined endorsements in double endorsement evidence are always signed") $ ev ^. V005.operationContentsDoubleEndorsementEvidence_op1 . V005.inlinedEndorsement_signature
        insertDoubleEndorsementEvidence blockHash chainId opHash blockLevel accusedLevel sig encodedOp1 possibles possiblesKeys
      _ -> return ()

insertDoubleBakingEvidence
  :: (MonadIO m, MonadReader s m, HasNodeDataSource s, MonadError e m, AsCacheError e, PostgresRaw m, MonadMask m, PersistBackend m)
  => BlockHash -> ChainId -> OperationHash -> RawLevel -> RawLevel -> Priority -> NodeQueryT m ()
insertDoubleBakingEvidence blockHash chainId opHash blockLevel accusedLevel accusedPriority = do
  baker <- fmap _bakingRights_delegate $ nodeQueryIxBakingRights1 blockHash accusedLevel accusedPriority
  void [executeQ|
    insert into "Accusation" (hash, "blockHash", level, chain, baker, "occurredLevel", "isBake")
    values (?opHash, ?blockHash, ?blockLevel, ?chainId, ?baker, ?accusedLevel, true)
    on conflict do nothing
    |]

insertDoubleEndorsementEvidence
  :: (MonadIO m, PostgresRaw m)
  => BlockHash -> ChainId -> OperationHash -> RawLevel -> RawLevel -> Signature -> ByteString -> Seq.Seq PublicKeyHash -> Seq.Seq PublicKey -> NodeQueryT m ()
insertDoubleEndorsementEvidence blockHash chainId opHash blockLevel accusedLevel sig encodedOp1 possibles possiblesKeys = do
  let actuals = Seq.filter (\(_,key) -> Sig.check key sig encodedOp1) $ Seq.zip possibles possiblesKeys
  for_ actuals $ \(baker,_) -> [executeQ|
    insert into "Accusation" (hash, "blockHash", level, chain, baker, "occurredLevel", "isBake")
    values (?opHash, ?blockHash, ?blockLevel, ?chainId, ?baker, ?accusedLevel, false)
    on conflict do nothing
    |]

loadPossibles
  :: (MonadIO m, MonadReader s m, HasNodeDataSource s, MonadError e m, AsCacheError e, PostgresRaw m, MonadMask m, PersistBackend m)
  => BlockHash -> RawLevel -> NodeQueryT m (Seq.Seq PublicKeyHash, Seq.Seq PublicKey)
loadPossibles blockHash accusedLevel = do
  possibles <- (fmap.fmap) _endorsingRights_delegate $ nodeQueryIx $ NodeQueryIx_EndorsingRights blockHash accusedLevel
  possiblesKeys <- traverse (nodeQueryDataSourceSafe . NodeQuery_PublicKey . Implicit) possibles
  pure (possibles, possiblesKeys)
