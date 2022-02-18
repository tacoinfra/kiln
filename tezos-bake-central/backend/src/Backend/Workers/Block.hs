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

import Control.Monad.Catch (MonadMask, throwM, try)
import Data.ByteString as BS (ByteString)
import Data.Either.Combinators (whenLeft, whenRight)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set (singleton)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Database.Groundhog.Core (PersistBackend)
import Database.Groundhog.Postgresql (Postgresql(..))
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, executeQ, fromOnly, queryQ)
import Rhyolite.Backend.Logging (runLoggingEnv)
import Safe (headMay)

import Tezos.Common.Binary as TBin
import Tezos.NodeRPC
import Tezos.Types
import qualified Tezos.V012.Types as V012
import qualified Tezos.V010.Types as V010
import qualified Tezos.V005.Types as V005
import Tezos.Signature.Verify as Sig

import Backend.Common (workerWithDelay)
import Backend.Config (AppConfig (..))
import Backend.IndexQueries (getLatestProtocolConstants)
import Backend.NodeRPC
import Common.Schema
import ExtraPrelude

blockWorker
  :: NominalDiffTime -- delay between checking for updates
  -> NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
blockWorker delay nds _appConfig db = workerWithDelay "blockWorker" (pure delay) $ const $ runLoggingEnv (_nodeDataSource_logger nds) $ do
  headBlockOrErr <- flip runReaderT nds $ runExceptT @KilnRpcError $ runNodeQueryT $ fmap fst getLatestProtocolConstants
  whenRight headBlockOrErr $ \headBlock -> do
    now <- liftIO getCurrentTime
    let headBlockTime = headBlock ^. timestamp
        maxTimeDiff = fromInteger 600 -- 10 minutes, picked somewhat arbitrarily
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
      (mbLargestParsedLvl :: Maybe RawLevel) <- fmap (headMay . fmap fromOnly) $ runDb (Identity db) [queryQ|
        select "level" from "AccusationBlock" where "chain" = ?chainId order by "level" desc limit 1
      |]
      let blockQueryLength = min historyLength $ headBlockLevel - fromMaybe 0 mbLargestParsedLvl
      blocksOrErr <- if blockQueryLength > 0
        then flip runReaderT nds $ runExceptT @KilnRpcError $ runNodeQueryT $ nodeQueryDataSourceSafe $ NodeQuery_Blocks headBlockHash blockQueryLength
        else return $ Right mempty

      whenRight blocksOrErr $ \blocks -> do
        -- note: we want to clear old entries first because the loop just below
        -- may be interrupted before finishing
        void $ runDb (Identity db) [executeQ|
          delete from "AccusationBlock" where "level" < ?cutoffLevel and "chain" = ?chainId;
        |]

        -- Parse blocks from oldest to newest, so that we always keep the
        -- invariance that 'mbLargestParsedLvl' is always older than any block
        -- that still needs to be handled.
        -- Note that the loop runs in 'ExceptT' so no computation will follow
        -- the first one throwing an error/'Left'.
        loopResult <- try $ flip runReaderT nds $
          for_ (Seq.reverse blocks) $ \blockHash -> do
            blockOrErr <- runExceptT @KilnRpcError $ runNodeQueryT $ do
              block <- nodeQueryDataSourceSafe $ NodeQuery_Block blockHash
              blockCrossData
                (insertAccusationsV12 blockHash chainId)
                (blockCrossCata (insertAccusationsV9 blockHash chainId) (insertAccusationsV5 blockHash chainId))
                block
              return block
            case blockOrErr of
              Right block -> do
                let blockLevel = block ^. level
                void $ runDb (Identity db) [executeQ|
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

-- TODO: This could use a better abstraction here.
insertAccusationsV12
  :: ( MonadIO m, MonadReader s m, HasNodeDataSource s, MonadError e m, AsKilnRpcError e
     , PostgresRaw m, MonadMask m, PersistBackend m
     )
  => BlockHash -> ChainId -> V012.Block -> NodeQueryT m ()
insertAccusationsV12 blockHash chainId block = do
  -- Operations into a block are divided into 4 subsections.  Accusations
  -- are always in the third of these sections.
  let mightBeAccusations = fold $ Seq.lookup 2 $ V012._block_operations block
  for_ mightBeAccusations $ \op -> do
    let
      opHash = V012._operation_hash op
      blockLevel = block ^. level
    for_ (V012._operation_contents op) $ \case
      -- TODO [#112]: handle double preendorsement
      V012.OperationContents_DoubleBakingEvidence ev -> do
        round' <- nodeQueryDataSourceSafe $ NodeQuery_Round blockHash
        let
          accusedLevel = ev ^. V012.operationContentsDoubleBakingEvidence_bh1 . V012.blockHeaderFull_level
          accusedPriority = fromIntegral round'
        insertDoubleBakingEvidence blockHash chainId opHash blockLevel accusedLevel accusedPriority
      V012.OperationContents_DoubleEndorsementEvidence ev -> do
        let
          accusedLevel = ev ^. V012.operationContentsDoubleEndorsementEvidence_op1 . V012.inlinedEndorsement_operations . V012.endorsementMempoolContents_level
        (possibles,possiblesKeys) <- loadPossibles blockHash accusedLevel
        let
          encodedOp1 = TBin.encode $ V012.Envelope_Endorsement chainId $ V012.outlineEndorsement $ ev ^. V012.operationContentsDoubleEndorsementEvidence_op1
          sig = fromMaybe (error "inlined endorsements in double endorsement evidence are always signed") $ ev ^. V012.operationContentsDoubleEndorsementEvidence_op1 . V012.inlinedEndorsement_signature
        insertDoubleEndorsementEvidence blockHash chainId opHash blockLevel accusedLevel sig encodedOp1 possibles possiblesKeys
      _ -> return ()

insertAccusationsV9
  :: ( MonadIO m, MonadReader s m, HasNodeDataSource s, MonadError e m, AsKilnRpcError e
     , PostgresRaw m, MonadMask m, PersistBackend m
     )
  => BlockHash -> ChainId -> V010.Block -> NodeQueryT m ()
insertAccusationsV9 blockHash chainId block = do
  -- Operations into a block are divided into 4 subsections.  Accusations
  -- are always in the third of these sections.
  let mightBeAccusations = fold $ Seq.lookup 2 $ V010._block_operations block
  for_ mightBeAccusations $ \op -> do
    let
      opHash = V010._operation_hash op
      blockLevel = block ^. level
    for_ (V010._operation_contents op) $ \case
      V010.OperationContents_DoubleBakingEvidence ev -> do
        let
          accusedLevel = ev ^. V010.operationContentsDoubleBakingEvidence_bh1 . V010.blockHeaderFull_level
          accusedPriority = ev ^. V010.operationContentsDoubleBakingEvidence_bh1 . V010.blockHeaderFull_priority
        insertDoubleBakingEvidence blockHash chainId opHash blockLevel accusedLevel accusedPriority
      V010.OperationContents_DoubleEndorsementEvidence ev -> do
        let
          accusedLevel = ev ^. V010.operationContentsDoubleEndorsementEvidence_op1 . V010.inlinedEndorsement_operations . V010.inlinedEndorsementContents_level
        (possibles,possiblesKeys) <- loadPossibles blockHash accusedLevel
        let
          encodedOp1 = TBin.encode $ V010.Envelope_Endorsement chainId $ V010.outlineEndorsement $ ev ^. V010.operationContentsDoubleEndorsementEvidence_op1
          sig = fromMaybe (error "inlined endorsements in double endorsement evidence are always signed") $ ev ^. V010.operationContentsDoubleEndorsementEvidence_op1 . V010.inlinedEndorsement_signature
        insertDoubleEndorsementEvidence blockHash chainId opHash blockLevel accusedLevel sig encodedOp1 possibles possiblesKeys
      _ -> return ()

insertAccusationsV5
  :: ( MonadIO m, MonadReader s m, HasNodeDataSource s, MonadError e m, AsKilnRpcError e
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
      blockLevel = block ^. V005.level
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
  :: (MonadIO m, MonadReader s m, HasNodeDataSource s, MonadError e m, AsKilnRpcError e, PostgresRaw m, MonadMask m, PersistBackend m)
  => BlockHash -> ChainId -> OperationHash -> RawLevel -> RawLevel -> Priority -> NodeQueryT m ()
insertDoubleBakingEvidence blockHash chainId opHash blockLevel accusedLevel accusedPriority = do
  baker <- fmap (view bakingRightsCrossCompat_delegate) $ nodeQueryIxBakingRights1 blockHash accusedLevel accusedPriority
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
  :: (MonadIO m, MonadReader s m, HasNodeDataSource s, MonadError e m, AsKilnRpcError e, PostgresRaw m, MonadMask m, PersistBackend m)
  => BlockHash -> RawLevel -> NodeQueryT m (Seq.Seq PublicKeyHash, Seq.Seq PublicKey)
loadPossibles blockHash accusedLevel = do
  possibles <- fmap (mconcat . toList) $
    (fmap.fmap) (view endorsingRightsCrossCompat_delegates) $ nodeQueryIx $ NodeQueryIx_EndorsingRights blockHash (Set.singleton accusedLevel)
  possiblesKeys <- traverse (nodeQueryDataSourceSafe . NodeQuery_PublicKey . Implicit) possibles
  pure (possibles, possiblesKeys)
