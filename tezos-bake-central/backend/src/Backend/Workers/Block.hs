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

import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger (logErrorSH, logDebugSH)
import Data.ByteString as BS
import Data.Either.Combinators (whenRight)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set (fromList, notMember, singleton)
import Data.Time (NominalDiffTime)
import Database.Groundhog.Core (PersistBackend)
import Database.Groundhog.Postgresql (Postgresql(..))
import Rhyolite.Backend.DB (runDb)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, executeQ, fromOnly, queryQ)
import Rhyolite.Backend.Logging (runLoggingEnv)

import Tezos.Common.Binary as TBin
import Tezos.NodeRPC
import Tezos.Types
import qualified Tezos.V010.Types as V010
import qualified Tezos.V005.Types as V005
import Tezos.Signature.Verify as Sig

import Backend.CachedNodeRPC
import Backend.Common (workerWithDelay)
import Backend.Config (AppConfig (..))
import Backend.IndexQueries (getLatestProtocolConstants)
import Common.Schema
import ExtraPrelude

blockWorker
  :: NominalDiffTime -- delay between checking for updates
  -> NodeDataSource
  -> AppConfig
  -> Pool Postgresql
  -> IO (IO ())
blockWorker delay nds _appConfig db = workerWithDelay "blockWorker" (pure delay) $ const $ runLoggingEnv (_nodeDataSource_logger nds) $ do
  headBlockOrErr <- flip runReaderT nds $ runExceptT @CacheError $ runNodeQueryT $ fmap fst getLatestProtocolConstants
  whenRight headBlockOrErr $ \headBlock -> do
    let historyLength = 720
        headBlockLevel = headBlock ^. level
        headBlockHash = headBlock ^. hash
        cutoffLevel = headBlockLevel - historyLength
        chainId = _nodeDataSource_chain nds
    blocksOrErr <- flip runReaderT nds $ runExceptT @CacheError $ runNodeQueryT $ nodeQueryDataSourceSafe $ NodeQuery_Blocks headBlockHash historyLength
    (parsedAccusationBlocks :: Set BlockHash) <- fmap (Set.fromList . fmap fromOnly) $ runDb (Identity db) [queryQ|
        select "hash" from "AccusationBlock" where "level" between ?cutoffLevel and ?headBlockLevel and "chain" = ?chainId
      |]
    whenRight blocksOrErr $ \blocks -> for_ blocks $ \blockHash -> do
      couldBeBlock <- flip runReaderT nds $ runExceptT @CacheError $ runNodeQueryT $ nodeQueryDataSourceSafe $ NodeQuery_Block blockHash
      case couldBeBlock of
        Left (CacheError_RpcError (RpcError_UnexpectedStatus _url 404 _)) ->
          $(logDebugSH) ("blockWorker" :: Text, "Error (404) in retrieving block from available nodes" :: Text, toBase58Text blockHash)
        Left (CacheError_NoSuitableNode q reasons) ->
          $(logDebugSH) ("blockWorker" :: Text, noSuitableNodeLogMessage q reasons)
        Left CacheError_NotEnoughHistory -> $(logDebugSH) ("blockWorker" :: Text, "Not enough history" :: Text, toBase58Text blockHash)
        Left e -> $(logErrorSH) $ cacheErrorLogMessage "blockWorker" e
        Right block -> when (Set.notMember blockHash parsedAccusationBlocks) $ do
          let blockLevel = block ^. level
          void $ flip runReaderT nds $ runExceptT @CacheError $ runNodeQueryT $
            blockCrossCata (insertAccusationsV9 blockHash chainId) (insertAccusationsV5 blockHash chainId) block
          void $ runDb (Identity db) [executeQ|
            insert into "AccusationBlock" ("hash", "level", "chain")
            values (?blockHash, ?blockLevel, ?chainId)
          |]

    void $ runDb (Identity db) [executeQ|
      delete from "AccusationBlock" where "level" < ?cutoffLevel and "chain" = ?chainId;
    |]

-- TODO: This could use a better abstraction here.
insertAccusationsV9
  :: ( MonadIO m, MonadReader s m, HasNodeDataSource s, MonadError e m, AsCacheError e
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
  possibles <- (fmap.fmap) _endorsingRights_delegate $ nodeQueryIx $ NodeQueryIx_EndorsingRights blockHash (Set.singleton accusedLevel)
  possiblesKeys <- traverse (nodeQueryDataSourceSafe . NodeQuery_PublicKey . Implicit) possibles
  pure (possibles, possiblesKeys)
