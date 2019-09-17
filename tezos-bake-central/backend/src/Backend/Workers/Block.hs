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

import qualified Data.Aeson.Text as Aeson
import Control.Lens ((^..))
import Control.Monad.Logger (LoggingT, logDebug, logErrorSH, logDebugSH)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Sequence as Seq
import Data.String.Here.Interpolated (i)
import Data.Text.Lazy (toStrict)
import Data.Time (NominalDiffTime)
import Database.Groundhog.Postgresql (Postgresql, insert_, insert)
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
  workerWithDelay (pure delay) $ const $ (runLoggingEnv :: LoggingEnv -> LoggingT IO () -> IO ()) (_nodeDataSource_logger nds) $ do
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

    for_ (queuedBlockOrNot ^.. _Right . traverse) $ \queuedBlock -> (either ($(logErrorSH) . \e -> ("blockWorker" :: Text,queuedBlock,e)) pure =<<) $ flip runReaderT nds $ runExceptT @CacheError $ runNodeQueryT $ do
      $(logDebug) $ "Scrape block " <> toBase58Text (_blockTodo_hash queuedBlock) <> "."
      couldBeBlock <- unliftEither $ nodeQueryDataSourceSafe $ NodeQuery_Block (_blockTodo_hash queuedBlock)
      case couldBeBlock of
        Left (CacheError_RpcError (RpcError_UnexpectedStatus 404 _)) ->
          $(logDebugSH) ("blockWorker"::Text,"Error (404) in retrieving block from available nodes"::Text,toBase58Text (_blockTodo_hash queuedBlock))
        Left CacheError_NoSuitableNode ->
          $(logDebugSH) ("blockWorker"::Text,"No suitable node to obtain block:"::Text,toBase58Text (_blockTodo_hash queuedBlock))
        Left e -> nqThrowError e
        Right block -> do
          let blockHash = _block_hash block

          let parentHash = block ^. predecessor
              parentLevel = block ^. level - 1
           in void [executeQ|
                insert into "BlockTodo" (hash, level, chain, "claimedBy", "claimedAt", "parsedParent", "parsedAccusations")
                values (?parentHash, ?parentLevel, ?chainId, null, null, false, false)
                on conflict do nothing
                |]
          -- Operations into a block are divided into 4 subsections.  Accusations
          -- are always in the third of these sections.
          let mightBeAccusations = fold $ Seq.lookup 2 $ _block_operations block
          for_ mightBeAccusations $ \op -> do
            let
              opHash = _operation_hash op
              blockLevel = block ^. level
            for_ (_operation_contents op) $ \case
              OperationContents_DoubleBakingEvidence ev -> do
                let
                  accusedLevel = ev ^. operationContentsDoubleBakingEvidence_bh1 . blockHeaderFull_level
                  accusedPriority = ev ^. operationContentsDoubleBakingEvidence_bh1 . blockHeaderFull_priority
                baker <- fmap _bakingRights_delegate $ nodeQueryIxBakingRights1 blockHash accusedLevel accusedPriority
                void [executeQ|
                  insert into "Accusation" (hash, "blockHash", level, chain, baker, "occurredLevel", "isBake")
                  values (?opHash, ?blockHash, ?blockLevel, ?chainId, ?baker, ?accusedLevel, true)
                  on conflict do nothing
                  |]
              OperationContents_DoubleEndorsementEvidence ev -> do
                let
                  accusedLevel = ev ^. operationContentsDoubleEndorsementEvidence_op1 . inlinedEndorsement_operations . inlinedEndorsementContents_level
                possibles <- (fmap.fmap) _endorsingRights_delegate $ nodeQueryIx $ NodeQueryIx_EndorsingRights blockHash accusedLevel
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

          void [executeQ|
            update "BlockTodo"
            set "claimedBy" = null,
                "claimedAt" = null,
                "parsedParent" = true,
                "parsedAccusations" = true
            where chain = ?chainId and hash = ?blockHash
            |]

indexerBlockWorker
  :: NominalDiffTime -- delay between checking for updates
  -> NodeDataSource
  -> IO (IO ())
indexerBlockWorker delay nds = runLoggingEnv (_nodeDataSource_logger nds) $ do
  let chainId = _nodeDataSource_chain nds
  let claimTimeout = "15 seconds" :: Text
  workerWithDelay (pure delay) $ const $ (runLoggingEnv :: LoggingEnv -> LoggingT IO () -> IO ()) (_nodeDataSource_logger nds) $ do
    queuedBlockOrNot :: Either CacheError [IxBlockTodo] <- flip runReaderT nds $ runExceptT $ runNodeQueryT $ do
      let cutoffLevel = (2 :: RawLevel)
      [queryQ|
        update "IxBlockTodo"
        set "claimedBy" = 1
          , "claimedAt" = now()
        where hash = (
          select hash
          from "IxBlockTodo"
          where (not "parsedParent") and chain = ?chainId and ("claimedBy" is null or "claimedAt" < now() - interval ?claimTimeout) and level >= ?cutoffLevel
          order by level desc
          limit 1
          for update skip locked
          )
        returning hash, level, chain, "claimedBy", "claimedAt" at time zone 'UTC', "parsedParent"
        |] <&> fmap (\(a,b,c,d,e,f) -> IxBlockTodo a b c d e f)
    -- c.f. https://blog.2ndquadrant.com/what-is-select-skip-locked-for-in-postgresql-9-5/
    -- Note that we don't actually do a very long-running transaction here,
    -- since we're doing something idempotent and it's okay for backends to
    -- step on each other as long as it's rare, so it's better to let
    -- leases on work items time out and let other backends just steal them,
    -- rather than making postgres the central arbiter of locking.

    for_ (queuedBlockOrNot ^.. _Right . traverse) $ \queuedBlock -> (either ($(logErrorSH) . \e -> ("ixBlockWorker" :: Text,queuedBlock,e)) pure =<<) $ flip runReaderT nds $ runExceptT @CacheError $ runNodeQueryT $ do
      $(logDebug) [i| "Scrape block ${toBase58Text (_ixBlockTodo_hash queuedBlock)}|]
      couldBeBlock <- unliftEither $ nodeQueryDataSourceSafe $ NodeQuery_Block (_ixBlockTodo_hash queuedBlock)
      case couldBeBlock of
        Left (CacheError_RpcError (RpcError_UnexpectedStatus 404 _)) ->
          $(logDebugSH) ("blockWorker"::Text,"Error (404) in retrieving block from available nodes"::Text,toBase58Text (_ixBlockTodo_hash queuedBlock))
        Left CacheError_NoSuitableNode ->
          $(logDebugSH) ("blockWorker"::Text,"No suitable node to obtain block:"::Text,toBase58Text (_ixBlockTodo_hash queuedBlock))
        Left e -> nqThrowError e
        Right block -> do
          let blockHash = _block_hash block

          let parentHash = block ^. predecessor
              parentLevel = block ^. level - 1
           in void [executeQ|
                insert into "IxBlockTodo" (hash, level, chain, "claimedBy", "claimedAt", "parsedParent")
                values (?parentHash, ?parentLevel, ?chainId, null, null, false)
                on conflict do nothing
                |]

          for_ (_block_operations block) $ traverse $ \op -> do
            let
              opHash = _operation_hash op
            opKinds <- for (_operation_contents op) $ \case
              OperationContents_Endorsement _ -> pure OpKindIx_Endorsement
              OperationContents_SeedNonceRevelation _ -> pure OpKindIx_SeedNonceRevelation
              OperationContents_DoubleEndorsementEvidence _ -> pure OpKindIx_DoubleEndorsementEvidence
              OperationContents_DoubleBakingEvidence _ -> pure OpKindIx_DoubleBakingEvidence
              OperationContents_ActivateAccount _ -> pure OpKindIx_ActivateAccount
              OperationContents_Proposals _ -> pure OpKindIx_Proposals
              OperationContents_Ballot _ -> pure OpKindIx_Ballot
              OperationContents_Reveal _ -> pure OpKindIx_Reveal
              OperationContents_Origination _ -> pure OpKindIx_Origination
              OperationContents_Delegation _ -> pure OpKindIx_Delegation
              OperationContents_Transaction t -> do
                let
                  params = fmap (toStrict . Aeson.encodeToLazyText)
                    (_operationContentsTransaction_parameters t)
                  txIx = TransactionIndex
                    { _transactionIndex_operation = opHash
                    , _transactionIndex_source = _operationContentsTransaction_source t
                    , _transactionIndex_destination = _operationContentsTransaction_destination t
                    , _transactionIndex_fee = _operationContentsTransaction_fee t
                    , _transactionIndex_counter = _operationContentsTransaction_counter t
                    , _transactionIndex_gasLimit = _operationContentsTransaction_gasLimit t
                    , _transactionIndex_storageLimit = _operationContentsTransaction_storageLimit t
                    , _transactionIndex_amount = _operationContentsTransaction_amount t
                    , _transactionIndex_parameters = params
                    }
                insert_ txIx
                pure OpKindIx_Transaction
            let
              opKind
                | elem OpKindIx_Transaction opKinds = OpKindIx_Transaction
                | Just o <- Seq.lookup 0 opKinds = o
                | otherwise = OpKindIx_Transaction
              opIx = OperationIndex
                { _operationIndex_hash = opHash
                , _operationIndex_chainId = _operation_chainId op
                , _operationIndex_branch = _block_hash block
                , _operationIndex_kind = opKind
                }
            insert opIx

          void [executeQ|
            update "IxBlockTodo"
            set "claimedBy" = null,
                "claimedAt" = null,
                "parsedParent" = true
            where chain = ?chainId and hash = ?blockHash
            |]
