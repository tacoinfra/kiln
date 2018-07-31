{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- TODO: move this to ~lib?
module Backend.CachedNodeRPC where

import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Dependent.Map (DMap)
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Sequence (Seq)
import Say (say, sayErr, sayShow)
import qualified Data.Dependent.Map as DMap
import qualified Data.Set as Set

import Tezos.Lenses
import Tezos.NodeRPC
import Tezos.Types

data NodeQuery a where
  NodeQuery_GenesisParameters :: ChainId -> NodeQuery ProtoInfo
  NodeQuery_BakingRights :: ChainId -> BlockHash -> RawLevel -> NodeQuery (Seq BakingRights)
  NodeQuery_Baker :: ChainId -> BlockHash -> RawLevel -> NodeQuery (PublicKeyHash, Priority)

nodeQueryDataSourceCached
  :: (MonadIO m, MonadReader s m, HasNodeRPC s, MonadError RpcError m)
  => IORef (DMap NodeQuery Identity) -> NodeQuery a -> m a
nodeQueryDataSourceCached cacheRef q = do
  cache <- liftIO $ readIORef cacheRef
  case DMap.lookup q cache of
    Just (Identity a) -> pure a
    Nothing -> do
      res <- nodeQueryDataSource (nodeQueryDataSourceCached cacheRef) q
      liftIO $ atomicModifyIORef' cacheRef $ \oldMap ->
        (DMap.insert q (Identity res) oldMap, ())
      pure res

nodeQueryDataSource
  :: (MonadIO m, MonadReader s m, HasNodeRPC s, MonadError RpcError m)
  => (forall a. NodeQuery a -> m a) -> NodeQuery a -> m a
nodeQueryDataSource self = \case
  NodeQuery_GenesisParameters chainId -> do
    currentHead <- nodeRPC $ RBlock $ headId' chainId
    let headLevel = currentHead ^. block_header . blockHeader_level
    --TODO: if headLevel == 0 then error "error"
    nodeRPC $ RProtoConstants $ blockHashIdPred' chainId (_block_hash currentHead) (headLevel - 1)

  NodeQuery_BakingRights chainId branch targetLevel -> do
    proto <- self $ NodeQuery_GenesisParameters chainId
    let
      cycleForLevel n = Cycle $ unRawLevel $ n `div` _protoInfo_blocksPerCycle proto
      cycleDeterminingRightsForLevel n = max 0 $ cycleForLevel n - _protoInfo_preservedCycles proto

      cycleToQuery = Set.singleton $ Right $ cycleDeterminingRightsForLevel targetLevel

    branchBlock <- nodeRPC $ RBlock $ blockHashId' chainId branch
    let
      blockLevel = branchBlock ^. block_metadata . blockMetadata_level
      cyclesAgo = blockLevel ^. level_cycle - cycleDeterminingRightsForLevel targetLevel
      levelsAgo = RawLevel (unCycle cyclesAgo) * _protoInfo_blocksPerCycle proto - blockLevel ^. level_cyclePosition

    if
      | levelsAgo < 0 -> error "request for future stake"
      | levelsAgo == 0 ->
        nodeRPC $ RBakingRights (blockHashId' chainId branch) cycleToQuery
      | otherwise -> do
        targetBlock <- nodeRPC $ RBlock $ blockHashIdPred' chainId branch levelsAgo
        self $ NodeQuery_BakingRights chainId
          (targetBlock ^. block_hash)
          (targetLevel `div` _protoInfo_blocksPerCycle proto * _protoInfo_blocksPerCycle proto)

  NodeQuery_Baker chainId branch rawLevel -> do
    branchBlock <- nodeRPC $ RBlock $ blockHashId' chainId branch
    let levelsAgo = branchBlock ^. block_header . blockHeader_level - rawLevel
    targetBlock <- nodeRPC $ RBlock $ blockHashIdPred' chainId branch levelsAgo
    pure ( targetBlock ^. block_metadata . blockMetadata_baker
         , targetBlock ^. block_header . blockHeader_priority
         )

deriveGEq ''NodeQuery
deriveGCompare ''NodeQuery
