{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Tezos.NodeRPC.Helpers where

import Control.Lens (each, folded, uncons)
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Dependent.Sum (DSum (..))
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import Data.Word (Word64)

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Encoding (emptyObject_)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Types.Method as Http (Method, methodGet, methodPost)

import Tezos.Common.Chain
import Tezos.Common.NetworkStat (NetworkStat)
import Tezos.Common.NodeRPC.Types
import Tezos.NodeRPC.Network (HasNodeRPC, nodeRPC)
import Tezos.V005.Micheline
import Tezos.V005.Michelson
import Tezos.V005.NodeRPC.Class
import Tezos.V004.Operation (OperationWithMetadata)
import qualified Tezos.V005.Types as V005
import qualified Tezos.V005.ProtocolConstants as V005
import qualified Tezos.V005.NodeRPC.CrossCompat as V005

dryRunEndpoint
  :: (MonadIO m
    , MonadError e m
    , AsRpcError e
    , MonadReader s m
    , HasNodeRPC s
    , MonadLogger m
    , ToMicheline a)
  => V005.ChainId
  -> V005.BlockHash
  -> V005.ContractId
  -> V005.ContractId
  -> Text
  -> a
  -> m OperationWithMetadata
dryRunEndpoint chain block account contract endpoint argument = do
  error "dryRunEndpoint not implemented for Babylon"
  {--
  protocolConstants <- nodeRPC $ rProtoConstants chain block
  let gas_max = V005._protoInfo_hardGasLimitPerOperation protocolConstants
  let storage_max = V005._protoInfo_hardStorageLimitPerOperation protocolConstants
  callingAccount <- nodeRPC $ rContract account ChainTag_Main block
  let counter = succ $ case callingAccount of
         V005.AccountV004 acc -> V004._account_counter acc
         V005.AccountV005 acc -> fromMaybe 0 $ V005._account_counter acc
  contractAccount <- nodeRPC $ rContract contract ChainTag_Main block
  let script = case contractAccount of
        V005.AccountV004 acc -> V004._account_script acc
        V005.AccountV005 acc -> V005._account_script acc
  case flip (wrapEndpointCall endpoint) (toMicheline argument) =<< script of
    Nothing -> error "Could not find endpoint"
    Just contractParameter -> do
      let opTransfer = V005.OpContentsTransaction 0 contract $ Just contractParameter
          opContents = V005.OpContentsList_Single $ V005.OpContents_Transaction $ V005.OpContentsManager account 10 counter gas_max storage_max opTransfer
      -- This needs to fit the format of a valid signature, but is never looked at past that.
          dummySignature = Just "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q"
          op = (V005.OpsKindTag_Single (V005.OpKindTag_Manager V005.OpKindManagerTag_Transaction)) :=> V005.Op { V005._op_branch = block, V005._op_contents = opContents, V005._op_signature = dummySignature }
      nodeRPC $ rRunOperation chain block op
  --}


callViewEndpoint
  :: ( MonadIO m
     , MonadError e m
     , AsRpcError e
     , MonadReader s m
     , HasNodeRPC s
     , MonadLogger m
     , ToMicheline a
     , FromMicheline b)
  => V005.ChainId
  -> V005.BlockHash
  -> V005.ContractId
  -> V005.ContractId
  -> V005.ContractId
  -> Text
  -> a
  -> m (Either String b)
callViewEndpoint chain block account contract tgtContract endpoint argument = do
  error "callViewEndpoint not implemented for Babylon"
  {--
  let finalArgument = Pair (toMicheline argument) $ toMicheline $ tgtContract
  result <- dryRunEndpoint chain block account contract endpoint finalArgument
  case result ^? valueFromResult of
    Just expr -> return $ fromMicheline expr
    _ -> error "Couldn't read operation"
  where
    valueFromResult
      = operationWithMetadata_contents
      . traverse
      . _OperationContents_Transaction
      . operationContentsTransaction_metadata
      . managerOperationMetadata_internalOperationResults
      . _Just
      . traverse
      . _InternalOperationResult_Transaction
      . internalOperationContentsTransaction_result
      . operationResultTransaction_storage
      . _Just
  --}
