{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Tezos.NodeRPC.Helpers where

import Control.Lens (uncons, folded, each)
import Control.Lens.Operators
import Control.Lens.Combinators
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Except
import Data.Dependent.Sum (DSum(..))
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
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Types.Method as Http (Method, methodGet, methodPost)

import Tezos.Chain (ChainTag(ChainTag_Hash))
import Tezos.NodeRPC.Types (NetworkStat)
import Tezos.NodeRPC
import Tezos.Operation
import Tezos.Types
import Tezos.Micheline
import Tezos.Michelson
import Tezos.Account

dryRunEndpoint
  :: (MonadIO m
    , MonadError e m
    , AsRpcError e
    , MonadReader s m
    , HasNodeRPC s
    , MonadLogger m
    , ToMicheline a)
  => ChainId
  -> BlockHash
  -> ContractId
  -> ContractId
  -> Text
  -> a
  -> m OperationWithMetadata
dryRunEndpoint chain block account contract endpoint argument = do
  protocolConstants <- nodeRPC $ rProtoConstants chain block
  let gas_max = _protoInfo_hardGasLimitPerOperation protocolConstants
  let storage_max = _protoInfo_hardStorageLimitPerOperation protocolConstants
  callingAccount <- nodeRPC $ rContract account ChainTag_Main block
  let counter = succ $ _account_counter callingAccount
  contractAccount <- nodeRPC $ rContract contract ChainTag_Main block
  case flip (wrapEndpointCall endpoint) (toMicheline argument) =<< _account_script contractAccount of
    Nothing -> error "Could not find endpoint"
    Just contractParameter -> do
      let opTransfer = OpContentsTransaction 0 contract $ Just contractParameter
          opContents = OpContentsList_Single $ OpContents_Transaction $ OpContentsManager account 10 counter gas_max storage_max opTransfer
      -- This needs to fit the format of a valid signature, but is never looked at past that.
          dummySignature = Just "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q"
          op = (OpsKindTag_Single (OpKindTag_Manager OpKindManagerTag_Transaction)) :=> Op { _op_branch = block, _op_contents = opContents, _op_signature = dummySignature }
      nodeRPC $ rRunOperation chain block op


callViewEndpoint
  :: ( MonadIO m
     , MonadError e m
     , AsRpcError e
     , MonadReader s m
     , HasNodeRPC s
     , MonadLogger m
     , ToMicheline a
     , FromMicheline b)
  => ChainId
  -> BlockHash
  -> ContractId
  -> ContractId
  -> ContractId
  -> Text
  -> a
  -> m (Either String b)
callViewEndpoint chain block account contract tgtContract endpoint argument = do
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
