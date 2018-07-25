{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Tezos.NodeRPC.Types where

import Data.Int
import Data.Semigroup
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable
import Data.Word
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Tezos.Types
import Tezos.Json (deriveTezosJson)

data NodeRPCRequest a where
  RComplete :: BlockPrefix -> NodeRPCRequest [BlockHash]
  RBlock :: BlockId -> NodeRPCRequest Block
  RBlocks :: DynamicParamChainId -> RawLevel -> Set BlockHash -> NodeRPCRequest (Map BlockHash (Seq BlockHash)) -- the predecessors of the requested block.
  RProtoConstants :: BlockId -> NodeRPCRequest ProtoInfo
  RContract :: BlockId -> PublicKeyHash -> NodeRPCRequest Account
  RConnections :: NodeRPCRequest Word64 -- just a count for now, but there's more data there we may someday be interested in

  -- This only produces results when the cycles requested are between within
  -- PRESERVED_CYCLES of the BlockId requested. for older data, use an older block as context
  RBakingRights :: BlockId -> Set (Either RawLevel Cycle) -> NodeRPCRequest (Seq BakingRights)
  REndorsingRights :: BlockId -> Set (Either RawLevel Cycle) -> NodeRPCRequest (Seq EndorsingRights)
  RNetworkStat :: NodeRPCRequest NetworkStat

  RMonitorHeads :: (RpcResponse MonitorBlock -> IO ()) -> DynamicParamChainId -> NodeRPCRequest (IO ())

type RpcResponse = Either RpcError
data RpcError
  = RpcError_HttpException Text
  | RpcError_UnexpectedStatus Int BS.ByteString
  | RpcError_NonJSON String LBS.ByteString
  deriving (Eq, Ord, Show, Typeable)


-- RPC "dynamic parameter"
data BlockId = BlockId
  { _blockId_chainId :: DynamicParamChainId
  , _blockId_blockHash :: DynamicParamBlockHash
  , _blockId_predecessor :: Maybe RawLevel --  Number predecessors prior to block
  }
  deriving (Eq, Ord, Show, Typeable)

data DynamicParamChainId
  = DynamicParamChainId_ChainId ChainId
  | DynamicParamChainId_Main
  | DynamicParamChainId_Test
  deriving (Eq, Ord, Show, Typeable)

data DynamicParamBlockHash
  = DynamicParamBlockHash_BlockHash BlockHash
  | DynamicParamBlockHash_Genesis
  | DynamicParamBlockHash_Head
  | DynamicParamBlockHash_TestHead
  deriving (Eq, Ord, Show, Typeable)

chainIdToUrl :: DynamicParamChainId -> Text
chainIdToUrl chainId = case chainId of
  DynamicParamChainId_ChainId x -> toBase58Text x
  DynamicParamChainId_Main -> "main"
  DynamicParamChainId_Test -> "test"

blockIdToUrl :: BlockId -> Text
blockIdToUrl (BlockId chainId blockId offset) = "/chains/" <> chainIdToUrl chainId <> "/blocks/" <> blockId' <> offset'
  where
    blockId' = case blockId of
      DynamicParamBlockHash_BlockHash x -> toBase58Text x
      DynamicParamBlockHash_Genesis -> "genesis"
      DynamicParamBlockHash_Head -> "head"
      DynamicParamBlockHash_TestHead -> "test_head"
    offset' = maybe "" (("~" <>) . T.pack . show . unRawLevel) offset


data NetworkStat = NetworkStat
  { _networkStat_totalSent :: TezosWord64 -- bytes
  , _networkStat_totalRecv :: TezosWord64 -- bytes
  , _networkStat_currentInflow :: Int32 -- bytes/s
  , _networkStat_currentOutflow :: Int32 -- bytes/s
  } deriving (Eq, Ord, Show, Typeable)

newtype BlockPrefix = BlockPrefix Text
  deriving (Eq, Show, Typeable)

-- Smart constructors for "dynamic" url patterns in NodeRPC
blockHashId :: BlockHash -> BlockId
blockHashId x = BlockId DynamicParamChainId_Main (DynamicParamBlockHash_BlockHash x) Nothing

blockHashId' :: ChainId -> BlockHash -> BlockId
blockHashId' chain x = BlockId (DynamicParamChainId_ChainId chain) (DynamicParamBlockHash_BlockHash x) Nothing

blockHashIdPred :: BlockHash -> RawLevel -> BlockId
blockHashIdPred x = BlockId DynamicParamChainId_Main (DynamicParamBlockHash_BlockHash x) . Just

blockHashIdPred' :: ChainId -> BlockHash -> RawLevel -> BlockId
blockHashIdPred' chain x = BlockId (DynamicParamChainId_ChainId chain) (DynamicParamBlockHash_BlockHash x) . Just

genesisId :: BlockId
genesisId = BlockId DynamicParamChainId_Main DynamicParamBlockHash_Genesis Nothing

genesisId' :: ChainId -> BlockId
genesisId' chain = BlockId (DynamicParamChainId_ChainId chain) DynamicParamBlockHash_Genesis Nothing

chainHeadId :: ChainId -> BlockId
chainHeadId chain = BlockId (DynamicParamChainId_ChainId chain) DynamicParamBlockHash_Head Nothing

headId :: BlockId
headId = BlockId DynamicParamChainId_Main DynamicParamBlockHash_Head Nothing

headId' :: ChainId -> BlockId
headId' chain = BlockId (DynamicParamChainId_ChainId chain) DynamicParamBlockHash_Head Nothing

testHeadId :: BlockId
testHeadId = BlockId DynamicParamChainId_Main DynamicParamBlockHash_TestHead Nothing

concat <$> traverse deriveTezosJson
  [ ''NetworkStat
  ]
