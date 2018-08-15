{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Tezos.NodeRPC.Class where

import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Word (Word64)

import Tezos.NodeRPC.Types (NetworkStat, RpcResponse)
import Tezos.Types


class QueryChain repr where
  rChain :: repr ChainId

class QueryBlock repr where -- tzscan
  type BlockType repr
  rHead :: ChainId -> repr (BlockType repr)
  rBlock :: ChainId -> BlockHash -> repr (BlockType repr)

class QueryHistory repr where -- blockscale
  rBlocks :: ChainId -> RawLevel -> Set BlockHash -> repr (Map BlockHash (Seq BlockHash)) -- the predecessors of the requested block.
  rBlockPred :: ChainId -> BlockHash -> RawLevel -> repr (BlockType repr)

  rProtoConstants :: ChainId -> BlockHash -> repr ProtoInfo
  rContract :: ChainId -> BlockHash -> ContractId -> repr Account

  -- This only produces results when the cycles requested are between within
  -- PRESERVED_CYCLES of the BlockId requested. for older data, use an older block as context
  rBakingRights :: ChainId -> BlockHash -> Set (Either RawLevel Cycle) -> repr (Seq BakingRights)
  rEndorsingRights :: ChainId -> BlockHash -> Set (Either RawLevel Cycle) -> repr (Seq EndorsingRights)

class QueryNode repr where -- my node
  rConnections :: repr Word64 -- just a count for now, but there's more data there we may someday be interested in
  rNetworkStat :: repr NetworkStat

class MonitorHeads repr where
  rMonitorHeads :: ChainId -> (RpcResponse MonitorBlock -> IO ()) -> repr (IO ())
