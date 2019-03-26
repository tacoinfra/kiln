{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Tezos.NodeRPC.Class where

import Control.Lens (uncons)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Word (Word64)

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Types.Method as Http (Method, methodGet)

import Tezos.NodeRPC.Types (NetworkStat)
import Tezos.Types


class QueryChain repr where
  rChain :: repr ChainId

class QueryBlock repr where
  type BlockType repr
  rHead :: ChainId -> repr (BlockType repr)
  rBlock :: ChainId -> BlockHash -> repr (BlockType repr)

class QueryHistory repr where -- blockscale
  rBlocks :: ChainId -> RawLevel -> Set BlockHash -> repr (Map BlockHash (Seq BlockHash)) -- the predecessors of the requested block.
  rBlockPred :: RawLevel -> ChainId -> BlockHash -> repr (BlockType repr)

  rProtoConstants :: ChainId -> BlockHash -> repr ProtoInfo
  rAnyConstants :: ChainId -> repr ProtoInfo
  rContract :: ContractId -> ChainId -> BlockHash -> repr Account

  rBallots :: ChainId -> BlockHash -> repr Ballots
  rProposals :: ChainId -> BlockHash -> repr (Seq ProposalVotes)

  rManagerKey :: ContractId -> ChainId -> BlockHash -> repr ManagerKey

  -- This only produces results when the cycles requested are between within
  -- PRESERVED_CYCLES of the BlockId requested. for older data, use an older block as context
  rBakingRights :: Set (Either RawLevel Cycle) -> ChainId -> BlockHash -> repr (Seq BakingRights)
  rBakingRightsFull :: Set (Either RawLevel Cycle) -> Int -> ChainId -> BlockHash -> repr (Seq BakingRights)
  rEndorsingRights :: Set (Either RawLevel Cycle) -> ChainId -> BlockHash -> repr (Seq EndorsingRights)

  rDelegateInfo :: PublicKeyHash -> ChainId -> BlockHash -> repr DelegateInfo

class QueryNode repr where -- my node
  rConnections :: repr Word64 -- just a count for now, but there's more data there we may someday be interested in
  rNetworkStat :: repr NetworkStat

class MonitorHeads repr where
  rMonitorHeads :: ChainId -> repr MonitorBlock

data RpcQuery a = RpcQuery
  { _RpcQuery_decoder :: LBS.ByteString -> Either String a
  , _RpcQuery_method :: Http.Method
  , _RpcQuery_resource :: Text
  } deriving Functor

plainNodeRequest :: FromJSON a => Http.Method -> Text -> RpcQuery a
plainNodeRequest = RpcQuery Aeson.eitherDecode'

newtype PlainNode a = PlainNode (RpcQuery a)
newtype PlainNodeStream a = PlainNodeStream (RpcQuery a)

instance QueryChain RpcQuery where
  rChain = _block_chainId <$> plainNodeRequest Http.methodGet "/chains/main/blocks/head"

instance QueryBlock RpcQuery where
  type BlockType RpcQuery = Block
  --rComplete (BlockPrefix pfx) = RpcQuery $ nodeRPCImpl methodPost (blockIdToUrl headId <> "/complete/" <> pfx)
  rHead = chainAPI "/blocks/head"
  rBlock = blockAPI ""

instance QueryHistory RpcQuery where
  rBlockPred (RawLevel levelsBack) = blockAPI $ "~" <> T.pack (show levelsBack)
  rBlocks chainId (RawLevel len) heads = byHead <$> chainAPI ("/blocks?length=" <> T.pack (show len) <> foldMap blk2param heads) chainId
    where
      byHead :: [Seq BlockHash] -> Map.Map BlockHash (Seq BlockHash)
      byHead = foldMap $ maybe mempty (uncurry Map.singleton) . uncons
      blk2param :: BlockHash -> Text
      blk2param blkHash = "&head=" <> toBase58Text blkHash
  rProtoConstants = blockAPI "/context/constants"
  rAnyConstants = chainAPI "/blocks/head/context/constants"
  rContract contractId = blockAPI ("/context/contracts/" <> toContractIdText contractId)
  rBallots = blockAPI "/votes/ballots/"
  rProposals = blockAPI "/votes/proposals/"
  rManagerKey contractId = blockAPI ("/context/contracts/" <> toContractIdText contractId <> "/manager_key")
  rBakingRights params = blockAPI $ "/helpers/baking_rights"
      <> (if null params then "" else "?" <> T.intercalate "&" (dynamicParamRightsRangeToQueryArg <$> toList params))
  rBakingRightsFull levelishes prio = blockAPI $ "/helpers/baking_rights?all&"
      <> T.intercalate "&" (("max_priority=" <> T.pack (show prio)) : (dynamicParamRightsRangeToQueryArg <$> toList levelishes))
  rEndorsingRights params = blockAPI $ "/helpers/endorsing_rights"
      <> (if null params then "" else "?" <> T.intercalate "&" (dynamicParamRightsRangeToQueryArg <$> toList params))
  rDelegateInfo publicKeyHash = blockAPI ("/context/delegates/" <> toPublicKeyHashText publicKeyHash)

chainAPI :: FromJSON a => Text -> ChainId -> RpcQuery a
chainAPI path chainId = plainNodeRequest Http.methodGet $ "/chains/" <> toBase58Text chainId <> path

blockAPI :: FromJSON a => Text -> ChainId -> BlockHash -> RpcQuery a
blockAPI path chainId blockHash = plainNodeRequest Http.methodGet (chainBlockUrl chainId blockHash <> path)

instance QueryNode RpcQuery where
  rConnections = decoder <$> plainNodeRequest Http.methodGet "/network/connections"
    where
      decoder :: [Aeson.Value] -> Word64
      decoder = fromIntegral . length
  rNetworkStat = plainNodeRequest Http.methodGet "/network/stat"

instance MonitorHeads PlainNodeStream where
  rMonitorHeads chainId = PlainNodeStream $ plainNodeRequest Http.methodGet ("/monitor/heads/" <> toBase58Text chainId)

chainBlockUrl :: ChainId -> BlockHash -> Text
chainBlockUrl chainId blockHash = "/chains/" <> toBase58Text chainId <> "/blocks/" <> toBase58Text blockHash

dynamicParamRightsRangeToQueryArg :: Either RawLevel Cycle -> Text
dynamicParamRightsRangeToQueryArg = \case
  Left (RawLevel x) -> "level=" <> T.pack (show x)
  Right (Cycle x) -> "cycle=" <> T.pack (show x)
