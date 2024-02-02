{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
module Tezos.NodeRPC.Class where

import Control.Lens (uncons)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Encoding (emptyObject_)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Map (Map)
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Word (Word64)
import qualified Network.HTTP.Types.Method as Http (Method, methodGet, methodPost)

import qualified Data.Map as Map
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Tezos.Common.Base58Check (BlockHash(..), blockHashToBase58Text)
import Tezos.Common.Contract
import Tezos.Common.Level
import Tezos.Common.PublicKeyHash
import Tezos.CrossCompat.Account
import Tezos.CrossCompat.Block
import Tezos.CrossCompat.Vote
import Tezos.Oxford.Types

class QueryChain repr where
  rChain :: repr ChainId

type family ChainType (repr :: * -> *) :: *
type instance ChainType RpcQuery = ChainTag

-- | Datatype that represents possible block queries.
data BlockQuery
  = BlockQueryOffset (Either RawLevel BlockHash) RawLevel
  deriving (Show, Eq, Ord)

(~~) :: BlockHash -> RawLevel -> BlockQuery
(~~) blk offset = BlockQueryOffset (Right blk) offset

class ToBlockQuery a where
  toBlockQuery :: a -> BlockQuery

instance ToBlockQuery BlockHash where
  toBlockQuery blkHash = BlockQueryOffset (Right blkHash) 0

instance ToBlockQuery BlockQuery where
  toBlockQuery = id

instance ToBlockQuery RawLevel where
  toBlockQuery rawLevel = BlockQueryOffset (Left rawLevel) 0

class QueryBlock (repr :: * -> *) where
  type BlockType repr
  type BlockHeaderType repr
  rHead :: ChainType repr -> repr (BlockType repr)
  rBlock :: ChainType repr -> BlockQuery -> repr (BlockType repr)
  rBlockHeader :: ChainType repr -> BlockQuery -> repr (BlockHeaderType repr)

class QueryHistory repr where -- blockscale
  rBlocks :: ChainId -> RawLevel -> Set BlockHash -> repr (Map BlockHash (Seq BlockHash)) -- the predecessors of the requested block.

  rProtoConstants :: ChainId -> BlockQuery -> repr ProtoInfo
  rContract :: ContractId -> ChainType repr -> BlockQuery -> repr AccountCrossCompat

  rBallots :: ChainId -> BlockQuery -> repr BallotsCrossCompat
  rBallotList :: ChainId -> BlockQuery -> repr BallotList
  rListings :: ChainId -> BlockQuery -> repr VoterListingsCrossCompat
  rProposals :: ChainId -> BlockQuery -> repr ProposalVotesListCrossCompat
  rCurrentProposal :: ChainId -> BlockQuery -> repr (Maybe ProtocolHash)
  rCurrentQuorum :: ChainId -> BlockQuery -> repr Int
  rProposalVote :: ChainId -> BlockQuery -> PublicKeyHash -> repr (Set ProtocolHash)

  -- This only produces results when the cycles requested are between within
  -- PRESERVED_CYCLES of the BlockId requested. for older data, use an older block as context
  rBakingRights :: Either Cycle (Set RawLevel) -> ChainId -> BlockQuery -> repr (Seq BakingRightsCrossCompat)
  rBakingRightsFull :: Either Cycle (Set RawLevel) -> Round -> ChainId -> BlockQuery -> repr (Seq BakingRightsCrossCompat)
  rEndorsingRights :: Either Cycle (Set RawLevel) -> ChainId -> BlockQuery -> repr (Seq EndorsingRightsCrossCompat)

  rBalance :: PublicKeyHash -> ChainId -> BlockQuery -> repr Tez
  rDelegateInfo :: PublicKeyHash -> ChainId -> BlockQuery -> repr DelegateInfoCrossCompat
  rParticipationInfo :: PublicKeyHash -> ChainId -> BlockQuery -> repr ParticipationInfo
  rRound :: ChainId -> BlockQuery -> repr Int32

instance QueryBlock RpcQuery where
  type BlockType RpcQuery = BlockCrossCompat
  type BlockHeaderType RpcQuery = BlockHeader
  rHead = chainAPI' "/blocks/head"
  rBlock = blockAPI' ""
  rBlockHeader = blockAPI' "/header"

instance QueryHistory RpcQuery where
  rBlocks chainId (RawLevel len) heads = byHead <$> chainAPI ("/blocks?length=" <> T.pack (show len) <> foldMap blk2param heads) chainId
    where
      byHead :: [Seq BlockHash] -> Map.Map BlockHash (Seq BlockHash)
      byHead = foldMap $ maybe mempty (uncurry Map.singleton) . uncons
      blk2param :: BlockHash -> Text
      blk2param blkHash = "&head=" <> blockHashToBase58Text blkHash
  rProtoConstants = blockAPI "/context/constants"
  rContract contractId = blockAPI' ("/context/contracts/" <> toContractIdText contractId)
  rBallots = blockAPI "/votes/ballots/"
  rBallotList = blockAPI "/votes/ballot_list"
  rListings = blockAPI "/votes/listings/"
  rProposals = blockAPI "/votes/proposals/"
  rCurrentProposal = blockAPI "/votes/current_proposal/"
  rCurrentQuorum = blockAPI "/votes/current_quorum/"
  rProposalVote chain block pkh = S.fromList . map fst . filter (elem @[] pkh . snd) <$> blockAPI "/context/raw/json/votes/proposals?depth=1" chain block
  rBakingRights params = blockAPI $ "/helpers/baking_rights"
    <> (if null params then "" else "?" <> rightsLevelsOrCycleToQueryArgs params)
  rBakingRightsFull levelishes (Round rnd) = blockAPI $ "/helpers/baking_rights?all=true&"
    <> T.intercalate "&"
      [ "max_round=" <> T.pack (show rnd)
        -- We use both 'max_round' and 'max_priority' to provide backwards
        -- compatibility with old protocols. An inappropriate parameter for the
        -- protocol will be ignored.
      , "max_priority=" <> T.pack (show rnd)
      , rightsLevelsOrCycleToQueryArgs levelishes
      ]
  rEndorsingRights params = blockAPI $ "/helpers/endorsing_rights"
      <> (if null params then "" else "?" <> rightsLevelsOrCycleToQueryArgs params)
  rBalance publicKeyHash = blockAPI $ "/context/contracts/" <> toPublicKeyHashText publicKeyHash <> "/balance"
  rDelegateInfo publicKeyHash = blockAPI ("/context/delegates/" <> toPublicKeyHashText publicKeyHash)
  rParticipationInfo publicKeyHash = blockAPI $
      "/context/delegates/" <> toPublicKeyHashText publicKeyHash <> "/participation"
  rRound = blockAPI "/helpers/round"

class QueryNode repr where -- my node
  rConnections :: repr Word64 -- just a count for now, but there's more data there we may someday be interested in
  rNetworkStat :: repr NetworkStat
  rIsBootstrapped :: ChainId -> repr IsBootstrapped
  rConfig :: repr NodeConfig

class MonitorHeads repr where
  rMonitorHeads :: ChainId -> repr MonitorBlock

data RpcQuery a = RpcQuery
  { _RpcQuery_decoder :: LBS.ByteString -> Either String a
  , _RpcQuery_body :: Aeson.Encoding
  , _RpcQuery_method :: Http.Method
  , _RpcQuery_resource :: Text
  } deriving Functor

instance MonitorHeads PlainNodeStream where
  rMonitorHeads chainId = PlainNodeStream $ plainNodeRequest Http.methodGet ("/monitor/heads/" <> toBase58Text chainId)

plainNodeRequest :: FromJSON a => Http.Method -> Text -> RpcQuery a
plainNodeRequest = RpcQuery Aeson.eitherDecode' emptyObject_

postNodeRequest :: (Aeson.ToJSON b, FromJSON a) => b -> Text -> RpcQuery a
postNodeRequest b = RpcQuery Aeson.eitherDecode' (Aeson.toEncoding b) Http.methodPost

newtype PlainNode a = PlainNode (RpcQuery a)
newtype PlainNodeStream a = PlainNodeStream (RpcQuery a)

instance QueryChain RpcQuery where
  rChain = _block_chainId <$> plainNodeRequest Http.methodGet "/chains/main/blocks/head"

chainAPI :: FromJSON a => Text -> ChainId -> RpcQuery a
chainAPI = (. ChainTag_Hash) . chainAPI'

chainAPI' :: FromJSON a => Text -> ChainTag -> RpcQuery a
chainAPI' path chainId = plainNodeRequest Http.methodGet $ "/chains/" <> toChainTagText chainId <> path

blockAPI :: FromJSON a => Text -> ChainId -> BlockQuery -> RpcQuery a
blockAPI = (. ChainTag_Hash) . blockAPI'

blockAPI' :: FromJSON a => Text -> ChainTag -> BlockQuery -> RpcQuery a
blockAPI' path chainId blockHash = plainNodeRequest Http.methodGet (chainBlockUrl' chainId blockHash <> path)

instance QueryNode RpcQuery where
  rConnections = decoder <$> plainNodeRequest Http.methodGet "/network/connections"
    where
      decoder :: [Aeson.Value] -> Word64
      decoder = fromIntegral . length
  rNetworkStat = plainNodeRequest Http.methodGet "/network/stat"
  rIsBootstrapped = chainAPI "/is_bootstrapped"
  rConfig = plainNodeRequest Http.methodGet "/config"

chainBlockUrl :: ChainId -> BlockQuery -> Text
chainBlockUrl = chainBlockUrl' . ChainTag_Hash

chainBlockUrl' :: ChainTag -> BlockQuery -> Text
chainBlockUrl' chainId (BlockQueryOffset queryBase offset) =
  "/chains/" <> toChainTagText chainId <> "/blocks/" <> blockQueryBaseToText queryBase <> "~" <> T.pack (show $ unRawLevel offset)
  where
    blockQueryBaseToText = \case
      Left lvl -> T.pack $ show $ unRawLevel lvl
      Right blkHash -> blockHashToBase58Text blkHash


-- In Ithaca instead of an optional list of cycle arguments, '/helpers/baking_rights' and
-- '/helpers/endorsing_rights' RPC endpoints only take one optional cycle argument.
--
-- See https://tezos.gitlab.io/protocols/tenderbake.html#rpcs
rightsLevelsOrCycleToQueryArgs :: Either Cycle (Set RawLevel) -> Text
rightsLevelsOrCycleToQueryArgs = \case
  Left (Cycle c) -> "cycle=" <> T.pack (show c)
  Right levels -> T.intercalate "&" $ fmap (\l -> "level=" <> T.pack (show $ unRawLevel l)) $ toList levels
