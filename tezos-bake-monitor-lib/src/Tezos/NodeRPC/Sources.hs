{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Tezos.NodeRPC.Sources where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types.Method (methodGet)
import Text.URI (URI)
import qualified Text.URI as Uri
import qualified Text.URI.QQ as Uri

import Tezos.Base58Check (toBase58Text)
import Tezos.Block (Block, TzScanBlock (..))
import Tezos.NodeRPC.Class (MonitorHeads (..), QueryBlock (..), QueryChain (..), QueryHistory (..),
                            QueryNode (..))
import Tezos.NodeRPC.Network (NodeRPCContext (..), QueryNodeImpl (nodeRPC), nodeRPCImpl)
import Tezos.NodeRPC.Types (AsRpcError)

data NamedChain
  = --NamedChain_Mainnet
    NamedChain_Betanet
  | NamedChain_Alphanet
  | NamedChain_Zeronet
  deriving (Eq, Ord, Bounded, Enum, Generic, Typeable, Read, Show)
instance FromJSON NamedChain
instance ToJSON NamedChain

data DataSource
  = DataSource_PlainNode PlainNode
  | DataSource_BlockscaleNode BlockscaleNode
  | DataSource_TzScan TzScanNode
  deriving (Eq, Ord, Generic, Typeable, Show)
instance FromJSON DataSource
instance ToJSON DataSource

newtype QDataSource t a = QDataSource
  { querySource :: forall e m. (MonadIO m, MonadError e m, AsRpcError e) => Http.Manager -> t -> m a
  } deriving (Functor)


-- PLAIN NODE --
newtype PlainNode = PlainNode Text
  deriving (Eq, Ord, Show, Generic, Typeable, FromJSON, ToJSON)

instance QueryChain (QDataSource PlainNode) where
  rChain = runNodeRpcPlain rChain

instance QueryBlock (QDataSource PlainNode) where
  type BlockType (QDataSource PlainNode) = Block
  rHead chainId = runNodeRpcPlain $ rHead chainId
  rBlock chainId blockHash = runNodeRpcPlain $ rBlock chainId blockHash

instance QueryHistory (QDataSource PlainNode) where
  rBlockPred chainId blockHash levelsBack = runNodeRpcPlain $ rBlockPred chainId blockHash levelsBack
  rBlocks chainId numLevels blockHashes = runNodeRpcPlain $ rBlocks chainId numLevels blockHashes
  rProtoConstants chainId blockHash = runNodeRpcPlain $ rProtoConstants chainId blockHash
  rContract chainId blockHash contractId = runNodeRpcPlain $ rContract chainId blockHash contractId
  rBakingRights chainId blockHash places = runNodeRpcPlain $ rBakingRights chainId blockHash places
  rEndorsingRights chainId blockHash places = runNodeRpcPlain $ rEndorsingRights chainId blockHash places

instance QueryNode (QDataSource PlainNode) where
  rConnections = runNodeRpcPlain rConnections
  rNetworkStat = runNodeRpcPlain rNetworkStat

instance MonitorHeads (QDataSource PlainNode) where
  rMonitorHeads chainId f = runNodeRpcPlain $ rMonitorHeads chainId f

runNodeRpcPlain :: forall a. QueryNodeImpl a -> QDataSource PlainNode a
runNodeRpcPlain q = QDataSource $ \httpMgr (PlainNode addr) -> runReaderT (nodeRPC q) (NodeRPCContext httpMgr addr)


-- BLOCKSCALE (FOUNDATION) NODE --
newtype BlockscaleNode = BlockscaleNode NamedChain
  deriving (Eq, Ord, Show, Generic, Typeable, FromJSON, ToJSON)

instance QueryChain (QDataSource BlockscaleNode) where
  rChain = runNodeRpcBlockscale rChain

instance QueryBlock (QDataSource BlockscaleNode) where
  type BlockType (QDataSource BlockscaleNode) = Block
  rHead chainId = runNodeRpcBlockscale $ rHead chainId
  rBlock chainId blockHash = runNodeRpcBlockscale $ rBlock chainId blockHash

instance QueryHistory (QDataSource BlockscaleNode) where
  rBlockPred chainId blockHash levelsBack = runNodeRpcBlockscale $ rBlockPred chainId blockHash levelsBack
  rBlocks chainId numLevels blockHashes = runNodeRpcBlockscale $ rBlocks chainId numLevels blockHashes
  rProtoConstants chainId blockHash = runNodeRpcBlockscale $ rProtoConstants chainId blockHash
  rContract chainId blockHash contractId = runNodeRpcBlockscale $ rContract chainId blockHash contractId
  rBakingRights chainId blockHash places = runNodeRpcBlockscale $ rBakingRights chainId blockHash places
  rEndorsingRights chainId blockHash places = runNodeRpcBlockscale $ rEndorsingRights chainId blockHash places

runNodeRpcBlockscale :: forall a. QueryNodeImpl a -> QDataSource BlockscaleNode a
runNodeRpcBlockscale q = QDataSource $ \httpMgr (BlockscaleNode chain) ->
  runReaderT (nodeRPC q) (NodeRPCContext httpMgr $ Uri.render $ blockscaleNodeUri chain)

blockscaleNodeUri :: NamedChain -> URI
blockscaleNodeUri = \case
  NamedChain_Zeronet  -> [Uri.uri|https://rpczero.tzbeta.net|]
  NamedChain_Alphanet -> [Uri.uri|https://rpcalpha.tzbeta.net|]
  NamedChain_Betanet  -> [Uri.uri|https://rpc.tzbeta.net|]


-- TZSCAN --
newtype TzScanNode = TzScanNode NamedChain
  deriving (Eq, Ord, Show, Generic, Typeable, FromJSON, ToJSON)

instance QueryChain (QDataSource TzScanNode) where
  rChain = _tzScanBlock_network <$> runNodeRpcTzScan "/v2/head"

instance QueryBlock (QDataSource TzScanNode) where
  type BlockType (QDataSource TzScanNode) = TzScanBlock
  rHead _chainId = runNodeRpcTzScan "/v2/head"
  rBlock _chainId blockHash = runNodeRpcTzScan $ "/v2/block/" <> toBase58Text blockHash

runNodeRpcTzScan :: forall a. FromJSON a => Text -> QDataSource TzScanNode a
runNodeRpcTzScan q = QDataSource $ \httpMgr (TzScanNode chain) ->
  runReaderT (nodeRPCImpl methodGet q) (NodeRPCContext httpMgr $ Uri.render $ tzScanApiUri chain)

tzScanApiUri :: NamedChain -> URI
tzScanApiUri = \case
  NamedChain_Zeronet  -> [Uri.uri|https://zeronet-api.tzscan.io|]
  NamedChain_Alphanet -> [Uri.uri|https://alphanet-api.tzscan.io|]
  NamedChain_Betanet  -> [Uri.uri|https://api.tzscan.io|]

tzScanUri :: NamedChain -> URI
tzScanUri = \case
  NamedChain_Zeronet  -> [Uri.uri|https://zeronet.tzscan.io|]
  NamedChain_Alphanet -> [Uri.uri|https://alphanet.tzscan.io|]
  NamedChain_Betanet  -> [Uri.uri|https://tzscan.io|]
