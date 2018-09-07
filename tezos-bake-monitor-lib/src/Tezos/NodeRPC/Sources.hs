{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Tezos.NodeRPC.Sources where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (asks, MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Control.Lens (Lens', Prism', view, (^.), re)
import Control.Lens.TH (makePrisms, makeLenses)
import Data.Semigroup ((<>))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Types.Method (methodGet)
import Text.URI (URI)
import qualified Text.URI.QQ as Uri
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Text as T

import Tezos.Level (RawLevel(..))
import Tezos.Base58Check (toBase58Text, BlockHash, ChainId)
import Tezos.Block (TzScanBlock (..), VeryBlockLike(..), mkVeryBlockLike, BlockLike, hash, level)
import Tezos.Chain (NamedChain (..))
import Tezos.NodeRPC.Class
import Tezos.NodeRPC.Network (HasNodeRPC, nodeRPCContext, NodeRPCContext (..), nodeRPC)
import Tezos.NodeRPC.Types (AsRpcError, RpcError(..), asRpcError)

type DataSource = (PublicNode, Either NamedChain ChainId, URI)

data PublicNode
  = PublicNode_Blockscale
  | PublicNode_TzScan
  | PublicNode_Obsidian
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Typeable)

instance ToJSON PublicNode
instance FromJSON PublicNode

canFetchHistory :: PublicNode -> Bool
canFetchHistory PublicNode_Blockscale = True
canFetchHistory PublicNode_TzScan = False
canFetchHistory PublicNode_Obsidian = True


getPublicNodeUri :: PublicNode -> NamedChain -> URI
getPublicNodeUri PublicNode_Obsidian NamedChain_Zeronet  = [Uri.uri|https://tezos.obsidian.systems/zeronet|]
getPublicNodeUri PublicNode_Obsidian NamedChain_Alphanet = [Uri.uri|https://tezos.obsidian.systems/alphanet|]
getPublicNodeUri PublicNode_Obsidian NamedChain_Betanet  = [Uri.uri|https://tezos.obsidian.systems/|]
getPublicNodeUri PublicNode_Blockscale NamedChain_Zeronet  = [Uri.uri|https://rpczero.tzbeta.net|]
getPublicNodeUri PublicNode_Blockscale NamedChain_Alphanet = [Uri.uri|https://rpcalpha.tzbeta.net|]
getPublicNodeUri PublicNode_Blockscale NamedChain_Betanet  = [Uri.uri|https://rpc.tzbeta.net|]
getPublicNodeUri PublicNode_TzScan NamedChain_Zeronet  = [Uri.uri|https://zeronet-api.tzscan.io|]
getPublicNodeUri PublicNode_TzScan NamedChain_Alphanet = [Uri.uri|https://alphanet-api.tzscan.io|]
getPublicNodeUri PublicNode_TzScan NamedChain_Betanet  = [Uri.uri|https://api.tzscan.io|]

tzScanUri :: NamedChain -> URI
tzScanUri = \case
  NamedChain_Zeronet  -> [Uri.uri|https://zeronet.tzscan.io|]
  NamedChain_Alphanet -> [Uri.uri|https://alphanet.tzscan.io|]
  NamedChain_Betanet  -> [Uri.uri|https://tzscan.io|]

data PublicNodeContext = PublicNodeContext
  { _publicNodeContext_nodeCtx :: !NodeRPCContext
  , _publicNodeContext_api :: !(Maybe PublicNode)
  }

data PublicNodeError
  = PublicNodeError_RpcError RpcError
  | PublicNodeError_FeatureNotSupported
  deriving (Eq, Ord, Show, Generic, Typeable)

makeLenses 'PublicNodeContext
makePrisms ''PublicNodeError

-- maybe we should really use a `ProxiedNode` wrapper so we don't unwittingly
-- use public caches as regular nodes?  For now, we do so wittingly...
class HasNodeRPC r => HasPublicNodeContext r where
  publicNodeContext :: Lens' r PublicNodeContext

class AsRpcError e => AsPublicNodeError e where
  asPublicNodeError :: Prism' e PublicNodeError

instance HasPublicNodeContext PublicNodeContext where
  publicNodeContext = id

instance HasNodeRPC PublicNodeContext where
  nodeRPCContext = publicNodeContext_nodeCtx

instance AsRpcError PublicNodeError where
  asRpcError = _PublicNodeError_RpcError

instance AsPublicNodeError PublicNodeError where
  asPublicNodeError = id

throwFeatureNotSupported :: forall e m a.  (MonadError e m, AsPublicNodeError e) => m a
throwFeatureNotSupported = throwError (PublicNodeError_FeatureNotSupported ^. re asPublicNodeError)

getNodeChain :: forall e r m.
  ( MonadIO m
  , MonadError e m , AsRpcError e
  , MonadReader r m, HasPublicNodeContext r
  )
  => m ChainId
getNodeChain = (asks $ view (publicNodeContext . publicNodeContext_api)) >>= \case
    Nothing                    -> nodeRPC rChain
    Just PublicNode_Blockscale -> nodeRPC rChain
    Just PublicNode_TzScan     -> nodeRPC $ _tzScanBlock_network <$> plainNodeRequest methodGet "/v2/head/"
    Just PublicNode_Obsidian   -> nodeRPC $ plainNodeRequest methodGet "/v1/chain"

getCurrentHead :: forall e r m.
  ( MonadIO m
  , MonadError e m , AsRpcError e
  , MonadReader r m, HasPublicNodeContext r
  )
  => ChainId -> m VeryBlockLike
getCurrentHead chain = (asks $ view (publicNodeContext . publicNodeContext_api)) >>= \case
  Nothing                    -> nodeRPC $ mkVeryBlockLike <$> rHead chain
  Just PublicNode_Blockscale -> nodeRPC $ mkVeryBlockLike <$> rHead chain
  Just PublicNode_TzScan     -> nodeRPC $ mkVeryBlockLike @ TzScanBlock <$> plainNodeRequest methodGet "/v2/head/"
  Just PublicNode_Obsidian   -> nodeRPC $                                   plainNodeRequest methodGet $ "/v1/" <> toBase58Text chain <> "/head"

canGetHistory :: PublicNode -> Bool
canGetHistory PublicNode_Blockscale = True
canGetHistory PublicNode_Obsidian = True
canGetHistory PublicNode_TzScan = False

obsidianLCA :: (BlockLike blk, Foldable f) => ChainId -> blk -> f BlockHash -> RpcQuery VeryBlockLike
obsidianLCA chain blk branches = plainNodeRequest methodGet $
  "/v1/" <> toBase58Text chain <> "/lca?block=" <> toBase58Text (blk ^. hash) <> foldMap (\b' -> "&block=" <> toBase58Text b') branches

obsidianAncestors :: ChainId -> BlockHash -> RawLevel -> RpcQuery (Seq BlockHash)
obsidianAncestors chain branch levels = plainNodeRequest methodGet $
  "/v1/" <> toBase58Text chain <> "/ancestors?branch=" <> toBase58Text branch <> "&=level" <> (T.pack $ show levels)

-- fetch some history, starting at head, for at most n levels, optionally stop at ancestors of branches
getHistory :: forall blk e r m.
  ( MonadIO m
  , MonadError e m , AsPublicNodeError e
  , MonadReader r m, HasPublicNodeContext r
  , BlockLike blk
  )
  => ChainId -> blk -> RawLevel -> Set BlockHash -> m (Seq BlockHash)
getHistory chain blk levels branches = (asks $ view (publicNodeContext . publicNodeContext_api)) >>= \case
  Nothing                    -> theNormalWay
  Just PublicNode_Blockscale -> theNormalWay
  Just PublicNode_TzScan     -> throwFeatureNotSupported
  Just PublicNode_Obsidian   -> do
    levels' <-
      if null branches
        then pure levels
        else do
          lca <- nodeRPC $ obsidianLCA chain blk branches
          pure $ min levels $ (blk ^. level) - (lca ^. level)
    nodeRPC $ obsidianAncestors chain blkHash levels'

  where
    blkHash = blk ^. hash
    throwBadResponse = throwError (PublicNodeError_RpcError (RpcError_HttpException "not enough data")  ^. re asPublicNodeError)
    theNormalWay = maybe throwBadResponse pure =<< nodeRPC (Map.lookup blkHash <$> rBlocks chain levels (Set.singleton blkHash))

getBlock ::
  ( MonadIO m
  , MonadError e m , AsRpcError e
  , MonadReader r m, HasPublicNodeContext r
  ) => ChainId -> BlockHash -> m VeryBlockLike
getBlock chainId blockHash = (asks $ view (publicNodeContext . publicNodeContext_api)) >>= \case
  Nothing                    -> nodeRPC $ mkVeryBlockLike <$> rBlock chainId blockHash
  Just PublicNode_Blockscale -> nodeRPC $ mkVeryBlockLike <$> rBlock chainId blockHash
  Just PublicNode_TzScan     -> nodeRPC $ mkVeryBlockLike @ TzScanBlock <$> plainNodeRequest methodGet ("/v2/block/" <> toBase58Text blockHash)

  Just PublicNode_Obsidian   -> nodeRPC $ plainNodeRequest methodGet
    ("/v1/" <> toBase58Text chainId <> "/block/" <> toBase58Text blockHash)
