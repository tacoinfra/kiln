{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Tezos.Common.NodeRPC.Sources where

import Control.Lens.TH (makePrisms)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.URI (URI, mkURI)
import qualified Text.URI.QQ as Uri

import Tezos.Common.NodeRPC.Types
import Tezos.Common.Chain (NamedChain(..))
import Tezos.Common.Base58Check (ChainId)

type DataSource = (PublicNode, Either NamedChain ChainId, NonEmpty URI)

data PublicNode
  = PublicNode_Blockscale
  | PublicNode_TzScan
  | PublicNode_Obsidian
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Typeable)

instance ToJSON PublicNode
instance FromJSON PublicNode
instance ToJSONKey PublicNode
instance FromJSONKey PublicNode


canFetchHistory :: PublicNode -> Bool
canFetchHistory PublicNode_Blockscale = True
canFetchHistory PublicNode_TzScan = False
canFetchHistory PublicNode_Obsidian = True

publicNodeShortName :: PublicNode -> T.Text
publicNodeShortName = \case
  PublicNode_Obsidian -> "Obsidian Systems"
  PublicNode_Blockscale -> "Tezos Foundation"
  PublicNode_TzScan -> "tzscan.io"


getPublicNodeUri :: PublicNode -> NamedChain -> NonEmpty URI
getPublicNodeUri PublicNode_Obsidian NamedChain_Zeronet    = pure [Uri.uri|https://zeronet-tezos-api.obsidian.systems/api|]
getPublicNodeUri PublicNode_Obsidian NamedChain_Alphanet   = pure [Uri.uri|https://alphanet-tezos-api.obsidian.systems/api|]
getPublicNodeUri PublicNode_Obsidian NamedChain_Mainnet    = pure [Uri.uri|https://tezos-api.obsidian.systems/api|]
getPublicNodeUri PublicNode_Blockscale NamedChain_Zeronet  = pure [Uri.uri|https://rpczero.tzbeta.net|]
getPublicNodeUri PublicNode_Blockscale NamedChain_Alphanet = pure [Uri.uri|https://rpcalpha.tzbeta.net|]
getPublicNodeUri PublicNode_Blockscale NamedChain_Mainnet  = pure [Uri.uri|https://rpc.tzbeta.net|]
getPublicNodeUri PublicNode_TzScan NamedChain_Zeronet      = pure [Uri.uri|https://api.zeronet.tzscan.io|]
getPublicNodeUri PublicNode_TzScan NamedChain_Alphanet     = pure [Uri.uri|https://api.alphanet.tzscan.io|]
getPublicNodeUri PublicNode_TzScan NamedChain_Mainnet      = fromJust . mkURI . ("https://api" <>) . (<> ".tzscan.io") . T.pack . show <$> ((1 :: Int) :| [2..6])

tzScanUri :: NamedChain -> URI
tzScanUri = \case
  NamedChain_Zeronet  -> [Uri.uri|https://zeronet.tzscan.io|]
  NamedChain_Alphanet -> [Uri.uri|https://alphanet.tzscan.io|]
  NamedChain_Mainnet  -> [Uri.uri|https://tzscan.io|]

data PublicNodeError
  = PublicNodeError_RpcError RpcError
  | PublicNodeError_FeatureNotSupported
  deriving (Eq, Ord, Show, Generic, Typeable)


makePrisms ''PublicNodeError
