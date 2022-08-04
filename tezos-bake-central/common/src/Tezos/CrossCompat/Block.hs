{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module contains data types similat to the ones from 'Tezos.V*.Block'
-- but represented as unions to provide cross compatibility between protocols
-- in case of RPC schema changes.
module Tezos.CrossCompat.Block where

import Control.Lens (lens, view, (.~))
import Control.Monad (mzero)
import Data.Aeson

import Tezos.Common.Accusation
import Tezos.Common.Block
import Tezos.Common.BlockHeader
import qualified Tezos.Genesis.Block as Genesis
import Tezos.V014.Block (HasBlockMetadata(..))
import qualified Tezos.V014.Types as V014


data BlockCrossCompat
  = BlockGenesis Genesis.Block
  | BlockV014 V014.Block

instance FromJSON BlockCrossCompat where
  parseJSON jv@(Object o) = do
    pv :: String <- o .: "protocol"
    case pv of
      "PtJakart2xVj7pYXJBXrqHgd82rdkLey5ZeeGwDgPp9rhQUbSqY" -> BlockV014 <$> parseJSON jv
      "PtKathmankSpLLDALzWw7CGD2j2MtyveTwboEYokqUCP4a1LxMg" -> BlockV014 <$> parseJSON jv
      _ -> BlockGenesis <$> parseJSON jv
  parseJSON _ = mzero

blockCrossData :: (Genesis.Block -> a) -> (V014.Block -> a) -> BlockCrossCompat -> a
blockCrossData fGenesis f13 = \case
  BlockGenesis b -> fGenesis b
  BlockV014 b -> f13 b

instance HasProtocolHash BlockCrossCompat where
  protocolHash = lens
    (blockCrossData (view protocolHash) (view protocolHash))
    (\b ph -> blockCrossData
      (BlockGenesis . (protocolHash .~ ph))
      (BlockV014 . (protocolHash .~ ph))
      b)

instance HasChainId BlockCrossCompat where
  chainIdL = lens
    (blockCrossData (view chainIdL) (view chainIdL))
    (\b ph -> blockCrossData
      (BlockGenesis . (chainIdL .~ ph))
      (BlockV014 . (chainIdL .~ ph))
      b)

instance HasBlockMetadata BlockCrossCompat where
  blockMetadata = lens
    (blockCrossData (error "Genesis block doesn't provide useful metadata") (view blockMetadata))
    (\b ph -> blockCrossData
      (error "Genesis block doesn't provide useful metadata")
      (BlockV014 . (blockMetadata .~ ph))
      b)

instance MayHaveAccusations BlockCrossCompat where
  getAccusations = blockCrossData getAccusations getAccusations

instance BlockLike BlockCrossCompat where
  hash = lens
    (blockCrossData (view hash) (view hash))
    (\b ph -> blockCrossData (BlockGenesis . (hash .~ ph)) (BlockV014 . (hash .~ ph)) b)
  predecessor = lens
    (blockCrossData (view predecessor) (view predecessor))
    (\b ph -> blockCrossData (BlockGenesis . (predecessor .~ ph)) (BlockV014 . (predecessor .~ ph)) b)
  level = lens
    (blockCrossData (view level) (view level))
    (\b ph -> blockCrossData (BlockGenesis . (level .~ ph)) (BlockV014 . (level .~ ph)) b)
  fitness = lens
    (blockCrossData (view fitness) (view fitness))
    (\b ph -> blockCrossData (BlockGenesis . (fitness .~ ph)) (BlockV014 . (fitness .~ ph)) b)
  timestamp = lens
    (blockCrossData (view timestamp) (view timestamp))
    (\b ph -> blockCrossData (BlockGenesis . (timestamp .~ ph)) (BlockV014 . (timestamp .~ ph)) b)

instance HasBlockHeaderFull BlockCrossCompat where
  blockHeaderFull = lens
    (blockCrossData (view blockHeaderFull) (view blockHeaderFull))
    (\b ph -> blockCrossData
      (BlockGenesis . (blockHeaderFull .~ ph))
      (BlockV014 . (blockHeaderFull .~ ph))
      b)

blockCrossCompatToBlockHeader :: BlockCrossCompat -> BlockHeader
blockCrossCompatToBlockHeader = \case
  BlockGenesis b -> Genesis.toBlockHeader b
  BlockV014 b -> V014.toBlockHeader b
