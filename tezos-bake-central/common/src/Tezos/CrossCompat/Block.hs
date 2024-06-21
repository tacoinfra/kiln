{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module contains data types similat to the ones from 'Tezos.V*.Block'
-- but represented as unions to provide cross compatibility between protocols
-- in case of RPC schema changes.
module Tezos.CrossCompat.Block where

import Control.Lens (lens, view, (^.), (.~))
import Control.Monad (mzero)
import Data.Aeson

import Tezos.Common.Accusation
import Tezos.Common.Block
import Tezos.Common.BlockHeader
import qualified Tezos.Genesis.Block as Genesis
import Tezos.Base.Block (HasBlockMetadata(..))
import qualified Tezos.Base.Types as Base


data BlockCrossCompat
  = BlockGenesis Genesis.Block
  | BlockBase Base.Block
  deriving (Show)

instance FromJSON BlockCrossCompat where
  parseJSON jv@(Object o) = do
    pv :: String <- o .: "protocol"
    case pv of
      "PtParisBxoLz5gzMmn3d9WBQNoPSZakgnkMC2VNuQ3KXfUtUQeZ" -> BlockBase <$> parseJSON jv
      "PsParisCZo7KAh1Z1smVd9ZMZ1HHn5gkzbM94V3PLCpknFWhUAi" -> BlockBase <$> parseJSON jv
      "ProxfordYmVfjWnRcgjWH36fW6PArwqykTFzotUxRs6gmTcZDuH" -> BlockBase <$> parseJSON jv
      _ -> BlockGenesis <$> parseJSON jv
  parseJSON _ = mzero

blockCrossData :: (Genesis.Block -> a) -> (Base.Block -> a) -> BlockCrossCompat -> a
blockCrossData fGenesis f13 = \case
  BlockGenesis b -> fGenesis b
  BlockBase b -> f13 b

instance HasProtocolHash BlockCrossCompat where
  protocolHash = lens
    (blockCrossData (view protocolHash) (view protocolHash))
    (\b ph -> blockCrossData
      (BlockGenesis . (protocolHash .~ ph))
      (BlockBase . (protocolHash .~ ph))
      b)

instance HasChainId BlockCrossCompat where
  chainIdL = lens
    (blockCrossData (view chainIdL) (view chainIdL))
    (\b ph -> blockCrossData
      (BlockGenesis . (chainIdL .~ ph))
      (BlockBase . (chainIdL .~ ph))
      b)

instance HasBlockMetadata BlockCrossCompat where
  blockMetadata = lens
    (blockCrossData (error "Genesis block doesn't provide useful metadata") (view blockMetadata))
    (\b ph -> blockCrossData
      (error "Genesis block doesn't provide useful metadata")
      (BlockBase . (blockMetadata .~ ph))
      b)

instance MayHaveAccusations BlockCrossCompat where
  getAccusations = blockCrossData getAccusations getAccusations

instance BlockLike BlockCrossCompat where
  hash = lens
    (blockCrossData (view hash) (view hash))
    (\b ph -> blockCrossData (BlockGenesis . (hash .~ ph)) (BlockBase . (hash .~ ph)) b)
  predecessor = lens
    (blockCrossData (view predecessor) (view predecessor))
    (\b ph -> blockCrossData (BlockGenesis . (predecessor .~ ph)) (BlockBase . (predecessor .~ ph)) b)
  level = lens
    (blockCrossData (view level) (view level))
    (\b ph -> blockCrossData (BlockGenesis . (level .~ ph)) (BlockBase . (level .~ ph)) b)
  fitness = lens
    (blockCrossData (view fitness) (view fitness))
    (\b ph -> blockCrossData (BlockGenesis . (fitness .~ ph)) (BlockBase . (fitness .~ ph)) b)
  timestamp = lens
    (blockCrossData (view timestamp) (view timestamp))
    (\b ph -> blockCrossData (BlockGenesis . (timestamp .~ ph)) (BlockBase . (timestamp .~ ph)) b)

instance HasBlockHeaderFull BlockCrossCompat where
  blockHeaderFull = lens
    (blockCrossData (view blockHeaderFull) (view blockHeaderFull))
    (\b ph -> blockCrossData
      (BlockGenesis . (blockHeaderFull .~ ph))
      (BlockBase . (blockHeaderFull .~ ph))
      b)

blockCrossCompatToBlockHeader :: BlockCrossCompat -> BlockHeader
blockCrossCompatToBlockHeader = \case
  BlockGenesis b -> Genesis.toBlockHeader b
  BlockBase b -> Base.toBlockHeader b

mkBranchInfo :: BlockCrossCompat -> Base.BranchInfo
mkBranchInfo blk =
  let levelInfo = blk ^. blockMetadata . Base.blockMetadata_levelInfo in
    Base.BranchInfo (WithProtocolHash (mkVeryBlockLike blk) (blk ^. protocolHash))
      (levelInfo ^. Base.levelInfo_cycle) (levelInfo ^. Base.levelInfo_cyclePosition)
