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
import Tezos.Mumbai.Block (HasBlockMetadata(..))
import qualified Tezos.Mumbai.Types as Mumbai


data BlockCrossCompat
  = BlockGenesis Genesis.Block
  | BlockMumbai Mumbai.Block
  deriving (Show)

instance FromJSON BlockCrossCompat where
  parseJSON jv@(Object o) = do
    pv :: String <- o .: "protocol"
    case pv of
      "PtLimaPtLMwfNinJi9rCfDPWea8dFgTZ1MeJ9f1m2SRic6ayiwW" -> BlockMumbai <$> parseJSON jv
      "PtMumbai2TmsJHNGRkD8v8YDbtao7BLUC3wjASn1inAKLFCjaH1" -> BlockMumbai <$> parseJSON jv
      _ -> BlockGenesis <$> parseJSON jv
  parseJSON _ = mzero

blockCrossData :: (Genesis.Block -> a) -> (Mumbai.Block -> a) -> BlockCrossCompat -> a
blockCrossData fGenesis f13 = \case
  BlockGenesis b -> fGenesis b
  BlockMumbai b -> f13 b

instance HasProtocolHash BlockCrossCompat where
  protocolHash = lens
    (blockCrossData (view protocolHash) (view protocolHash))
    (\b ph -> blockCrossData
      (BlockGenesis . (protocolHash .~ ph))
      (BlockMumbai . (protocolHash .~ ph))
      b)

instance HasChainId BlockCrossCompat where
  chainIdL = lens
    (blockCrossData (view chainIdL) (view chainIdL))
    (\b ph -> blockCrossData
      (BlockGenesis . (chainIdL .~ ph))
      (BlockMumbai . (chainIdL .~ ph))
      b)

instance HasBlockMetadata BlockCrossCompat where
  blockMetadata = lens
    (blockCrossData (error "Genesis block doesn't provide useful metadata") (view blockMetadata))
    (\b ph -> blockCrossData
      (error "Genesis block doesn't provide useful metadata")
      (BlockMumbai . (blockMetadata .~ ph))
      b)

instance MayHaveAccusations BlockCrossCompat where
  getAccusations = blockCrossData getAccusations getAccusations

instance BlockLike BlockCrossCompat where
  hash = lens
    (blockCrossData (view hash) (view hash))
    (\b ph -> blockCrossData (BlockGenesis . (hash .~ ph)) (BlockMumbai . (hash .~ ph)) b)
  predecessor = lens
    (blockCrossData (view predecessor) (view predecessor))
    (\b ph -> blockCrossData (BlockGenesis . (predecessor .~ ph)) (BlockMumbai . (predecessor .~ ph)) b)
  level = lens
    (blockCrossData (view level) (view level))
    (\b ph -> blockCrossData (BlockGenesis . (level .~ ph)) (BlockMumbai . (level .~ ph)) b)
  fitness = lens
    (blockCrossData (view fitness) (view fitness))
    (\b ph -> blockCrossData (BlockGenesis . (fitness .~ ph)) (BlockMumbai . (fitness .~ ph)) b)
  timestamp = lens
    (blockCrossData (view timestamp) (view timestamp))
    (\b ph -> blockCrossData (BlockGenesis . (timestamp .~ ph)) (BlockMumbai . (timestamp .~ ph)) b)

instance HasBlockHeaderFull BlockCrossCompat where
  blockHeaderFull = lens
    (blockCrossData (view blockHeaderFull) (view blockHeaderFull))
    (\b ph -> blockCrossData
      (BlockGenesis . (blockHeaderFull .~ ph))
      (BlockMumbai . (blockHeaderFull .~ ph))
      b)

blockCrossCompatToBlockHeader :: BlockCrossCompat -> BlockHeader
blockCrossCompatToBlockHeader = \case
  BlockGenesis b -> Genesis.toBlockHeader b
  BlockMumbai b -> Mumbai.toBlockHeader b

mkBranchInfo :: BlockCrossCompat -> Mumbai.BranchInfo
mkBranchInfo blk =
  let levelInfo = blk ^. blockMetadata . Mumbai.blockMetadata_levelInfo in
    Mumbai.BranchInfo (WithProtocolHash (mkVeryBlockLike blk) (blk ^. protocolHash))
      (levelInfo ^. Mumbai.levelInfo_cycle) (levelInfo ^. Mumbai.levelInfo_cyclePosition)
