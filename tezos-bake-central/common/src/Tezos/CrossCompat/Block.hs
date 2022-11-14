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
import Tezos.Lima.Block (HasBlockMetadata(..))
import qualified Tezos.Lima.Types as Lima


data BlockCrossCompat
  = BlockGenesis Genesis.Block
  | BlockLima Lima.Block
  deriving (Show)

instance FromJSON BlockCrossCompat where
  parseJSON jv@(Object o) = do
    pv :: String <- o .: "protocol"
    case pv of
      "PtKathmankSpLLDALzWw7CGD2j2MtyveTwboEYokqUCP4a1LxMg" -> BlockLima <$> parseJSON jv
      "PtLimaPtLMwfNinJi9rCfDPWea8dFgTZ1MeJ9f1m2SRic6ayiwW" -> BlockLima <$> parseJSON jv
      _ -> BlockGenesis <$> parseJSON jv
  parseJSON _ = mzero

blockCrossData :: (Genesis.Block -> a) -> (Lima.Block -> a) -> BlockCrossCompat -> a
blockCrossData fGenesis f13 = \case
  BlockGenesis b -> fGenesis b
  BlockLima b -> f13 b

instance HasProtocolHash BlockCrossCompat where
  protocolHash = lens
    (blockCrossData (view protocolHash) (view protocolHash))
    (\b ph -> blockCrossData
      (BlockGenesis . (protocolHash .~ ph))
      (BlockLima . (protocolHash .~ ph))
      b)

instance HasChainId BlockCrossCompat where
  chainIdL = lens
    (blockCrossData (view chainIdL) (view chainIdL))
    (\b ph -> blockCrossData
      (BlockGenesis . (chainIdL .~ ph))
      (BlockLima . (chainIdL .~ ph))
      b)

instance HasBlockMetadata BlockCrossCompat where
  blockMetadata = lens
    (blockCrossData (error "Genesis block doesn't provide useful metadata") (view blockMetadata))
    (\b ph -> blockCrossData
      (error "Genesis block doesn't provide useful metadata")
      (BlockLima . (blockMetadata .~ ph))
      b)

instance MayHaveAccusations BlockCrossCompat where
  getAccusations = blockCrossData getAccusations getAccusations

instance BlockLike BlockCrossCompat where
  hash = lens
    (blockCrossData (view hash) (view hash))
    (\b ph -> blockCrossData (BlockGenesis . (hash .~ ph)) (BlockLima . (hash .~ ph)) b)
  predecessor = lens
    (blockCrossData (view predecessor) (view predecessor))
    (\b ph -> blockCrossData (BlockGenesis . (predecessor .~ ph)) (BlockLima . (predecessor .~ ph)) b)
  level = lens
    (blockCrossData (view level) (view level))
    (\b ph -> blockCrossData (BlockGenesis . (level .~ ph)) (BlockLima . (level .~ ph)) b)
  fitness = lens
    (blockCrossData (view fitness) (view fitness))
    (\b ph -> blockCrossData (BlockGenesis . (fitness .~ ph)) (BlockLima . (fitness .~ ph)) b)
  timestamp = lens
    (blockCrossData (view timestamp) (view timestamp))
    (\b ph -> blockCrossData (BlockGenesis . (timestamp .~ ph)) (BlockLima . (timestamp .~ ph)) b)

instance HasBlockHeaderFull BlockCrossCompat where
  blockHeaderFull = lens
    (blockCrossData (view blockHeaderFull) (view blockHeaderFull))
    (\b ph -> blockCrossData
      (BlockGenesis . (blockHeaderFull .~ ph))
      (BlockLima . (blockHeaderFull .~ ph))
      b)

blockCrossCompatToBlockHeader :: BlockCrossCompat -> BlockHeader
blockCrossCompatToBlockHeader = \case
  BlockGenesis b -> Genesis.toBlockHeader b
  BlockLima b -> Lima.toBlockHeader b

mkBranchInfo :: BlockCrossCompat -> Lima.BranchInfo
mkBranchInfo blk =
  let levelInfo = blk ^. blockMetadata . Lima.blockMetadata_levelInfo in
    Lima.BranchInfo (WithProtocolHash (mkVeryBlockLike blk) (blk ^. protocolHash))
      (levelInfo ^. Lima.levelInfo_cycle) (levelInfo ^. Lima.levelInfo_cyclePosition)
