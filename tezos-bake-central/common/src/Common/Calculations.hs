{-# LANGUAGE OverloadedStrings #-}

module Common.Calculations where

import Tezos.Types

import Common.Schema
import ExtraPrelude

levelToCycleSameProtocol :: (HasProtocolHash blk, BlockLike blk) => ProtocolIndex -> blk -> Either Text Cycle
levelToCycleSameProtocol proto blk
  | blk ^. protocolHash /= proto ^. protocolHash = Left $ "levelToCycle: ProtocolIndex hash (" <> tshow (proto ^. protocolHash) <> ") is different from block hash (" <> tshow (blk ^. protocolHash) <> ")"
  | proto ^. level > blk ^. level = Left $ "levelToCycle: Block level (" <> tshow (blk ^. level) <> ") precedes first known block on its protocol (" <> tshow (proto ^. level) <> ")"
  | otherwise = Right $ Cycle $
      unCycle (proto ^. protocolIndex_firstBlockCycle) +
      unRawLevel (blk ^. level - proto ^. level) `div` unRawLevel (proto ^. protocolIndex_constants . protoInfo_blocksPerCycle)
