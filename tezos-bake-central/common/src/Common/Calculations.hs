{-# LANGUAGE OverloadedStrings #-}

module Common.Calculations where

import Tezos.Types
import Tezos.Unsafe

import Common.Schema
import ExtraPrelude

levelToCycleSameProtocol :: (HasProtocolHash blk, BlockLike blk) => ProtocolIndex -> blk -> Either Text Cycle
levelToCycleSameProtocol proto blk
  | blk ^. protocolHash /= proto ^. protocolHash = Left $ "levelToCycle: ProtocolIndex hash (" <> tshow (proto ^. protocolHash) <> ") is different from block hash (" <> tshow (blk ^. protocolHash) <> ")"
  | otherwise = Right $ unsafeAssumptionLevelToCycle (proto ^. protocolIndex_constants) (blk ^. level)
