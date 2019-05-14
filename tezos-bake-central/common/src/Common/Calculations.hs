{-# LANGUAGE OverloadedStrings #-}

module Common.Calculations where

import Tezos.Block
import Tezos.Level
import Tezos.ProtocolConstants (protoInfo_blocksPerCycle)

import Common.Schema
import ExtraPrelude

levelToCycle :: (HasProtocolHash blk, BlockLike blk) => KnownProtocol -> blk -> Either Text Cycle
levelToCycle proto blk
  | blk ^. protocolHash /= proto ^. protocolHash = Left $ "levelToCycle: KnownProtocol hash (" <> tshow (proto ^. protocolHash) <> ") is different from block hash (" <> tshow (blk ^. protocolHash) <> ")"
  | proto ^. level > blk ^. level = Left $ "levelToCycle: Block level (" <> tshow (blk ^. level) <> ") precedes first known block on its protocol (" <> tshow (proto ^. level) <> ")"
  | otherwise = Right $ Cycle $
      unCycle (proto ^. knownProtocol_firstCycle) +
      unRawLevel (blk ^. level - proto ^. level) `div` unRawLevel (proto ^. knownProtocol_constants . protoInfo_blocksPerCycle)
