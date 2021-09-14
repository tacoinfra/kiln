{-# LANGUAGE OverloadedStrings #-}

module Common.Calculations where

import Tezos.Types

import Common.Schema
import ExtraPrelude

levelToCycleSameProtocol :: ProtocolIndex -> BranchInfo -> Either Text Cycle
levelToCycleSameProtocol proto branchInfo
  | branchInfo ^. protocolHash /= proto ^. protocolHash =
    Left $ "levelToCycle: ProtocolIndex hash (" <> tshow (proto ^. protocolHash) <> ") is different from block hash (" <> tshow (branchInfo ^. protocolHash) <> ")"
  | otherwise = Right $ branchInfo ^. branchInfo_cycle
