{-# LANGUAGE OverloadedStrings #-}
module Common.Config where

import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Tezos.Types (ChainId, NamedChain (..))

changelogUrl :: Text
changelogUrl = "https://gitlab.com/obsidian.systems/tezos-bake-monitor/tree/develop/CHANGELOG.md"

db :: FilePath
db = "db"

route :: FilePath
route = "route"

emailFromAddress :: FilePath
emailFromAddress = "email-from"

chain :: FilePath
chain = "chain"

defaultChain :: Either NamedChain ChainId
defaultChain = Left NamedChain_Betanet

checkForUpgrade :: FilePath
checkForUpgrade = "check-for-upgrade"

checkForUpgradeDefault :: Bool
checkForUpgradeDefault = True

upgradeBranch :: FilePath
upgradeBranch = "upgrade-branch"

upgradeBranchDefault :: Text
upgradeBranchDefault = "master"

parseBool :: Text -> Bool
parseBool txt
  | v `elem` trues = True
  | v `elem` falses = False
  | otherwise = error $ T.unpack $
      "Expecting one of " <> T.intercalate "/" trues <> " or " <> T.intercalate "/" falses
  where
    trues = ["t", "true", "yes", "on", "enable", "enabled"]
    falses = ["f", "false", "no", "off", "disable", "disabled"]
    v = T.toLower $ T.strip txt
