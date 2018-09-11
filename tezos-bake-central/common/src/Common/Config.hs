{-# LANGUAGE OverloadedStrings #-}
module Common.Config where

import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.URI as Uri
import Text.URI (URI)
import System.IO.Unsafe (unsafePerformIO)

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
chain = "network"

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

serveNodeCache :: FilePath
serveNodeCache = "serve-node-cache"

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

parseURIUnsafe :: Text -> URI
parseURIUnsafe = unsafePerformIO . Uri.mkURI -- (mkURI is MonadThrow m => Text -> m URI;  we want it to just actually throw.

tzscanApiUri :: FilePath
tzscanApiUri = "tzscan-api-uri"
blockscaleApiUri :: FilePath
blockscaleApiUri = "blockscale-api-uri"
obsidianApiUri :: FilePath
obsidianApiUri = "obsidian-api-uri"
