{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Config where

import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Version (Version)
import qualified Network.URI.Encode as UriEncode
import Text.URI as Uri
import Text.URI (URI)

import Tezos.Types (ChainId, NamedChain (..))

import Common (defaultTezosCompatJsonOptions)
import ExtraPrelude

changelogUrl :: Text -> Text
changelogUrl branch = "https://gitlab.com/obsidian.systems/tezos-bake-monitor/tree/" <> UriEncode.encodeText branch <> "/CHANGELOG.md"

pgConnectionString :: FilePath
pgConnectionString = "pg-connection"

db :: FilePath
db = "db"

route :: FilePath
route = "route"

emailFromAddress :: FilePath
emailFromAddress = "email-from"

chain :: FilePath
chain = "network"

defaultChain :: Either NamedChain ChainId
defaultChain = Left NamedChain_Mainnet

checkForUpgrade :: FilePath
checkForUpgrade = "check-for-upgrade"

checkForUpgradeDefault :: Bool
checkForUpgradeDefault = True

upgradeBranch :: FilePath
upgradeBranch = "upgrade-branch"

upgradeBranchDefault :: Text
upgradeBranchDefault = "master"

networkGitLabProjectIdDefault :: Text
networkGitLabProjectIdDefault = "3836952"

serveNodeCache :: FilePath
serveNodeCache = "serve-node-cache"

parseBool :: Text -> Bool
parseBool txt
  | v `elem` trues = True
  | v `elem` falses = False
  | otherwise = error $ T.unpack $
      "Can't parse '" <> txt <> "': Expecting one of " <> T.intercalate "/" trues <> " or " <> T.intercalate "/" falses
  where
    trues = ["t", "true", "y", "yes", "on", "enable", "enabled"]
    falses = ["f", "false", "n", "no", "off", "disable", "disabled"]
    v = T.toLower $ T.strip txt

parseURIUnsafe :: Text -> URI
parseURIUnsafe uri = either (\msg -> error $ "Invalid URI '" <> T.unpack uri <> "': " <> show msg) id $ Uri.mkURI uri

tzscanApiUri :: FilePath
tzscanApiUri = "tzscan-api-uri"

blockscaleApiUri :: FilePath
blockscaleApiUri = "blockscale-api-uri"

obsidianApiUri :: FilePath
obsidianApiUri = "obsidian-api-uri"


nodes :: FilePath
nodes = "nodes"

parseNodes :: Text -> Set URI
parseNodes = Set.fromList . map parseURIUnsafe . filter (not . T.null) . map T.strip . T.splitOn ","

networkGitLabProjectId :: FilePath
networkGitLabProjectId = "network-gitlab-project-id"

data FrontendConfig = FrontendConfig
  { _frontendConfig_chain :: !(Either NamedChain ChainId)
  , _frontendConfig_chainId :: !ChainId
  , _frontendConfig_upgradeBranch :: !(Maybe Text)
  , _frontendConfig_appVersion :: !Version
  } deriving (Eq, Ord, Show, Generic, Typeable)

class HasFrontendConfig r where
  frontendConfig :: Lens' r FrontendConfig

instance HasFrontendConfig FrontendConfig where
  frontendConfig = id

makeLenses ''FrontendConfig
concat <$> traverse (deriveJSON $ defaultTezosCompatJsonOptions { Aeson.omitNothingFields = True })
  [ 'FrontendConfig
  ]
