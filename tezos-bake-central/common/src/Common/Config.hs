{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Common.Config where

import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.String (IsString)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Version (Version)
import qualified Network.URI.Encode as UriEncode
import Text.URI (URI)

import Tezos.Base58Check (HashBase58Error(..))
import Tezos.PublicKeyHash (tryReadPublicKeyHashText)
import Tezos.Types (ChainId, NamedChain (..), PublicKeyHash)

import Common (defaultTezosCompatJsonOptions)
import Common.URI (Port, mkRootUri)
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

enableOsPublicNode :: FilePath
enableOsPublicNode = "enable-obsidian-node"

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

conjList :: Text -> Text -> NE.NonEmpty Text -> Text
conjList comma conj = go
  where
    go xs = case NE.uncons xs of
      (x, Nothing) -> x
      (x, Just (y :| [])) -> x <> conj <> y
      (x, Just xs') -> x <> comma <> go xs'

parseBakerAddr :: Text -> Either Text PublicKeyHash
parseBakerAddr v = do
  unless (T.take 3 v `elem` okPrefixes) $ do
    Left $ (if T.take 3 v == "KT1" then "\"KT1\" addresses cannot bake. Address" else "Baker address") <> " must begin with " <> conjList ", " " or " (NE.map tshow okPrefixes) <> "."
  for_ (T.find (isNothing . flip T.find "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" . (==)) v) $ \ch ->
    Left $ "The character " <> tshow ch <> " is not allowed in a baker address."
  when (T.length v /= 36) $ Left $ "Baker address is too " <> (if T.length v < 36 then "short" else "long") <> " (must be 36 characters)."
  flip first (tryReadPublicKeyHashText v) $ \case
    HashBase58Error_InvalidPrefix {} -> "This address is outside the valid range for " <> T.take 3 v <> " addresses."
    HashBase58Error_BadChecksum {} -> "This address failed the integrity check. Please check that it has been copied correctly."
    e -> "An unknown error happened, please report this as a bug: " <> tshow e
  where
    okPrefixes :: NE.NonEmpty Text
    okPrefixes = "tz1" :| ["tz2", "tz3"]

parseRootURIUnsafe :: Text -> URI
parseRootURIUnsafe = unsafeParse "URI" mkRootUri

tzscanApiUri :: FilePath
tzscanApiUri = "tzscan-api-uri"

blockscaleApiUri :: FilePath
blockscaleApiUri = "blockscale-api-uri"

obsidianApiUri :: FilePath
obsidianApiUri = "obsidian-api-uri"

nodes :: FilePath
nodes = "nodes"

bakers :: FilePath
bakers = "bakers"

parseWithAlias :: (Text -> a) -> Text -> (a, Maybe Text)
parseWithAlias parse txt = case T.breakOn "@" txt of
  (a, T.drop 1 -> b) -> (parse a, if T.null b then Nothing else Just b)

parseNodesUnsafe :: Text -> Map.Map URI (Maybe Text)
parseNodesUnsafe = Map.fromList . parseCommaList (parseWithAlias parseRootURIUnsafe)

parseCommaList :: (Text -> a) -> Text -> [a]
parseCommaList parse = map parse . filter (not . T.null) . map T.strip . T.splitOn ","

parsePublicKeyHashUnsafe :: Text -> PublicKeyHash
parsePublicKeyHashUnsafe = unsafeParse "public key hash" parseBakerAddr

parseBakersUnsafe :: Text -> Map.Map PublicKeyHash (Maybe Text)
parseBakersUnsafe = Map.fromList . parseCommaList (parseWithAlias parsePublicKeyHashUnsafe)

networkGitLabProjectId :: FilePath
networkGitLabProjectId = "network-gitlab-project-id"

kilnNodeNetPort :: FilePath
kilnNodeNetPort = "kiln-node-net-port"

defaultKilnNodeNetPort :: Port
defaultKilnNodeNetPort = 9733

kilnNodeRpcPort :: FilePath
kilnNodeRpcPort = "kiln-node-rpc-port"

defaultKilnNodeRpcPort :: Port
defaultKilnNodeRpcPort = 8733

kilnDataDir :: FilePath
kilnDataDir = "kiln-data-dir"

defaultKilnDataDir :: FilePath
defaultKilnDataDir = "./.kiln"

kilnNodeCustomArgs :: FilePath
kilnNodeCustomArgs = "kiln-node-custom-args"

binaryPaths :: FilePath
binaryPaths = "binary-paths"

singleQuoted :: (IsString a, Semigroup a) => a -> a
singleQuoted s = "'" <> s <> "'"

unsafeParse :: Text -> (Text -> Either Text a) -> Text -> a
unsafeParse name parse txt = either
  (\msg -> error $ T.unpack $ T.intercalate " "
    ["Invalid"
    , name
    , singleQuoted txt <> ":"
    , msg
    ])
  id
  (parse txt)

parsePortUnsafe :: Text -> Port
parsePortUnsafe = unsafeParse "port number" $ \a -> case readMaybe (T.unpack a) of
  Nothing -> Left "Not a port number"
  Just b -> Right b

data FrontendConfig = FrontendConfig
  { _frontendConfig_chain :: !(Either NamedChain ChainId)
  , _frontendConfig_chainId :: !ChainId
  , _frontendConfig_upgradeBranch :: !(Maybe Text)
  , _frontendConfig_appVersion :: !Version
  , _frontendConfig_usingOsPublicNode :: !Bool
  } deriving (Eq, Ord, Show, Generic, Typeable)

class HasFrontendConfig r where
  frontendConfig :: Lens' r FrontendConfig

instance HasFrontendConfig FrontendConfig where
  frontendConfig = id

makeLenses ''FrontendConfig
concat <$> traverse (deriveJSON $ defaultTezosCompatJsonOptions { Aeson.omitNothingFields = True })
  [ 'FrontendConfig
  ]
