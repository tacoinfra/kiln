{-# LANGUAGE OverloadedStrings #-}
module Common.Config where

import Data.Text (Text)

changelogUrl :: Text
changelogUrl = "https://gitlab.com/obsidian.systems/tezos-bake-monitor/tree/develop/CHANGELOG.md"

db :: FilePath
db = "db"

route :: FilePath
route = "route"

emailFromAddress :: FilePath
emailFromAddress = "email-from"

blockExplorer :: FilePath
blockExplorer = "block-explorer"

chain :: FilePath
chain = "chain-id"

