{-# LANGUAGE OverloadedStrings #-}

module Backend.Common.TezosRelease where

import Control.Error
import Control.Lens
import Control.Monad

import Data.Aeson
import Data.Aeson.Lens
import Data.Attoparsec.Text hiding (try)
import Data.Ord
import Data.Text

import Common.Schema

getRelease :: AsValue s => Maybe Text -> (Value -> Maybe c) -> s -> Maybe c
getRelease mr f = case mr of
   Nothing ->
       maximumByOf values (comparing $ (^? key "tag_name" . _String) >=> hush . parseMajorMinorVersion) >=> f
   Just release ->
       findOf values ((== Just release) . (^? key "tag_name" . _String)) >=> f

getReleaseTag :: Value -> Maybe MajorMinorVersion
getReleaseTag = (^? key "tag_name" . _String) >=> hush . parseMajorMinorVersion

parseMajorMinorVersion :: Text -> Either String MajorMinorVersion
parseMajorMinorVersion = parseOnly $ do
    option () (choice [skip (== 'v'), void $ string "octez-v"])
    major <- decimal
    skip (== '.')
    minor <- decimal
    extra <- option Nothing $ do
      skip (== '.')
      Just <$> decimal
    endOfInput
    return $ MajorMinorVersion major minor extra Release
