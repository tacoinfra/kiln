{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Tezos.Common.Chain where

import Control.Monad ((<=<))
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.List (find)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Tezos.Common.Base58Check (ChainId, HashBase58Error, fromBase58, toBase58Text)

data NamedChain
  = NamedChain_Mainnet
  | NamedChain_Ghostnet
  | NamedChain_ParisCnet
  | NamedChain_Quebecnet
  deriving (Eq, Ord, Bounded, Enum, Generic, Typeable, Read, Show)
instance FromJSON NamedChain
instance ToJSON NamedChain

showNamedChain :: NamedChain -> Text
showNamedChain = \case
  NamedChain_Mainnet -> "mainnet"
  NamedChain_Ghostnet -> "ghostnet"
  NamedChain_ParisCnet -> "pariscnet"
  NamedChain_Quebecnet -> "quebecnet"

parseNamedChain :: Text -> Maybe NamedChain
parseNamedChain x = find (\namedChain -> showNamedChain namedChain == T.toLower x)
  [minBound .. maxBound]

showChain :: Either NamedChain ChainId -> Text
showChain = either showNamedChain toBase58Text

parseChain :: MonadError Text m => Text -> m (Either NamedChain ChainId)
parseChain x = case parseNamedChain x of
  Nothing -> either (throwError . T.pack . show) (pure . Right) (fromBase58 $ T.encodeUtf8 x)
  Just n -> pure $ Left n

getNamedChainId :: NamedChain -> Maybe ChainId
getNamedChainId = \case
  NamedChain_Mainnet -> Just "NetXdQprcVkpaWU"
  NamedChain_Ghostnet -> Just "NetXnHfVqm9iesp"
  NamedChain_ParisCnet -> Just "NetXXWAHLEvre9b"
  NamedChain_Quebecnet -> Just "NetXuTeGinLEqxp"

identifyChain :: ChainId -> Maybe NamedChain
identifyChain cid = lookup cid namedChainAssoc
  where
    namedChainAssoc :: [(ChainId, NamedChain)]
    namedChainAssoc = mapMaybe (\nc -> (,nc) <$> getNamedChainId nc) $ enumFrom minBound

data ChainTag
  = ChainTag_Main
  | ChainTag_Test
  | ChainTag_Hash ChainId
  deriving (Eq, Ord, Generic, Typeable, Read, Show)

toChainTagText :: ChainTag -> Text
toChainTagText = \case
  ChainTag_Main -> "main"
  ChainTag_Test -> "test"
  ChainTag_Hash h -> toBase58Text h

parseChainTagText :: MonadError HashBase58Error m => Text -> m ChainTag
parseChainTagText t
  | t == "main" = pure ChainTag_Main
  | t == "test" = pure ChainTag_Test
  | otherwise = either throwError (pure . ChainTag_Hash) $ fromBase58 $ T.encodeUtf8 t

instance ToJSON ChainTag where
  toJSON = toJSON . toChainTagText

instance FromJSON ChainTag where
  parseJSON = either (fail . show) pure <=< (runExceptT . parseChainTagText) <=< parseJSON
