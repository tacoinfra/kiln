{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Tezos.Chain where

import Control.Monad ((<=<))
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Tezos.Base58Check (ChainId, HashBase58Error, fromBase58, toBase58Text)

data NamedChain
  = NamedChain_Mainnet
  | NamedChain_Alphanet
  | NamedChain_Zeronet
  deriving (Eq, Ord, Bounded, Enum, Generic, Typeable, Read, Show)
instance FromJSON NamedChain
instance ToJSON NamedChain

showNamedChain :: NamedChain -> Text
showNamedChain = \case
  NamedChain_Zeronet -> "zeronet"
  NamedChain_Alphanet -> "alphanet"
  NamedChain_Mainnet -> "mainnet"

parseNamedChain :: Text -> Maybe NamedChain
parseNamedChain x = case T.toLower x of
  "zeronet" -> Just NamedChain_Zeronet
  "alphanet" -> Just NamedChain_Alphanet
  "betanet" -> Just NamedChain_Mainnet
  "mainnet" -> Just NamedChain_Mainnet
  _ -> Nothing

showChain :: Either NamedChain ChainId -> Text
showChain = either showNamedChain toBase58Text

parseChain :: MonadError Text m => Text -> m (Either NamedChain ChainId)
parseChain x = case parseNamedChain x of
  Nothing -> either (throwError . T.pack . show) (pure . Right) (fromBase58 $ T.encodeUtf8 x)
  Just n -> pure $ Left n

mainnetChainId :: ChainId
mainnetChainId = "NetXdQprcVkpaWU"

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
