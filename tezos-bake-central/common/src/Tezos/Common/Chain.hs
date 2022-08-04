{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

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
  | NamedChain_Zeronet
  | NamedChain_Babylonnet
  | NamedChain_Carthagenet
  | NamedChain_Delphinet
  | NamedChain_Edonet
  | NamedChain_Edo2net
  | NamedChain_Florencenet
  | NamedChain_Granadanet
  | NamedChain_Hangzhounet
  | NamedChain_Ithacanet
  | NamedChain_Jakartanet
  | NamedChain_Ghostnet
  deriving (Eq, Ord, Bounded, Enum, Generic, Typeable, Read, Show)
instance FromJSON NamedChain
instance ToJSON NamedChain

showNamedChain :: NamedChain -> Text
showNamedChain = \case
  NamedChain_Zeronet -> "zeronet"
  NamedChain_Mainnet -> "mainnet"
  NamedChain_Babylonnet -> "babylonnet"
  NamedChain_Carthagenet -> "carthagenet"
  NamedChain_Delphinet -> "delphinet"
  NamedChain_Edonet -> "edonet"
  NamedChain_Edo2net -> "edo2net"
  NamedChain_Florencenet -> "florencenet"
  NamedChain_Granadanet -> "granadanet"
  NamedChain_Hangzhounet -> "hangzhounet"
  NamedChain_Ithacanet -> "ithacanet"
  NamedChain_Jakartanet -> "jakartanet"
  NamedChain_Ghostnet -> "ghostnet"

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
  NamedChain_Zeronet -> Nothing  -- changes unpredictably each reset
  NamedChain_Babylonnet -> Just "NetXUdfLh6Gm88t"
  NamedChain_Carthagenet -> Just "NetXjD3HPJJjmcd"
  NamedChain_Delphinet -> Just "NetXm8tYqnMWky1"
  NamedChain_Edonet -> Just "NetXdQprcVkpaWU"
  NamedChain_Edo2net -> Just "NetXSgo1ZT2DRUG"
  NamedChain_Florencenet -> Just "NetXxkAx4woPLyu"
  NamedChain_Granadanet -> Just "NetXz969SFaFn8k"
  NamedChain_Hangzhounet -> Just "NetXZSsxBpMQeAT"
  NamedChain_Ithacanet -> Just "NetXnHfVqm9iesp"
  NamedChain_Jakartanet -> Just "NetXLH1uAxK7CCh"
  NamedChain_Ghostnet -> Just "NetXnHfVqm9iesp"

identifyChain :: ChainId -> Maybe NamedChain
identifyChain cid = lookup cid namedChainAssoc
  where
    namedChainAssoc :: [(ChainId, NamedChain)]
    namedChainAssoc = mapMaybe (\x -> fmap (flip (,) x) (getNamedChainId x)) $ enumFrom minBound

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
