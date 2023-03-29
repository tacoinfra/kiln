{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Backend.Process.Errors
  ( BakerBootstrapError (..)
  , ErrorEvent (..)
  , ErrorEventType (..)
  , ErrorTrace (..)
  ) where

import Data.Aeson (FromJSON(..), encode, withObject, (.:))
import Data.Aeson.Types (Object, Parser, Value(..))
import Data.ByteString.Lazy (toStrict)
import qualified Data.HashMap.Lazy as Map
import Data.Text (Text, isPrefixOf, unpack)
import Data.Text.Encoding (decodeUtf8)
import Fmt (Buildable(..), blockListF, (+|), (|+))

import Tezos.Types

data BakerBootstrapError
  = BakerBootstrapError (Maybe ProtocolHash)

instance Buildable BakerBootstrapError where
  build = \case
    BakerBootstrapError mbProto ->
      "tezos-baker is not available for the given protocol: " +| build (maybe "<unknown protocol>" toBase58Text mbProto)

data ErrorTrace
  = ErrorTrace_LedgerNotFound
  | ErrorTrace_LedgerError Text
  | ErrorTrace_Unknown Text

instance Buildable ErrorTrace where
  build = \case
    ErrorTrace_LedgerNotFound -> "Ledger device not found"
    ErrorTrace_LedgerError msg -> "Ledger interaction reported an error: " +| msg |+ ""
    ErrorTrace_Unknown msg -> "Unknown error occured: " +| msg |+ ""

data ErrorEventType
  = ErrorEventType_SkippingPreendoresement
  | ErrorEventType_SkippingEndoresement
  | ErrorEventType_FailingToInjectPreendorsement

data ErrorEvent = ErrorEvent
  { _errorEvent_type :: ErrorEventType
  , _errorEvent_delegate :: PublicKeyHash
  , _errorEvent_trace :: [ErrorTrace]
  }

instance Buildable ErrorEvent where
  build ErrorEvent{..} =
    "Delegate " +| toPublicKeyHashText _errorEvent_delegate |+
    eventTypeDescription _errorEvent_type +| " due to:\n" +| blockListF _errorEvent_trace |+ ""
    where
      eventTypeDescription = \case
        ErrorEventType_SkippingPreendoresement -> "skipped preendorsement"
        ErrorEventType_SkippingEndoresement -> "skipped endorsement"
        ErrorEventType_FailingToInjectPreendorsement -> "failed to inject preendorsement"

instance FromJSON ErrorTrace where
  parseJSON = withObject "errorTrace" $ \o -> do
    errorId :: Text <- o .: "id"
    case errorId of
      "signer.ledger" -> do
        ErrorTrace_LedgerError <$> o .: "ledger-error"
      "failure" -> do
        msg <- o .: "msg"
        case msg of
          x | "Found no ledger corresponding to ledger" `isPrefixOf` x -> pure ErrorTrace_LedgerNotFound
          _ -> pure $ ErrorTrace_Unknown msg
      _ -> pure $ ErrorTrace_Unknown $ decodeUtf8 $ toStrict $ encode o

instance FromJSON ErrorEvent where
  parseJSON j = do
    o <- parseJSON j
    case Map.toList (o :: Object) of
      [("fd-sink-item.v0", Object item)] -> do
        event <- item .: "event"
        parsedEvent <- parseJSON event
        case Map.toList (parsedEvent :: Object) of
          [(eventTag, Object eventItem)] -> do
            eventType <- case eventTag of
              -- See 'level:Error' events from 'Actions' module here
              -- https://gitlab.com/tezos/tezos/-/blob/79b313526e849f079b3ed1c95eca3bc690243396/src/proto_012_Psithaca/lib_delegate/baking_events.ml#L491
              "skipping_preendorsement.v0" -> pure ErrorEventType_SkippingPreendoresement
              "skipping_endorsement.v0" -> pure ErrorEventType_SkippingEndoresement
              "failed_to_inject_preendorsement.v0" -> pure ErrorEventType_FailingToInjectPreendorsement
              _ -> fail $ "ErrorEvent: unexpected event: " <> unpack (decodeUtf8 $ toStrict $ encode o)
            delegate <- parseDelegate eventItem
            trace <- eventItem .: "trace"
            pure $ ErrorEvent eventType delegate trace
          _ -> fail "ErrorEvent: unexpected event structure"
      _ -> fail "ErrorEvent: unexpected toplevel tag"
    where
      parseDelegate :: Object -> Parser PublicKeyHash
      parseDelegate o = do
        (delegate :: Object) <- o .: "delegate"
        delegate .: "public_key_hash"
