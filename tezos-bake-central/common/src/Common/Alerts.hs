{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Common.Alerts where

import Data.Aeson
import Data.Foldable (sequenceA_)
import Rhyolite.Schema (Json (..))

import Tezos.Types (BlockHash, BlockLike (..), RawLevel (..))
import Reflex (FunctorMaybe, ffilter)

import Common.Schema (ErrorLog(..), ErrorLogBadNodeHead (..))
import ExtraPrelude

data AlertsFilter = AlertsFilter_All | AlertsFilter_UnresolvedOnly | AlertsFilter_ResolvedOnly
  deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Generic)

alertsFilter :: FunctorMaybe f => (a -> ErrorLog) -> AlertsFilter -> f a -> f a
alertsFilter f = \case
  AlertsFilter_All -> id
  AlertsFilter_UnresolvedOnly -> ffilter (isNothing . _errorLog_stopped . f)
  AlertsFilter_ResolvedOnly -> ffilter (isJust . _errorLog_stopped . f)

instance FromJSON AlertsFilter
instance FromJSONKey AlertsFilter
instance ToJSON AlertsFilter
instance ToJSONKey AlertsFilter

badNodeHeadMessage
  :: Applicative f
  => (Text -> f ())
  -> (BlockHash -> f ())
  -> ErrorLogBadNodeHead
  -> (Text, f ())
badNodeHeadMessage text blockHashLink l =
  case _errorLogBadNodeHead_lca l of
    Nothing ->
      ( branchHeader
      , sequenceA_
          [ text "The node's head of "
          , blockHashLink $ nodeHead ^. hash
          , text " has no common history with the latest known head of "
          , blockHashLink $ latestHead ^. hash
          , text "."
          ]
      )
    Just (Json lca)
      | levelsBehindNode > 0 ->
          ( branchHeader
          , sequenceA_
              [ text $ "The node is on a branch " <> tshow (unRawLevel levelsBehindNode) <> " blocks long."
              , text " The branch began at "
              , blockHashLink $ lca ^. hash
              , text $ " which is " <> tshow (unRawLevel levelsBehindHead) <> " blocks behind the latest head of "
              , blockHashLink $ latestHead ^. hash
              , text ". (Node's head is "
              , blockHashLink $ nodeHead ^. hash
              , text ")"
              ]
          )
      | otherwise ->
          ( behindHeader
          , sequenceA_
              [ text "The node's head at "
              , blockHashLink $ nodeHead ^. hash
              , text $ " is " <> tshow (unRawLevel levelsBehindHead) <> " blocks behind the latest head of "
              , blockHashLink $ latestHead ^. hash
              , text "."
              ]
          )
      where
        levelsBehindHead = latestHead ^. level - lca ^. level
        levelsBehindNode = nodeHead ^. level - lca ^. level

  where
    Json nodeHead = _errorLogBadNodeHead_nodeHead l
    Json latestHead = _errorLogBadNodeHead_latestHead l

    branchHeader = "Node is on a branch"
    behindHeader = "Node is behind"

