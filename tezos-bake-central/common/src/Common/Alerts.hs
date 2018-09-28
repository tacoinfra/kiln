{-# LANGUAGE OverloadedStrings #-}

module Common.Alerts where

import Control.Lens ((^.))
import Data.Foldable (sequenceA_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Rhyolite.Schema (Json (..))

import Tezos.Types (BlockHash, BlockLike (..), RawLevel (..))

import Common (tshow)
import Common.Schema (ErrorLogBadNodeHead (..))

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

