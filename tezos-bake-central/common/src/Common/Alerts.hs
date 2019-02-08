{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Alerts where

import Data.Aeson
import Data.Dependent.Sum (DSum(..))
import Data.Foldable (sequenceA_)
import Data.String (IsString(..))
import qualified Data.Text as T
import Rhyolite.Schema (Json (..))

import Tezos.Chain (NamedChain, showNamedChain)
import Tezos.Types (BlockHash, BlockLike (..), Cycle(..), RawLevel (..))
import Reflex (FunctorMaybe, ffilter)

import Common.Schema
import ExtraPrelude

data AlertsFilter = AlertsFilter_All | AlertsFilter_UnresolvedOnly | AlertsFilter_ResolvedOnly
  deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Generic)

instance FromJSON AlertsFilter
instance FromJSONKey AlertsFilter
instance ToJSON AlertsFilter
instance ToJSONKey AlertsFilter

alertsFilter :: FunctorMaybe f => (a -> ErrorLog) -> AlertsFilter -> f a -> f a
alertsFilter f = \case
  AlertsFilter_All -> id
  AlertsFilter_UnresolvedOnly -> ffilter (isNothing . _errorLog_stopped . f)
  AlertsFilter_ResolvedOnly -> ffilter (isJust . _errorLog_stopped . f)

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

data ErrorDescription
  = ErrorDescription_Plain Text
  | ErrorDescription_Emphasis Text
  | ErrorDescription_Concat ErrorDescription ErrorDescription
  deriving (Eq, Ord)

instance IsString ErrorDescription where
  fromString = ErrorDescription_Plain . T.pack

instance Semigroup ErrorDescription where
  (<>) = ErrorDescription_Concat

plaintextErrorDescription :: ErrorDescription -> Text
plaintextErrorDescription = \case
  ErrorDescription_Plain t -> t
  ErrorDescription_Emphasis t -> t
  ErrorDescription_Concat t t' -> ((<>) `on` plaintextErrorDescription) t t'

errorVar :: Text -> ErrorDescription
errorVar = ErrorDescription_Emphasis

errorPlain :: Text -> ErrorDescription
errorPlain = ErrorDescription_Plain

data BakerErrorDescriptions = BakerErrorDescriptions
  { _bakerErrorDescriptions_title :: !Text
  , _bakerErrorDescriptions_tile :: !Text
  , _bakerErrorDescriptions_notification :: !Text
  , _bakerErrorDescriptions_problem :: !ErrorDescription
  , _bakerErrorDescriptions_warning :: !(Maybe Text)
  , _bakerErrorDescriptions_fix :: !Text
  , _bakerErrorDescriptions_resolved :: !(Baker -> (Text, Text))
  , _bakerErrorDescriptions_userResolvable :: !(Maybe (DSum LogTag Identity))
  }

bakerDeactivationRiskDescriptions :: ErrorLogBakerDeactivationRisk -> BakerErrorDescriptions
bakerDeactivationRiskDescriptions elog = BakerErrorDescriptions
  { _bakerErrorDescriptions_title = "Baker will be marked as inactive."
  , _bakerErrorDescriptions_tile = "Will be marked as inactive."
  , _bakerErrorDescriptions_notification = "This baker address has not had any activity on the blockchain for almost " <> tshow preserved <> " cycles and will soon be marked as inactive."
  , _bakerErrorDescriptions_problem = "In the past " <> errorVar (tshow $ preserved - 1) <> " cycles this baker has not signed any blocks or endorsements, or received any deposits. It will be marked as inactive by the network at the end of this cycle if none of these events occur."
  , _bakerErrorDescriptions_warning = Just $ "Once marked as inactive this baker will not receive any new baking or endorsing rights until " <> tshow (preserved + 2) <> " cycles after it is re-registered and will not be able to sign previously assigned blocks or endorsements."
  , _bakerErrorDescriptions_fix = "If this baker signs a block or endorsement, or receives a minimum deposit of 1µꜩ this cycle it will not be marked as inactive"
  , _bakerErrorDescriptions_resolved = \b ->
      let (primary, secondary) = bakerIdentification b
      in ("Resolved: Baker no longer at risk of being marked as inactive."
         , "Baker " <> primary <> maybe "" (" at " <>) secondary <> " is no longer at risk of being marked as inactive."
         )
  , _bakerErrorDescriptions_userResolvable = Nothing
  }
  where
    preserved = unCycle $ _errorLogBakerDeactivationRisk_preservedCycles elog

bakerDeactivatedDescriptions :: ErrorLogBakerDeactivated -> BakerErrorDescriptions
bakerDeactivatedDescriptions elog = BakerErrorDescriptions
  { _bakerErrorDescriptions_title = "Baker has been marked as inactive."
  , _bakerErrorDescriptions_tile = "Has been marked as inactive."
  , _bakerErrorDescriptions_notification = "This baker has not had any activity on the blockchain for " <> tshow preserved <> " cycles and has been marked as inactive."
  , _bakerErrorDescriptions_problem = "This baker has not had any activity for " <> errorPlain (tshow preserved) <> " cycles, causing it to be marked as inactive. Inactive bakers cannot sign blocks or endorsements and they no longer receive baking and endorsing rights."
  , _bakerErrorDescriptions_warning = Nothing
  , _bakerErrorDescriptions_fix = "Re-register this baker."
  , _bakerErrorDescriptions_resolved = \b ->
      let (primary, secondary) = bakerIdentification b
      in ("Resolved: Baker no longer inactive"
         , "Baker " <> primary <> maybe "" (" at " <>) secondary <> " has been re-registered. The earliest signing operation may be assigned to this baker is " <> tshow (preserved + 2) <> " cycles."
         )
  , _bakerErrorDescriptions_userResolvable = Nothing
  }
  where
    preserved = unCycle $ _errorLogBakerDeactivated_preservedCycles elog

bakerMissedDescriptions :: ErrorLogBakerMissed -> BakerErrorDescriptions
bakerMissedDescriptions elog = BakerErrorDescriptions
  { _bakerErrorDescriptions_title = "Baker missed " <> aRight
  , _bakerErrorDescriptions_tile = "Missed " <> aRight
  , _bakerErrorDescriptions_notification = "This baker missed its chance " <> toRight <> " block level " <> lvl <> "."
  , _bakerErrorDescriptions_problem = "This baker missed its chance " <> errorPlain toRight <> errorVar (" block level " <> lvl) <> "."
  , _bakerErrorDescriptions_warning = Nothing
  , _bakerErrorDescriptions_fix = "Baker and node logs may provide additional insight as to why this happened"
  , _bakerErrorDescriptions_resolved = const ("Dismissed", "Dismissed")
  , _bakerErrorDescriptions_userResolvable = Just $ LogTag_Baker BakerLogTag_BakerMissed :=> pure elog
  }
  where
    lvl = tshow $ unRawLevel $ _errorLogBakerMissed_level elog
    (aRight, toRight) = case _errorLogBakerMissed_right elog of
      RightKind_Baking -> ("a bake", "to bake")
      RightKind_Endorsing -> ("an endorsement", "to endorse")

-- Skip the final sentence as there is no easy way to abstract over doing or not
-- doing the link.
networkUpdateDescription :: NamedChain -> (Text, Text)
networkUpdateDescription namedChain = (,)
  ("New Tezos '" <> name <> "' software version.")
  (mconcat
    [ "There is a new version of the ", name
    , " software available on GitLab. To find further information about this release check Obsidian's Baker Slack channel, the Tezos Riot chat, or other social channels."
    ])
  where name = showNamedChain namedChain
