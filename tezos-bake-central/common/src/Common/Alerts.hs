{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Common.Alerts where

import Prelude hiding (cycle)
import Data.Aeson
import Data.Foldable (sequenceA_)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time (UTCTime, TimeZone)
import Data.Witherable (Filterable)
import Rhyolite.Schema (Json (..))

import Tezos.Chain (NamedChain, showNamedChain)
import Tezos.Types (BlockHash, BlockLike (..), Cycle(..), RawLevel (..), VotingPeriodKind(..))
import Reflex (ffilter)

import Common (nominalDiffTimeToSeconds)
import Common.Schema
import ExtraPrelude

data AlertsFilter = AlertsFilter_All | AlertsFilter_UnresolvedOnly | AlertsFilter_ResolvedOnly
  deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Generic)

instance FromJSON AlertsFilter
instance FromJSONKey AlertsFilter
instance ToJSON AlertsFilter
instance ToJSONKey AlertsFilter

standardTimeFormat :: String
standardTimeFormat = "%A, %b %-d, %Y @ %-l:%M%P %Z"

alertsFilter :: Filterable f => (a -> ErrorLog) -> AlertsFilter -> f a -> f a
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

errorEmphasis :: Text -> ErrorDescription
errorEmphasis = ErrorDescription_Emphasis

errorPlain :: Text -> ErrorDescription
errorPlain = ErrorDescription_Plain

data BakerErrorDescriptions = BakerErrorDescriptions
  { _bakerErrorDescriptions_title :: !Text
  , _bakerErrorDescriptions_tile :: !Text
  , _bakerErrorDescriptions_notification :: !Text
  , _bakerErrorDescriptions_problem :: ![ ErrorDescription ]
  , _bakerErrorDescriptions_warning :: !(Maybe Text)
  , _bakerErrorDescriptions_fix :: !Text
  , _bakerErrorDescriptions_resolved :: !(Baker -> (Text, Text))
  }

data ErrorLogMessage = ErrorLogMessage
  { _errorLogMessage_resolved :: Bool
  , _errorLogMessage_subject :: Text
  , _errorLogMessage_content :: Text
  }
data ErrorLogWidgets m = ErrorLogWidgets
  { _errorLogWidgets_tile :: m ()
  , _errorLogWidgets_notification :: m ()
  , _errorLogWidgets_banner :: m ()
  }

bakerLedgerDisconnectedDescriptions :: ErrorLogBakerLedgerDisconnected -> BakerErrorDescriptions
bakerLedgerDisconnectedDescriptions _elog = BakerErrorDescriptions
  { _bakerErrorDescriptions_title = "Ledger device is disconnected"
  , _bakerErrorDescriptions_tile = "Disconnected"
  , _bakerErrorDescriptions_notification = "The Ledger device for this baker is disconected."
  , _bakerErrorDescriptions_problem = ["The Ledger device for this baker is disconnected, and so it cannot process any baking or signing activity."]
  , _bakerErrorDescriptions_warning = Nothing
  , _bakerErrorDescriptions_fix = "Make sure the Ledger device is connected to your computer and has the Tezos Baking app open."
  , _bakerErrorDescriptions_resolved = const
     ( "Resolved: The Ledger device has been re-connected."
     , "The Ledger device has been re-connected."
     )
  }

bakerVotingReminderDescriptions :: ErrorLogVotingReminder -> Time.NominalDiffTime -> BakerErrorDescriptions
bakerVotingReminderDescriptions elog periodEndsIn = BakerErrorDescriptions
  { _bakerErrorDescriptions_title = title
  , _bakerErrorDescriptions_tile = "Should vote"
  , _bakerErrorDescriptions_notification = description
  , _bakerErrorDescriptions_problem = []
  , _bakerErrorDescriptions_warning = Nothing
  , _bakerErrorDescriptions_fix = "Open the menu on your Kiln baker tile and click “Vote” to vote."
  , _bakerErrorDescriptions_resolved = const ("Voted", "Voted")
  }
  where
    previouslyVoted = _errorLogVotingReminder_previouslyVoted elog
    periodKind = _errorLogVotingReminder_periodKind elog
    rangeMax = _errorLogVotingReminder_rangeMax elog

    timeLeft
      | periodEndsIn <= 0 = Nothing
      | otherwise = Just $ case nominalDiffTimeToSeconds periodEndsIn `divMod` (60 * 60) of
        (0, m) -> tshow (max 0 m `div` 60) <> " minutes"
        (h, _) -> tshow (max 0 h) <> " hours"

    singleVotePeriod periodName
      | rangeMax > 90 = periodName <> " Period " <> maybe " is over" (" ends in " <>) timeLeft
      | rangeMax > 50 = "You have not yet voted in this " <> periodName <> " Period"
      | otherwise = periodName <> " Period has begun"

    title = case periodKind of
      VotingPeriodKind_Proposal -> state
        where state = if previouslyVoted
                      then "Proposals have been submitted since you last voted"
                      else "Proposals are available for voting"
      VotingPeriodKind_TestingVote -> singleVotePeriod "Exploration"
      VotingPeriodKind_Testing -> "" -- impossible
      VotingPeriodKind_PromotionVote -> singleVotePeriod "Promotion"

    description = case periodKind of
      VotingPeriodKind_Proposal -> maybe
        "The Proposal Period is over."
        (<> " remain before voting closes.")
        timeLeft
      _ -> "Remember to vote!"


bakerDeactivationRiskDescriptions :: ErrorLogBakerDeactivationRisk -> BakerErrorDescriptions
bakerDeactivationRiskDescriptions elog = BakerErrorDescriptions
  { _bakerErrorDescriptions_title = "Baker will be marked as inactive"
  , _bakerErrorDescriptions_tile = "Will be marked as inactive."
  , _bakerErrorDescriptions_notification = "This baker address has not had any activity on the blockchain for almost " <> tshow preserved <> " cycles and will soon be marked as inactive."
  , _bakerErrorDescriptions_problem = ["In the past " <> numCycles (preserved - 1) <> " this baker has not signed any blocks or endorsements, or received any deposits. It will be marked as inactive by the network at the end of this cycle if none of these events occur."]
  , _bakerErrorDescriptions_warning = Just $ "Once marked as inactive this baker will not receive any new baking or endorsing rights until " <> tshow (preserved + 2) <> " cycles after it is re-registered and will not be able to sign previously assigned blocks or endorsements."
  , _bakerErrorDescriptions_fix = "If this baker signs a block or endorsement, or receives a minimum deposit of 1µꜩ this cycle it will not be marked as inactive"
  , _bakerErrorDescriptions_resolved = \b ->
      let (primary, secondary) = bakerIdentification b
      in ("Resolved: Baker no longer at risk of being marked as inactive."
         , "Baker " <> primary <> maybe "" (" at " <>) secondary <> " is no longer at risk of being marked as inactive."
         )
  }
  where
    numCycles 1 = "cycle"
    numCycles n = errorEmphasis (tshow n) <> " cycles"

    preserved = unCycle $ _errorLogBakerDeactivationRisk_preservedCycles elog

bakerDeactivatedDescriptions :: ErrorLogBakerDeactivated -> BakerErrorDescriptions
bakerDeactivatedDescriptions elog = BakerErrorDescriptions
  { _bakerErrorDescriptions_title = "Baker has been marked as inactive"
  , _bakerErrorDescriptions_tile = "Has been marked as inactive."
  , _bakerErrorDescriptions_notification = "This baker has not had any activity on the blockchain for " <> tshow preserved <> " cycles and has been marked as inactive."
  , _bakerErrorDescriptions_problem = [
      "This baker has not had any activity for " <> errorPlain (tshow preserved) <> " cycles, causing it to be marked as inactive. Inactive bakers cannot sign blocks or endorsements and they no longer receive baking and endorsing rights."
      ]
  , _bakerErrorDescriptions_warning = Nothing
  , _bakerErrorDescriptions_fix = "Re-register this baker."
  , _bakerErrorDescriptions_resolved = \b ->
      let (primary, secondary) = bakerIdentification b
      in ("Resolved: Baker no longer inactive"
         , "Baker " <> primary <> maybe "" (" at " <>) secondary <> " has been re-registered. The earliest signing operation may be assigned to this baker is " <> tshow (preserved + 2) <> " cycles."
         )
  }
  where
    preserved = unCycle $ _errorLogBakerDeactivated_preservedCycles elog

bakerMissedDescriptions :: ErrorLogBakerMissed -> BakerErrorDescriptions
bakerMissedDescriptions elog = BakerErrorDescriptions
  { _bakerErrorDescriptions_title = "Baker missed " <> aRight
  , _bakerErrorDescriptions_tile = "Missed " <> aRight <> "."
  , _bakerErrorDescriptions_notification = "This baker failed " <> toRight <> " a block at level " <> lvl <> "."
  , _bakerErrorDescriptions_problem = [
      "This baker missed its chance " <> errorPlain toRight <> errorEmphasis (" block level " <> lvl) <> "."
      ]
  , _bakerErrorDescriptions_warning = Nothing
  , _bakerErrorDescriptions_fix = "Baker and node logs may provide additional insight as to why this happened"
  , _bakerErrorDescriptions_resolved = const ("Dismissed", "Dismissed")
  }
  where
    lvl = tshow $ unRawLevel $ _errorLogBakerMissed_level elog
    (aRight, toRight) = case _errorLogBakerMissed_right elog of
      RightKind_Baking -> ("a bake", "to bake")
      RightKind_Endorsing -> ("an endorsement", "to endorse")

bakerGroupedMissedDescriptions :: TimeZone -> Int -> (RawLevel, UTCTime) -> (RawLevel, UTCTime) -> RightKind -> BakerErrorDescriptions
bakerGroupedMissedDescriptions tz count (fb, ft) (lb, lt) rightKind = BakerErrorDescriptions
  { _bakerErrorDescriptions_title = "Baker missed " <> aRight
  , _bakerErrorDescriptions_tile = "Missed " <> aRight <> "."
  , _bakerErrorDescriptions_notification = "This baker failed " -- TODO: ... failed what
  , _bakerErrorDescriptions_problem =
      [ "This baker has missed " <> errorEmphasis (tshow count <> " " <> opportunity) <> "."
      , "The first " <> theRight <> " missed was for "
        <> errorEmphasis ("block level " <> tshow (unRawLevel fb))
        <> " on " <> errorEmphasis (localTime ft) <> "."
      , "The latest " <> theRight <> " missed was for "
        <> errorEmphasis ("block level " <> tshow (unRawLevel lb))
        <> " on " <> errorEmphasis (localTime lt) <> "."
      ]
  , _bakerErrorDescriptions_warning = Nothing
  , _bakerErrorDescriptions_fix = "Baker and node logs may provide additional insight as to why this happened"
  , _bakerErrorDescriptions_resolved = const ("Dismissed", "Dismissed")
  }
  where
    localTime ts = T.pack $ Time.formatTime Time.defaultTimeLocale standardTimeFormat $ Time.utcToZonedTime tz ts
    (aRight, opportunity, theRight) = case rightKind of
      RightKind_Baking -> ("a bake", "bake opportunities", "bake")
      RightKind_Endorsing -> ("an endorsement", "endorsement operations", "endorsement")

bakerInsufficientFundsDescriptions :: ErrorLogInsufficientFunds -> BakerErrorDescriptions
bakerInsufficientFundsDescriptions _ = BakerErrorDescriptions
  { _bakerErrorDescriptions_title = "Baker staking balance is insufficient to receive rights"
  , _bakerErrorDescriptions_tile = "Insufficient stake to receive rights."
  , _bakerErrorDescriptions_notification = "This baker’s staking balance is less than 1 roll and cannot receive any baking or endorsing rights."
  , _bakerErrorDescriptions_problem = [
      "Bakers receive baking and endorsing rights based on the number of rolls (1 roll = 10,000ꜩ) in their staking balance (the baker’s balance plus any tez delegated to them). This baker’s staking balance is less than one roll and will not receive any baking or endorsing rights."
      ]
  , _bakerErrorDescriptions_warning = Just ""
  , _bakerErrorDescriptions_fix = "Transfer tez or have other accounts delegate their tez to this baker so its staking balance is at least 1 roll."
  , _bakerErrorDescriptions_resolved = \_ ->
      ( "Resolved: Baker has sufficient funds to receive rights"
      , "This baker now has a large enough staking balance to receive baking rights.")
  }

bakerAccusedDescriptions :: ErrorLogBakerAccused -> BakerErrorDescriptions
bakerAccusedDescriptions elog = BakerErrorDescriptions
  { _bakerErrorDescriptions_title = "Baker has been accused of double " <> right
  , _bakerErrorDescriptions_tile = "Accused of double " <> right <> "."
  , _bakerErrorDescriptions_notification =
      plaintextErrorDescription firstParagraph
      <> "\nSecurity deposits and rewards may have been confiscated."
      <> bool "" ("\n\n" <> turnOffShort) accusedInSameCycle
  , _bakerErrorDescriptions_problem = [
      firstParagraph
      , errorEmphasis ("All security deposits and rewards earned in cycle "
                        <> cycle <> upTo <> " not previously confiscated by "
                        <> "prior accusations have been confiscated by the "
                        <> "network.")
      , "Half of the security deposits "
      <> "are delivered to the baker of "
      <> errorEmphasis ("block level " <> accusedLevel)
      <> ". The other half, along with any rewards, are burned."
      , bool "" (errorEmphasis ("This baker may be re-accused for this "
                                 <> "offense (and any new deposits and "
                                 <> "rewards confiscated) for each block or "
                                 <> "endorsement it signs in the remainder "
                                 <> "of cycle " <> cycle <> "."))
                 accusedInSameCycle
      ]
  , _bakerErrorDescriptions_warning = Nothing
  , _bakerErrorDescriptions_fix = bool
      ("Because this accusation was made in a cycle following that in which "
       <> "the double " <> rightI <> " occurred no further tez can be "
       <> "confiscated and it is safe to continue running your baker.")
      turnOffShort
      accusedInSameCycle
  , _bakerErrorDescriptions_resolved = const ("Dismissed", "Dismissed")
  }
  where
    firstParagraph =
      "This baker has been accused of double " <> errorPlain right
      <> " " <> errorEmphasis ("block level " <> lvl)
      <> " in " <> errorEmphasis ("cycle " <> cycle) <> ". The accusation was baked at "
      <> errorEmphasis ("block level " <> accusedLevel) <> ".\n\n"
    turnOffShort =
      "\n\nThis baker should be turned off for the remainder of the cycle to "
      <> "avoid losing deposits and rewards for upcoming rights."
    cycle = tshow $ unCycle $ _errorLogBakerAccused_cycle elog
    lvl = tshow $ unRawLevel $ _errorLogBakerAccused_level elog
    accusedLevel = tshow $ unRawLevel $ _errorLogBakerAccused_accusedLevel elog
    right = case _errorLogBakerAccused_right elog of
      RightKind_Baking -> "baking"
      RightKind_Endorsing -> "endorsement"
    rightI = case _errorLogBakerAccused_right elog of
      RightKind_Baking -> "bake"
      RightKind_Endorsing -> "endorsement"
    upTo = bool "" (" up to block level " <> accusedLevel) accusedInSameCycle
    accusedInSameCycle = liftA2 (==) _errorLogBakerAccused_cycle _errorLogBakerAccused_accusedCycle elog

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
