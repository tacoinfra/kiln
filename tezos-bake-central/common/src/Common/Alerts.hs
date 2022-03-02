{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Common.Alerts where

import Prelude hiding (cycle)
import Data.Aeson
import Data.Foldable (sequenceA_)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time (UTCTime, TimeZone)
import Data.Witherable (Filterable)
import Rhyolite.Schema (Json (..))

import Tezos.Types (getMicroTez, BlockHash, BlockLike (..), Cycle(..), RawLevel (..), SyncState(..), Tez(..), VotingPeriodKind(..))
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
  case (_errorLogBadNodeHead_bootstrapped l, _errorLogBadNodeHead_chainStatus l) of
    (False, _) ->
      ( behindHeader
      , sequenceA_ $
        [ text "The node's head is "
        , blockHashLink $ nodeHead ^. hash
        , text $ " at level " <> tshow (unRawLevel $ nodeHead ^. level)
        ] <> maybe [] (\(Json latestHead) ->
        [ text " the latest know head is "
        , blockHashLink $ latestHead ^. hash
        , text $ " at level " <> tshow (unRawLevel $ latestHead ^. level)
        ]) mbLatestHead
      )
    (True, SyncState_Stuck) ->
      ( stuckHeader
      , text "The node considers itself synchronized with its peers but \
             \the chain seems to be halted from its viewpoint"
      )
    (True, SyncState_Unsynced) ->
      ( branchHeader
      , text "The node is not currently synchronized with its peers.\
             \This can mean that node is currently running on a branch."
      )
    _ -> error "Node is bootstrapped and synced, but we're trying to report an error"

  where
    Json nodeHead = _errorLogBadNodeHead_nodeHead l
    mbLatestHead = _errorLogBadNodeHead_latestHead l

    branchHeader = "Node is on a branch"
    behindHeader = "Node is behind"
    stuckHeader = "Node is stuck"

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
  { _bakerErrorDescriptions_title :: Text
  , _bakerErrorDescriptions_tile :: Text
  , _bakerErrorDescriptions_notification :: Text
  , _bakerErrorDescriptions_problem :: [ ErrorDescription ]
  , _bakerErrorDescriptions_warning :: Maybe Text
  , _bakerErrorDescriptions_fix :: Text
  , _bakerErrorDescriptions_resolved :: Baker -> (Text, Text)
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
bakerLedgerDisconnectedDescriptions elog = BakerErrorDescriptions
  { _bakerErrorDescriptions_title = title
  , _bakerErrorDescriptions_tile = title
  , _bakerErrorDescriptions_notification = notification
  , _bakerErrorDescriptions_problem = problem
  , _bakerErrorDescriptions_warning = Nothing
  , _bakerErrorDescriptions_fix = fixMessage
  , _bakerErrorDescriptions_resolved = const
     ( resolved
     , ""
     )
  }
  where
    isWrongApp = elog ^.errorLogBakerLedgerDisconnected_isWrongApp
    title = bool "Ledger Device is disconnected" "Tezos Wallet app is opened on the Ledger Device" isWrongApp
    notification = bool
      "The Ledger Device for this baker is disconected."
      "The Ledger Device for this baker is connected, but the Tezos Wallet app is opened on it."
      isWrongApp
    problem = bool
      ["The Ledger Device that is used by this baker is not connected and will cause this baker to miss any baking or endorsing rights that occur while the device is disconnected."]
      ["The Ledger Device for this baker is connected, but the Tezos Wallet app is opened on it, this will cause this baker to miss any baking or endorsing rights that occur while the Tezos Baking app is not opened."]
      isWrongApp
    fixMessage = bool
      "Make sure the Ledger Device is connected to your computer and has the Tezos Baking app open."
      "Open the Tezos Baking app on the Ledger Device."
      isWrongApp
    resolved = bool
      "The Ledger Device that is used by this baker has been re-connected."
      "Tezos Baking app was opened on the Ledger Device."
      isWrongApp

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
      VotingPeriodKind_Exploration -> singleVotePeriod "Exploration"
      VotingPeriodKind_Cooldown -> "" -- impossible
      VotingPeriodKind_Promotion -> singleVotePeriod "Promotion"
      VotingPeriodKind_Adoption -> singleVotePeriod "Adoption"

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

bakerMissedEndorsementBonusDescriptions
  :: ErrorLogBakerMissedEndorsementBonus
  -> BakerErrorDescriptions
bakerMissedEndorsementBonusDescriptions elog =
  BakerErrorDescriptions
  { _bakerErrorDescriptions_title = "Baker missed the endorsement bonus"
  , _bakerErrorDescriptions_tile = "Missed endorsement bonus"
  , _bakerErrorDescriptions_notification = "This baker missed the endorsement bonus for the block at level " <> lvl <> "."
  , _bakerErrorDescriptions_problem = [
      "This baker failed to get the bonus for including extra endorsements for the " <> errorEmphasis (" block at level " <> lvl ) <> "."
    ]
  , _bakerErrorDescriptions_warning = Nothing
  , _bakerErrorDescriptions_fix = "Baker and node logs may provide additional insight as to why this happened"
  , _bakerErrorDescriptions_resolved = const ("Dismissed", "Dismissed")
  }
  where
    lvl = tshow $ unRawLevel $ _errorLogBakerMissedEndorsementBonus_level elog

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

bakerInsufficientFundsDescriptions :: Maybe Tez -> ErrorLogInsufficientFunds -> BakerErrorDescriptions
bakerInsufficientFundsDescriptions mTokensPerRoll _ = BakerErrorDescriptions
    { _bakerErrorDescriptions_title = "Baker staking balance is insufficient to receive rights"
    , _bakerErrorDescriptions_tile = "Insufficient stake to receive rights."
    , _bakerErrorDescriptions_notification = "This baker’s staking balance is less than 1 roll and cannot receive any baking or endorsing rights."
    , _bakerErrorDescriptions_problem = [
        "Bakers receive baking and endorsing rights based on the number of rolls (1 roll = " <> roll <> "ꜩ) in their staking balance (the baker’s balance plus any tez delegated to them). This baker’s staking balance is less than one roll and will not receive any baking or endorsing rights."
        ]
    , _bakerErrorDescriptions_warning = Just ""
    , _bakerErrorDescriptions_fix = "Transfer tez or have other accounts delegate their tez to this baker so its staking balance is at least 1 roll."
    , _bakerErrorDescriptions_resolved = \_ ->
        ( "Resolved: Baker has sufficient funds to receive rights"
        , "This baker now has a large enough staking balance to receive baking rights.")
    }
  where
    roll = fromString $ formatCommas $ maybe 8000 ((`div` 1000000) . getMicroTez) mTokensPerRoll
    formatCommas = reverse . intercalate "," . chunksOf 3 . reverse . show

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
    right = case _errorLogBakerAccused_accusationType elog of
      AccusationType_DoubleBake -> "baking"
      AccusationType_DoubleEndorsement -> "endorsement"
      AccusationType_DoublePreendorsement -> "preendorsement"
    rightI = case _errorLogBakerAccused_accusationType elog of
      AccusationType_DoubleBake -> "bake"
      AccusationType_DoubleEndorsement -> "endorsement"
      AccusationType_DoublePreendorsement -> "preendorsement"
    upTo = bool "" (" up to block level " <> accusedLevel) accusedInSameCycle
    accusedInSameCycle = liftA2 (==) _errorLogBakerAccused_cycle _errorLogBakerAccused_accusedCycle elog

-- Skip the final sentence as there is no easy way to abstract over doing or not
-- doing the link.
networkUpdateDescription :: (Text, Text)
networkUpdateDescription = (,)
  "New Tezos software version."
  (mconcat
    [ "There is a new version of the "
    , " software available on GitLab. To find further information about this release, check the Tezos Baking Slack channel, the Tezos Riot chat, or other social channels."
    ])
