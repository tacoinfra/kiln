{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend.Amendment where

import Control.Monad.Fix (MonadFix)
import Data.List (sortOn)
import Data.Ord (Down (..))
import GHCJS.DOM.Types (MonadJSM)
import Obelisk.Generated.Static (static)
import Reflex.Dom.Core
import Rhyolite.Frontend.App (MonadRhyoliteFrontendWidget)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Time as Time

import Tezos.Types

import Common.App
import Common.Schema hiding (Event)
import ExtraPrelude
import Frontend.Common
import Frontend.Watch

currentCyclePosition :: Amendment -> ProtoInfo -> Cycle
currentCyclePosition a info = fromIntegral $ _amendment_position a `div` _protoInfo_blocksPerCycle info + 1

cyclesPerPeriod :: ProtoInfo -> Cycle
cyclesPerPeriod info = fromIntegral $ _protoInfo_blocksPerVotingPeriod info `div` _protoInfo_blocksPerCycle info

textPeriod :: VotingPeriodKind -> Text
textPeriod = \case
  VotingPeriodKind_Proposal -> "Proposal"
  VotingPeriodKind_TestingVote -> "Exploration"
  VotingPeriodKind_Testing -> "Testing"
  VotingPeriodKind_PromotionVote -> "Promotion"

periodHasVote :: VotingPeriodKind -> Bool
periodHasVote = \case
  VotingPeriodKind_Proposal -> True
  VotingPeriodKind_TestingVote -> True
  VotingPeriodKind_Testing -> False
  VotingPeriodKind_PromotionVote -> True

amendmentPopup
  :: (MonadReader r m, HasTimeZone r, DomBuilder t m, MonadJSM (Performable m), MonadRhyoliteFrontendWidget Bake t m)
  => Dynamic t Amendment
  -- ^ The current period
  -> Dynamic t (Map.Map VotingPeriodKind Amendment)
  -- ^ All periods we know about (past + current)
  -> Dynamic t ProtocolIndex
  -- ^ Protocol information
  -> m ()
amendmentPopup amendment amendments knownProto = divClass "amendment-popup" $ do
  rec
    chosenPeriod <- holdDyn Nothing $ Just <$> choosePeriod
    selectedPeriod <- holdUniqDyn $ fromMaybe . _amendment_period <$> amendment <*> chosenPeriod
    let selectedPeriodDemux = demux selectedPeriod
    choosePeriod <- divClass "menu" $ do
      e <- fmap leftmost $ for periods $ \p -> do
        let selected = demuxed selectedPeriodDemux p
            enabled = (p <=) . _amendment_period <$> amendment
            itemConf = ffor2 enabled selected $ \e s -> "class" =: T.unwords
              (catMaybes [Just "item", "active" <$ guard s, "disabled" <$ guard (not e)])
        (e, _) <- elDynAttr' "div" itemConf $ do
          divClass "title" $ do
            text $ textPeriod p <> " Period"
            when (periodHasVote p) $ elClass "i" "blue icon-vote-badge icon" blank
          divClass "date" $ do
            tz <- asks (^. timeZone)
            let startTime = getStartTimeForPeriod p <$> amendment <*> amendments <*> protoInfo
                endTime = getEndTimeForPeriod p <$> amendment <*> amendments <*> protoInfo
                dateOnly = "%b %d, %Y"
                showDate (t, isEstimated) = T.concat
                  [ T.pack $ Time.formatTime Time.defaultTimeLocale dateOnly $ Time.utcToZonedTime tz t
                  , if isEstimated then "*" else ""
                  ]
            dynText $ showDate <$> startTime
            text " - "
            dynText $ showDate <$> endTime
          uncurry progressDots $ splitDynPure $ ffor2 amendment protoInfo $ \a i -> case compare p (_amendment_period a) of
            LT -> (cyclesPerPeriod i + 1, cyclesPerPeriod i)
            EQ -> (currentCyclePosition a i, cyclesPerPeriod i)
            GT -> (0, cyclesPerPeriod i)
          elAttr "img" ("class" =: "arrow" <> "src" =: static @"images/angle-right.svg") blank
        pure $ p <$ gate (current enabled) (domEvent Click e)
      divClass "estimated-date" $ text "* Estimated date."
      pure e

  divClass "detail" $ do
    divClass "period" $ do
      dynText $ textPeriod <$> selectedPeriod
      text " Period"
      elClass "span" "cycles" $ do
        text "Cycles "
        dynText $ ffor3 selectedPeriod amendment protoInfo $ \p a info ->
          let coeff = fromIntegral $ fromEnum p - fromEnum (_amendment_period a)
              calcCycle n = fromIntegral $ (_amendment_startLevel a + _protoInfo_blocksPerVotingPeriod info * n) `div` _protoInfo_blocksPerCycle info
           in textWithCommas (calcCycle coeff) <> " - " <> textWithCommas (calcCycle (succ coeff) - 1)
    dyn_ $ ffor selectedPeriod $ \case
      VotingPeriodKind_Proposal -> withLoader periodProposals =<< watchProposals
      VotingPeriodKind_TestingVote -> withLoader (periodVote "Test Period" . fmap _periodTestingVote_periodVote) =<< watchPeriodTestingVote
      VotingPeriodKind_Testing -> withLoader periodTest =<< watchPeriodTesting
      VotingPeriodKind_PromotionVote -> withLoader (periodVote "mainnet" . fmap _periodPromotionVote_periodVote) =<< watchPeriodPromotionVote

  pure ()
  where
    periods = [VotingPeriodKind_Proposal, VotingPeriodKind_TestingVote, VotingPeriodKind_Testing, VotingPeriodKind_PromotionVote]
    protoInfo = view protocolIndex_constants <$> knownProto

-- | Display a natural number with comma separation
textWithCommas :: Int -> Text
textWithCommas = T.pack . reverse . f . reverse . show
  where f = \case
          (a0 : a1 : a2 : as) | as /= [] -> a0 : a1 : a2 : ',' : f as
          as -> as

withLoader
  :: (Reflex t, MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
  => (Dynamic t a -> m ()) -> Dynamic t (Maybe a) -> m ()
withLoader f d = maybeDyn d >>= \m -> dyn_ $ ffor m $ \case
  Nothing -> divClass "ui active loader" blank
  Just a -> f a

-- TODO: do we display *all* proposals? what to display when there are no proposals?
periodProposals
  :: (DomBuilder t m, MonadFix m, PostBuild t m, MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m))
  => Dynamic t [PeriodProposal] -> m ()
periodProposals proposals = el "table" $ do
  el "thead" $ do
    el "tr" $ do
      el "th" $ text "Proposal Hash"
      el "th" $ text "Votes"
  el "tbody" $ void $ simpleList (sortOn (Down . _periodProposal_votes) <$> proposals) $ \proposal -> el "tr" $ do
    el "td" $ do
      let protoHash = toBase58Text . _periodProposal_hash <$> proposal
      copyButton $ current protoHash
      dynText protoHash
    el "td" $ dynText $ textWithCommas . _periodProposal_votes <$> proposal

periodTest
  :: forall t m. (DomBuilder t m, MonadJSM (Performable m), PostBuild t m, MonadFix m, PerformEvent t m, TriggerEvent t m, MonadHold t m)
  => Dynamic t PeriodTesting -> m ()
periodTest test = el "dl" $ do
  el "dt" $ text "Proposal Hash"
  el "dd" $ do
    let proposalHash = toBase58Text . _periodTesting_proposal <$> test
    copyButton $ current proposalHash
    dynText proposalHash
  mChain <- maybeDyn $ _periodTesting_testChainId <$> test
  whenJustDyn mChain $ \chainId -> do
    el "dt" $ text "Chain ID"
    el "dd" $ dynText $ toBase58Text <$> chainId
  mBlockLevel <- maybeDyn $ _periodTesting_startingLevel <$> test
  whenJustDyn mBlockLevel $ \lvl -> do
    el "dt" $ text "Starting Block Level"
    el "dd" $ dynText $ textWithCommas . fromIntegral . unRawLevel <$> lvl
  el "dt" $ text "Chain Status"
  el "dd" $ dynText $ ffor test $ \t -> case _periodTesting_status t of
    TestChainStatus_Running -> "Running"
    TestChainStatus_Forking -> "Forking"
    TestChainStatus_NotRunning -> "Not yet started"

periodVote
  :: forall t m. (DomBuilder t m, MonadJSM (Performable m), PostBuild t m, MonadFix m, PerformEvent t m, TriggerEvent t m, MonadHold t m)
  => Text -> Dynamic t PeriodVote -> m ()
periodVote promote vote = el "dl" $ do
  el "dt" $ text "Proposal Hash"
  el "dd" $ do
    let proposalHash = toBase58Text . _periodVote_proposal <$> vote
    copyButton $ current proposalHash
    dynText proposalHash
  el "dt" $ text $ "Promote to " <> promote <> " Vote Breakdown"
  el "dd" $ el "table" $ el "tbody" $ el "tr" $ do
    el "td" $ do
      dynText $ textWithCommas . _ballots_yay . _periodVote_ballots <$> vote
      text " Yea"
    el "td" $ do
      dynText $ textWithCommas . _ballots_nay . _periodVote_ballots <$> vote
      text " Nay"
    el "td" $ do
      dynText $ textWithCommas . _ballots_pass . _periodVote_ballots <$> vote
      text " Pass"
  el "dt" $ text "Supermajority Needed | Current"
  let indicator dv dx = elDynAttr "span" (ffor2 dv dx $ \v x -> "class" =: (if v >= x then "positive" else "negative"))
  el "dd" $ do
    let required = 8000
    text $ intPercentage required
    text " | "
    let supermajority = ffor vote $ \pv ->
          let v = _periodVote_ballots pv
              total = _ballots_yay v + _ballots_nay v
          in if total <= 0 then 0 else (10000 * _ballots_yay v) `div` total
    indicator supermajority (pure required) $ dynText $ intPercentage <$> supermajority
  el "dt" $ text "Quorum Needed | Current"
  el "dd" $ do
    let quorum = fromIntegral . _periodVote_quorum <$> vote
    dynText $ intPercentage <$> quorum
    text " | "
    let participation = ffor vote $ \pv ->
          let v = _periodVote_ballots pv
          in if _periodVote_totalRolls pv <= 0 then 0 else (10000 * (_ballots_yay v + _ballots_nay v + _ballots_pass v)) `div` _periodVote_totalRolls pv
    indicator participation quorum $ dynText $ intPercentage <$> participation

-- | Display tezos style 'Int' percentages (e.g. 5500) as percentages (55.00%)
intPercentage :: Int -> Text
intPercentage i = tshow wholes <> "." <> T.drop 1 (tshow $ 100 + decimals) <> "%"
  where (wholes, decimals) = i `divMod` 100

-- | Get or estimate the start time of a period. Return 'Bool' indicates if the date is estimated
getStartTimeForPeriod :: VotingPeriodKind -> Amendment -> Map.Map VotingPeriodKind Amendment -> ProtoInfo -> (Time.UTCTime, Bool)
getStartTimeForPeriod p a as proto
  | Just a' <- Map.lookup p as = (_amendment_start a', False)
  | otherwise = (estimate, True)
  where estimate = Time.addUTCTime (timeBetweenBlocks * blocksPerPeriod * periodDiff) (_amendment_start a)
        blocksPerPeriod = fromIntegral $ _protoInfo_blocksPerVotingPeriod proto
        timeBetweenBlocks = calcTimeBetweenBlocks proto
        periodDiff = fromIntegral $ fromEnum p - fromEnum (_amendment_period a)

-- | Get or estimate the end time of a period. Return 'Bool' indicates if the date is estimated
getEndTimeForPeriod :: VotingPeriodKind -> Amendment -> Map.Map VotingPeriodKind Amendment -> ProtoInfo -> (Time.UTCTime, Bool)
getEndTimeForPeriod p a as proto
  | Just p' <- safeSucc p, Just a' <- Map.lookup p' as = (_amendment_start a', False)
  | otherwise = (estimate, True)
  where estimate = Time.addUTCTime (timeBetweenBlocks * blocksPerPeriod * periodDiff) (_amendment_start a)
        blocksPerPeriod = fromIntegral $ _protoInfo_blocksPerVotingPeriod proto
        timeBetweenBlocks = calcTimeBetweenBlocks proto
        periodDiff = fromIntegral $ fromEnum p - fromEnum (_amendment_period a) + 1

safeSucc :: (Eq a, Enum a, Bounded a) => a -> Maybe a
safeSucc a = if a /= maxBound then Just (succ a) else Nothing

-- | Display green progress dots for given cycles
progressDots
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t Cycle
  -- ^ Current cycle
  -> Dynamic t Cycle
  -- ^ Total cycles
  -> m ()
progressDots currentCycle' maxCycle' = do
  currentCycle <- holdUniqDyn currentCycle'
  maxCycle <- holdUniqDyn maxCycle'
  divClass "progress" $ dyn_ $ ffor maxCycle $ \mc -> for_ [1..mc] $ \m -> elDynAttr "i" (("class" =:) . f m <$> currentCycle) blank
  where
    f m x = case x `compare` m of
      GT -> "completed"
      EQ -> "current"
      LT -> "upcoming"
