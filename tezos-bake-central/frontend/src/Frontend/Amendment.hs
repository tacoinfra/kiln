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
import Rhyolite.Api (public)
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import qualified Data.Text as T
import qualified Data.Time as Time

import Tezos.Types hiding (protocolHash)

import Common.Api
import Common.App
import Common.Config
import Common.Schema
import ExtraPrelude
import Frontend.Common
import Frontend.Watch

currentCyclePosition :: Amendment -> ProtoInfo -> Cycle
currentCyclePosition a info = fromIntegral $ _amendment_position a `div` _protoInfo_blocksPerCycle info + 1

cyclesPerPeriod :: ProtoInfo -> Cycle
cyclesPerPeriod info = fromIntegral $ _protoInfo_blocksPerVotingPeriod info `div` _protoInfo_blocksPerCycle info

textBallot :: Ballot -> Text
textBallot = \case
  Ballot_Yay -> "Yea"
  Ballot_Nay -> "Nay"
  Ballot_Pass -> "Pass"

textPeriod :: VotingPeriodKind -> Text
textPeriod = \case
  VotingPeriodKind_Proposal -> "Proposal"
  VotingPeriodKind_TestingVote -> "Exploration"
  VotingPeriodKind_Testing -> "Testing"
  VotingPeriodKind_PromotionVote -> "Promotion"
  VotingPeriodKind_Adoption -> "Adoption"

isVotingPeriod :: VotingPeriodKind -> Bool
isVotingPeriod = \case
  VotingPeriodKind_Proposal -> True
  VotingPeriodKind_TestingVote -> True
  VotingPeriodKind_Testing -> False
  VotingPeriodKind_PromotionVote -> True
  VotingPeriodKind_Adoption -> False

amendmentPopup
  :: (MonadReader r m, HasTimeZone r, DomBuilder t m, MonadJSM (Performable m), MonadAppWidget t m)
  => Dynamic t Amendment
  -- ^ The current period
  -> Dynamic t (Map.Map VotingPeriodKind Amendment)
  -- ^ All periods we know about (past + current)
  -> Dynamic t ProtoInfo
  -- ^ Protocol information
  -> m ()
amendmentPopup amendment amendments protoInfo = divClass "amendment-popup" $ do
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
            when (isVotingPeriod p) $ elClass "i" "blue icon-vote-badge icon" blank
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
      VotingPeriodKind_Proposal -> periodProposals =<< watchProposals
      VotingPeriodKind_TestingVote -> withLoader (periodVote "Test Period") =<< watchPeriodTestingVote
      VotingPeriodKind_Testing -> withLoader periodTest =<< watchPeriodTesting
      VotingPeriodKind_PromotionVote -> withLoader (periodVote "mainnet") =<< watchPeriodPromotionVote
      VotingPeriodKind_Adoption -> withLoader dyn_  =<< watchPeriodAdoption

  pure ()
  where
    periods = [minBound .. maxBound]

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

periodProposals
  :: (DomBuilder t m, MonadFix m, PostBuild t m, MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m))
  => Dynamic t (Map.Map (Id PeriodProposal) (PeriodProposal, Maybe Bool)) -> m ()
periodProposals proposals' = do
  let proposals = sortOn (Down . _periodProposal_votes . fst) . Map.elems <$> proposals'
  el "table" $ do
    el "thead" $ do
      el "tr" $ do
        el "th" $ text "Proposal Hash"
        el "th" $ text "Votes"
    el "tbody" $ void $ simpleList proposals $ \proposal -> el "tr" $ do
      el "td" $ do
        let protocolHash = toBase58Text . _periodProposal_hash . fst <$> proposal
        copyButton $ current protocolHash
        dynText protocolHash
      el "td" $ dynText $ textWithCommas . _periodProposal_votes . fst <$> proposal
  elDynAttr "div" (ffor proposals $ \ps -> "class" =: ("no-proposals" <> if null ps then "" else " transition hidden")) $ do
    text "No proposals have been submitted for this voting period yet."

periodTest
  :: forall t m. (DomBuilder t m, MonadJSM (Performable m), PostBuild t m, MonadFix m, PerformEvent t m, TriggerEvent t m, MonadHold t m)
  => Dynamic t ((Id PeriodProposal, PeriodProposal), PeriodTesting) -> m ()
periodTest test = el "dl" $ do
  el "dt" $ text "Proposal Hash"
  el "dd" $ do
    let proposalHash = toBase58Text . _periodProposal_hash . snd . fst <$> test
    copyButton $ current proposalHash
    dynText proposalHash
  mBlockLevel <- maybeDyn $ _periodTesting_startingLevel . snd <$> test
  whenJustDyn mBlockLevel $ \lvl -> do
    el "dt" $ text "Starting Block Level"
    el "dd" $ dynText $ textWithCommas . fromIntegral . unRawLevel <$> lvl

periodVote
  :: forall t m. (DomBuilder t m, MonadJSM (Performable m), PostBuild t m, MonadFix m, PerformEvent t m, TriggerEvent t m, MonadHold t m)
  => Text -> Dynamic t ((Id PeriodProposal, PeriodProposal), PeriodVote) -> m ()
periodVote promote vote = el "dl" $ do
  el "dt" $ text "Proposal Hash"
  el "dd" $ do
    let proposalHash = toBase58Text . _periodProposal_hash . snd . fst <$> vote
    copyButton $ current proposalHash
    dynText proposalHash
  el "dt" $ text $ "Promote to " <> promote <> " Vote Breakdown"
  el "dd" $ el "table" $ el "tbody" $ el "tr" $ do
    el "td" $ do
      dynText $ textWithCommas . _ballots_yay . _periodVote_ballots . snd <$> vote
      text " Yea"
    el "td" $ do
      dynText $ textWithCommas . _ballots_nay . _periodVote_ballots . snd <$> vote
      text " Nay"
    el "td" $ do
      dynText $ textWithCommas . _ballots_pass . _periodVote_ballots . snd <$> vote
      text " Pass"
  el "dt" $ text "Supermajority Needed | Current"
  let indicator dv dx = elDynAttr "span" (ffor2 dv dx $ \v x -> "class" =: (if v >= x then "positive" else "negative"))
  el "dd" $ do
    let required = 8000
    text $ intPercentage required
    text " | "
    let supermajority = ffor vote $ \(_, pv) ->
          let v = _periodVote_ballots pv
              total = _ballots_yay v + _ballots_nay v
          in if total <= 0 then 0 else (10000 * _ballots_yay v) `div` total
    indicator supermajority (pure required) $ dynText $ intPercentage <$> supermajority
  el "dt" $ text "Quorum Needed | Current"
  el "dd" $ do
    let quorum = fromIntegral . _periodVote_quorum . snd <$> vote
    dynText $ intPercentage <$> quorum
    text " | "
    let participation = ffor vote $ \(_, pv) ->
          let v = _periodVote_ballots pv
          in if _periodVote_totalRolls pv <= 0 then 0 else (10000 * (_ballots_yay v + _ballots_nay v + _ballots_pass v)) `div` _periodVote_totalRolls pv
    indicator participation quorum $ dynText $ intPercentage <$> participation

-- | Display tezos style 'Int' percentages (e.g. 5500) as percentages (55.00%)
intPercentage :: Int -> Text
intPercentage i = tshow wholes <> "." <> T.drop 1 (tshow $ 100 + decimals) <> "%"
  where (wholes, decimals) = i `divMod` 100

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

-- | Modal for voting
voteModal :: forall r t m.
  ( MonadAppWidget t m
  , MonadReader r m, HasFrontendConfig r
  , MonadJSM (Performable m)
  , HasTimer t r, HasTimeZone r
  )
  => (PublicKeyHash, SecretKey)
  -- ^ Baker to vote with
  -> Dynamic t ProtoInfo
  -- ^ Protocol info
  -> Dynamic t Amendment
  -- ^ Curent amendment period
  -> Event t () -> m (Event t ())
voteModal (bakerPkh, sk) protoInfo amendment close = do
  votingPeriodKind <- holdUniqDyn $ _amendment_period <$> amendment
  pb <- getPostBuild
  rec
    (_, replaced) <- runWithReplace blank $ leftmost
      [ newPeriod <$> updated votingPeriodKind
      , attachWith (\p () -> selectPeriod p) (current votingPeriodKind) pb
      , attachWith (\p () -> selectPeriod p) (current votingPeriodKind) replaces
      ]
    (replaces, closes) <- fanEither <$> switchHold never replaced
  pure $ close <> closes
  where
    voteButton t = divClass "vote-buttons" $ uiDynButton (pure "primary") $ text t
    -- | When a new period arrives, we display this interstitial to inform the
    -- user why they were redirected
    newPeriod :: VotingPeriodKind -> m (Event t (Either () ()))
    newPeriod p = do
      divClass "ui header" $ text "The period has changed."
      el "p" $ text $ "The network is now in the " <> textPeriod p <> " Period."
      (fmap . fmap) Left $ uiDynButton (pure "primary") $ text "Continue"

    selectPeriod :: VotingPeriodKind -> m (Event t (Either () ()))
    selectPeriod = fmap (fmap Right . switchDyn) . \case
      VotingPeriodKind_Proposal -> workflow proposalFlow
      VotingPeriodKind_TestingVote -> workflow explorationFlow
      VotingPeriodKind_Testing -> pure <$> getPostBuild -- TODO: close immediately
      VotingPeriodKind_PromotionVote -> workflow promotionFlow
      VotingPeriodKind_Adoption -> workflow _adoptionFlow

    headerWithCycles header detail extras = do
      divClass "header" $ do
        elClass "i" "blue icon-vote-badge icon" blank
        text header
        elClass "span" "detail" $ do
          text "Cycles "
          dynText $ ffor2 amendment protoInfo $ \a info ->
            let startLevel = _amendment_startLevel a
                endLevel = startLevel + _protoInfo_blocksPerVotingPeriod info
                toCycle n = fromIntegral $ n `div` _protoInfo_blocksPerCycle info
            in textWithCommas (toCycle startLevel) <> " - " <> textWithCommas (toCycle endLevel - 1)
      divClass "detail" $ text detail
      divClass "vote-cast-as" $ do
        elAttr "img" ("class" =: "kiln-icon" <> "src" =: static @"images/logo.svg") blank
        divClass "item" $ do
          divClass "title" $ text "Votes will be cast as your Kiln Baker."
          divClass "detail" $ text $ toPublicKeyHashText bakerPkh
        extras

    proposalFlow :: Workflow t m (Event t ())
    proposalFlow = Workflow $ do
      proposals <- watchProposals
      headerWithCycles
        "Proposal Period"
        ("During the Proposal Period a baker may upvote up to " <> tshow maxProposalUpvotes <> " proposals. The proposal with the most upvotes will advance to the Exploration Period, where bakers may vote on whether it should be tested.")
        (divClass "item" $ do
          divClass "title" $ dynText $ ffor proposals $ \ps ->
            tshow (Map.size $ Map.filter (isJust . snd) ps) <> " / " <> tshow maxProposalUpvotes
          divClass "detail" $ text "Votes Cast")
      divClass "proposals" $ do
        el "label" $ text "Filter Proposals by Hash"
        hashFilter <- divClass "ui fluid input" $ fmap value $ inputElement $ def
            & initialAttributes .~ "placeholder" =: "Proposal Hash"
        vote <- el "table" $ do
          el "thead" $ el "tr" $ do
            el "th" $ text "Proposal Hash"
            el "th" $ text "Votes"
            el "th" $ text "Cast Vote"
          -- TODO handle empty list gracefully
          voteE <- el "tbody" $ listViewWithKey proposals $ \_ pp -> do
            let protocolHash = toBase58Text . _periodProposal_hash . fst <$> pp
                attrs = ffor2 protocolHash hashFilter $ \h h' ->
                  if T.strip (T.toCaseFold h') `T.isInfixOf` T.toCaseFold h
                  then mempty
                  else "class" =: "filtered"
            elDynAttr "tr" attrs $ do
              el "td" $ do
                copyButton $ current protocolHash
                dynText protocolHash
              el "td" $ dynText $ textWithCommas . _periodProposal_votes . fst <$> pp
              el "td" $ do
                let lookuped = ffor pp $ \(p, included) -> let ph = _periodProposal_hash p in (ph, included)
                    classes = ffor lookuped $ \(_, l) -> case l of
                      Nothing -> ""
                      Just True -> "voted"
                      Just False -> "pending"
                vote <- uiDynButton classes $ dynText $ ffor lookuped $ \(_, l) -> case l of
                  Nothing -> "Vote"
                  Just True -> "Voted"
                  Just False -> "Pending"
                pure $ attachWithMaybe (\(p, m) () -> case m of Nothing -> Just p; _ -> Nothing) (current lookuped) vote
          pure $ fmapMaybe (fmap fst . Map.minViewWithKey) voteE
        elDynAttr "div" (ffor proposals $ \ps -> "class" =: ("no-proposals" <> if null ps then "" else " transition hidden")) $ do
          text "No proposals have been submitted for this voting period yet."
        pure (never, waitForWalletAppFlow . castVoteFlow False Nothing <$> vote)

    explorationFlow :: Workflow t m (Event t ())
    explorationFlow = someVotingPeriodFlow "Exploration Period"
      "Votes in this period will decide if the proposal under consideration should be tested in an immediately following Test Period. If it does not pass, Promotion Period will begin again."
      "Test Period"
      (maybeDyn =<< watchPeriodTestingVote)

    promotionFlow :: Workflow t m (Event t ())
    promotionFlow = Workflow $ do
      chainText <- asks $ showChain . _frontendConfig_chain . view frontendConfig
      unWorkflow $ someVotingPeriodFlow "Promotion Period"
        ("Votes in this period will decide if the proposal under consideration should be promoted to " <> chainText <> ". If it does not pass the current protocol will remain in place. If it passes, the proposed protocol will take affect at the end of this Promotion Period.")
        chainText
        (maybeDyn =<< watchPeriodPromotionVote)

    _adoptionFlow :: Workflow t m (Event t ())
    _adoptionFlow = undefined

    someVotingPeriodFlow
      :: Text -- ^ Header
      -> Text -- ^ Explanation
      -> Text -- ^ Promote to <X>
      -> m (Dynamic t (Maybe (Dynamic t ((Id PeriodProposal, PeriodProposal), PeriodVote)))) -- ^ Watch relevant vote
      -> Workflow t m (Event t ())
    someVotingPeriodFlow header explanation promote getPeriodVote = Workflow $ do
      mPeriodVote <- getPeriodVote
      headerWithCycles header explanation blank
      vote <- switchHold never <=< dyn $ ffor mPeriodVote $ \case
        Nothing -> divClass "ui active loader" $ pure never
        Just pv -> do
          divClass "detail" $ text "Proposal Hash"
          let proposal = _periodProposal_hash . snd . fst <$> pv
              hashText = toBase58Text <$> proposal
          divClass "proposal-hash" $ do
            copyButton $ current hashText
            dynText hashText
          elAttr "p" ("class" =: "promote") $ do
            text $ "Promote this proposal to " <> promote <> "?"
          let ballotButton b = (b <$) <$> uiDynButton (pure "primary") (text $ textBallot b)
          vote <- divClass "vote-buttons" $ leftmost <$> traverse ballotButton [Ballot_Yay, Ballot_Nay, Ballot_Pass]
          pure $ attachWith (\((pid, pp), _) b -> castVoteFlow False (Just b) (pid, _periodProposal_hash pp)) (current pv) vote
      pure (never, waitForWalletAppFlow <$> vote)

    waitForWalletAppFlow
      :: Workflow t m (Event t ()) -- ^ Workflow to redirect to when the wallet app is detected
      -> Workflow t m (Event t ())
    waitForWalletAppFlow nextFlow = Workflow $ divClass "looking-tezos-wallet" $ do
      devFound <- ledgerDeviceIcon
      divClass "ui header centered" $ do
        elClass "span" "ui active inline loader small blue" blank
        elClass "span" "" $ text "Looking for Tezos Wallet app on Ledger device..."
      el "p" $ text "Voting requires the Tezos Wallet app version 1.5.0 or higher to be open. Voting cannot be done using the Tezos Baking app. If you have not installed Tezos Wallet, do so now."
      nextBakingRights
      divClass "detail" $ do
        text "To install the Tezos Wallet app:"
        el "ol" $ do
          el "li" $ text "Install and open Ledger Live: https://www.ledger.com/pages/ledger-live"
          el "li" $ text "Go to Manager and search for “Tezos”"
          el "li" $ text "Install the “Tezos Wallet” app"
          el "li" $ text "Open the Tezos Wallet app on your ledger"
      let walletReady = ffilter ((==) (Just True)) $ updated devFound
      pure (never, nextFlow <$ walletReady)

    castVoteFlow :: Bool -> Maybe Ballot -> (Id PeriodProposal, ProtocolHash) -> Workflow t m (Event t ())
    castVoteFlow isTimedOut mBallot (proposalId, proposalHash) = Workflow $ do
      ledgerStatus <- ledgerDeviceIcon
      when isTimedOut $ do
        divClass "ui message" $ do
          el "div" $ do
            icon "icon-x red"
          divClass "title" $ do
            text "The Ledger prompt was rejected or timed out. Please try again."
      divClass "bigtitle" $ text $ case mBallot of
        Nothing -> "Cast a vote for this proposal?"
        Just ballot -> "Cast a ‘" <> textBallot ballot <> "’ vote for this proposal?"
      divClass "cast-vote-protocol" $ text $ toBase58Text proposalHash
      cast <- voteButton "Cast Vote"
      _ <- requestingIdentity $ public (PublicRequest_DoVote sk proposalId mBallot) <$ cast
      let appLost = ffilter ((/=) (Just True)) $ updated ledgerStatus
          retryFlow = waitForWalletAppFlow $ castVoteFlow isTimedOut mBallot (proposalId, proposalHash)
      pure (never, leftmost [respondToPromptFlow retryFlow (proposalId, proposalHash) mBallot <$ cast, ledgerDisconnectedFlow retryFlow <$ appLost])

    respondToPromptFlow
      :: Workflow t m (Event t ())
      -- ^ Workflow to return to after retry
      -> (Id PeriodProposal, ProtocolHash) -> Maybe Ballot -> Workflow t m (Event t ())
    respondToPromptFlow retryFlow proposal mBallot = Workflow $ do
      ledgerStatus <- ledgerDeviceIcon
      pb <- getPostBuild
      promptStep <- watchVotePrompting sk
      let changed = leftmost [updated promptStep, tag (current promptStep) pb]
          next = fforMaybe changed $ \case
            Just vs | Just (First step) <- _voteState_step vs -> case step of
              -- TODO: go to proper flow
              VoteStep_Done -> Just $ voteCastSuccessfullyFlow $ Left ()
              VoteStep_Disconnected -> Just $ ledgerDisconnectedFlow retryFlow
              VoteStep_Declined -> Just $ castVoteFlow True mBallot proposal
              VoteStep_Failed _ -> Just $ castVoteFlow True mBallot proposal
              VoteStep_Prompting -> Nothing
              VoteStep_WrongPeriod -> Just $ castVoteFlow True mBallot proposal -- This case should be caught by the outer runWithReplace
            _ -> Nothing
      divClass "bigtitle" $ do
        elClass "span" "icon" $ elClass "span" "ui active inline loader small blue" blank
        text "Respond to the prompt on your Ledger Device..."
      divClass "centered-grey" $ text "Your Ledger Device should show the following prompt:"
      el "div" $ do
        case mBallot of
          Nothing ->
            divClass "confirm-title" $ text "Confirm Proposal"
          Just ballot -> do
            divClass "confirm-title" $ text "Confirm Vote"
            divClass "confirm-content" $ text $ textBallot ballot
        divClass "confirm-title" $ text "Source"
        divClass "confirm-content" $ text $ toPublicKeyHashText bakerPkh
        divClass "confirm-title" $ text "Protocol"
        divClass "confirm-content" $ text $ toBase58Text $ snd proposal
        divClass "confirm-title" $ text "Period"
        divClass "confirm-content" $ display $ unRawLevel . _amendment_votingPeriod <$> amendment
      let appLost = ffilter ((/=) (Just True)) $ updated ledgerStatus
      pure (never, leftmost [next, ledgerDisconnectedFlow retryFlow <$ appLost])

    ledgerDisconnectedFlow
      :: Workflow t m (Event t ())
      -- ^ Workflow to return to after retry
      -> Workflow t m (Event t ())
    ledgerDisconnectedFlow retryFlow = Workflow $ do
      _ <- ledgerDeviceIcon
      divClass "bigtitle" $ text "Ledger Device was disconnected."
      retry <- voteButton "Restart"
      pure (never, retryFlow <$ retry)

    voteCastSuccessfullyFlow
      :: Either () (Workflow t m (Event t ())) -- ^ Upon success, either close the dialog or redirect to another workflow
      -> Workflow t m (Event t ())
    voteCastSuccessfullyFlow whereToGo = Workflow $ do
      _ <- ledgerDeviceIcon
      divClass "bigtitle" $ do
        icon "icon-check blue"
        text "Your vote has been cast."
      divClass "confirm-content" $ text "Kiln will confirm when your vote has been included in the blockchain."
      divClass "ui message" $ do
        el "div" $ do
          icon "icon-warning big orange"
        el "div" $ do
          divClass "title" $ do
            text "Reopen the Tezos Baking app."
          divClass "description" $ text "You will not be able to sign blocks or endorsements while outside the Tezos Baking app."
      continue <- voteButton "Continue"
      pure $ fanEither $ whereToGo <$ continue

    -- searching device / wrong device found : show only identifier, no marks
    -- found : show green tick mark
    -- not found : show red cross mark
    ledgerDeviceIcon = divClass "ledger-device-status" $ do
      connectedLedger <- watchConnectedLedgerForced
      let
        devFound :: Dynamic t (Maybe Bool)
        devFound = ffor connectedLedger (>>= \cl -> ffor (_connectedLedger_ledgerIdentifier cl) $ \li ->
          li == _secretKey_ledgerIdentifier sk && isJust (_connectedLedger_walletAppVersion cl))
        iconType :: Dynamic t (Maybe Text)
        iconType = ffor devFound $ fmap $ \b -> if b
            then "icon-check blue"
            else "icon-x-thick red"
      divClass "" $ do
        elAttr "img" ("src" =: static @"images/ledger.svg") blank
        dyn_ $ ffor iconType $ mapM $ \it ->
          elClass "span" "mark" $ icon $ "small circular " <> it
      divClass "centered-grey" $ text $ unLedgerIdentifier $ _secretKey_ledgerIdentifier sk
      pure devFound

    nextBakingRights = do
      mBakerDyn <- fmap (MMap.lookup bakerPkh) <$> watchBakerAddresses
      let
        mLevel = ffor (fmap _bakerSummary_nextRight <$> mBakerDyn) $ \case
          Just (BakerNextRight_KnownRights (_,l)) -> pure l
          _ -> Nothing
      dyn_ $ ffor mLevel $ \case
        Nothing -> blank
        Just l -> divClass "ui message" $ do
          (latestHead, knownProto) <- watchHeadWithProtocol
          let
            dparameters = (fmap . fmap) _protocolIndex_constants knownProto
            mNextOp :: Dynamic t (Maybe Time.UTCTime)
            -- mNextOp = getCompose $ predictFutureTimestamp <$> Compose dparameters <*> (Compose $ constDyn $ Just l) <*> Compose latestHead
            mNextOp = getCompose $ undefined <$> Compose dparameters <*> (Compose $ constDyn $ Just l) <*> Compose latestHead
          el "div" $ do
            icon "icon-warning big orange"
          el "div" $ do
            divClass "bigtitle" $ do
              text "Your baker's next opportunity is "
              etaDyn <- maybeDyn mNextOp
              dyn_ $ ffor etaDyn $ maybe blank localHumanizedTimestampBasicWithoutTZ
            divClass "description" $ text "You will not be able to sign blocks or endorsements while outside the Tezos Baking app. Be sure you have a few minutes to vote before your baker's next opportunity."
