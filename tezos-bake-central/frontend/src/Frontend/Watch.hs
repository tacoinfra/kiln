{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend.Watch where

import qualified Data.List.NonEmpty as NEL
import Data.Dependent.Map (DMap, DSum(..), Some (..))
import qualified Data.Dependent.Map as DMap
import Data.Map (Map)
import qualified Data.Map.Monoidal as MMap
import Data.Ord (Down(..))
import Data.Semigroup (Min (..))
import Data.Semigroup.Foldable (fold1)
import Data.Time (UTCTime)
import Data.Universe (universe)
import Prelude hiding (log)
import Reflex.Dom.Core
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (MonadRhyoliteFrontendWidget, watchViewSelector)
import Rhyolite.Schema (Email)
import Safe (minimumMay)

import Tezos.NodeRPC.Sources (PublicNode)
import Tezos.Types

import Common.Api
import Common.App
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import qualified Common.AppendIntervalMap as AppendIMap
import Common.Config (FrontendConfig(..))
import Common.Schema hiding (Event)
import Common.Vassal
import Common.Alerts (AlertsFilter(..))
import ExtraPrelude

watchFrontendConfig :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe FrontendConfig))
watchFrontendConfig =
  (fmap . fmap) (getMaybeView . _bakeView_config) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_config = viewJust 1
    }

watchProtoInfo :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe ProtoInfo))
watchProtoInfo =
  (fmap . fmap) (getMaybeView . _bakeView_parameters) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_parameters = viewJust 1
    }

watchLatestHead :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe VeryBlockLike))
watchLatestHead =
  (fmap . fmap) (getMaybeView . _bakeView_latestHead) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_latestHead = viewJust 1
    }

watchInternalBaker :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (PublicKeyHash, BakerInternalData)))
watchInternalBaker = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_bakerAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> listToMaybe $ MMap.toList $ fmapMaybe (preview _Right . _bakerSummary_baker) $ fmapMaybe getFirst $ getRangeView' (_bakeView_bakerAddresses v')

watchInternalNode :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (Id Node, ProcessData)))
watchInternalNode = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodeAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> listToMaybe $ MMap.toList $ fmapMaybe (preview _Right . _nodeSummary_node) $ fmapMaybe getFirst $ getRangeView' (_bakeView_nodeAddresses v')

watchNodeAddresses :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap (Id Node) NodeSummary))
watchNodeAddresses = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodeAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> fmapMaybe getFirst $ getRangeView' (_bakeView_nodeAddresses v')

watchNodeAddressesValid :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (MonoidalMap (Id Node) NodeSummary)))
watchNodeAddressesValid = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodeAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> validatingRange (fmapMaybe getFirst . getRangeView') (_bakeView_nodeAddresses v')

watchNodeDetails :: (MonadRhyoliteFrontendWidget Bake t m) => Id Node -> m (Dynamic t (Maybe NodeDetailsData))
watchNodeDetails nid = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodeDetails = viewRangeExactly (Bounded nid) 1
    }
  return $ ffor theView $ \v' -> MMap.lookup nid $ getRangeView' (_bakeView_nodeDetails v')

watchBakerAddresses :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap PublicKeyHash BakerSummary))
watchBakerAddresses = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_bakerAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> fmapMaybe getFirst $ getRangeView' (_bakeView_bakerAddresses v')

watchBakerAddressesValid :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (MonoidalMap PublicKeyHash BakerSummary)))
watchBakerAddressesValid = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_bakerAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> validatingRange (fmapMaybe getFirst . getRangeView') (_bakeView_bakerAddresses v')

watchBakerDetails :: MonadRhyoliteFrontendWidget Bake t m => PublicKeyHash -> m (Dynamic t (Maybe BakerDetails))
watchBakerDetails pkh = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_bakerDetails = viewRangeExactly (Bounded pkh) 1
    }
  return $ ffor theView $ \v' -> MMap.lookup pkh $ fmapMaybe getFirst $ getRangeView' (_bakeView_bakerDetails v')

watchBakerStats :: (MonadRhyoliteFrontendWidget Bake t m) => Dynamic t (Set PublicKeyHash) -> m (Dynamic t (MonoidalMap PublicKeyHash (BakeEfficiency, Account)))
watchBakerStats bakers = do
  let levels :: (RawLevel, RawLevel) = (0, 30)
      --levels' :: ClosedInterval RawLevel = ClosedInterval 0 30
  _theView <- watchViewSelector $ ffor bakers $ \ds -> mempty
    { _bakeViewSelector_bakerStats = viewCompose $ viewRangeSet ds $ viewRangeBetween levels 1
    }
  holdDyn MMap.empty never
  -- return $ ffor theView $ uncurry (mergeMMap
  --     (\_ acc -> Just (mempty, acc))
  --     (\_ _ -> Nothing)
  --     (\pkh acc (AppendIMMap.AppendIntervalMap effs) -> Just (fold $ IMMap.findWithDefault mempty levels' effs, acc))
  --   ) . second (fmap getRangeView) . first getRangeView . getComposeView . _bakeView_bakerStats

watchMailServer
  :: MonadRhyoliteFrontendWidget Bake t m
  => m (Dynamic t (Maybe (Maybe MailServerView)))
watchMailServer =
  (fmap . fmap) (getMaybeView . _bakeView_mailServer) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_mailServer = viewJust 1 }

watchNotificatees :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (Maybe [Email])))
watchNotificatees = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_mailServer = viewJust 1
    }
  return $ fmap ((fmap . fmap) _mailServerView_notificatees . getMaybeView . _bakeView_mailServer) theView

watchErrors
  :: MonadRhyoliteFrontendWidget Bake t m
  => Dynamic t (Maybe AlertsFilter)
  -> Dynamic t (Set (ClosedInterval (WithInfinity UTCTime)))
  -> m (Dynamic t (MMap.MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView)))
watchErrors mAlert intervals = watchErrorsByTag mAlert allLogTags intervals
  where allLogTags = constDyn $ DMap.fromList $ map (\(This l) -> l :=> Const ()) (universe :: [Some LogTag])

-- It is possible to to query each LogTag with different Interval
watchErrorsByTag
  :: MonadRhyoliteFrontendWidget Bake t m
  => Dynamic t (Maybe AlertsFilter)
  -> Dynamic t (DMap LogTag (Const ()))
  -> Dynamic t (Set (ClosedInterval (WithInfinity UTCTime)))
  -> m (Dynamic t (MMap.MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView)))
watchErrorsByTag mAlert logSet intervals = do
  v <- watchViewSelector $ ffor3 mAlert logSet intervals $ \mAlert' logSet' ivals -> flip foldMap mAlert' $ \a -> mempty
    { _bakeViewSelector_errors = MMap.singleton a $ viewCompose $ MapSelector $ MMap.fromList $ map (,viewIntervalSet ivals 1) $ DMap.keys logSet'
    }
  -- TOOD: maybe we should just fix up IntervalSelector to operate on some semigroup instead of Set
  pure $ ffor3 mAlert logSet v $ \mAlert' logSet' v' ->
    let
      (_, lower) = getComposeView $ fold $ do
        alert <- mAlert'
        MMap.lookup alert $ _bakeView_errors v'
      allTags = fold $ mapMaybe (\ltag -> MMap.lookup ltag lower) (DMap.keys logSet')
    in fmapMaybe (getFirst . fst . getFirst) $ _intervalView_elements allTags

watchErrorsByNode
  :: MonadRhyoliteFrontendWidget Bake t m
  => Dynamic t (Set (ClosedInterval (WithInfinity UTCTime)))
  -> m (Dynamic t (MonoidalMap (Id Node) (NonEmpty (ErrorLog, NodeErrorLogView))))
watchErrorsByNode alertWindow = do
  dXs <- watchErrors (pure $ Just AlertsFilter_UnresolvedOnly) alertWindow
  pure $ ffor dXs $ \xs -> MMap.fromListWith (<>)
    [ (k, pure (l, t'))
    | (l@ErrorLog{_errorLog_stopped = Nothing}, t) <- MMap.elems xs
    , Just t' <- [nodeErrorViewOnly t]
    , let k = nodeIdForNodeErrorLogView t'
    ]

watchBakerAlerts
  :: MonadRhyoliteFrontendWidget Bake t m
  => m (Dynamic t (MonoidalMap PublicKeyHash (NonEmpty BakerAlert)))
watchBakerAlerts = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_bakerAlerts = viewRangeAll 1
    }
  return $ ffor theView $ \v' ->  getRangeView' (_bakeView_bakerAlerts v')

data CollectiveNodesFailure
  = CollectiveNodesFailure_NoNodes
  | CollectiveNodesFailure_AllNodesDownSince UTCTime
  -- ^ The last time any of the node was up, there must have been up
  deriving (Eq, Ord, Show)

nodeSummaryStateIfInternal :: NodeSummary -> Maybe ProcessState
nodeSummaryStateIfInternal = preview $ nodeSummary_node . _Right . processData_state

watchCollectiveNodesStatus
  :: MonadRhyoliteFrontendWidget Bake t m
  => Dynamic t (Set (ClosedInterval (WithInfinity UTCTime)))
  -> m (Dynamic t (Either CollectiveNodesFailure ()))
watchCollectiveNodesStatus alertWindow = do
  dUsingOsPublicNode <- (fmap . fmap) _frontendConfig_usingOsPublicNode <$> watchFrontendConfig
  dNodes <- watchNodeAddresses
  let dmNids = NEL.nonEmpty
        <$> MMap.keys
        <$> ffilter (maybe True (== ProcessState_Running)
                     . nodeSummaryStateIfInternal)
        <$> dNodes
  ebn <- watchErrorsByNode alertWindow
  holdUniqDyn $ ffor3 dUsingOsPublicNode dmNids ebn $ \case
    Just True -> const $ const $ Right ()
    _ -> \case
      Nothing -> const $ Left CollectiveNodesFailure_NoNodes
      Just nids -> \nodeErrors -> case
          -- Use `Min` and `Down` instead of `Max` so that Nothing effectively is
          -- the greatest element rather than least element.
          getMin $ fold1 $ ffor nids $ \nid ->
            Min $
            fmap Down $
            -- if there are errors, we went "ill" when the first one started
            minimumMay $ _errorLog_started . fst
              <$> maybe [] toList (MMap.lookup nid nodeErrors)
        of
          Nothing -> Right ()
          Just (Down time) -> Left $ CollectiveNodesFailure_AllNodesDownSince time

watchPublicNodeConfig :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap PublicNode PublicNodeConfig))
watchPublicNodeConfig =
  (fmap . fmap) (getRangeView . _bakeView_publicNodeConfig) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_publicNodeConfig = viewRangeAll 1 }

watchPublicNodeConfigValid :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (MonoidalMap PublicNode PublicNodeConfig)))
watchPublicNodeConfigValid =
  (fmap . fmap) (validatingRange getRangeView . _bakeView_publicNodeConfig) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_publicNodeConfig = viewRangeAll 1 }

watchPublicNodeHeads :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap (Id PublicNodeHead) PublicNodeHead))
watchPublicNodeHeads =
  (fmap . fmap) (getRangeView' . _bakeView_publicNodeHeads) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_publicNodeHeads = viewRangeAll 1 }

watchTelegramConfig :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (Maybe TelegramConfig)))
watchTelegramConfig =
  (fmap . fmap) (getMaybeView . _bakeView_telegramConfig) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_telegramConfig = viewJust 1 }

watchTelegramRecipients :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap (Id TelegramRecipient) (Maybe TelegramRecipient)))
watchTelegramRecipients =
  (fmap . fmap) (fmap getFirst . getRangeView' . _bakeView_telegramRecipients) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_telegramRecipients = viewRangeAll 1 }

watchUpstreamVersion :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe UpstreamVersion))
watchUpstreamVersion = holdUniqDyn <=<
  (fmap . fmap) (getMaybeView . _bakeView_upstreamVersion) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_upstreamVersion = viewJust 1 }

watchAlertCount :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (DMap LogTag (Const Int))))
watchAlertCount =
  (fmap . fmap) (getMaybeView . _bakeView_alertCount) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_alertCount = viewJust 1
    }

watchSnapshotMeta :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe SnapshotMeta))
watchSnapshotMeta =
  (fmap . fmap) (getMaybeView . _bakeView_snapshotMeta) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_snapshotMeta = viewJust 1
    }

watchConnectedLedger :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe ConnectedLedger))
watchConnectedLedger = do
  -- this is in lieu of a nicer libusb solution to avoid constantly polling the device
  poll <- tickLossyFromPostBuildTime 5
  _ <- requestingIdentity $ public PublicRequest_PollLedgerDevice <$ poll
  (fmap . fmap) (join . getMaybeView . _bakeView_connectedLedger) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_connectedLedger = viewJust 1
    }

watchLedgerAccounts :: MonadRhyoliteFrontendWidget Bake t m => Dynamic t [SecretKey] -> m (Dynamic t (MonoidalMap SecretKey (PublicKeyHash, Tez)))
watchLedgerAccounts dkeys =
  (fmap . fmap) (fmapMaybe getFirst . getRangeView . _bakeView_showLedger) $ watchViewSelector $ ffor dkeys $ \keys -> mempty
    { _bakeViewSelector_showLedger = RangeSelector $ AppendIMap.fromList $ ffor keys $ \k -> (ClosedInterval k k, 1)
    }

watchAmendment :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Map VotingPeriodKind Amendment))
watchAmendment =
  (fmap . fmap) (MMap.getMonoidalMap . fmapMaybe getFirst . getRangeView . _bakeView_amendment) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_amendment = viewRangeAll 1 }

watchProposals :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Map (Id PeriodProposal) (PeriodProposal, Maybe Bool)))
watchProposals =
  (fmap . fmap) (MMap.getMonoidalMap . fmapMaybe getFirst . getRangeView' . _bakeView_proposals) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_proposals = viewRangeAll 1
    }

watchProposal :: MonadRhyoliteFrontendWidget Bake t m => Dynamic t (Id PeriodProposal) -> m (Dynamic t (Maybe (PeriodProposal, Maybe Bool)))
watchProposal pid = do
  m <- (fmap . fmap) (fmapMaybe getFirst . getRangeView' . _bakeView_proposals) $ watchViewSelector $ ffor pid $ \p -> mempty
    { _bakeViewSelector_proposals = viewRangeExactly (Bounded p) 1
    }
  pure $ ffor2 pid m MMap.lookup

watchBakerVote :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe BakerVote))
watchBakerVote =
  (fmap . fmap) (join . getMaybeView . _bakeView_bakerVote) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_bakerVote = viewJust 1
    }

watchPeriodTestingVote :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe ((Id PeriodProposal, PeriodProposal), PeriodVote)))
watchPeriodTestingVote = do
  vote <- (fmap . fmap) (join . getMaybeView . _bakeView_periodTestingVote) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_periodTestingVote = viewJust 1
    }
  mId <- holdUniqDyn $ fmap _periodTestingVote_proposal <$> vote
  dm <- (fmap . fmap) (fmapMaybe getFirst . getRangeView' . _bakeView_proposals) $ watchViewSelector $ ffor mId $ \mp -> mempty
    { _bakeViewSelector_proposals = maybe mempty (\p -> viewRangeExactly (Bounded p) 1) mp
    }
  pure $ ffor2 vote dm $ \mv m -> do
    v <- mv
    let pid = _periodTestingVote_proposal v
    (pp, _) <- MMap.lookup pid m
    pure ((pid, pp), _periodTestingVote_periodVote v)

watchPeriodTesting :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe ((Id PeriodProposal, PeriodProposal), PeriodTesting)))
watchPeriodTesting = do
  test <- (fmap . fmap) (join . getMaybeView . _bakeView_periodTesting) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_periodTesting = viewJust 1
    }
  mId <- holdUniqDyn $ fmap _periodTesting_proposal <$> test
  dm <- (fmap . fmap) (fmapMaybe getFirst . getRangeView' . _bakeView_proposals) $ watchViewSelector $ ffor mId $ \mp -> mempty
    { _bakeViewSelector_proposals = maybe mempty (\p -> viewRangeExactly (Bounded p) 1) mp
    }
  pure $ ffor2 test dm $ \mt m -> do
    t <- mt
    let pid = _periodTesting_proposal t
    (pp, _) <- MMap.lookup pid m
    pure ((pid, pp), t)

watchPeriodPromotionVote :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe ((Id PeriodProposal, PeriodProposal), PeriodVote)))
watchPeriodPromotionVote = do
  vote <- (fmap . fmap) (join . getMaybeView . _bakeView_periodPromotionVote) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_periodPromotionVote = viewJust 1
    }
  mId <- holdUniqDyn $ fmap _periodPromotionVote_proposal <$> vote
  dm <- (fmap . fmap) (fmapMaybe getFirst . getRangeView' . _bakeView_proposals) $ watchViewSelector $ ffor mId $ \mp -> mempty
    { _bakeViewSelector_proposals = maybe mempty (\p -> viewRangeExactly (Bounded p) 1) mp
    }
  pure $ ffor2 vote dm $ \mv m -> do
    v <- mv
    let pid = _periodPromotionVote_proposal v
    (pp, _) <- MMap.lookup pid m
    pure ((pid, pp), _periodPromotionVote_periodVote v)

watchPrompting :: MonadRhyoliteFrontendWidget Bake t m => SecretKey -> m (Dynamic t (Maybe SetupState))
watchPrompting sk = do
  (fmap . fmap) (MMap.lookup sk . fmapMaybe getFirst . getRangeView . _bakeView_prompting) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_prompting = RangeSelector $ AppendIMap.singleton (ClosedInterval sk sk) 1
    }

watchVotePrompting :: MonadRhyoliteFrontendWidget Bake t m => SecretKey -> m (Dynamic t (Maybe VoteState))
watchVotePrompting sk = do
  (fmap . fmap) (MMap.lookup sk . fmapMaybe getFirst . getRangeView . _bakeView_votePrompting) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_votePrompting = RangeSelector $ AppendIMap.singleton (ClosedInterval sk sk) 1
    }

watchBakerRegistered :: MonadRhyoliteFrontendWidget Bake t m => SecretKey -> PublicKeyHash -> m (Dynamic t (Maybe Bool))
watchBakerRegistered sk pkh = do
  pb <- getPostBuild
  _ <- requestingIdentity $ public (PublicRequest_CheckIfRegistered sk pkh) <$ pb
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_bakerRegistered = viewRangeExactly (Bounded pkh) 1
    }
  return $ ffor theView $ \v' -> MMap.lookup pkh $ getRangeView' (_bakeView_bakerRegistered v')

watchRightNotificationLimit :: MonadRhyoliteFrontendWidget Bake t m => RightKind -> m (Dynamic t (Maybe RightNotificationLimit))
watchRightNotificationLimit rk = do
  (fmap . fmap) (MMap.lookup rk . fmapMaybe getFirst . getRangeView . _bakeView_rightNotificationSettings) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_rightNotificationSettings = RangeSelector $ AppendIMap.singleton (ClosedInterval rk rk) 1
    }

validatingRange :: (View (RangeSelector e v) a -> b) -> (View (RangeSelector e v) a -> Maybe b)
validatingRange f v =
  if null $ _rangeView_support v
    then Nothing
    else Just $ f v
