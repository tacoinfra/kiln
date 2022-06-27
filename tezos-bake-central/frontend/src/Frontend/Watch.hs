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

import Data.Bifunctor
import qualified Data.List.NonEmpty as NEL
import Data.Dependent.Map (DMap, DSum(..), Some (..))
import qualified Data.Dependent.Map as DMap
import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map.Monoidal as MMap
import Data.Ord (Down(..))
import Data.Semigroup (Min (..))
import Data.Semigroup.Foldable (fold1)
import Data.Time (UTCTime)
import Data.Universe (universe)
import Data.Validation
import Prelude hiding (log)
import Reflex.Dom.Core
import Rhyolite.Api (public)
import Rhyolite.Frontend.App (watchViewSelector)
import Rhyolite.Schema (Email)
import Safe (minimumMay)

import Tezos.Types

import Common.Api
import Common.App
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import qualified Common.AppendIntervalMap as AppendIMap
import Common.Config (FrontendConfig(..))
import Common.Schema
import Common.Vassal
import Common.Alerts (AlertsFilter(..))
import ExtraPrelude
import Frontend.Common

watchFrontendConfig :: MonadAppWidget js t m => m (Dynamic t (Maybe FrontendConfig))
watchFrontendConfig =
  (fmap . fmap) (getMaybeView . _bakeView_config) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_config = viewJust 1
    }

watchProtocolConstants :: MonadAppWidget js t m => Dynamic t ProtocolHash -> m (Dynamic t (Maybe ProtocolIndex))
watchProtocolConstants protocol = do
  mmap <- (fmap . fmap) (unMapView . _bakeView_parameters) $
    watchViewSelector $
      ffor protocol $ \protoHash -> mempty
        { _bakeViewSelector_parameters = MapSelector $ MMap.singleton protoHash 1
        }
  pure $ liftA2 (\p m -> getFirst . fst <$> MMap.lookup p m) protocol mmap

watchHeadWithProtocol
  :: forall js t m. MonadAppWidget js t m
  => m (Dynamic t (Maybe BranchInfo), Dynamic t (Maybe ProtocolIndex))
watchHeadWithProtocol = do
  latestHead <- watchLatestHead
  protoHash' <- maybeDyn $ (fmap.fmap) (^. protocolHash) latestHead
  protoConstantsEvt :: Event t (Dynamic t (Maybe ProtocolIndex)) <- dyn $ ffor protoHash' $ \case
    Nothing -> pure $ pure Nothing
    Just protoHash -> do
      protoHashUniq <- holdUniqDyn protoHash
      watchProtocolConstants protoHashUniq
  protoConstants <- join <$> holdDyn (pure Nothing) protoConstantsEvt
  pure (latestHead, protoConstants)

watchLatestProtoInfo :: MonadAppWidget js t m => m (Dynamic t (Maybe ProtoInfo))
watchLatestProtoInfo = do
  (_, knownProto) <- watchHeadWithProtocol
  pure $ fmap _protocolIndex_constants <$> knownProto

watchLatestHead :: MonadAppWidget js t m => m (Dynamic t (Maybe BranchInfo))
watchLatestHead =
  (fmap . fmap) (getMaybeView . _bakeView_latestHead) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_latestHead = viewJust 1
    }

watchLatestProtocolHash :: MonadAppWidget js t m => m (Dynamic t (Maybe ProtocolHash))
watchLatestProtocolHash = do
  latestHeadDyn <- watchLatestHead
  let protoHashDyn = (fmap . fmap) (_withProtocolHash_protocolHash . _branchInfo_block) latestHeadDyn
  return protoHashDyn

watchInternalBaker :: MonadAppWidget js t m => m (Dynamic t (Maybe (PublicKeyHash, BakerInternalData)))
watchInternalBaker = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_bakerAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> listToMaybe $ MMap.toList $ fmapMaybe (preview _Right . _bakerSummary_baker) $ fmapMaybe getFirst $ getRangeView' (_bakeView_bakerAddresses v')

watchInternalNode :: MonadAppWidget js t m => m (Dynamic t (Maybe (Id Node, ProcessData)))
watchInternalNode = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodeAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> listToMaybe $ MMap.toList $ fmapMaybe (preview _Right . _nodeSummary_node) $ fmapMaybe getFirst $ getRangeView' (_bakeView_nodeAddresses v')

watchNodeAddresses :: MonadAppWidget js t m => m (Dynamic t (MonoidalMap (Id Node) NodeSummary))
watchNodeAddresses = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodeAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> fmapMaybe getFirst $ getRangeView' (_bakeView_nodeAddresses v')

watchNodeAddressesValid :: MonadAppWidget js t m => m (Dynamic t (Maybe (MonoidalMap (Id Node) NodeSummary)))
watchNodeAddressesValid = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodeAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> validatingRange (fmapMaybe getFirst . getRangeView') (_bakeView_nodeAddresses v')

watchNodeDetails :: (MonadAppWidget js t m) => Id Node -> m (Dynamic t (Maybe NodeDetailsData))
watchNodeDetails nid = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodeDetails = viewRangeExactly (Bounded nid) 1
    }
  return $ ffor theView $ \v' -> MMap.lookup nid $ getRangeView' (_bakeView_nodeDetails v')

watchLatestTezosRelease :: (MonadAppWidget js t m) => m (Dynamic t (Maybe MajorMinorVersion))
watchLatestTezosRelease =
  (fmap . fmap) (join . getMaybeView . _bakeView_latestTezosRelease) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_latestTezosRelease = viewJust 1
    }

watchTezosVersion :: (MonadAppWidget js t m) => Id Node -> m (Dynamic t (Maybe (Maybe TezosVersion)))
watchTezosVersion nid = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_nodeVersions = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> MMap.lookup nid $ getRangeView' (_bakeView_nodeVersions v')

watchBakerAddresses :: MonadAppWidget js t m => m (Dynamic t (MonoidalMap PublicKeyHash BakerSummary))
watchBakerAddresses = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_bakerAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> fmapMaybe getFirst $ getRangeView' (_bakeView_bakerAddresses v')

watchBakerAddressesValid :: MonadAppWidget js t m => m (Dynamic t (Maybe (MonoidalMap PublicKeyHash BakerSummary)))
watchBakerAddressesValid = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_bakerAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> validatingRange (fmapMaybe getFirst . getRangeView') (_bakeView_bakerAddresses v')

watchBakerDetails :: MonadAppWidget js t m => PublicKeyHash -> m (Dynamic t (Maybe BakerDetails))
watchBakerDetails pkh = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_bakerDetails = viewRangeExactly (Bounded pkh) 1
    }
  return $ ffor theView $ \v' -> MMap.lookup pkh $ fmapMaybe getFirst $ getRangeView' (_bakeView_bakerDetails v')

watchBakerDetailsFull :: MonadAppWidget js t m => m (Dynamic t (MonoidalMap PublicKeyHash BakerDetails))
watchBakerDetailsFull = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_bakerDetails = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> fmapMaybe getFirst $ getRangeView' (_bakeView_bakerDetails v')

watchMailServer
  :: MonadAppWidget js t m
  => m (Dynamic t (Maybe (Maybe MailServerView)))
watchMailServer =
  (fmap . fmap) (getMaybeView . _bakeView_mailServer) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_mailServer = viewJust 1 }

watchNotificatees :: MonadAppWidget js t m => m (Dynamic t (Maybe (Maybe [Email])))
watchNotificatees = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_mailServer = viewJust 1
    }
  return $ fmap ((fmap . fmap) _mailServerView_notificatees . getMaybeView . _bakeView_mailServer) theView

watchErrors
  :: MonadAppWidget js t m
  => Dynamic t (Maybe AlertsFilter)
  -> Dynamic t (Set (ClosedInterval (WithInfinity UTCTime)))
  -> m (Dynamic t (MMap.MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView)))
watchErrors mAlert intervals = watchErrorsByTag mAlert allLogTags intervals
  where allLogTags = constDyn $ DMap.fromList $ map (\(Some l) -> l :=> Const ()) (universe :: [Some LogTag])

-- It is possible to to query each LogTag with different Interval
watchErrorsByTag
  :: MonadAppWidget js t m
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
  :: MonadAppWidget js t m
  => Dynamic t (Set (ClosedInterval (WithInfinity UTCTime)))
  -> m (Dynamic t (MonoidalMap (Id Node) (NonEmpty (ErrorLog, NodeErrorLogView))))
watchErrorsByNode alertWindow = do
  let nodeTags = DMap.fromList $ map (\(Some t) -> LogTag_Node t :=> Const ()) universe
  dXs <- watchErrorsByTag (pure $ Just AlertsFilter_UnresolvedOnly) (constDyn nodeTags) alertWindow
  pure $ ffor dXs $ \xs -> MMap.fromListWith (<>)
    [ (k, pure (l, t'))
    | (l@ErrorLog{_errorLog_stopped = Nothing}, t) <- MMap.elems xs
    , Just t' <- [nodeErrorViewOnly t]
    , let k = nodeIdForNodeErrorLogView t'
    ]

watchBakerAlerts
  :: MonadAppWidget js t m
  => m (Dynamic t (MonoidalMap PublicKeyHash (NonEmpty BakerAlert)))
watchBakerAlerts = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_bakerAlerts = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> MMap.mapMaybe getFirst $ getRangeView' (_bakeView_bakerAlerts v')

data CollectiveNodesFailure
  = CollectiveNodesFailure_NoNodes
  | CollectiveNodesFailure_AllNodesDownSince UTCTime
  -- ^ The last time any of the node was up, there must have been up
  deriving (Eq, Ord, Show)

nodeSummaryStateIfInternal :: NodeSummary -> Maybe ProcessState
nodeSummaryStateIfInternal = preview $ nodeSummary_node . _Right . processData_state

watchCollectiveNodesStatus
  :: forall js t m. MonadAppWidget js t m
  => Dynamic t (Set (ClosedInterval (WithInfinity UTCTime)))
  -> m (Dynamic t (Either CollectiveNodesFailure ()))
watchCollectiveNodesStatus alertWindow = do
  dNodes <- watchNodeAddresses
  let dmNids = NEL.nonEmpty
        <$> MMap.keys
        <$> ffilter (maybe True (== ProcessState_Running)
                     . nodeSummaryStateIfInternal)
        <$> dNodes
  let nodeTags = DMap.fromList $ map (\(Some t) -> LogTag_Node t :=> Const ()) $ universe \\
                   [Some NodeLogTag_NodeInvalidPeerCount]
  dXs <- watchErrorsByTag (pure $ Just AlertsFilter_UnresolvedOnly) (constDyn nodeTags) alertWindow
  let ebn :: (Dynamic t (MonoidalMap (Id Node) (NonEmpty ErrorLog)))
      ebn = ffor dXs $ \xs -> MMap.fromListWith (<>)
              [ (k, pure l)
              | (l@ErrorLog{_errorLog_stopped = Nothing}, t) <- MMap.elems xs
              , Just t' <- [nodeErrorViewOnly t]
              , let k = nodeIdForNodeErrorLogView t'
              ]
  holdUniqDyn $ ffor2 dmNids ebn $ \case
    Nothing -> const $ Left CollectiveNodesFailure_NoNodes
    Just nids -> \nodeErrors -> case
        -- Use `Min` and `Down` instead of `Max` so that Nothing effectively is
        -- the greatest element rather than least element.
        getMin $ fold1 $ ffor nids $ \nid ->
          Min $
          fmap Down $
          -- if there are errors, we went "ill" when the first one started
          minimumMay $ _errorLog_started
            <$> maybe [] toList (MMap.lookup nid nodeErrors)
      of
        Nothing -> Right ()
        Just (Down time) -> Left $ CollectiveNodesFailure_AllNodesDownSince time

watchTelegramConfig :: MonadAppWidget js t m => m (Dynamic t (Maybe (Maybe TelegramConfig)))
watchTelegramConfig =
  (fmap . fmap) (getMaybeView . _bakeView_telegramConfig) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_telegramConfig = viewJust 1 }

watchTelegramRecipients :: MonadAppWidget js t m => m (Dynamic t (MonoidalMap (Id TelegramRecipient) (Maybe TelegramRecipient)))
watchTelegramRecipients =
  (fmap . fmap) (fmap getFirst . getRangeView' . _bakeView_telegramRecipients) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_telegramRecipients = viewRangeAll 1 }

watchUpstreamVersion :: MonadAppWidget js t m => m (Dynamic t (Maybe UpstreamVersion))
watchUpstreamVersion = holdUniqDyn <=<
  (fmap . fmap) (getMaybeView . _bakeView_upstreamVersion) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_upstreamVersion = viewJust 1 }

watchAlertCount :: MonadAppWidget js t m => m (Dynamic t (Maybe (DMap LogTag (Const Int))))
watchAlertCount =
  (fmap . fmap) (getMaybeView . _bakeView_alertCount) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_alertCount = viewJust 1
    }

watchSnapshotMeta :: MonadAppWidget js t m => m (Dynamic t (Maybe SnapshotMeta))
watchSnapshotMeta =
  (fmap . fmap) (getMaybeView . _bakeView_snapshotMeta) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_snapshotMeta = viewJust 1
    }

watchConnectedLedger :: MonadAppWidget js t m => m (Dynamic t (Maybe ConnectedLedger))
watchConnectedLedger = do
  (fmap . fmap) (join . getMaybeView . _bakeView_connectedLedger) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_connectedLedger = viewJust 1
    }

watchConnectedLedgerForced :: MonadAppWidget js t m => m (Dynamic t (Maybe ConnectedLedger))
watchConnectedLedgerForced = do
  -- this is in lieu of a nicer libusb solution to avoid constantly polling the device
  poll <- tickLossyFromPostBuildTime 5
  _ <- requestingIdentity $ public PublicRequest_PollLedgerDevice <$ poll
  watchConnectedLedger

watchLedgerAccounts :: MonadAppWidget js t m => Dynamic t [SecretKey] -> m (Dynamic t (MonoidalMap SecretKey (Either Text (PublicKeyHash, Maybe Tez))))
watchLedgerAccounts dkeys =
  -- (fmap . fmap) (fmapMaybe getFirst . getRangeView . _bakeView_showLedger) $ watchViewSelector $ ffor dkeys $ \keys -> mempty
  (fmap . fmap) (fmap (bimap getFirst id . toEither) . getRangeView . _bakeView_showLedger) $ watchViewSelector $ ffor dkeys $ \keys -> mempty
    { _bakeViewSelector_showLedger = RangeSelector $ AppendIMap.fromList $ ffor keys $ \k -> (ClosedInterval k k, 1)
    }

watchAmendment :: MonadAppWidget js t m => m (Dynamic t (Map VotingPeriodKind Amendment))
watchAmendment =
  (fmap . fmap) (MMap.getMonoidalMap . fmapMaybe getFirst . getRangeView . _bakeView_amendment) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_amendment = viewRangeAll 1 }

watchProposals :: MonadAppWidget js t m => m (Dynamic t (Map (Id PeriodProposal) (PeriodProposal, Maybe Bool)))
watchProposals =
  (fmap . fmap) (MMap.getMonoidalMap . fmapMaybe getFirst . getRangeView' . _bakeView_proposals) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_proposals = viewRangeAll 1
    }

watchProposal :: MonadAppWidget js t m => Dynamic t (Id PeriodProposal) -> m (Dynamic t (Maybe (PeriodProposal, Maybe Bool)))
watchProposal pid = do
  m <- (fmap . fmap) (fmapMaybe getFirst . getRangeView' . _bakeView_proposals) $ watchViewSelector $ ffor pid $ \p -> mempty
    { _bakeViewSelector_proposals = viewRangeExactly (Bounded p) 1
    }
  pure $ ffor2 pid m MMap.lookup

watchBakerVote :: MonadAppWidget js t m => m (Dynamic t (Maybe BakerVote))
watchBakerVote =
  (fmap . fmap) (join . getMaybeView . _bakeView_bakerVote) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_bakerVote = viewJust 1
    }

watchPeriodTestingVote :: MonadAppWidget js t m => m (Dynamic t (Maybe ((Id PeriodProposal, PeriodProposal), PeriodVote)))
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

watchPeriodTesting :: MonadAppWidget js t m => m (Dynamic t (Maybe ((Id PeriodProposal, PeriodProposal), PeriodTesting)))
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

watchPeriodPromotionVote :: MonadAppWidget js t m => m (Dynamic t (Maybe ((Id PeriodProposal, PeriodProposal), PeriodVote)))
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

watchPeriodAdoption :: MonadAppWidget js t m => m (Dynamic t (Maybe (Id PeriodProposal, PeriodProposal)))
watchPeriodAdoption = do
  adoption <- (fmap . fmap) (join . getMaybeView . _bakeView_periodAdoption) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_periodAdoption = viewJust 1
    }
  mId <- holdUniqDyn $ fmap _periodAdoption_proposal <$> adoption
  dm <- (fmap . fmap) (fmapMaybe getFirst . getRangeView' . _bakeView_proposals) $ watchViewSelector $ ffor mId $ \mp -> mempty
    { _bakeViewSelector_proposals = maybe mempty (\p -> viewRangeExactly (Bounded p) 1) mp
    }
  pure $ ffor2 adoption dm $ \mv m -> do
    v <- mv
    let pid = _periodAdoption_proposal v
    (pp, _) <- MMap.lookup pid m
    pure (pid, pp)

watchPrompting :: MonadAppWidget js t m => SecretKey -> m (Dynamic t (Maybe SetupState))
watchPrompting sk = do
  (fmap . fmap) (MMap.lookup sk . fmapMaybe getFirst . getRangeView . _bakeView_prompting) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_prompting = RangeSelector $ AppendIMap.singleton (ClosedInterval sk sk) 1
    }

watchVotePrompting :: MonadAppWidget js t m => SecretKey -> m (Dynamic t (Maybe VoteState))
watchVotePrompting sk = do
  (fmap . fmap) (MMap.lookup sk . fmapMaybe getFirst . getRangeView . _bakeView_votePrompting) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_votePrompting = RangeSelector $ AppendIMap.singleton (ClosedInterval sk sk) 1
    }

watchBakerRegistered :: MonadAppWidget js t m => PublicKeyHash -> m (Dynamic t (Maybe Bool))
watchBakerRegistered pkh = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_bakerRegistered = viewRangeExactly (Bounded pkh) 1
    }
  return $ ffor theView $ \v' -> MMap.lookup pkh $ getRangeView' (_bakeView_bakerRegistered v')

watchRightNotificationLimit :: MonadAppWidget js t m => RightKind -> m (Dynamic t (Maybe RightNotificationLimit))
watchRightNotificationLimit rk = do
  (fmap . fmap) (MMap.lookup rk . fmapMaybe getFirst . getRangeView . _bakeView_rightNotificationSettings) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_rightNotificationSettings = RangeSelector $ AppendIMap.singleton (ClosedInterval rk rk) 1
    }

validatingRange :: (View (RangeSelector e v) a -> b) -> (View (RangeSelector e v) a -> Maybe b)
validatingRange f v =
  if null $ _rangeView_support v
    then Nothing
    else Just $ f v
