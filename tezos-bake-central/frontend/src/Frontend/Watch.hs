{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Watch where

import Data.Fixed (Micro)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (Max (..), Min (..))
import Data.Semigroup.Foldable (foldMap1)
import Data.Time (UTCTime)
import Prelude hiding (log)
import Reflex.Dom.Core
import Rhyolite.Frontend.App (MonadRhyoliteFrontendWidget, watchViewSelector)
import Rhyolite.Schema (Email)
import Text.URI (URI)

import Tezos.NodeRPC.Sources (PublicNode)
import Tezos.Types

import Common.App
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import Common.Config (FrontendConfig)
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

watchClient :: (MonadRhyoliteFrontendWidget Bake t m) => Dynamic t (Id Client) -> m (Dynamic t (MonoidalMap (Id Client) ClientInfo))
watchClient cidDyn = do
  theView <- watchViewSelector . ffor cidDyn $ \cid -> mempty
    { _bakeViewSelector_clients = viewRangeExactly cid 1
    }
  return $ ffor theView $ \v -> fmapMaybe getFirst $ getRangeView (_bakeView_clients v)

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

watchClientAddresses :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (MonoidalMap (Id Client) URI))
watchClientAddresses = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_clientAddresses = viewRangeAll 1
    }
  return $ ffor theView $ \v' -> fmapMaybe getFirst $ getRangeView' $ _bakeView_clientAddresses v'

watchMailServer
  :: MonadRhyoliteFrontendWidget Bake t m
  => m (Dynamic t (Maybe (Maybe MailServerView)))
watchMailServer =
  (fmap . fmap) (getMaybeView . _bakeView_mailServer) $
    watchViewSelector $ pure $ mempty
      { _bakeViewSelector_mailServer = viewJust 1 }

watchSummary :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (Report, Int)))
watchSummary = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_summary = viewJust 1
    }
  improvingMaybe $ ffor theView $ \v -> getMaybeView $ _bakeView_summary v

watchNotificatees :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (Maybe [Email])))
watchNotificatees = do
  theView <- watchViewSelector . pure $ mempty
    { _bakeViewSelector_mailServer = viewJust 1
    }
  return $ fmap ((fmap . fmap) _mailServerView_notificatees . getMaybeView . _bakeView_mailServer) theView

watchSummaryGraph :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe (Micro, Text)))
watchSummaryGraph = holdDyn Nothing never -- "big" "TODO"
-- watchSummaryGraph = do
--   theView <- watchViewSelector . pure $ mempty
--     { _bakeViewSelector_summary = Just 1
--     }
--   improvingMaybe $ ffor theView $ \v -> join $ getSingle $ _bakeView_summaryGraph v

-- TODO: filter by alert type (that is, ErrorLogView constructor, or logical groups of such)
watchErrors
  :: MonadRhyoliteFrontendWidget Bake t m
  => Dynamic t AlertsFilter
  -> Dynamic t (Set (ClosedInterval (WithInfinity UTCTime)))
  -> m (Dynamic t (MMap.MonoidalMap (Id ErrorLog) (ErrorLog, ErrorLogView)))
watchErrors alerts intervals = do
  v <- watchViewSelector $ ffor2 alerts intervals $ \alert ivals -> mempty
    { _bakeViewSelector_errors = MMap.singleton alert $ viewIntervalSet ivals 1
    }
  -- TOOD: maybe we should just fix up IntervalSelector to operate on some semigroup instead of Set
  return $ fmap  (fmapMaybe (getFirst . fst . getFirst) . _intervalView_elements . fold) $ MMap.lookup <$> alerts <*> (_bakeView_errors <$> v)

watchErrorsByNode
  :: MonadRhyoliteFrontendWidget Bake t m
  => Dynamic t (Set (ClosedInterval (WithInfinity UTCTime)))
  -> m (Dynamic t (MonoidalMap (Id Node) (NonEmpty (ErrorLog, NodeErrorLogView))))
watchErrorsByNode alertWindow = do
  dXs <- watchErrors (pure AlertsFilter_UnresolvedOnly) alertWindow
  pure $ ffor dXs $ \xs -> MMap.fromListWith (<>)
    [ (k, pure (l, t'))
    | (l@ErrorLog{_errorLog_stopped = Nothing}, t) <- MMap.elems xs
    , Just t' <- [nodeErrorViewOnly t]
    , let k = nodeIdForNodeErrorLogView t'
    ]

watchErrorsByBaker
  :: MonadRhyoliteFrontendWidget Bake t m
  => Dynamic t (Set (ClosedInterval (WithInfinity UTCTime)))
  -> m (Dynamic t (MonoidalMap PublicKeyHash (NonEmpty (ErrorLog, BakerErrorLogView))))
watchErrorsByBaker alertWindow = do
  dXs <- watchErrors (pure AlertsFilter_UnresolvedOnly) alertWindow
  pure $ ffor dXs $ \xs -> MMap.fromListWith (<>)
    [ (k, pure (l, t'))
    | (l@ErrorLog{_errorLog_stopped = Nothing}, t) <- MMap.elems xs
    , Just t' <- [bakerErrorViewOnly t]
    , let k = bakerIdForBakerErrorLogView t'
    ]

data CollectiveNodesFailure
  = CollectiveNodesFailure_NoNodes
  | CollectiveNodesFailure_AllNodesDownSince UTCTime
  -- ^ The last time any of the node was up, there must have been up
  deriving (Eq, Ord, Show)

watchCollectiveNodesStatus
  :: MonadRhyoliteFrontendWidget Bake t m
  => Dynamic t (Set (ClosedInterval (WithInfinity UTCTime)))
  -> m (Dynamic t (Either CollectiveNodesFailure ()))
watchCollectiveNodesStatus alertWindow = do
  dNodes <- watchNodeAddresses -- $ pure $ viewRangeAll ()
  let dmNids = NEL.nonEmpty . MMap.keys <$> dNodes
  ebn <- watchErrorsByNode alertWindow
  holdUniqDyn $ ffor2 dmNids ebn $ \case
    Nothing -> const $ Left $ CollectiveNodesFailure_NoNodes
    Just nids -> \nodeErrors -> case
        NEL.nonEmpty $ fforMaybe (NEL.toList nids) $ \nid ->
          fmap getMin $ foldMap (Just . Min) $ (_errorLog_started . fst)
            <$> maybe [] toList (MMap.lookup nid nodeErrors)
      of
        Nothing -> Right ()
        Just errors -> Left $ CollectiveNodesFailure_AllNodesDownSince $ getMax $ foldMap1 Max errors

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

watchAlertCount :: MonadRhyoliteFrontendWidget Bake t m => m (Dynamic t (Maybe Int))
watchAlertCount =
  (fmap . fmap) (getMaybeView . _bakeView_alertCount) $ watchViewSelector $ pure $ mempty
    { _bakeViewSelector_alertCount = viewJust 1
    }

validatingRange :: (View (RangeSelector e v) a -> b) -> (View (RangeSelector e v) a -> Maybe b)
validatingRange f v =
  if null $ _rangeView_support v
    then Nothing
    else Just $ f v
