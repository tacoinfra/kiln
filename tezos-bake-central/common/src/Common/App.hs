{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Common.App where

import Data.Bifunctor
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Align (Align (alignWith, nil))
import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as Map
import Data.Fixed (Micro)
import Data.Semigroup (First (..), Semigroup, (<>), Option(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.These (These (That, This), mergeThese, these)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Version (Version)
import Data.Word (Word16)
import GHC.Generics (Generic)
import Reflex (Additive, FunctorMaybe (..), Group (..))
import Reflex.Aeson.Orphans ()
import Reflex.Query.Class (Query (QueryResult, crop), SelectedCount)
import Rhyolite.App (HasView, View, ViewSelector) -- that Single is not so good eh.
import Rhyolite.Schema (Email, Id)

import Tezos.Types

import Common
import Common.AppendIntervalMap (AppendIntervalMap, ClosedInterval, WithInfinity)
import qualified Common.AppendIntervalMap as AppendIMap
import Common.Schema
import qualified Rhyolite.SemiMap as Rhyolite
import qualified Data.Map.Monoidal as MonoidalMap

restrictKeys :: Ord k => AppendMap k a -> Set k -> AppendMap k a
restrictKeys m ks = Map.filterWithKey (\k _ -> k `Set.member` ks) m

data UniversalMap k a = UniversalMap
  { _universalMap_universe :: !(Maybe a)
  , _universalMap_only :: !(AppendMap k a)
  } deriving (Eq, Ord, Show, Generic, Typeable, Functor, Traversable, Foldable)

instance (Ord k, FromJSON k, FromJSON a) => FromJSON (UniversalMap k a)
instance (Ord k, ToJSON k, ToJSON a) => ToJSON (UniversalMap k a)

instance (Ord k, Semigroup a) => Semigroup (UniversalMap k a) where
  a <> b = UniversalMap
    { _universalMap_universe = _universalMap_universe a <> _universalMap_universe b
    , _universalMap_only = _universalMap_only a <> _universalMap_only b
    }

instance (Ord k, Semigroup a) => Monoid (UniversalMap k a) where
  mempty = UniversalMap Nothing mempty
  mappend = (<>)

instance Ord k => Align (UniversalMap k) where
  nil = UniversalMap nil nil
  alignWith f u v = UniversalMap
    { _universalMap_universe = alignWith f (_universalMap_universe u) (_universalMap_universe v)
    , _universalMap_only = alignWith f (_universalMap_only u) (_universalMap_only v)
    }

instance FunctorMaybe (UniversalMap k) where
  fmapMaybe f a = UniversalMap
    { _universalMap_universe = fmapMaybe f (_universalMap_universe a)
    , _universalMap_only = fmapMaybe f (_universalMap_only a)
    }

newtype Single t a = Single { unSingle :: Maybe (t, a) }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic, Typeable)

instance Semigroup a => Semigroup (Single t a) where
  (<>) (Single Nothing) y = y
  (<>) x (Single Nothing) = x
  (<>) (Single (Just (t, a))) (Single (Just (_t, a'))) = Single $ Just (t, a <> a')

instance Bifunctor Single where
  bimap f g (Single x) = Single $ fmap (bimap f g) x

instance Semigroup a => Monoid (Single t a) where
  mempty = Single Nothing
  mappend = (<>)

instance FunctorMaybe (Single t) where
  fmapMaybe f (Single (Just (t, x))) | Just y <- f x = Single (Just (t, y))
  fmapMaybe _ _ = Single Nothing

instance (FromJSON t, FromJSON a) => FromJSON (Single t a)
instance (ToJSON t, ToJSON a) => ToJSON (Single t a)

single :: t -> a -> Single t a
single t a = Single $ Just (t, a)

getSingle :: Single t a -> Maybe t
getSingle = fmap fst . unSingle


universe :: (Ord k, Semigroup a) => a -> UniversalMap k a
universe a = UniversalMap (Just a) mempty

usingleton :: k -> a -> UniversalMap k a
usingleton k a = UniversalMap Nothing (Map.singleton k a)

uintersectionWith :: Ord k => (a -> b -> c) -> AppendMap k a -> UniversalMap k b -> AppendMap k c
uintersectionWith f as bs = case _universalMap_universe bs of
  Nothing -> Map.intersectionWith f as (_universalMap_only bs)
  Just b -> fmap (flip f b) as -- Ignore the "only" keys since the universal key always wins

ulookup :: Ord k => k -> UniversalMap k a -> Maybe a
ulookup k m = case _universalMap_universe m of
  Nothing -> Map.lookup k (_universalMap_only m)
  Just a -> Just a -- Ignore the "only" keys since the universal key always wins

data Bake = Bake

type TimeWindow = ClosedInterval (WithInfinity UTCTime)

newtype SemiSet k = SemiSet { unSemiSet :: Rhyolite.SemiSet k }
  deriving (Eq, Ord, Show, Read, Generic, Typeable, FromJSON, ToJSON, Semigroup, Monoid)

fromListSemiSet :: Ord k => [k] -> SemiSet k
fromListSemiSet = completeSemiSet . Set.fromList

completeSemiSet :: Set k -> SemiSet k
completeSemiSet = SemiSet . Rhyolite.SemiMap_Complete . MonoidalMap.fromSet (const ())

deleteSemiMap :: k -> SemiSet k
deleteSemiMap = SemiSet .  Rhyolite.SemiMap_Partial . flip MonoidalMap.singleton (First $ Nothing)

insertSemiMap :: k -> SemiSet k
insertSemiMap = SemiSet .  Rhyolite.SemiMap_Partial . flip MonoidalMap.singleton (First $ Just ())

getSemiSet = Rhyolite.knownKeysSet . unSemiSet

instance Foldable SemiSet where
  foldMap f = foldMap f . Rhyolite.knownKeysSet . unSemiSet
  length = length . Rhyolite.knownKeysSet . unSemiSet

data BakeViewSelector a = BakeViewSelector
  { _bakeViewSelector_summary :: !(Maybe a)
  , _bakeViewSelector_clientAddresses :: !(Maybe a)
  , _bakeViewSelector_clients :: !(AppendMap (Id Client) a)
  , _bakeViewSelector_parameters :: !(Maybe a)
  , _bakeViewSelector_nodeAddresses :: !(Maybe a)
  , _bakeViewSelector_tzscan :: !(Maybe a)
  , _bakeViewSelector_nodes :: !(UniversalMap (Id Node) a)
  , _bakeViewSelector_delegates :: !(Maybe a)
  , _bakeViewSelector_delegateStats :: !(AppendMap (PublicKeyHash, RawLevel) a)
  , _bakeViewSelector_notificatees :: !(Maybe a)
  , _bakeViewSelector_mailServer :: !(Maybe a)
  , _bakeViewSelector_errors :: !(AppendIntervalMap TimeWindow a)
  , _bakeViewSelector_upgrade :: Maybe a
  } deriving (Show, Eq, Ord, Functor, Generic, Typeable, Traversable, Foldable)

data BakeView a = BakeView
  { _bakeView_clientAddresses :: !(AppendMap (Id Client) (First (Maybe ClientAddress), a))
  , _bakeView_clients :: !(AppendMap (Id Client) (First (Maybe ClientInfo), a))
  , _bakeView_parameters :: !(Single (Maybe ProtoInfo) a)
  , _bakeView_nodeAddresses :: !(AppendMap (Id Node) (First (Maybe ClientAddress), a))
  , _bakeView_tzscan :: !(Single (Maybe TzScan) a)
  , _bakeView_nodes :: !(AppendMap (Id Node) (First (Maybe Node), a))
  , _bakeView_delegates :: !(Option ((SemiSet PublicKeyHash), a))
  , _bakeView_delegateStats :: !(AppendMap (PublicKeyHash, RawLevel) (First (Maybe (BakeEfficiency, Account)), a))
  , _bakeView_notificatees :: !(AppendMap (Id Notificatee) (First (Maybe Email), a))
  , _bakeView_mailServer :: !(Single (Maybe MailServerView) a)
  , _bakeView_summary :: !(Single (Maybe (Report, Int)) a) -- The Int is the number of bakers we've yet to get a report from.
  , _bakeView_summaryGraph :: !(Single (Maybe (Micro, Text)) a)
  , _bakeView_graphs :: !(AppendMap (Id Client) (First (Maybe (Micro, Text)), a))
  , _bakeView_errors :: !(AppendIntervalMap TimeWindow (Set (Id ErrorLog), a))
  , _bakeView_errorsById :: !(AppendMap (Id ErrorLog) (First (Maybe (ErrorLog, ErrorLogView))))
  , _bakeView_upgrade :: !(Single (Maybe (ErrorLog, Either UpgradeCheckError Version)) a)
  } deriving (Show, Eq, Functor, Generic, Typeable, Traversable, Foldable)


data MailServerView = MailServerView
  { _mailServerView_hostName :: Text
  , _mailServerView_portNumber :: Word16
  , _mailServerView_smtpProtocol :: SmtpProtocol
  , _mailServerView_userName :: Text
  } deriving (Eq, Generic, Typeable, Read, Show)
instance FromJSON MailServerView
instance ToJSON MailServerView

data ErrorLogView
  = ErrorLogView_InaccessibleEndpoint ErrorLogInaccessibleEndpoint
  | ErrorLogView_BakerNoHeartbeat ErrorLogBakerNoHeartbeat
  | ErrorLogView_NodeOnFork ErrorLogNodeOnFork
  | ErrorLogView_MultipleBakersForSameDelegate ErrorLogMultipleBakersForSameDelegate
  deriving (Eq, Generic, Typeable, Show)
instance FromJSON ErrorLogView
instance ToJSON ErrorLogView

mailServerConfigToView :: MailServerConfig -> MailServerView
mailServerConfigToView x = MailServerView
  { _mailServerView_hostName = _mailServerConfig_hostName x
  , _mailServerView_portNumber = _mailServerConfig_portNumber x
  , _mailServerView_smtpProtocol = _mailServerConfig_smtpProtocol x
  , _mailServerView_userName = _mailServerConfig_userName x
  }

cropBakeView :: (Semigroup a, Monoid a) => BakeViewSelector a -> BakeView a -> BakeView a
cropBakeView vs v =
  let clientAddresses = case _bakeViewSelector_clientAddresses vs of
        Nothing -> mempty
        Just _ -> _bakeView_clientAddresses v
      clients = Map.intersectionWith const (_bakeView_clients v) (_bakeViewSelector_clients vs)
      parameters = case _bakeViewSelector_parameters vs of
        Nothing -> mempty
        Just _ -> _bakeView_parameters v
      nodeAddresses = case _bakeViewSelector_nodeAddresses vs of
        Nothing -> mempty
        Just _ -> _bakeView_nodeAddresses v
      delegates = case _bakeViewSelector_delegates vs of
        Nothing -> mempty
        Just _ -> _bakeView_delegates v
      tzscan = case _bakeViewSelector_tzscan vs of
        Nothing -> mempty
        Just _ -> _bakeView_tzscan v
      nodes = uintersectionWith const (_bakeView_nodes v) (_bakeViewSelector_nodes vs)
      delegateStats = Map.intersectionWith const (_bakeView_delegateStats v) (_bakeViewSelector_delegateStats vs)
      notificatees = case _bakeViewSelector_notificatees vs of
        Nothing -> mempty
        Just _ -> _bakeView_notificatees v
      mailServer = case _bakeViewSelector_mailServer vs of
        Nothing -> mempty
        Just _ -> _bakeView_mailServer v
      graphs = Map.intersectionWith const (_bakeView_graphs v) (_bakeViewSelector_clients vs)
      summary = case _bakeViewSelector_summary vs of
        Nothing -> mempty
        Just _ -> _bakeView_summary v
      summaryGraph = case _bakeViewSelector_summary vs of
        Nothing -> mempty
        Just _ -> _bakeView_summaryGraph v
      errors = AppendIMap.intersectionWith const (_bakeView_errors v) (_bakeViewSelector_errors vs)
      upgrade = case _bakeViewSelector_upgrade vs of
        Nothing -> mempty
        Just _ -> _bakeView_upgrade v
  in BakeView
      { _bakeView_clientAddresses = clientAddresses
      , _bakeView_clients = clients
      , _bakeView_parameters = parameters
      , _bakeView_nodeAddresses = nodeAddresses
      , _bakeView_tzscan = tzscan
      , _bakeView_nodes = nodes
      , _bakeView_delegates = delegates
      , _bakeView_delegateStats = delegateStats
      , _bakeView_notificatees = notificatees
      , _bakeView_mailServer = mailServer
      , _bakeView_graphs = graphs
      , _bakeView_summaryGraph = summaryGraph
      , _bakeView_summary = summary
      , _bakeView_errors = errors
      , _bakeView_errorsById = restrictKeys (_bakeView_errorsById v) (foldMap fst $ AppendIMap.elems errors)
      , _bakeView_upgrade = upgrade
      }

instance Align BakeViewSelector where
  nil = BakeViewSelector nil nil nil nil nil nil nil nil nil nil nil nil nil
  alignWith f u v = BakeViewSelector
    { _bakeViewSelector_clientAddresses = alignWith f (_bakeViewSelector_clientAddresses u) (_bakeViewSelector_clientAddresses v)
    , _bakeViewSelector_summary = alignWith f (_bakeViewSelector_summary u) (_bakeViewSelector_summary v)
    , _bakeViewSelector_clients = alignWith f (_bakeViewSelector_clients u) (_bakeViewSelector_clients v)
    , _bakeViewSelector_parameters = alignWith f (_bakeViewSelector_parameters u) (_bakeViewSelector_parameters v)
    , _bakeViewSelector_tzscan = alignWith f (_bakeViewSelector_tzscan u) (_bakeViewSelector_tzscan v)
    , _bakeViewSelector_nodes = alignWith f (_bakeViewSelector_nodes u) (_bakeViewSelector_nodes v)
    , _bakeViewSelector_delegates = alignWith f (_bakeViewSelector_delegates u) (_bakeViewSelector_delegates v)
    , _bakeViewSelector_delegateStats =  alignWith f (_bakeViewSelector_delegateStats u) (_bakeViewSelector_delegateStats v)
    , _bakeViewSelector_notificatees = alignWith f (_bakeViewSelector_notificatees u) (_bakeViewSelector_notificatees v)
    , _bakeViewSelector_mailServer = alignWith f (_bakeViewSelector_mailServer u) (_bakeViewSelector_mailServer v)
    , _bakeViewSelector_nodeAddresses = alignWith f (_bakeViewSelector_nodeAddresses u) (_bakeViewSelector_nodeAddresses v)
    , _bakeViewSelector_errors = alignWith f (_bakeViewSelector_errors u) (_bakeViewSelector_errors v)
    , _bakeViewSelector_upgrade = alignWith f (_bakeViewSelector_upgrade u) (_bakeViewSelector_upgrade v)
    }

instance FunctorMaybe BakeViewSelector where
  fmapMaybe f a = BakeViewSelector
    { _bakeViewSelector_clientAddresses = fmapMaybe f $ _bakeViewSelector_clientAddresses a
    , _bakeViewSelector_summary = fmapMaybe f $ _bakeViewSelector_summary a
    , _bakeViewSelector_clients = fmapMaybe f $ _bakeViewSelector_clients a
    , _bakeViewSelector_parameters = fmapMaybe f $ _bakeViewSelector_parameters a
    , _bakeViewSelector_tzscan = fmapMaybe f $ _bakeViewSelector_tzscan a
    , _bakeViewSelector_nodes = fmapMaybe f $ _bakeViewSelector_nodes a
    , _bakeViewSelector_delegates = fmapMaybe f $ _bakeViewSelector_delegates a
    , _bakeViewSelector_delegateStats = fmapMaybe f $ _bakeViewSelector_delegateStats a
    , _bakeViewSelector_notificatees = fmapMaybe f $ _bakeViewSelector_notificatees a
    , _bakeViewSelector_mailServer = fmapMaybe f $ _bakeViewSelector_mailServer a
    , _bakeViewSelector_nodeAddresses = fmapMaybe f $ _bakeViewSelector_nodeAddresses a
    , _bakeViewSelector_errors = fmapMaybe f $ _bakeViewSelector_errors a
    , _bakeViewSelector_upgrade = fmapMaybe f $ _bakeViewSelector_upgrade a
    }

deriving instance FunctorMaybe Option

instance FunctorMaybe BakeView where
  fmapMaybe f a = BakeView
    { _bakeView_clientAddresses = fmapMaybeSnd f $ _bakeView_clientAddresses a
    , _bakeView_clients = fmapMaybeSnd f $ _bakeView_clients a
    , _bakeView_parameters = fmapMaybe f $ _bakeView_parameters a
    , _bakeView_tzscan = fmapMaybe f $ _bakeView_tzscan a
    , _bakeView_nodes = fmapMaybeSnd f $ _bakeView_nodes a
    , _bakeView_delegates = fmapMaybe (traverse f) $ _bakeView_delegates a
    , _bakeView_delegateStats = fmapMaybeSnd f $ _bakeView_delegateStats a
    , _bakeView_notificatees = fmapMaybeSnd f $ _bakeView_notificatees a
    , _bakeView_mailServer = fmapMaybe f $ _bakeView_mailServer a
    , _bakeView_graphs = fmapMaybeSnd f $ _bakeView_graphs a
    , _bakeView_summaryGraph = fmapMaybe f $ _bakeView_summaryGraph a
    , _bakeView_summary = fmapMaybe f $ _bakeView_summary a
    , _bakeView_nodeAddresses = fmapMaybeSnd f (_bakeView_nodeAddresses a)
    , _bakeView_errors = errors
    , _bakeView_errorsById =
        -- Crop the 'ErrorLog's to only those with the IDs referenced in the cropped set of errors.
        restrictKeys (_bakeView_errorsById a) (foldMap fst $ AppendIMap.elems errors)
    , _bakeView_upgrade = fmapMaybe f $ _bakeView_upgrade a
    }
    where
      errors = fmapMaybeSnd f (_bakeView_errors a)

fmapMaybeSnd :: FunctorMaybe f => (a -> Maybe b) -> f (e, a) -> f (e, b)
fmapMaybeSnd f = fmapMaybe $ \(e, a) -> case f a of
  Nothing -> Nothing
  Just b -> Just (e, b)

alignTheseWith :: Align f => (These a b -> c) -> These (f a) (f b) -> f c
alignTheseWith f = these (fmap (f . This)) (fmap (f . That)) (alignWith f)

instance Monoid a => Monoid (BakeViewSelector a) where
  mempty = nil
  mappend = alignWith (mergeThese mappend)

instance Monoid a => Semigroup (BakeViewSelector a) where
  (<>) = mappend

instance Group (BakeViewSelector SelectedCount) where
  negateG = fmap negateG

instance Additive (BakeViewSelector SelectedCount)

instance (Semigroup a, Monoid a) => Monoid (BakeView a) where
  mempty = BakeView
    { _bakeView_clientAddresses = mempty
    , _bakeView_clients = mempty
    , _bakeView_parameters = mempty
    , _bakeView_tzscan = mempty
    , _bakeView_nodes = mempty
    , _bakeView_delegates = mempty
    , _bakeView_delegateStats = mempty
    , _bakeView_notificatees = mempty
    , _bakeView_mailServer = mempty
    , _bakeView_graphs = mempty
    , _bakeView_summaryGraph = mempty
    , _bakeView_summary = mempty
    , _bakeView_nodeAddresses = mempty
    , _bakeView_errors = mempty
    , _bakeView_errorsById = mempty
    , _bakeView_upgrade = mempty
    }
  mappend u v = u <> v

instance Semigroup a => Semigroup (BakeView a) where
  u <> v = BakeView
    { _bakeView_clientAddresses = _bakeView_clientAddresses u <> _bakeView_clientAddresses v
    , _bakeView_clients = _bakeView_clients u <> _bakeView_clients v
    , _bakeView_parameters = _bakeView_parameters u <> _bakeView_parameters v
    , _bakeView_tzscan = _bakeView_tzscan u <> _bakeView_tzscan v
    , _bakeView_nodes = _bakeView_nodes u <> _bakeView_nodes v
    , _bakeView_delegates = _bakeView_delegates u <> _bakeView_delegates v
    , _bakeView_delegateStats = _bakeView_delegateStats u <> _bakeView_delegateStats v
    , _bakeView_notificatees = _bakeView_notificatees u <> _bakeView_notificatees v
    , _bakeView_mailServer = _bakeView_mailServer u <> _bakeView_mailServer v
    , _bakeView_summaryGraph = _bakeView_summaryGraph u <> _bakeView_summaryGraph v
    , _bakeView_graphs = _bakeView_graphs u <> _bakeView_graphs v
    , _bakeView_summary = _bakeView_summary u <> _bakeView_summary v
    , _bakeView_nodeAddresses = _bakeView_nodeAddresses u <> _bakeView_nodeAddresses v
    , _bakeView_errors = _bakeView_errors u <> _bakeView_errors v
    , _bakeView_errorsById = _bakeView_errorsById u <> _bakeView_errorsById v
    , _bakeView_upgrade = _bakeView_upgrade u <> _bakeView_upgrade v
    }

instance (Monoid a, Semigroup a) => Query (BakeViewSelector a) where
  type QueryResult (BakeViewSelector a) = BakeView a
  crop = cropBakeView

instance (Semigroup a, FromJSON a) => FromJSON (BakeViewSelector a)
instance (Semigroup a, FromJSON a) => FromJSON (BakeView a)

instance ToJSON a => ToJSON (BakeViewSelector a)
instance ToJSON a => ToJSON (BakeView a)

instance HasView Bake where
  type View Bake = BakeView
  type ViewSelector Bake = BakeViewSelector

concat <$> traverse makeLenses
  [ 'BakeView
  , 'BakeViewSelector
  , 'MailServerView
  ]
