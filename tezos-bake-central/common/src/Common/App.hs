{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Foldable(fold)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Align (Align (alignWith, nil))
import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as Map
import Data.Functor.Compose
import Data.Fixed (Micro)
import Data.Semigroup (First (..), Semigroup, (<>), Option(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.These (These (..), mergeThese, these)
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
-- import Rhyolite.SemiMap (SemiMap(..))
-- import qualified Rhyolite.SemiMap as Rhyolite
import qualified Data.Map.Monoidal as MonoidalMap

import Common.Vassal

restrictKeys :: Ord k => AppendMap k a -> Set k -> AppendMap k a
restrictKeys m ks = Map.filterWithKey (\k _ -> k `Set.member` ks) m

summary :: Semigroup v => AppendMap k v -> Maybe v
summary = Map.lookup () . Map.mapKeysWith (<>) (const ())

-- universe :: a -> SemiMapSelector k a
-- universe = SemiMapSelector_Complete
-- 
-- usingleton :: k -> a -> SemiMapSelector k a
-- usingleton k a = SemiMapSelector_Partial (Map.singleton k a)
-- 
-- ulookup :: Ord k => k -> SemiMapSelector k a -> Maybe a
-- ulookup k (SemiMapSelector_Partial xs) = Map.lookup k xs
-- ulookup k (SemiMapSelector_Complete a) = Just a

data Bake = Bake

-- type TimeWindow = ClosedInterval (WithInfinity UTCTime)

-- fromListSemiSet :: Ord k => [k] -> SemiSet k
-- fromListSemiSet = completeSemiSet . Set.fromList
-- 
-- completeSemiSet :: Set k -> SemiSet k
-- completeSemiSet = SemiSet . Rhyolite.SemiMap_Complete . MonoidalMap.fromSet (const ())
-- 
-- deleteSemiMap :: k -> SemiSet k
-- deleteSemiMap = SemiSet .  Rhyolite.SemiMap_Partial . flip MonoidalMap.singleton (First $ Nothing)
-- 
-- insertSemiMap :: k -> SemiSet k
-- insertSemiMap = SemiSet .  Rhyolite.SemiMap_Partial . flip MonoidalMap.singleton (First $ Just ())
-- 
-- getSemiSet = Rhyolite.knownKeysSet . unSemiSet
-- 
-- instance Foldable SemiSet where
--   foldMap f = foldMap f . Rhyolite.knownKeysSet . unSemiSet
--   length = length . Rhyolite.knownKeysSet . unSemiSet

-- | a way to get sharing for things that could appear multipe times in a View

type ErrorInfo = (ErrorLog, ErrorLogView)

-- data BakeViewSummary = 
--   { report :: Report
--   , unreporting :: Int
--   , graph :: AppendMap (Id Client) (Micro, Text)
--   , 

data BakeViewSelector a = BakeViewSelector
  { _bakeViewSelector_clientAddresses ::  !(RangeSelector (Id Client) (Maybe ClientAddress) a)
  , _bakeViewSelector_clients ::          !(RangeSelector (Id Client) ClientInfo a)
  , _bakeViewSelector_delegateStats ::    !(ComposeSelector (RangeSelector PublicKeyHash Account) (IntervalSelector RawLevel BakeEfficiency) a)
  , _bakeViewSelector_delegates ::        !(RangeSelector PublicKeyHash () a)
  , _bakeViewSelector_errors ::           !(IntervalSelector UTCTime ErrorInfo a)
  , _bakeViewSelector_mailServer ::       !(MaybeSelector (Maybe MailServerView) a)
  , _bakeViewSelector_nodeAddresses ::    !(RangeSelector (Id Node) ClientAddress a)
  , _bakeViewSelector_nodes ::            !(RangeSelector (Id Node) Node a)
  , _bakeViewSelector_notificatees ::     !(RangeSelector (Id Notificatee) (First (Maybe Email)) a)
  , _bakeViewSelector_parameters ::       !(MaybeSelector ProtoInfo a)
  , _bakeViewSelector_summary ::          !(MaybeSelector (Report, Int) a) -- The Int is the number of bakers we've yet to get a report from.
  , _bakeViewSelector_tzscan ::           !(MaybeSelector TzScan a)
  , _bakeViewSelector_upgrade ::          !(MaybeSelector (ErrorLog, Either UpgradeCheckError Version) a)
  } deriving (Functor, Generic, Typeable, Traversable, Foldable)

deriving instance (Show a) => Show (BakeViewSelector a)
deriving instance (Eq a) => Eq (BakeViewSelector a)
deriving instance (Ord a) => Ord (BakeViewSelector a)

-- (Show (ComposeView (RangeSelector PublicKeyHash ()) (IntervalSelector RawLevel (BakeEfficiency, Account)) a))

data BakeView a = BakeView
  { _bakeView_clientAddresses ::  !(RangeView (Id Client) (Maybe ClientAddress) a)
  , _bakeView_clients ::          !(RangeView (Id Client) ClientInfo a)
  , _bakeView_delegateStats ::    !(ComposeView (RangeSelector PublicKeyHash Account) (IntervalSelector RawLevel BakeEfficiency) a)
  , _bakeView_delegates ::        !(RangeView PublicKeyHash () a)
  , _bakeView_errors ::           !(IntervalView UTCTime ErrorInfo a)
  , _bakeView_mailServer ::       !(MaybeView (Maybe MailServerView) a)
  , _bakeView_nodeAddresses ::    !(RangeView (Id Node) ClientAddress a)
  , _bakeView_nodes ::            !(RangeView (Id Node) Node a)
  , _bakeView_notificatees ::     !(RangeView (Id Notificatee) (First (Maybe Email)) a)
  , _bakeView_parameters ::       !(MaybeView ProtoInfo a)
  , _bakeView_summary ::          !(MaybeView (Report, Int) a) -- The Int is the number of bakers we've yet to get a report from.
  , _bakeView_tzscan ::           !(MaybeView TzScan a)
  , _bakeView_upgrade ::          !(MaybeView (ErrorLog, Either UpgradeCheckError Version) a)
  -- , _bakeView_graphs ::           !(AppendMap (Id Client) (First (Maybe (Micro, Text)), a))
  -- , _bakeView_summaryGraph ::     !(Single (Maybe (Micro, Text)) a)
  } deriving (Functor, Generic, Typeable, Traversable, Foldable)

deriving instance Show a => Show (BakeView a)
deriving instance Eq a => Eq (BakeView a)
deriving instance Ord a => Ord (BakeView a)



data MailServerView = MailServerView
  { _mailServerView_hostName :: Text
  , _mailServerView_portNumber :: Word16
  , _mailServerView_smtpProtocol :: SmtpProtocol
  , _mailServerView_userName :: Text
  } deriving (Eq, Ord, Generic, Typeable, Read, Show)
instance FromJSON MailServerView
instance ToJSON MailServerView

data ErrorLogView
  = ErrorLogView_InaccessibleEndpoint ErrorLogInaccessibleEndpoint
  | ErrorLogView_BakerNoHeartbeat ErrorLogBakerNoHeartbeat
  | ErrorLogView_NodeOnFork ErrorLogNodeOnFork
  | ErrorLogView_MultipleBakersForSameDelegate ErrorLogMultipleBakersForSameDelegate
  deriving (Eq, Ord, Generic, Typeable, Show)
instance FromJSON ErrorLogView
instance ToJSON ErrorLogView

mailServerConfigToView :: MailServerConfig -> MailServerView
mailServerConfigToView x = MailServerView
  { _mailServerView_hostName = _mailServerConfig_hostName x
  , _mailServerView_portNumber = _mailServerConfig_portNumber x
  , _mailServerView_smtpProtocol = _mailServerConfig_smtpProtocol x
  , _mailServerView_userName = _mailServerConfig_userName x
  }

cropBakeView :: (Semigroup a) => BakeViewSelector a -> BakeView b -> BakeView a
cropBakeView vs v = BakeView
      { _bakeView_clientAddresses = cropView (_bakeViewSelector_clientAddresses vs) (_bakeView_clientAddresses v)
      , _bakeView_clients         = cropView (_bakeViewSelector_clients         vs) (_bakeView_clients v)
      , _bakeView_parameters      = cropView (_bakeViewSelector_parameters      vs) (_bakeView_parameters v)
      , _bakeView_nodeAddresses   = cropView (_bakeViewSelector_nodeAddresses   vs) (_bakeView_nodeAddresses v)
      , _bakeView_tzscan          = cropView (_bakeViewSelector_tzscan          vs) (_bakeView_tzscan v)
      , _bakeView_nodes           = cropView (_bakeViewSelector_nodes           vs) (_bakeView_nodes v)
      , _bakeView_delegates       = cropView (_bakeViewSelector_delegates       vs) (_bakeView_delegates v)
      , _bakeView_delegateStats   = cropView (_bakeViewSelector_delegateStats   vs) (_bakeView_delegateStats v)
      , _bakeView_notificatees    = cropView (_bakeViewSelector_notificatees    vs) (_bakeView_notificatees v)
      , _bakeView_mailServer      = cropView (_bakeViewSelector_mailServer      vs) (_bakeView_mailServer v)
      , _bakeView_summary         = cropView (_bakeViewSelector_summary         vs) (_bakeView_summary v)
      , _bakeView_errors          = cropView (_bakeViewSelector_errors          vs) (_bakeView_errors v)
      , _bakeView_upgrade         = cropView (_bakeViewSelector_upgrade         vs) (_bakeView_upgrade v)
      -- , _bakeView_graphs = graphs
      -- , _bakeView_summaryGraph = summaryGraph
      }

-- instance Align BakeViewSelector where
--   nil = BakeViewSelector nil nil nil nil nil nil nil nil nil nil nil nil nil
--   alignWith f u v = BakeViewSelector
--     { _bakeViewSelector_clientAddresses = alignWith f (_bakeViewSelector_clientAddresses u) (_bakeViewSelector_clientAddresses v)
--     , _bakeViewSelector_summary = alignWith f (_bakeViewSelector_summary u) (_bakeViewSelector_summary v)
--     , _bakeViewSelector_clients = alignWith f (_bakeViewSelector_clients u) (_bakeViewSelector_clients v)
--     , _bakeViewSelector_parameters = alignWith f (_bakeViewSelector_parameters u) (_bakeViewSelector_parameters v)
--     , _bakeViewSelector_tzscan = alignWith f (_bakeViewSelector_tzscan u) (_bakeViewSelector_tzscan v)
--     , _bakeViewSelector_nodes = alignWith f (_bakeViewSelector_nodes u) (_bakeViewSelector_nodes v)
--     , _bakeViewSelector_delegates = alignWith f (_bakeViewSelector_delegates u) (_bakeViewSelector_delegates v)
--     , _bakeViewSelector_delegateStats =  alignWith f (_bakeViewSelector_delegateStats u) (_bakeViewSelector_delegateStats v)
--     , _bakeViewSelector_notificatees = alignWith f (_bakeViewSelector_notificatees u) (_bakeViewSelector_notificatees v)
--     , _bakeViewSelector_mailServer = alignWith f (_bakeViewSelector_mailServer u) (_bakeViewSelector_mailServer v)
--     , _bakeViewSelector_nodeAddresses = alignWith f (_bakeViewSelector_nodeAddresses u) (_bakeViewSelector_nodeAddresses v)
--     , _bakeViewSelector_errors = alignWith f (_bakeViewSelector_errors u) (_bakeViewSelector_errors v)
--     , _bakeViewSelector_upgrade = alignWith f (_bakeViewSelector_upgrade u) (_bakeViewSelector_upgrade v)
--     }

-- instance FunctorMaybe BakeViewSelector where
--   fmapMaybe f a = BakeViewSelector
--     { _bakeViewSelector_clientAddresses = fmapMaybe f $ _bakeViewSelector_clientAddresses a
--     , _bakeViewSelector_summary = fmapMaybe f $ _bakeViewSelector_summary a
--     , _bakeViewSelector_clients = fmapMaybe f $ _bakeViewSelector_clients a
--     , _bakeViewSelector_parameters = fmapMaybe f $ _bakeViewSelector_parameters a
--     , _bakeViewSelector_tzscan = fmapMaybe f $ _bakeViewSelector_tzscan a
--     , _bakeViewSelector_nodes = fmapMaybe f $ _bakeViewSelector_nodes a
--     , _bakeViewSelector_delegates = fmapMaybe f $ _bakeViewSelector_delegates a
--     , _bakeViewSelector_delegateStats = fmapMaybe f $ _bakeViewSelector_delegateStats a
--     , _bakeViewSelector_notificatees = fmapMaybe f $ _bakeViewSelector_notificatees a
--     , _bakeViewSelector_mailServer = fmapMaybe f $ _bakeViewSelector_mailServer a
--     , _bakeViewSelector_nodeAddresses = fmapMaybe f $ _bakeViewSelector_nodeAddresses a
--     , _bakeViewSelector_errors = fmapMaybe f $ _bakeViewSelector_errors a
--     , _bakeViewSelector_upgrade = fmapMaybe f $ _bakeViewSelector_upgrade a
--     }

-- instance FunctorMaybe BakeView where
--   fmapMaybe f a = BakeView
--     { _bakeView_clientAddresses = fmapMaybeSnd f $ _bakeView_clientAddresses a
--     , _bakeView_clients = fmapMaybeSnd f $ _bakeView_clients a
--     , _bakeView_parameters = fmapMaybe f $ _bakeView_parameters a
--     , _bakeView_tzscan = fmapMaybe f $ _bakeView_tzscan a
--     , _bakeView_nodes = fmapMaybeSnd f $ _bakeView_nodes a
--     , _bakeView_delegates = fmapMaybe (traverse f) $ _bakeView_delegates a
--     , _bakeView_delegateStats = fmapMaybeSnd f $ _bakeView_delegateStats a
--     , _bakeView_notificatees = fmapMaybeSnd f $ _bakeView_notificatees a
--     , _bakeView_mailServer = fmapMaybe f $ _bakeView_mailServer a
--     , _bakeView_graphs = fmapMaybeSnd f $ _bakeView_graphs a
--     , _bakeView_summaryGraph = fmapMaybe f $ _bakeView_summaryGraph a
--     , _bakeView_summary = fmapMaybe f $ _bakeView_summary a
--     , _bakeView_nodeAddresses = fmapMaybeSnd f (_bakeView_nodeAddresses a)
--     , _bakeView_errors = errors
--     , _bakeView_errorsById =
--         -- Crop the 'ErrorLog's to only those with the IDs referenced in the cropped set of errors.
--         restrictKeys (_bakeView_errorsById a) (foldMap fst $ AppendIMap.elems errors)
--     , _bakeView_upgrade = fmapMaybe f $ _bakeView_upgrade a
--     }
--     where
--       errors = fmapMaybeSnd f (_bakeView_errors a)

fmapMaybeSnd :: FunctorMaybe f => (a -> Maybe b) -> f (e, a) -> f (e, b)
fmapMaybeSnd f = fmapMaybe $ \(e, a) -> case f a of
  Nothing -> Nothing
  Just b -> Just (e, b)

alignTheseWith :: Align f => (These a b -> c) -> These (f a) (f b) -> f c
alignTheseWith f = these (fmap (f . This)) (fmap (f . That)) (alignWith f)

instance Semigroup a => Semigroup (BakeViewSelector a) where
  u <> v = BakeViewSelector
    { _bakeViewSelector_clientAddresses = (<>) (_bakeViewSelector_clientAddresses u) (_bakeViewSelector_clientAddresses v)
    , _bakeViewSelector_summary = (<>) (_bakeViewSelector_summary u) (_bakeViewSelector_summary v)
    , _bakeViewSelector_clients = (<>) (_bakeViewSelector_clients u) (_bakeViewSelector_clients v)
    , _bakeViewSelector_parameters = (<>) (_bakeViewSelector_parameters u) (_bakeViewSelector_parameters v)
    , _bakeViewSelector_tzscan = (<>) (_bakeViewSelector_tzscan u) (_bakeViewSelector_tzscan v)
    , _bakeViewSelector_nodes = (<>) (_bakeViewSelector_nodes u) (_bakeViewSelector_nodes v)
    , _bakeViewSelector_delegates = (<>) (_bakeViewSelector_delegates u) (_bakeViewSelector_delegates v)
    , _bakeViewSelector_delegateStats =  (<>) (_bakeViewSelector_delegateStats u) (_bakeViewSelector_delegateStats v)
    , _bakeViewSelector_notificatees = (<>) (_bakeViewSelector_notificatees u) (_bakeViewSelector_notificatees v)
    , _bakeViewSelector_mailServer = (<>) (_bakeViewSelector_mailServer u) (_bakeViewSelector_mailServer v)
    , _bakeViewSelector_nodeAddresses = (<>) (_bakeViewSelector_nodeAddresses u) (_bakeViewSelector_nodeAddresses v)
    , _bakeViewSelector_errors = (<>) (_bakeViewSelector_errors u) (_bakeViewSelector_errors v)
    , _bakeViewSelector_upgrade = (<>) (_bakeViewSelector_upgrade u) (_bakeViewSelector_upgrade v)
    }

instance (Semigroup a, Monoid a) => Monoid (BakeViewSelector a) where
  mempty = BakeViewSelector 
    { _bakeViewSelector_clientAddresses = mempty
    , _bakeViewSelector_summary = mempty
    , _bakeViewSelector_clients = mempty
    , _bakeViewSelector_parameters = mempty
    , _bakeViewSelector_tzscan = mempty
    , _bakeViewSelector_nodes = mempty
    , _bakeViewSelector_delegates = mempty
    , _bakeViewSelector_delegateStats = Compose mempty
    , _bakeViewSelector_notificatees = mempty
    , _bakeViewSelector_mailServer = mempty
    , _bakeViewSelector_nodeAddresses = mempty
    , _bakeViewSelector_errors = mempty
    , _bakeViewSelector_upgrade = mempty
    }
  mappend = (<>)


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
    -- , _bakeView_graphs = mempty
    -- , _bakeView_summaryGraph = mempty
    , _bakeView_summary = mempty
    , _bakeView_nodeAddresses = mempty
    , _bakeView_errors = mempty
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
    -- , _bakeView_summaryGraph = _bakeView_summaryGraph u <> _bakeView_summaryGraph v
    -- , _bakeView_graphs = _bakeView_graphs u <> _bakeView_graphs v
    , _bakeView_summary = _bakeView_summary u <> _bakeView_summary v
    , _bakeView_nodeAddresses = _bakeView_nodeAddresses u <> _bakeView_nodeAddresses v
    , _bakeView_errors = _bakeView_errors u <> _bakeView_errors v
    , _bakeView_upgrade = _bakeView_upgrade u <> _bakeView_upgrade v
    }

instance (Monoid a, Semigroup a) => Query (BakeViewSelector a) where
  type QueryResult (BakeViewSelector a) = BakeView a
  crop = cropBakeView

instance (Semigroup a, FromJSON a) => FromJSON (BakeViewSelector a)
instance (Semigroup a, FromJSON a) => FromJSON (BakeView a)

instance ToJSON a => ToJSON (BakeViewSelector a)
instance (Semigroup a, ToJSON a) => ToJSON (BakeView a)

instance HasView Bake where
  type View Bake = BakeView
  type ViewSelector Bake = BakeViewSelector

concat <$> traverse makeLenses
  [ 'BakeView
  , 'BakeViewSelector
  , 'MailServerView
  ]
