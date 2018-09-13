{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Common.App where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Align (Align, alignWith, nil)
import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as MMap
import Data.Bifunctor
import Data.Foldable (fold)
import Data.Functor.Compose
import Data.Semigroup (First (..), Semigroup, (<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.These (These (..), these)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Version (Version)
import Data.Word (Word16)
import GHC.Generics (Generic)
import Reflex (Additive, FunctorMaybe (..), Group (..))
import Reflex.Query.Class (Query (QueryResult, crop), SelectedCount)
import Rhyolite.App (HasView, View, ViewSelector)
import Rhyolite.Schema (Email, Id)
import Text.URI (URI)

import Tezos.NodeRPC.Sources (PublicNode)
import Tezos.Types

import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import Common.Schema
import Common.Vassal

restrictKeys :: Ord k => AppendMap k a -> Set k -> AppendMap k a
restrictKeys m ks = MMap.filterWithKey (\k _ -> k `Set.member` ks) m

summary :: Semigroup v => AppendMap k v -> Maybe v
summary = MMap.lookup () . MMap.mapKeysWith (<>) (const ())

data Bake = Bake

type ErrorInfo = (ErrorLog, ErrorLogView)
getErrorInterval :: (ErrorLog, ErrorLogView) -> First ((ErrorLog, ErrorLogView), ClosedInterval (WithInfinity UTCTime))
getErrorInterval ei@(el, _) = First (ei, ClosedInterval
  (Bounded $ _errorLog_started el)
  (maybe UpperInfinity Bounded $ _errorLog_stopped el))


type Deletable a = First (Maybe a)

data BakeViewSelector a = BakeViewSelector
  { _bakeViewSelector_clientAddresses :: !(RangeSelector' (Id Client) (Deletable URI) a)
  , _bakeViewSelector_clients :: !(RangeSelector (Id Client) (Deletable ClientInfo) a)
  , _bakeViewSelector_delegateStats :: !(ComposeSelector (RangeSelector PublicKeyHash Account) (RangeSelector RawLevel BakeEfficiency) a)
  , _bakeViewSelector_delegates :: !(RangeSelector' PublicKeyHash (Deletable ()) a)
  , _bakeViewSelector_errors :: !(IntervalSelector' UTCTime (Id ErrorLog) ErrorInfo a)
  , _bakeViewSelector_mailServer :: !(MaybeSelector (Maybe MailServerView) a)
  , _bakeViewSelector_nodeAddresses :: !(RangeSelector' (Id Node) (Deletable (URI, Maybe Text)) a)
  , _bakeViewSelector_nodes :: !(RangeSelector' (Id Node) (Deletable Node) a)
  , _bakeViewSelector_notificatees :: !(RangeSelector' (Id Notificatee) Email a)
  , _bakeViewSelector_parameters :: !(MaybeSelector ProtoInfo a)
  , _bakeViewSelector_summary :: !(MaybeSelector (Report, Int) a) -- The Int is the number of bakers we've yet to get a report from.
  , _bakeViewSelector_publicNodeConfig :: !(RangeSelector PublicNode PublicNodeConfig a)
  , _bakeViewSelector_publicNodeHeads :: !(RangeSelector' (Id PublicNodeHead) PublicNodeHead a)
  , _bakeViewSelector_upgrade :: !(MaybeSelector (ErrorLog, Either UpgradeCheckError Version) a)
  } deriving (Functor, Generic, Typeable, Traversable, Foldable, Show, Eq, Ord)


data BakeView a = BakeView
  { _bakeView_clientAddresses :: !(RangeView' (Id Client) (Deletable URI) a)
  , _bakeView_clients :: !(RangeView (Id Client) (Deletable ClientInfo) a)
  , _bakeView_delegateStats :: !(ComposeView (RangeSelector PublicKeyHash Account) (RangeSelector RawLevel BakeEfficiency) a)
  , _bakeView_delegates :: !(RangeView' PublicKeyHash (Deletable ()) a)
  , _bakeView_errors :: !(IntervalView' UTCTime (Id ErrorLog) ErrorInfo a)
  , _bakeView_mailServer :: !(MaybeView (Maybe MailServerView) a)
  , _bakeView_nodeAddresses :: !(RangeView' (Id Node) (Deletable (URI, Maybe Text)) a)
  , _bakeView_nodes :: !(RangeView' (Id Node) (Deletable Node) a)
  , _bakeView_notificatees :: !(RangeView' (Id Notificatee) Email a)
  , _bakeView_parameters :: !(MaybeView ProtoInfo a)
  , _bakeView_summary :: !(MaybeView (Report, Int) a) -- The Int is the number of bakers we've yet to get a report from.
  , _bakeView_publicNodeConfig :: !(RangeView PublicNode PublicNodeConfig a)
  , _bakeView_publicNodeHeads :: !(RangeView' (Id PublicNodeHead) PublicNodeHead a)
  , _bakeView_upgrade :: !(MaybeView (ErrorLog, Either UpgradeCheckError Version) a)
  -- , _bakeView_graphs       :: !(AppendMap (Id Client) (First (Maybe (Micro, Text)), a))
  -- , _bakeView_summaryGraph :: !(Single (Maybe (Micro, Text)) a)
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
  | ErrorLogView_NodeWrongChain ErrorLogNodeWrongChain
  | ErrorLogView_BakerNoHeartbeat ErrorLogBakerNoHeartbeat
  | ErrorLogView_BadNodeHead ErrorLogBadNodeHead
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
  , _bakeView_clients = cropView (_bakeViewSelector_clients vs) (_bakeView_clients v)
  , _bakeView_parameters = cropView (_bakeViewSelector_parameters vs) (_bakeView_parameters v)
  , _bakeView_nodeAddresses = cropView (_bakeViewSelector_nodeAddresses vs) (_bakeView_nodeAddresses v)
  , _bakeView_publicNodeConfig = cropView (_bakeViewSelector_publicNodeConfig vs) (_bakeView_publicNodeConfig v)
  , _bakeView_publicNodeHeads = cropView (_bakeViewSelector_publicNodeHeads vs) (_bakeView_publicNodeHeads v)
  , _bakeView_nodes = cropView (_bakeViewSelector_nodes vs) (_bakeView_nodes v)
  , _bakeView_delegates = cropView (_bakeViewSelector_delegates vs) (_bakeView_delegates v)
  , _bakeView_delegateStats = cropView (_bakeViewSelector_delegateStats vs) (_bakeView_delegateStats v)
  , _bakeView_notificatees = cropView (_bakeViewSelector_notificatees vs) (_bakeView_notificatees v)
  , _bakeView_mailServer = cropView (_bakeViewSelector_mailServer vs) (_bakeView_mailServer v)
  , _bakeView_summary = cropView (_bakeViewSelector_summary vs) (_bakeView_summary v)
  , _bakeView_errors = cropView (_bakeViewSelector_errors vs) (_bakeView_errors v)
  , _bakeView_upgrade = cropView (_bakeViewSelector_upgrade vs) (_bakeView_upgrade v)
  }

instance FunctorMaybe BakeViewSelector where
  fmapMaybe f a = BakeViewSelector
    { _bakeViewSelector_clientAddresses = fmapMaybe f $ _bakeViewSelector_clientAddresses a
    , _bakeViewSelector_clients = fmapMaybe f $ _bakeViewSelector_clients a
    , _bakeViewSelector_parameters = fmapMaybe f $ _bakeViewSelector_parameters a
    , _bakeViewSelector_publicNodeConfig = fmapMaybe f $ _bakeViewSelector_publicNodeConfig a
    , _bakeViewSelector_publicNodeHeads = fmapMaybe f $ _bakeViewSelector_publicNodeHeads a
    , _bakeViewSelector_nodes = fmapMaybe f $ _bakeViewSelector_nodes a
    , _bakeViewSelector_delegates = fmapMaybe f $ _bakeViewSelector_delegates a
    , _bakeViewSelector_delegateStats = fmapMaybe f $ _bakeViewSelector_delegateStats a
    , _bakeViewSelector_notificatees = fmapMaybe f $ _bakeViewSelector_notificatees a
    , _bakeViewSelector_mailServer = fmapMaybe f $ _bakeViewSelector_mailServer a
    , _bakeViewSelector_summary = fmapMaybe f $ _bakeViewSelector_summary a
    , _bakeViewSelector_nodeAddresses = fmapMaybe f $ _bakeViewSelector_nodeAddresses a
    , _bakeViewSelector_errors = fmapMaybe f $ _bakeViewSelector_errors a
    , _bakeViewSelector_upgrade = fmapMaybe f $ _bakeViewSelector_upgrade a
    }

instance Align BakeViewSelector where
  nil = BakeViewSelector
    { _bakeViewSelector_clientAddresses = nil
    , _bakeViewSelector_clients = nil
    , _bakeViewSelector_parameters = nil
    , _bakeViewSelector_publicNodeConfig = nil
    , _bakeViewSelector_publicNodeHeads = nil
    , _bakeViewSelector_nodes = nil
    , _bakeViewSelector_delegates = nil
    , _bakeViewSelector_delegateStats = nil
    , _bakeViewSelector_notificatees = nil
    , _bakeViewSelector_mailServer = nil
    , _bakeViewSelector_summary = nil
    , _bakeViewSelector_nodeAddresses = nil
    , _bakeViewSelector_errors = nil
    , _bakeViewSelector_upgrade = nil
    }

  alignWith :: forall a b c. (These a b -> c) -> BakeViewSelector a -> BakeViewSelector b -> BakeViewSelector c
  alignWith f xs ys = BakeViewSelector
    { _bakeViewSelector_clientAddresses = f' _bakeViewSelector_clientAddresses
    , _bakeViewSelector_clients = f' _bakeViewSelector_clients
    , _bakeViewSelector_parameters = f' _bakeViewSelector_parameters
    , _bakeViewSelector_publicNodeConfig = f' _bakeViewSelector_publicNodeConfig
    , _bakeViewSelector_publicNodeHeads = f' _bakeViewSelector_publicNodeHeads
    , _bakeViewSelector_nodes = f' _bakeViewSelector_nodes
    , _bakeViewSelector_delegates = f' _bakeViewSelector_delegates
    , _bakeViewSelector_delegateStats = f' _bakeViewSelector_delegateStats
    , _bakeViewSelector_notificatees = f' _bakeViewSelector_notificatees
    , _bakeViewSelector_mailServer = f' _bakeViewSelector_mailServer
    , _bakeViewSelector_summary = f' _bakeViewSelector_summary
    , _bakeViewSelector_nodeAddresses = f' _bakeViewSelector_nodeAddresses
    , _bakeViewSelector_errors = f' _bakeViewSelector_errors
    , _bakeViewSelector_upgrade = f' _bakeViewSelector_upgrade
    }
    where
      f' :: forall f. Align f => (forall x. BakeViewSelector x -> f x) -> f c
      f' p = alignWith f (p xs) (p ys)

instance FunctorMaybe BakeView where
  fmapMaybe f a = BakeView
    { _bakeView_clientAddresses = fmapMaybe f $ _bakeView_clientAddresses a
    , _bakeView_clients = fmapMaybe f $ _bakeView_clients a
    , _bakeView_parameters = fmapMaybe f $ _bakeView_parameters a
    , _bakeView_publicNodeConfig = fmapMaybe f $ _bakeView_publicNodeConfig a
    , _bakeView_publicNodeHeads = fmapMaybe f $ _bakeView_publicNodeHeads a
    , _bakeView_nodes = fmapMaybe f $ _bakeView_nodes a
    , _bakeView_delegates = fmapMaybe f $ _bakeView_delegates a
    , _bakeView_delegateStats = fmapMaybe f $ _bakeView_delegateStats a
    , _bakeView_notificatees = fmapMaybe f $ _bakeView_notificatees a
    , _bakeView_mailServer = fmapMaybe f $ _bakeView_mailServer a
    , _bakeView_summary = fmapMaybe f $ _bakeView_summary a
    , _bakeView_nodeAddresses = fmapMaybe f $ _bakeView_nodeAddresses a
    , _bakeView_errors = fmapMaybe f $ _bakeView_errors a
    , _bakeView_upgrade = fmapMaybe f $ _bakeView_upgrade a
    }

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
    , _bakeViewSelector_publicNodeConfig = (<>) (_bakeViewSelector_publicNodeConfig u) (_bakeViewSelector_publicNodeConfig v)
    , _bakeViewSelector_publicNodeHeads = (<>) (_bakeViewSelector_publicNodeHeads u) (_bakeViewSelector_publicNodeHeads v)
    , _bakeViewSelector_nodes = (<>) (_bakeViewSelector_nodes u) (_bakeViewSelector_nodes v)
    , _bakeViewSelector_delegates = (<>) (_bakeViewSelector_delegates u) (_bakeViewSelector_delegates v)
    , _bakeViewSelector_delegateStats = (<>) (_bakeViewSelector_delegateStats u) (_bakeViewSelector_delegateStats v)
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
    , _bakeViewSelector_publicNodeConfig = mempty
    , _bakeViewSelector_publicNodeHeads = mempty
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
    , _bakeView_publicNodeConfig = mempty
    , _bakeView_publicNodeHeads = mempty
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
    , _bakeView_publicNodeConfig = _bakeView_publicNodeConfig u <> _bakeView_publicNodeConfig v
    , _bakeView_publicNodeHeads = _bakeView_publicNodeHeads u <> _bakeView_publicNodeHeads v
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
