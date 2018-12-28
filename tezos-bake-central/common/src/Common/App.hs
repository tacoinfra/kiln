{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- 'deriveJSONGADT' produces seemingly redundant pattern matches.
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Common.App
  ( module Common.App

  -- Re-exports
  , AlertNotificationMethod (..)
  ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.GADT (deriveJSONGADT)
import Data.Align (Align (alignWith, nil))
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Sum.Orphans ()
import Data.Functor.Compose (Compose (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import qualified Data.Map.Monoidal as MMap
import Data.These (These (..), these)
import Data.Time (UTCTime)
import Data.Word (Word16)
import Reflex (Additive, FunctorMaybe (..), Group (..))
import Reflex.Query.Class (Query (QueryResult, crop), SelectedCount)
import Rhyolite.App (HasView, View, ViewSelector)
import Rhyolite.Schema (Email, Id)
import Text.URI (URI)

import Tezos.NodeRPC.Sources (PublicNode)
import Tezos.Types

import Common.Alerts (AlertsFilter (..))
import Common.AppendIntervalMap (ClosedInterval (..), WithInfinity (..))
import Common.Config (FrontendConfig)
import Common.Schema
import Common.Vassal
import ExtraPrelude

data Bake = Bake

type ErrorInfo = (ErrorLog, ErrorLogView)

getErrorInterval :: (ErrorLog, ErrorLogView) -> First ((ErrorLog, ErrorLogView), ClosedInterval (WithInfinity UTCTime))
getErrorInterval ei@(el, _) = First (ei, ClosedInterval
  (Bounded $ _errorLog_started el)
  (maybe UpperInfinity Bounded $ _errorLog_stopped el))

type Deletable a = First (Maybe a)

data BakerSummary = BakerSummary
  { _bakerSummary_address :: PublicKeyHash
  , _bakerSummary_alias :: Maybe Text
  , _bakerSummary_alertCount :: Int
  } deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON BakerSummary
instance ToJSON BakerSummary

data NodeSummary = NodeSummary
  { _nodeSummary_address :: URI
  , _nodeSummary_alias :: Maybe Text
  , _nodeSummary_alertCount :: Int
  } deriving (Eq, Ord, Show, Typeable, Generic)
instance FromJSON NodeSummary
instance ToJSON NodeSummary

data BakeViewSelector a = BakeViewSelector
  { _bakeViewSelector_config :: !(MaybeSelector FrontendConfig a)
  , _bakeViewSelector_clientAddresses :: !(RangeSelector' (Id Client) (Deletable URI) a)
  , _bakeViewSelector_clients :: !(RangeSelector (Id Client) (Deletable ClientInfo) a)
  , _bakeViewSelector_bakerAddresses :: !(RangeSelector' PublicKeyHash (Deletable BakerSummary) a)
  , _bakeViewSelector_bakerStats :: !(ComposeSelector (RangeSelector PublicKeyHash Account) (RangeSelector RawLevel BakeEfficiency) a)
  , _bakeViewSelector_bakerDetails :: !(RangeSelector' PublicKeyHash (Deletable BakerDetails) a)
  , _bakeViewSelector_errors :: !(MonoidalMap AlertsFilter (IntervalSelector' UTCTime (Id ErrorLog) (Deletable ErrorInfo) a))
  , _bakeViewSelector_mailServer :: !(MaybeSelector (Maybe MailServerView) a)
  , _bakeViewSelector_nodeAddresses :: !(RangeSelector' (Id Node) (Deletable NodeSummary) a)
  , _bakeViewSelector_nodes :: !(RangeSelector' (Id Node) (Deletable Node) a)
  , _bakeViewSelector_parameters :: !(MaybeSelector ProtoInfo a)
  , _bakeViewSelector_summary :: !(MaybeSelector (Report, Int) a) -- The Int is the number of bakers we've yet to get a report from.
  , _bakeViewSelector_latestHead :: !(MaybeSelector VeryBlockLike a)
  , _bakeViewSelector_publicNodeConfig :: !(RangeSelector PublicNode PublicNodeConfig a)
  , _bakeViewSelector_publicNodeHeads :: !(RangeSelector' (Id PublicNodeHead) PublicNodeHead a)
  , _bakeViewSelector_upstreamVersion :: !(MaybeSelector UpstreamVersion a)
  , _bakeViewSelector_telegramConfig :: !(MaybeSelector (Maybe TelegramConfig) a)
  , _bakeViewSelector_telegramRecipients :: !(RangeSelector' (Id TelegramRecipient) (Deletable TelegramRecipient) a)
  , _bakeViewSelector_alertCount :: !(MaybeSelector Int a)
  } deriving (Functor, Generic, Typeable, Traversable, Foldable, Show, Eq, Ord)

data BakeView a = BakeView
  { _bakeView_config :: !(MaybeView FrontendConfig a)
  , _bakeView_clientAddresses :: !(RangeView' (Id Client) (Deletable URI) a)
  , _bakeView_clients :: !(RangeView (Id Client) (Deletable ClientInfo) a)
  , _bakeView_bakerAddresses :: !(RangeView' PublicKeyHash (Deletable BakerSummary) a)
  , _bakeView_bakerStats :: !(ComposeView (RangeSelector PublicKeyHash Account) (RangeSelector RawLevel BakeEfficiency) a)
  , _bakeView_bakerDetails :: !(RangeView' PublicKeyHash (Deletable BakerDetails) a)
  -- TODO: I'm more than a little concerned about this approach for dealing
  -- with deletes in IntervalView.  I think in this particular case, we can get
  -- away with it; since we never go from Resolved to Unresolved, so the
  -- relevant selection window should *eventually* roll off for the resolved
  -- things and be dropped anyway.  In other cases, this approach is likely to
  -- leak memory in Reflex (deletes never really get to go away)
  , _bakeView_errors :: !(MonoidalMap AlertsFilter (IntervalView' UTCTime (Id ErrorLog) (Deletable ErrorInfo) a))
  , _bakeView_mailServer :: !(MaybeView (Maybe MailServerView) a)
  , _bakeView_nodeAddresses :: !(RangeView' (Id Node) (Deletable NodeSummary) a)
  , _bakeView_nodes :: !(RangeView' (Id Node) (Deletable Node) a)
  , _bakeView_parameters :: !(MaybeView ProtoInfo a)
  , _bakeView_summary :: !(MaybeView (Report, Int) a) -- The Int is the number of bakers we've yet to get a report from.
  , _bakeView_latestHead :: !(MaybeView VeryBlockLike a)
  , _bakeView_publicNodeConfig :: !(RangeView PublicNode PublicNodeConfig a)
  , _bakeView_publicNodeHeads :: !(RangeView' (Id PublicNodeHead) PublicNodeHead a)
  , _bakeView_upstreamVersion :: !(MaybeView UpstreamVersion a)
  , _bakeView_telegramConfig :: !(MaybeView (Maybe TelegramConfig) a)
  , _bakeView_telegramRecipients :: !(RangeView' (Id TelegramRecipient) (Deletable TelegramRecipient) a)
  , _bakeView_alertCount :: !(MaybeView Int a)
  -- , _bakeView_graphs       :: !(AppendMap (Id Client) (First (Maybe (Micro, Text)), a))
  -- , _bakeView_summaryGraph :: !(Single (Maybe (Micro, Text)) a)
  } deriving (Functor, Generic, Typeable, Traversable, Foldable, Show, Eq, Ord)


data MailServerView = MailServerView
  { _mailServerView_hostName :: !Text
  , _mailServerView_portNumber :: !Word16
  , _mailServerView_smtpProtocol :: !SmtpProtocol
  , _mailServerView_userName :: !Text
  , _mailServerView_enabled :: !Bool
  , _mailServerView_notificatees :: ![Email]
  } deriving (Eq, Ord, Generic, Typeable, Read, Show)
instance FromJSON MailServerView
instance ToJSON MailServerView

data LogTag a where
  LogTag_InaccessibleNode :: LogTag ErrorLogInaccessibleNode
  LogTag_NodeWrongChain :: LogTag ErrorLogNodeWrongChain
  LogTag_BakerNoHeartbeat :: LogTag ErrorLogBakerNoHeartbeat
  LogTag_BadNodeHead :: LogTag ErrorLogBadNodeHead
  LogTag_MultipleBakersForSameBaker :: LogTag ErrorLogMultipleBakersForSameBaker
  LogTag_UpgradeAvailable :: LogTag ErrorLogUpgradeAvailable

data NodeErrorLogView
  = NodeErrorLogView_InaccessibleNode !ErrorLogInaccessibleNode
  | NodeErrorLogView_NodeWrongChain !ErrorLogNodeWrongChain
  | NodeErrorLogView_BadNodeHead !ErrorLogBadNodeHead
  deriving (Eq, Ord, Generic, Typeable, Show)
instance FromJSON NodeErrorLogView
instance ToJSON NodeErrorLogView

data BakerErrorLogView
  = BakerErrorLogView_MultipleBakersForSameBaker !ErrorLogMultipleBakersForSameBaker
  deriving (Eq, Ord, Generic, Typeable, Show)
instance FromJSON BakerErrorLogView
instance ToJSON BakerErrorLogView

-- TODO: Switch to 'DSum LogTag Identity', also spit node and baker tags out of LogTag.
data ErrorLogView
  = ErrorLogView_NodeError NodeErrorLogView
  | ErrorLogView_BakerError BakerErrorLogView
  | ErrorLogView_BakerNoHeartbeat !ErrorLogBakerNoHeartbeat
  -- ^ Misc baker *daemon* error.
  | ErrorLogView_UpgradeAvailable !(Id ErrorLogUpgradeAvailable) !ErrorLogUpgradeAvailable
  deriving (Eq, Ord, Generic, Typeable, Show)
instance FromJSON ErrorLogView
instance ToJSON ErrorLogView

nodeErrorViewOnly :: ErrorLogView -> Maybe NodeErrorLogView
nodeErrorViewOnly = \case
  ErrorLogView_NodeError v -> Just v
  _ -> Nothing

nodeIdForNodeErrorLogView :: NodeErrorLogView -> Id Node
nodeIdForNodeErrorLogView = \case
  NodeErrorLogView_InaccessibleNode ein -> _errorLogInaccessibleNode_node ein
  NodeErrorLogView_NodeWrongChain enwc -> _errorLogNodeWrongChain_node enwc
  NodeErrorLogView_BadNodeHead ebnh -> _errorLogBadNodeHead_node ebnh

bakerErrorViewOnly :: ErrorLogView -> Maybe BakerErrorLogView
bakerErrorViewOnly = \case
  ErrorLogView_BakerError v -> Just v
  _ -> Nothing

bakerIdForBakerErrorLogView :: BakerErrorLogView -> PublicKeyHash
bakerIdForBakerErrorLogView = \case
  BakerErrorLogView_MultipleBakersForSameBaker embfb -> _errorLogMultipleBakersForSameBaker_publicKeyHash embfb

errorLogIdForErrorLogView :: ErrorLogView -> Id ErrorLog
errorLogIdForErrorLogView = \case
  ErrorLogView_NodeError ne -> case ne of
    NodeErrorLogView_InaccessibleNode ein -> _errorLogInaccessibleNode_log ein
    NodeErrorLogView_NodeWrongChain enwc -> _errorLogNodeWrongChain_log enwc
    NodeErrorLogView_BadNodeHead ebnh -> _errorLogBadNodeHead_log ebnh
  ErrorLogView_BakerError be -> case be of
    BakerErrorLogView_MultipleBakersForSameBaker emb -> _errorLogMultipleBakersForSameBaker_log emb
  ErrorLogView_BakerNoHeartbeat enhb -> _errorLogBakerNoHeartbeat_log enhb
  ErrorLogView_UpgradeAvailable _ ua -> _errorLogUpgradeAvailable_log ua

manuallyResolvable :: ErrorLogView -> Bool
manuallyResolvable = \case
  ErrorLogView_NodeError ne -> case ne of
    NodeErrorLogView_InaccessibleNode _ -> False
    NodeErrorLogView_NodeWrongChain _ -> False
    NodeErrorLogView_BadNodeHead _ -> False
  ErrorLogView_BakerError be -> case be of
    BakerErrorLogView_MultipleBakersForSameBaker _ -> False
  ErrorLogView_BakerNoHeartbeat _ -> False
  ErrorLogView_UpgradeAvailable {} -> False

mailServerConfigToView :: MailServerConfig -> [Email] -> MailServerView
mailServerConfigToView x ns = MailServerView
  { _mailServerView_hostName = _mailServerConfig_hostName x
  , _mailServerView_portNumber = _mailServerConfig_portNumber x
  , _mailServerView_smtpProtocol = _mailServerConfig_smtpProtocol x
  , _mailServerView_userName = _mailServerConfig_userName x
  , _mailServerView_enabled = _mailServerConfig_enabled x
  , _mailServerView_notificatees = ns
  }

cropBakeView :: (Semigroup a) => BakeViewSelector a -> BakeView b -> BakeView a
cropBakeView vs v = BakeView
  { _bakeView_config = cropView (_bakeViewSelector_config vs) (_bakeView_config v)
  , _bakeView_clientAddresses = cropView (_bakeViewSelector_clientAddresses vs) (_bakeView_clientAddresses v)
  , _bakeView_clients = cropView (_bakeViewSelector_clients vs) (_bakeView_clients v)
  , _bakeView_parameters = cropView (_bakeViewSelector_parameters vs) (_bakeView_parameters v)
  , _bakeView_nodeAddresses = cropView (_bakeViewSelector_nodeAddresses vs) (_bakeView_nodeAddresses v)
  , _bakeView_publicNodeConfig = cropView (_bakeViewSelector_publicNodeConfig vs) (_bakeView_publicNodeConfig v)
  , _bakeView_publicNodeHeads = cropView (_bakeViewSelector_publicNodeHeads vs) (_bakeView_publicNodeHeads v)
  , _bakeView_nodes = cropView (_bakeViewSelector_nodes vs) (_bakeView_nodes v)
  , _bakeView_bakerAddresses = cropView (_bakeViewSelector_bakerAddresses vs) (_bakeView_bakerAddresses v)
  , _bakeView_bakerDetails = cropView (_bakeViewSelector_bakerDetails vs) (_bakeView_bakerDetails v)
  , _bakeView_bakerStats = cropView (_bakeViewSelector_bakerStats vs) (_bakeView_bakerStats v)
  , _bakeView_mailServer = cropView (_bakeViewSelector_mailServer vs) (_bakeView_mailServer v)
  , _bakeView_summary = cropView (_bakeViewSelector_summary vs) (_bakeView_summary v)
  , _bakeView_errors = MMap.intersectionWith cropView (_bakeViewSelector_errors vs) (_bakeView_errors v)
  , _bakeView_latestHead = cropView (_bakeViewSelector_latestHead vs) (_bakeView_latestHead v)
  , _bakeView_upstreamVersion = cropView (_bakeViewSelector_upstreamVersion vs) (_bakeView_upstreamVersion v)
  , _bakeView_telegramConfig = cropView (_bakeViewSelector_telegramConfig vs) (_bakeView_telegramConfig v)
  , _bakeView_telegramRecipients = cropView (_bakeViewSelector_telegramRecipients vs) (_bakeView_telegramRecipients v)
  , _bakeView_alertCount = cropView (_bakeViewSelector_alertCount vs) (_bakeView_alertCount v)
  }

instance FunctorMaybe BakeViewSelector where
  fmapMaybe f a = BakeViewSelector
    { _bakeViewSelector_config = fmapMaybe f $ _bakeViewSelector_config a
    , _bakeViewSelector_clientAddresses = fmapMaybe f $ _bakeViewSelector_clientAddresses a
    , _bakeViewSelector_clients = fmapMaybe f $ _bakeViewSelector_clients a
    , _bakeViewSelector_parameters = fmapMaybe f $ _bakeViewSelector_parameters a
    , _bakeViewSelector_publicNodeConfig = fmapMaybe f $ _bakeViewSelector_publicNodeConfig a
    , _bakeViewSelector_publicNodeHeads = fmapMaybe f $ _bakeViewSelector_publicNodeHeads a
    , _bakeViewSelector_nodes = fmapMaybe f $ _bakeViewSelector_nodes a
    , _bakeViewSelector_bakerAddresses = fmapMaybe f $ _bakeViewSelector_bakerAddresses a
    , _bakeViewSelector_bakerDetails = fmapMaybe f $ _bakeViewSelector_bakerDetails a
    , _bakeViewSelector_bakerStats = fmapMaybe f $ _bakeViewSelector_bakerStats a
    , _bakeViewSelector_mailServer = fmapMaybe f $ _bakeViewSelector_mailServer a
    , _bakeViewSelector_summary = fmapMaybe f $ _bakeViewSelector_summary a
    , _bakeViewSelector_nodeAddresses = fmapMaybe f $ _bakeViewSelector_nodeAddresses a
    , _bakeViewSelector_errors = (fmap.fmapMaybe) f $ _bakeViewSelector_errors a
    , _bakeViewSelector_latestHead = fmapMaybe f $ _bakeViewSelector_latestHead a
    , _bakeViewSelector_upstreamVersion = fmapMaybe f $ _bakeViewSelector_upstreamVersion a
    , _bakeViewSelector_telegramConfig = fmapMaybe f (_bakeViewSelector_telegramConfig a)
    , _bakeViewSelector_telegramRecipients = fmapMaybe f (_bakeViewSelector_telegramRecipients a)
    , _bakeViewSelector_alertCount = fmapMaybe f (_bakeViewSelector_alertCount a)
    }

instance Align BakeViewSelector where
  nil = BakeViewSelector
    { _bakeViewSelector_config = nil
    , _bakeViewSelector_clientAddresses = nil
    , _bakeViewSelector_clients = nil
    , _bakeViewSelector_parameters = nil
    , _bakeViewSelector_publicNodeConfig = nil
    , _bakeViewSelector_publicNodeHeads = nil
    , _bakeViewSelector_nodes = nil
    , _bakeViewSelector_bakerAddresses = nil
    , _bakeViewSelector_bakerDetails = nil
    , _bakeViewSelector_bakerStats = nil
    , _bakeViewSelector_mailServer = nil
    , _bakeViewSelector_summary = nil
    , _bakeViewSelector_nodeAddresses = nil
    , _bakeViewSelector_errors = nil
    , _bakeViewSelector_latestHead = nil
    , _bakeViewSelector_upstreamVersion = nil
    , _bakeViewSelector_telegramConfig = nil
    , _bakeViewSelector_telegramRecipients = nil
    , _bakeViewSelector_alertCount = nil
    }

  alignWith :: forall a b c. (These a b -> c) -> BakeViewSelector a -> BakeViewSelector b -> BakeViewSelector c
  alignWith f xs ys = BakeViewSelector
    { _bakeViewSelector_config = f' _bakeViewSelector_config
    , _bakeViewSelector_clientAddresses = f' _bakeViewSelector_clientAddresses
    , _bakeViewSelector_clients = f' _bakeViewSelector_clients
    , _bakeViewSelector_parameters = f' _bakeViewSelector_parameters
    , _bakeViewSelector_publicNodeConfig = f' _bakeViewSelector_publicNodeConfig
    , _bakeViewSelector_publicNodeHeads = f' _bakeViewSelector_publicNodeHeads
    , _bakeViewSelector_nodes = f' _bakeViewSelector_nodes
    , _bakeViewSelector_bakerAddresses = f' _bakeViewSelector_bakerAddresses
    , _bakeViewSelector_bakerDetails = f' _bakeViewSelector_bakerDetails
    , _bakeViewSelector_bakerStats = f' _bakeViewSelector_bakerStats
    , _bakeViewSelector_mailServer = f' _bakeViewSelector_mailServer
    , _bakeViewSelector_summary = f' _bakeViewSelector_summary
    , _bakeViewSelector_nodeAddresses = f' _bakeViewSelector_nodeAddresses
    , _bakeViewSelector_errors = alignWith (these (fmap $ f . This) (fmap $ f . That) (alignWith f)) (_bakeViewSelector_errors xs) (_bakeViewSelector_errors ys)
    , _bakeViewSelector_latestHead = f' _bakeViewSelector_latestHead
    , _bakeViewSelector_upstreamVersion = f' _bakeViewSelector_upstreamVersion
    , _bakeViewSelector_telegramConfig = f' _bakeViewSelector_telegramConfig
    , _bakeViewSelector_telegramRecipients = f' _bakeViewSelector_telegramRecipients
    , _bakeViewSelector_alertCount = f' _bakeViewSelector_alertCount
    }
    where
      f' :: forall f. Align f => (forall x. BakeViewSelector x -> f x) -> f c
      f' p = alignWith f (p xs) (p ys)

instance FunctorMaybe BakeView where
  fmapMaybe f a = BakeView
    { _bakeView_config = fmapMaybe f $ _bakeView_config a
    , _bakeView_clientAddresses = fmapMaybe f $ _bakeView_clientAddresses a
    , _bakeView_clients = fmapMaybe f $ _bakeView_clients a
    , _bakeView_parameters = fmapMaybe f $ _bakeView_parameters a
    , _bakeView_publicNodeConfig = fmapMaybe f $ _bakeView_publicNodeConfig a
    , _bakeView_publicNodeHeads = fmapMaybe f $ _bakeView_publicNodeHeads a
    , _bakeView_nodes = fmapMaybe f $ _bakeView_nodes a
    , _bakeView_bakerAddresses = fmapMaybe f $ _bakeView_bakerAddresses a
    , _bakeView_bakerDetails = fmapMaybe f $ _bakeView_bakerDetails a
    , _bakeView_bakerStats = fmapMaybe f $ _bakeView_bakerStats a
    , _bakeView_mailServer = fmapMaybe f $ _bakeView_mailServer a
    , _bakeView_summary = fmapMaybe f $ _bakeView_summary a
    , _bakeView_nodeAddresses = fmapMaybe f $ _bakeView_nodeAddresses a
    , _bakeView_errors = (fmap.fmapMaybe) f $ _bakeView_errors a
    , _bakeView_latestHead = fmapMaybe f $ _bakeView_latestHead a
    , _bakeView_upstreamVersion = fmapMaybe f $ _bakeView_upstreamVersion a
    , _bakeView_telegramConfig = fmapMaybe f $ _bakeView_telegramConfig a
    , _bakeView_telegramRecipients = fmapMaybe f $ _bakeView_telegramRecipients a
    , _bakeView_alertCount = fmapMaybe f $ _bakeView_alertCount a
    }

fmapMaybeSnd :: FunctorMaybe f => (a -> Maybe b) -> f (e, a) -> f (e, b)
fmapMaybeSnd f = fmapMaybe $ \(e, a) -> case f a of
  Nothing -> Nothing
  Just b -> Just (e, b)

alignTheseWith :: Align f => (These a b -> c) -> These (f a) (f b) -> f c
alignTheseWith f = these (fmap (f . This)) (fmap (f . That)) (alignWith f)

instance Semigroup a => Semigroup (BakeViewSelector a) where
  u <> v = BakeViewSelector
    { _bakeViewSelector_config = (<>) (_bakeViewSelector_config u) (_bakeViewSelector_config v)
    , _bakeViewSelector_clientAddresses = (<>) (_bakeViewSelector_clientAddresses u) (_bakeViewSelector_clientAddresses v)
    , _bakeViewSelector_summary = (<>) (_bakeViewSelector_summary u) (_bakeViewSelector_summary v)
    , _bakeViewSelector_clients = (<>) (_bakeViewSelector_clients u) (_bakeViewSelector_clients v)
    , _bakeViewSelector_parameters = (<>) (_bakeViewSelector_parameters u) (_bakeViewSelector_parameters v)
    , _bakeViewSelector_publicNodeConfig = (<>) (_bakeViewSelector_publicNodeConfig u) (_bakeViewSelector_publicNodeConfig v)
    , _bakeViewSelector_publicNodeHeads = (<>) (_bakeViewSelector_publicNodeHeads u) (_bakeViewSelector_publicNodeHeads v)
    , _bakeViewSelector_nodes = (<>) (_bakeViewSelector_nodes u) (_bakeViewSelector_nodes v)
    , _bakeViewSelector_bakerAddresses = (<>) (_bakeViewSelector_bakerAddresses u) (_bakeViewSelector_bakerAddresses v)
    , _bakeViewSelector_bakerDetails = (<>) (_bakeViewSelector_bakerDetails u) (_bakeViewSelector_bakerDetails v)
    , _bakeViewSelector_bakerStats = (<>) (_bakeViewSelector_bakerStats u) (_bakeViewSelector_bakerStats v)
    , _bakeViewSelector_mailServer = (<>) (_bakeViewSelector_mailServer u) (_bakeViewSelector_mailServer v)
    , _bakeViewSelector_nodeAddresses = (<>) (_bakeViewSelector_nodeAddresses u) (_bakeViewSelector_nodeAddresses v)
    , _bakeViewSelector_errors = (<>) (_bakeViewSelector_errors u) (_bakeViewSelector_errors v)
    , _bakeViewSelector_latestHead = (<>) (_bakeViewSelector_latestHead u) (_bakeViewSelector_latestHead v)
    , _bakeViewSelector_upstreamVersion = (<>) (_bakeViewSelector_upstreamVersion u) (_bakeViewSelector_upstreamVersion v)
    , _bakeViewSelector_telegramConfig = (<>) (_bakeViewSelector_telegramConfig u) (_bakeViewSelector_telegramConfig v)
    , _bakeViewSelector_telegramRecipients = (<>) (_bakeViewSelector_telegramRecipients u) (_bakeViewSelector_telegramRecipients v)
    , _bakeViewSelector_alertCount = (<>) (_bakeViewSelector_alertCount u) (_bakeViewSelector_alertCount v)
    }

instance (Semigroup a, Monoid a) => Monoid (BakeViewSelector a) where
  mempty = BakeViewSelector
    { _bakeViewSelector_config = mempty
    , _bakeViewSelector_clientAddresses = mempty
    , _bakeViewSelector_summary = mempty
    , _bakeViewSelector_clients = mempty
    , _bakeViewSelector_parameters = mempty
    , _bakeViewSelector_publicNodeConfig = mempty
    , _bakeViewSelector_publicNodeHeads = mempty
    , _bakeViewSelector_nodes = mempty
    , _bakeViewSelector_bakerAddresses = mempty
    , _bakeViewSelector_bakerDetails = mempty
    , _bakeViewSelector_bakerStats = Compose mempty
    , _bakeViewSelector_mailServer = mempty
    , _bakeViewSelector_nodeAddresses = mempty
    , _bakeViewSelector_errors = mempty
    , _bakeViewSelector_latestHead = mempty
    , _bakeViewSelector_upstreamVersion = mempty
    , _bakeViewSelector_telegramConfig = mempty
    , _bakeViewSelector_telegramRecipients = mempty
    , _bakeViewSelector_alertCount = mempty
    }
  mappend = (<>)


instance Group (BakeViewSelector SelectedCount) where
  negateG = fmap negateG

instance Additive (BakeViewSelector SelectedCount)

instance (Semigroup a, Monoid a) => Monoid (BakeView a) where
  mempty = BakeView
    { _bakeView_config = mempty
    , _bakeView_clientAddresses = mempty
    , _bakeView_clients = mempty
    , _bakeView_parameters = mempty
    , _bakeView_publicNodeConfig = mempty
    , _bakeView_publicNodeHeads = mempty
    , _bakeView_nodes = mempty
    , _bakeView_bakerAddresses = mempty
    , _bakeView_bakerDetails = mempty
    , _bakeView_bakerStats = mempty
    , _bakeView_mailServer = mempty
    -- , _bakeView_graphs = mempty
    -- , _bakeView_summaryGraph = mempty
    , _bakeView_summary = mempty
    , _bakeView_nodeAddresses = mempty
    , _bakeView_errors = mempty
    , _bakeView_latestHead = mempty
    , _bakeView_upstreamVersion = mempty
    , _bakeView_telegramConfig = mempty
    , _bakeView_telegramRecipients = mempty
    , _bakeView_alertCount = mempty
    }
  mappend u v = u <> v

instance Semigroup a => Semigroup (BakeView a) where
  u <> v = BakeView
    { _bakeView_config = _bakeView_config u <> _bakeView_config v
    , _bakeView_clientAddresses = _bakeView_clientAddresses u <> _bakeView_clientAddresses v
    , _bakeView_clients = _bakeView_clients u <> _bakeView_clients v
    , _bakeView_parameters = _bakeView_parameters u <> _bakeView_parameters v
    , _bakeView_publicNodeConfig = _bakeView_publicNodeConfig u <> _bakeView_publicNodeConfig v
    , _bakeView_publicNodeHeads = _bakeView_publicNodeHeads u <> _bakeView_publicNodeHeads v
    , _bakeView_nodes = _bakeView_nodes u <> _bakeView_nodes v
    , _bakeView_bakerAddresses = _bakeView_bakerAddresses u <> _bakeView_bakerAddresses v
    , _bakeView_bakerDetails = _bakeView_bakerDetails u <> _bakeView_bakerDetails v
    , _bakeView_bakerStats = _bakeView_bakerStats u <> _bakeView_bakerStats v
    , _bakeView_mailServer = _bakeView_mailServer u <> _bakeView_mailServer v
    -- , _bakeView_summaryGraph = _bakeView_summaryGraph u <> _bakeView_summaryGraph v
    -- , _bakeView_graphs = _bakeView_graphs u <> _bakeView_graphs v
    , _bakeView_summary = _bakeView_summary u <> _bakeView_summary v
    , _bakeView_nodeAddresses = _bakeView_nodeAddresses u <> _bakeView_nodeAddresses v
    , _bakeView_errors = _bakeView_errors u <> _bakeView_errors v
    , _bakeView_latestHead = _bakeView_latestHead u <> _bakeView_latestHead v
    , _bakeView_upstreamVersion = _bakeView_upstreamVersion u <> _bakeView_upstreamVersion v
    , _bakeView_telegramConfig = _bakeView_telegramConfig u <> _bakeView_telegramConfig v
    , _bakeView_telegramRecipients = _bakeView_telegramRecipients u <> _bakeView_telegramRecipients v
    , _bakeView_alertCount = _bakeView_alertCount u <> _bakeView_alertCount v
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

fmap concat $ sequence $ concat
  [ map makeLenses
    [ 'BakeView
    , 'BakeViewSelector
    , 'MailServerView
    , 'NodeSummary
    ]
  , [ deriveArgDict ''LogTag
    , deriveGCompare ''LogTag
    , deriveGEq ''LogTag
    , deriveGShow ''LogTag
    , deriveJSONGADT ''LogTag
    ]
  ]
