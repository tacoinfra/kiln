{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Common.App where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Align
import Data.Align
import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as Map
import Data.Fixed
import Data.Fixed
import Data.Semigroup (First (..), Semigroup, (<>))
import Data.Semigroup (First (..), Semigroup, (<>))
import Data.Text (Text)
import Data.Text (Text)
import Data.These
import Data.These
import Data.Typeable
import Data.Typeable
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Reflex (Additive, FunctorMaybe (..), Group (..))
import Reflex (Additive, FunctorMaybe (..), Group (..))
import Reflex.Aeson.Orphans ()
import Reflex.Query.Class
import Rhyolite.App (HasView, View, ViewSelector)
import Rhyolite.Schema (Email, Id)

import Common.Schema


data Bake = Bake

data BakeViewSelector a = BakeViewSelector
  { _bakeViewSelector_summary :: Maybe a
  , _bakeViewSelector_clientAddresses :: Maybe a
  , _bakeViewSelector_clients :: AppendMap (Id Client) a
  , _bakeViewSelector_parameters :: Maybe a
  , _bakeViewSelector_nodes :: Maybe a
  , _bakeViewSelector_notificatees :: Maybe a
  , _bakeViewSelector_mailServers :: Maybe a
  }
  deriving (Show, Eq, Ord, Functor, Generic, Typeable, Traversable, Foldable)

data MailServerView = MailServerView
  { _mailServerView_hostName :: Text
  , _mailServerView_portNumber :: Word16
  , _mailServerView_smtpProtocol :: SmtpProtocol
  , _mailServerView_userName :: Text
  } deriving (Eq, Generic, Read, Show)

instance FromJSON MailServerView
instance ToJSON MailServerView

mailServerConfigToView :: MailServerConfig -> MailServerView
mailServerConfigToView x = MailServerView
  { _mailServerView_hostName = _mailServerConfig_hostName x
  , _mailServerView_portNumber = _mailServerConfig_portNumber x
  , _mailServerView_smtpProtocol = _mailServerConfig_smtpProtocol x
  , _mailServerView_userName = _mailServerConfig_userName x
  }

data BakeView a = BakeView
  { _bakeView_clientAddresses :: AppendMap (Id Client) (First (Maybe ClientAddress), a)
  , _bakeView_clients :: AppendMap (Id Client) (First (Maybe ClientInfo), a)
  , _bakeView_parameters :: AppendMap (Id Node) (First (Maybe ProtoInfo), a)
  , _bakeView_nodes :: AppendMap (Id Node) (First (Maybe Node), a)
  , _bakeView_notificatees :: AppendMap (Id Notificatee) (First (Maybe Email), a)
  , _bakeView_mailServers :: AppendMap (Id MailServerConfig) (First (Maybe MailServerView), a)
  , _bakeView_summary :: First (Maybe ((Report, Int), a)) -- The Int is the number of bakers we've yet to get a report from.
  , _bakeView_summaryGraph :: First (Maybe ((Micro, Text), a))
  , _bakeView_graphs :: AppendMap (Id Client) (First (Maybe (Micro, Text)), a)
  }
  deriving (Show, Eq, Functor, Generic, Typeable, Traversable, Foldable)

cropBakeView :: (Semigroup a, Monoid a) => BakeViewSelector a -> BakeView a -> BakeView a
cropBakeView vs v =
  let clientAddresses = case _bakeViewSelector_clientAddresses vs of
        Nothing -> mempty
        Just _ -> _bakeView_clientAddresses v
      clients = Map.intersectionWith const (_bakeView_clients v) (_bakeViewSelector_clients vs)
      parameters = case _bakeViewSelector_parameters vs of
        Nothing -> mempty
        Just _ -> _bakeView_parameters v
      nodes = case _bakeViewSelector_nodes vs of
        Nothing -> mempty
        Just _ -> _bakeView_nodes v
      notificatees = case _bakeViewSelector_notificatees vs of
        Nothing -> mempty
        Just _ -> _bakeView_notificatees v
      mailServers = case _bakeViewSelector_mailServers vs of
        Nothing -> mempty
        Just _ -> _bakeView_mailServers v
      graphs = Map.intersectionWith const (_bakeView_graphs v) (_bakeViewSelector_clients vs)
      summaryGraph = case _bakeViewSelector_summary vs of
        Nothing -> First Nothing
        Just _ -> _bakeView_summaryGraph v
      summary = case _bakeViewSelector_summary vs of
        Nothing -> First Nothing
        Just _ -> _bakeView_summary v
  in BakeView
      { _bakeView_clientAddresses = clientAddresses
      , _bakeView_clients = clients
      , _bakeView_parameters = parameters
      , _bakeView_nodes = nodes
      , _bakeView_notificatees = notificatees
      , _bakeView_mailServers = mailServers
      , _bakeView_graphs = graphs
      , _bakeView_summaryGraph = summaryGraph
      , _bakeView_summary = summary
      }

instance Align BakeViewSelector where
  nil = BakeViewSelector nil nil nil nil nil nil nil
  alignWith f u v = BakeViewSelector
    { _bakeViewSelector_clientAddresses = alignWith f (_bakeViewSelector_clientAddresses u) (_bakeViewSelector_clientAddresses v)
    , _bakeViewSelector_summary = alignWith f (_bakeViewSelector_summary u) (_bakeViewSelector_summary v)
    , _bakeViewSelector_clients = alignWith f (_bakeViewSelector_clients u) (_bakeViewSelector_clients v)
    , _bakeViewSelector_parameters = alignWith f (_bakeViewSelector_parameters u) (_bakeViewSelector_parameters v)
    , _bakeViewSelector_nodes = alignWith f (_bakeViewSelector_nodes u) (_bakeViewSelector_nodes v)
    , _bakeViewSelector_notificatees = alignWith f (_bakeViewSelector_notificatees u) (_bakeViewSelector_notificatees v)
    , _bakeViewSelector_mailServers = alignWith f (_bakeViewSelector_mailServers u) (_bakeViewSelector_mailServers v)
    }

instance FunctorMaybe BakeViewSelector where
  fmapMaybe f a = BakeViewSelector
    { _bakeViewSelector_clientAddresses = fmapMaybe f $ _bakeViewSelector_clientAddresses a
    , _bakeViewSelector_summary = fmapMaybe f $ _bakeViewSelector_summary a
    , _bakeViewSelector_clients = fmapMaybe f $ _bakeViewSelector_clients a
    , _bakeViewSelector_parameters = fmapMaybe f $ _bakeViewSelector_parameters a
    , _bakeViewSelector_nodes = fmapMaybe f $ _bakeViewSelector_nodes a
    , _bakeViewSelector_notificatees = fmapMaybe f $ _bakeViewSelector_notificatees a
    , _bakeViewSelector_mailServers = fmapMaybe f $ _bakeViewSelector_mailServers a
    }

{-
instance Align BakeView where
  nil = BakeView nil
  alignWith f u v = BakeView
    { _bakeView_clients = alignTheseWith f (_bakeView_clients u) (_bakeView_clients v)
    { _bakeView_parameters = alignTheseWith f (_bakeView_parameters u) (_bakeView_parameters v)
    }
-}

instance FunctorMaybe BakeView where
  fmapMaybe f a = BakeView
    { _bakeView_clientAddresses = fmapMaybeSnd f $ _bakeView_clientAddresses a
    , _bakeView_clients = fmapMaybeSnd f $ _bakeView_clients a
    , _bakeView_parameters = fmapMaybeSnd f $ _bakeView_parameters a
    , _bakeView_nodes = fmapMaybeSnd f $ _bakeView_nodes a
    , _bakeView_notificatees = fmapMaybeSnd f $ _bakeView_notificatees a
    , _bakeView_mailServers = fmapMaybeSnd f $ _bakeView_mailServers a
    , _bakeView_graphs = fmapMaybeSnd f $ _bakeView_graphs a
    , _bakeView_summaryGraph = fmapMaybeFirstMaybePair f (_bakeView_summaryGraph a)
    , _bakeView_summary = fmapMaybeFirstMaybePair f (_bakeView_summary a)
    }

fmapMaybeFirstMaybePair :: (a -> Maybe b) -> First (Maybe (x, a)) -> First (Maybe (x, b))
fmapMaybeFirstMaybePair f p = case p of
  First Nothing -> First Nothing
  First (Just (x,u)) -> case f u of
    Nothing -> First Nothing
    Just v -> First (Just (x,v))

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
    , _bakeView_nodes = mempty
    , _bakeView_notificatees = mempty
    , _bakeView_mailServers = mempty
    , _bakeView_graphs = mempty
    , _bakeView_summaryGraph = First Nothing
    , _bakeView_summary = First Nothing
    }
  mappend u v = u <> v

instance Semigroup a => Semigroup (BakeView a) where
  u <> v = BakeView
    { _bakeView_clientAddresses = _bakeView_clientAddresses u <> _bakeView_clientAddresses v
    , _bakeView_clients = _bakeView_clients u <> _bakeView_clients v
    , _bakeView_parameters = _bakeView_parameters u <> _bakeView_parameters v
    , _bakeView_nodes = _bakeView_nodes u <> _bakeView_nodes v
    , _bakeView_notificatees = _bakeView_notificatees u <> _bakeView_notificatees v
    , _bakeView_mailServers = _bakeView_mailServers u <> _bakeView_mailServers v
    , _bakeView_summaryGraph = _bakeView_summaryGraph u <> _bakeView_summaryGraph v
    , _bakeView_graphs = _bakeView_graphs u <> _bakeView_graphs v
    , _bakeView_summary = _bakeView_summary u <> _bakeView_summary v
    }

instance (Monoid a, Semigroup a) => Query (BakeViewSelector a) where
  type QueryResult (BakeViewSelector a) = BakeView a
  crop = cropBakeView

instance FromJSON a => FromJSON (BakeViewSelector a)
instance FromJSON a => FromJSON (BakeView a)

instance ToJSON a => ToJSON (BakeViewSelector a)
instance ToJSON a => ToJSON (BakeView a)

instance HasView Bake where
  type View Bake = BakeView
  type ViewSelector Bake = BakeViewSelector

concat <$> mapM makeLenses
  [ 'MailServerView
  ]
