{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-} -- for {Eq, Ord, Show} Notify

{-# OPTIONS_GHC -Wall -Werror #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Backend.Schema
  ( module Backend.Schema

  -- Re-exports
  , toId
  , Only(..)
  ) where

import Control.Lens (Field1, Field2)
import Data.Time (UTCTime, NominalDiffTime)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.GADT (deriveJSONGADT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short (fromShort, toShort)
import Data.Constraint (Dict(..))
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Dependent.Sum (DSum(..))
import Data.Fixed (Fixed (MkFixed), HasResolution)
import Data.GADT.Compare.TH (deriveGEq)
import Data.GADT.Compare.TH (deriveGCompare)
import Data.GADT.Show.TH (deriveGShow)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import Data.Some (Some(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Version (Version)
import qualified Data.Version as Version
import Data.Word (Word64)
import Database.Id.Class
import Database.Groundhog.Core
import Database.Id.Groundhog
import qualified Database.Groundhog.Expression as GH
import Database.Groundhog.Generic
import Database.Groundhog.Instances ()
import Database.Groundhog.Postgresql (AutoKeyField (..), PersistBackend, executeRaw, get, update, (==.))
import qualified Database.Groundhog.Postgresql.Array as Groundhog
import Database.Groundhog.TH (groundhog)
import Database.PostgreSQL.Simple (Binary (..), Only (..), fromBinary, (:.)(..) )
import Database.PostgreSQL.Simple.FromField hiding (Binary, Field)
import Database.PostgreSQL.Simple.ToField (ToField (toField), Action(Plain))
import Database.PostgreSQL.Simple.Types (PGArray (..))
import qualified Formatting as Fmt
import Language.Haskell.TH (conE)
import Language.Haskell.TH (conT)
import Language.Haskell.TH (mkName)
import Language.Haskell.TH (nameBase)
import Rhyolite.Backend.Account ()
import Rhyolite.Backend.Listen (HasNotification (..), NotificationType (..), DbNotification (..), getSchemaName, notifyChannel)
import Rhyolite.Backend.Schema.Class (DefaultKeyIsUnique)
import Rhyolite.Backend.Schema.Class (DefaultKeyUnique)
import Rhyolite.Backend.Schema.Class (defaultKeyToKey)
import Rhyolite.Backend.Schema.Class (HasSingleConstructor)
import Rhyolite.Backend.Schema.Class (SingleConstructor)
import Rhyolite.Backend.Schema.Class (singleConstructor)
import Rhyolite.Backend.Schema.TH (makeDefaultKeyIdInt64, mkRhyolitePersist)
import Rhyolite.Schema (Json (..), SchemaName (..))
import Text.Read (readMaybe)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.NodeRPC (PublicNode (..))
import Tezos.Types hiding (TestChainStatus)

import Backend.Version (parseVersion)
import Common.AppendIntervalMap (WithInfinity(..))
import Common.App (SetupState, VoteState)
import Common.Schema
import ExtraPrelude

stripOnly :: Coercible (f (Only a)) (f a) => f (Only a) -> f a
stripOnly = coerce

data NotifyTag a where
  NotifyTag_Baker :: NotifyTag (Id Baker, Maybe BakerData)
  NotifyTag_BakerDetails :: NotifyTag BakerDetails
  NotifyTag_BakerRightsProgress :: NotifyTag (Id BakerRightsCycleProgress, BakerRightsCycleProgress, [BakerRight])
  NotifyTag_ErrorLog :: LogTag b -> NotifyTag (Id b)
  NotifyTag_ProtocolIndex :: NotifyTag (Id ProtocolIndex)
  NotifyTag_UpstreamVersion :: NotifyTag (Id UpstreamVersion, UpstreamVersion)
  NotifyTag_MailServerConfig :: NotifyTag (Id MailServerConfig, MailServerConfig)
  NotifyTag_NodeExternal :: NotifyTag (Id Node, Maybe NodeExternalData)
  NotifyTag_NodeInternal :: NotifyTag (Id Node, Maybe ProcessData)
  NotifyTag_NodeDetails :: NotifyTag (Id Node, Maybe NodeDetailsData)
  NotifyTag_Notificatee :: NotifyTag (Id Notificatee)
  NotifyTag_PublicNodeConfig :: NotifyTag (Id PublicNodeConfig, PublicNodeConfig)
  NotifyTag_PublicNodeHead :: NotifyTag (Id PublicNodeHead, Maybe PublicNodeHead)
  NotifyTag_SnapshotMeta :: NotifyTag SnapshotMeta
  NotifyTag_TelegramConfig :: NotifyTag (Id TelegramConfig, TelegramConfig)
  NotifyTag_TelegramRecipient :: NotifyTag (Id TelegramRecipient, Maybe TelegramRecipient)
  NotifyTag_ConnectedLedger :: NotifyTag (Maybe ConnectedLedger)
  NotifyTag_ShowLedger :: NotifyTag (SecretKey, Maybe (PublicKeyHash, Tez))
  NotifyTag_Prompting :: NotifyTag (SecretKey, Maybe SetupState)
  NotifyTag_VotePrompting :: NotifyTag (SecretKey, Maybe VoteState)
  NotifyTag_RightNotificationSettings :: NotifyTag (RightKind, Maybe RightNotificationLimit)
  NotifyTag_Amendment :: NotifyTag (VotingPeriodKind, Maybe Amendment)
  NotifyTag_Proposals :: NotifyTag (Id PeriodProposal, Maybe (PeriodProposal, Maybe Bool))
  NotifyTag_PeriodTestingVote :: NotifyTag (Maybe PeriodTestingVote)
  NotifyTag_PeriodTesting :: NotifyTag (Maybe PeriodTesting)
  NotifyTag_PeriodPromotionVote :: NotifyTag (Maybe PeriodPromotionVote)
  NotifyTag_BakerVote :: NotifyTag (Maybe BakerVote)
  NotifyTag_BakerRegistered :: NotifyTag (PublicKeyHash, Bool)
  deriving Typeable

mkNotify :: PersistBackend m => n a -> a -> m (DbNotification n)
mkNotify n a = do
  schemaName <- getSchemaName
  pure DbNotification
    { _dbNotification_schemaName = SchemaName $ T.pack schemaName
    , _dbNotification_notificationType = NotificationType_Update
    , _dbNotification_message = n :=> Identity a
    }

notify'
  :: ( PersistBackend m
     , Has' ToJSON n Identity
     , ForallF ToJSON n
     )
  => DbNotification n -> m ()
notify' n = do
  let cmd = "NOTIFY " <> notifyChannel <> ", ?"
  void $ executeRaw False cmd [PersistString $ T.unpack $ T.decodeUtf8 $ LBS.toStrict $ Aeson.encode n]

notify
  :: ( PersistBackend m
     , Has' ToJSON n Identity
     , ForallF ToJSON n
     )
  => n a -> a -> m ()
notify n a = notify' =<< mkNotify n a

notifyDefault
  :: ( PersistBackend m
     , HasDefaultNotify a
     )
  => a -> m ()
notifyDefault x = notify' =<< mkDefaultNotify x

mkNodeNotify :: NodeLogTag a -> NotifyTag (Id a)
mkNodeNotify = NotifyTag_ErrorLog . LogTag_Node

mkBakerNotify :: BakerLogTag a -> NotifyTag (Id a)
mkBakerNotify = NotifyTag_ErrorLog . LogTag_Baker

class HasDefaultNotify a where
  mkDefaultNotify :: PersistBackend m => a -> m (DbNotification NotifyTag)
  default mkDefaultNotify :: (PersistBackend m, HasNotification NotifyTag b, a ~ Id b) => a -> m (DbNotification NotifyTag)
  mkDefaultNotify = mkNotify $ notification Proxy

instance HasDefaultNotify (DSum LogTag Id) where
  mkDefaultNotify (t :=> v) = mkNotify (NotifyTag_ErrorLog t) v
instance HasDefaultNotify (DSum NodeLogTag Id) where
  mkDefaultNotify (t :=> v) = mkDefaultNotify $ LogTag_Node t :=> v
instance HasDefaultNotify (DSum BakerLogTag Id) where
  mkDefaultNotify (t :=> v) = mkDefaultNotify $ LogTag_Baker t :=> v

instance HasDefaultNotify (Id ErrorLogBadNodeHead)
instance HasDefaultNotify (Id ErrorLogBakerAccused)
instance HasDefaultNotify (Id ErrorLogBakerDeactivated)
instance HasDefaultNotify (Id ErrorLogBakerDeactivationRisk)
instance HasDefaultNotify (Id ErrorLogBakerLedgerDisconnected)
instance HasDefaultNotify (Id ErrorLogBakerMissed)
instance HasDefaultNotify (Id ErrorLogBakerNoHeartbeat)
instance HasDefaultNotify (Id ErrorLogInaccessibleNode)
instance HasDefaultNotify (Id ErrorLogInsufficientFunds)
instance HasDefaultNotify (Id ErrorLogInternalNodeFailed)
instance HasDefaultNotify (Id ErrorLogNetworkUpdate)
instance HasDefaultNotify (Id ErrorLogNodeInvalidPeerCount)
instance HasDefaultNotify (Id ErrorLogNodeVersionMismatch)
instance HasDefaultNotify (Id ErrorLogNodeWrongChain)
instance HasDefaultNotify (Id ErrorLogVotingReminder)
instance HasDefaultNotify (Id ProtocolIndex)

instance HasNotification NotifyTag ProtocolIndex where
  notification _ = NotifyTag_ProtocolIndex

instance HasNotification NotifyTag ErrorLogNodeVersionMismatch where
  notification _ = mkNodeNotify NodeLogTag_VersionMismatch
instance HasNotification NotifyTag ErrorLogNodeWrongChain where
  notification _ = mkNodeNotify NodeLogTag_NodeWrongChain
instance HasNotification NotifyTag ErrorLogNodeInvalidPeerCount where
  notification _ = mkNodeNotify NodeLogTag_NodeInvalidPeerCount
instance HasNotification NotifyTag ErrorLogBadNodeHead where
  notification _ = mkNodeNotify NodeLogTag_BadNodeHead
instance HasNotification NotifyTag ErrorLogInaccessibleNode where
  notification _ = mkNodeNotify NodeLogTag_InaccessibleNode

instance HasNotification NotifyTag ErrorLogBakerLedgerDisconnected where
  notification _ = mkBakerNotify BakerLogTag_BakerLedgerDisconnected
instance HasNotification NotifyTag ErrorLogBakerAccused where
  notification _ = mkBakerNotify BakerLogTag_BakerAccused
instance HasNotification NotifyTag ErrorLogBakerDeactivated where
  notification _ = mkBakerNotify BakerLogTag_BakerDeactivated
instance HasNotification NotifyTag ErrorLogBakerDeactivationRisk where
  notification _ = mkBakerNotify BakerLogTag_BakerDeactivationRisk
instance HasNotification NotifyTag ErrorLogBakerMissed where
  notification _ = mkBakerNotify BakerLogTag_BakerMissed
instance HasNotification NotifyTag ErrorLogInsufficientFunds where
  notification _ = mkBakerNotify BakerLogTag_InsufficientFunds
instance HasNotification NotifyTag ErrorLogVotingReminder where
  notification _ = mkBakerNotify BakerLogTag_VotingReminder

instance HasNotification NotifyTag ErrorLogNetworkUpdate where
  notification _ = NotifyTag_ErrorLog LogTag_NetworkUpdate
instance HasNotification NotifyTag ErrorLogInternalNodeFailed where
  notification _ = NotifyTag_ErrorLog LogTag_InternalNodeFailed
instance HasNotification NotifyTag ErrorLogBakerNoHeartbeat where
  notification _ = NotifyTag_ErrorLog LogTag_BakerNoHeartbeat

instance HasDefaultNotify (Id Notificatee) where
  mkDefaultNotify = mkNotify NotifyTag_Notificatee
instance HasDefaultNotify BakerDetails where
  mkDefaultNotify = mkNotify NotifyTag_BakerDetails

class HasDefaultNotifyUnique f where
  mkDefaultNotifyUnique :: PersistBackend m => Id f -> f -> m (DbNotification NotifyTag)

instance HasDefaultNotifyUnique MailServerConfig where
  mkDefaultNotifyUnique = curry $ mkNotify NotifyTag_MailServerConfig
instance HasDefaultNotifyUnique TelegramConfig where
  mkDefaultNotifyUnique = curry $ mkNotify NotifyTag_TelegramConfig

type EntityWithId a = (DefaultKeyId a, DefaultKey a ~ Key a BackendSpecific, PersistEntity a, PrimitivePersistField (Key a BackendSpecific))

getId :: (PersistBackend m, EntityWithId a) => Id a -> m (Maybe a)
getId = get . fromId

type EntityWithIdBy u a = (DefaultKeyId a, DefaultKey a ~ Key a (Unique u), PersistEntity a, IsUniqueKey (Key a (Unique u)))
getIdBy :: (PersistBackend m, EntityWithIdBy u a) => Id a -> m (Maybe a)
getIdBy = getBy . fromId

-- Version of 'deleteAll' that doesn't entice you to use 'undefined'.
deleteAll' :: forall v m proxy. (PersistBackend m, PersistEntity v) => proxy v -> m ()
deleteAll' _ = deleteAll (error "deleteAll argument was demanded" :: v)

updateId
  :: (EntityWithId a, GH.Expression (PhantomDb m) (RestrictionHolder v c) (DefaultKey a), PersistEntity v, PersistBackend m, GH.Unifiable (AutoKeyField v c) (DefaultKey a), _)
  => Id a
  -> [Update (PhantomDb m) (RestrictionHolder v c)]
  -> m ()
updateId tid dt = update dt (AutoKeyField ==. fromId tid)

insert' :: (EntityWithId a, AutoKey a ~ Key a BackendSpecific, PersistBackend m) => a -> m (Id a)
insert' r = toId <$> insert r

updateIdNotify
  :: (HasDefaultNotify (Id a), EntityWithId a, GH.Expression (PhantomDb m) (RestrictionHolder v c) (DefaultKey a), PersistEntity v, PersistBackend m, GH.Unifiable (AutoKeyField v c) (DefaultKey a), _)
  => Id a
  -> [Update (PhantomDb m) (RestrictionHolder v c)]
  -> m ()
updateIdNotify tid dt = do
  updateId tid dt
  notifyDefault tid

updateIdNotifyUnique
  :: (HasDefaultNotifyUnique a, EntityWithId a, GH.Expression (PhantomDb m) (RestrictionHolder v c) (DefaultKey a), PersistEntity v, PersistBackend m, GH.Unifiable (AutoKeyField v c) (DefaultKey a), _)
  => Id a
  -> [Update (PhantomDb m) (RestrictionHolder v c)]
  -> m ()
updateIdNotifyUnique tid dt = do
  updateId tid dt
  newRow <- getId tid >>= \case
    Nothing -> fail "impossible got nothing back after insertion in DB transaction"
    Just x -> pure x
  notify' =<< mkDefaultNotifyUnique tid newRow

insertNotify :: (HasDefaultNotify (Id a), EntityWithId a, AutoKey a ~ Key a BackendSpecific, PersistBackend m) => a -> m (Id a)
insertNotify a = do
  primaryKey <- insert' a
  notifyDefault primaryKey
  pure primaryKey

insertNotifyUnique :: (HasDefaultNotifyUnique a, EntityWithId a, AutoKey a ~ Key a BackendSpecific, PersistBackend m) => a -> m (Id a)
insertNotifyUnique a = do
  primaryKey <- insert' a
  notify' =<< mkDefaultNotifyUnique primaryKey a
  pure primaryKey

selectIds
  :: forall a (m :: * -> *) v (c :: (* -> *) -> *) t.
     ( ProjectionDb t (PhantomDb m)
     , ProjectionRestriction t (RestrictionHolder v c), DefaultKeyId v
     , Projection t v, EntityConstr v c
     , HasSelectOptions a (PhantomDb m) (RestrictionHolder v c)
     , PersistBackend m, AutoKey v ~ DefaultKey v)
  => t -- ^ Constructor
  -> a -- ^ Select options
  -> m [(Id v, v)]
selectIds constr = fmap (fmap (first toId)) . project (AutoKeyField, constr)

data CacheBakingRights = CacheBakingRights
  { _cacheBakingRights_context :: !BlockHash
  , _cacheBakingRights_level :: !RawLevel
  , _cacheBakingRights_result :: !(Json Aeson.Value)
  }
  deriving (Eq, Show, Typeable)

data CacheEndorsingRights = CacheEndorsingRights
  { _cacheEndorsingRights_context :: !BlockHash
  , _cacheEndorsingRights_level :: !RawLevel
  , _cacheEndorsingRights_result :: !(Json Aeson.Value)
  }
  deriving (Eq, Show, Typeable)

data RawCacheEntry = RawCacheEntry
  { _rawCacheEntry_chainId :: !ChainId
  , _rawCacheEntry_key :: !(Json Aeson.Value)
  , _rawCacheEntry_value :: !LBS.ByteString
  } deriving (Eq, Generic, Show, Typeable)
instance HasId RawCacheEntry

instance FromField Word64 where
  fromField f b = fromInteger <$> fromField f b -- is this sign-correct?

-- TODO: Move all of this into postgresql-simple
instance ToField (Fixed a) where
  toField (MkFixed x) = toField x

instance FromField (Fixed a) where
  fromField f b = MkFixed . toInteger @Int64 <$> fromField f b

instance HasResolution a => PrimitivePersistField (Fixed a) where
  toPrimitivePersistValue p (MkFixed x) = toPrimitivePersistValue p (fromInteger x :: Int64)
  fromPrimitivePersistValue p x = MkFixed (toInteger (fromPrimitivePersistValue p x :: Int64))

instance HasResolution a => PersistField (Fixed a) where
  persistName _ = "Fixed"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbInt64 False Nothing Nothing

instance PrimitivePersistField (Vector Tez) where
  toPrimitivePersistValue p x = toPrimitivePersistValue p ( Groundhog.Array $ V.toList x)
  fromPrimitivePersistValue p = V.fromList . unArray . fromPrimitivePersistValue p

instance PersistField (Vector Tez) where
  persistName _ = "VectorTez"
  toPersistValues = toPersistValues . Groundhog.Array . V.toList
  fromPersistValues vs = first (V.fromList . unArray) <$> fromPersistValues vs
  dbType p x = dbType p (Groundhog.Array (V.toList x))

instance PrimitivePersistField Tez where
  toPrimitivePersistValue p (Tez x) = toPrimitivePersistValue p x
  fromPrimitivePersistValue p v = Tez $ fromPrimitivePersistValue p v

instance FromField NominalDiffTime where
  fromField f b = toEnum <$> fromField f b

instance PrimitivePersistField NominalDiffTime where
  toPrimitivePersistValue p x = toPrimitivePersistValue p $ fromEnum x
  fromPrimitivePersistValue p v = toEnum $ fromPrimitivePersistValue p v

instance PersistField NominalDiffTime where
  persistName _ = "NominalDiffTime"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType p x = dbType p (fromEnum x)

instance PersistField NamedChainOrChainId where
  persistName _ = "NamedChainOrChainId"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType p x = dbType p (error "dbType for NamedChainOrChainId forced" :: String)

instance PrimitivePersistField NamedChainOrChainId where
  toPrimitivePersistValue p (NamedChainOrChainId c) = toPrimitivePersistValue p $ showChain c
  fromPrimitivePersistValue p v = NamedChainOrChainId $ parseChainOrError $ fromPrimitivePersistValue p v

instance ToField NamedChainOrChainId where
  toField (NamedChainOrChainId v) = toField (showChain v)

instance ToField PublicNode where
  toField = toField . show

deriving instance ToField Tez
deriving instance FromField Tez

instance ToField Version where
  toField v = toField (Version.showVersion v)

instance FromField Version where
  fromField f b = parseVersionOrError <$> fromField f b

instance ToField UpgradeCheckError where
  toField v = toField (show v)

instance FromField UpgradeCheckError where
  fromField f b = read <$> fromField f b

instance ToField NamedChain where
  toField v = toField (show v)

instance FromField NamedChain where
  fromField f b = read <$> fromField f b

instance FromField ProcessState where
  fromField f b = read <$> fromField f b

instance ToField ProcessState where
  toField v = toField (show v)

instance FromField ProcessControl where
  fromField f = maybe (fail "Invalid value for ProcessControl") pure . readMaybe <=< fromField f

instance ToField ProcessControl where
  toField v = toField (show v)

instance FromField VotingPeriodKind where
  fromField f = maybe (fail "Invalid value for VotingPeriodKind") pure . readMaybe <=< fromField f

instance ToField VotingPeriodKind where
  toField v = toField (show v)

instance PersistField Tez where
  persistName _ = "Tez"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType p x = dbType p (getTez x)

instance PersistField PeriodSequence where
  persistName _ = "PeriodSequence"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType p x = dbType p (error "dbType for PeriodSequence forced" :: Json (NonEmpty TezosWord64))

instance PrimitivePersistField PeriodSequence where
  toPrimitivePersistValue p (PeriodSequence x) = toPrimitivePersistValue p (Json x)
  fromPrimitivePersistValue p x = PeriodSequence $ unJson $ fromPrimitivePersistValue p x

instance NeverNull (HashedValue a)
-- instance NeverNull (Json BlockInfo)
instance NeverNull Cycle
instance NeverNull Fitness
instance NeverNull LedgerIdentifier
instance NeverNull NetworkStat
instance NeverNull PublicKeyHash
instance NeverNull RawLevel
instance NeverNull Tez
instance NeverNull TezosWord64
instance NeverNull Version
instance NeverNull VeryBlockLike

parseVersionOrError :: Text -> Version
parseVersionOrError = fromMaybe (error "Invalid version") . parseVersion

-- instance TezosBinary a => PersistField (Base16ByteString a) where
--   persistName _ = "Base16ByteString"
--   toPersistValues = primToPersistValue . encodeBinary . unbase16ByteString
--   fromPersistValues = (fmap.first) (Base16ByteString . unsafeParseBinary) . primFromPersistValue
--   dbType p x = dbType p (encodeBinary x)

instance FromField (HashedValue t) where
  fromField f b = HashedValue . toShort . fromBinary <$> fromField f b

instance ToField (HashedValue t) where
  toField (HashedValue a) = toField $ Binary $ fromShort a


instance {-PrimitivePersistField a =>-} PersistField (HashedValue t) where
  persistName _ = "HashedValue"
  toPersistValues = primToPersistValue . fromShort . unHashedValue
  fromPersistValues = (fmap.first) (HashedValue . toShort) . primFromPersistValue
  dbType p _ = dbType p (error "dbType for HashedValue forced" :: ByteString)

deriving instance ToField TezosWord64
deriving instance FromField TezosWord64

deriving instance ToField RawLevel
deriving instance FromField RawLevel

deriving instance ToField Cycle
deriving instance FromField Cycle

instance PrimitivePersistField TezosWord64 where
  toPrimitivePersistValue x (TezosWord64 v) = toPrimitivePersistValue x v
  fromPrimitivePersistValue x v = TezosWord64 $ fromPrimitivePersistValue x v

instance PrimitivePersistField RawLevel where
  toPrimitivePersistValue x (RawLevel v) = toPrimitivePersistValue x v
  fromPrimitivePersistValue x v = RawLevel $ fromPrimitivePersistValue x v

instance PrimitivePersistField Cycle where
  toPrimitivePersistValue x (Cycle v) = toPrimitivePersistValue x v
  fromPrimitivePersistValue x v = Cycle $ fromPrimitivePersistValue x v

instance PrimitivePersistField (HashedValue t) where
  toPrimitivePersistValue x (HashedValue v) = toPrimitivePersistValue x $ fromShort v
  fromPrimitivePersistValue x v = HashedValue $ toShort $ fromPrimitivePersistValue x v

instance PersistField TezosWord64 where
  persistName _ = "TezosWord64"
  toPersistValues = primToPersistValue . unTezosWord64
  fromPersistValues = (fmap . first) TezosWord64 . primFromPersistValue
  dbType p (TezosWord64 x) = dbType p x

instance PersistField RawLevel where
  persistName _ = "RawLevel"
  toPersistValues (RawLevel x) = primToPersistValue x
  fromPersistValues = (fmap . first) RawLevel . primFromPersistValue
  dbType p (RawLevel x) = dbType p x

instance PersistField Cycle where
  persistName _ = "Cycle"
  toPersistValues (Cycle x) = primToPersistValue x
  fromPersistValues = (fmap . first) Cycle . primFromPersistValue
  dbType p (Cycle x) = dbType p x

instance PrimitivePersistField Version where
  toPrimitivePersistValue x v = toPrimitivePersistValue x (Version.showVersion v)
  fromPrimitivePersistValue x v = parseVersionOrError $ fromPrimitivePersistValue x v

instance PersistField Version where
  persistName _ = "Version"
  toPersistValues x = primToPersistValue (Version.showVersion x)
  fromPersistValues = fmap (first parseVersionOrError) . primFromPersistValue
  dbType p x = dbType p ("" :: String)

instance PersistField PublicKeyHash where
  persistName _ = "PublicKeyHash"
  toPersistValues (PublicKeyHash_Ed25519 x) = primToPersistValue $ toBase58Text x
  toPersistValues (PublicKeyHash_Secp256k1 x) = primToPersistValue $ toBase58Text x
  toPersistValues (PublicKeyHash_P256 x) = primToPersistValue $ toBase58Text x
  fromPersistValues = (fmap.first) toPublicKeyHash . primFromPersistValue
    where
      toPublicKeyHash = either (error . show) id . tryFromBase58 publicKeyHashConstructorDecoders . T.encodeUtf8
  dbType p _ = dbType p ("" :: Text)

leftPad :: Int -> Text
leftPad n = if T.length n' > 4 then error "too dang big" else n'
  where
    n' = LT.toStrict $ Fmt.format (Fmt.left 4 '0') $ Fmt.format Fmt.hex n

unArray :: Groundhog.Array a -> [a]
unArray (Groundhog.Array a) = a

-- prefix fitness arrays with length so that they naturally order correctly
--
-- FOOTGUN ALERT: groundhog makes this look like VARCHAR[], but to
-- postgresql-simple it looks like TEXT[].  you probably need a cast anyplace
-- the two types may interact
toDBFitness :: ToJSON a => FitnessF a -> [Text]
toDBFitness (FitnessF x) = ((leftPad $ length x) :) .  toList . fmap (T.decodeUtf8 . LBS.toStrict . Aeson.encode) $ x
fromDBFitness :: FromJSON a => [Text] -> FitnessF a
fromDBFitness = FitnessF . Seq.fromList . fmap ( fromJust . Aeson.decode . LBS.fromStrict . T.encodeUtf8 ) . tail

instance (ToJSON a, FromJSON a) => PrimitivePersistField (FitnessF a) where
  toPrimitivePersistValue p x = toPrimitivePersistValue p ( Groundhog.Array $ toDBFitness x)
  fromPrimitivePersistValue p = fromDBFitness . unArray . fromPrimitivePersistValue p

instance (FromJSON a, ToJSON a) => PersistField (FitnessF a) where
  persistName _ = "Fitness"
  toPersistValues = toPersistValues . Groundhog.Array . toDBFitness
  fromPersistValues vs = first (fromDBFitness . unArray) <$> fromPersistValues vs
  dbType p x = dbType p (Groundhog.Array $ toDBFitness x) -- p (Json (Seq.empty :: Seq.Seq (Base16ByteString a)))

instance (ToJSON a) => ToField (FitnessF a) where
  toField v = toField $ PGArray $ toDBFitness v
instance (FromJSON a) => FromField (FitnessF a) where
  fromField a b = fromDBFitness . fromPGArray <$> fromField a b

instance PrimitivePersistField PublicKeyHash where
  toPrimitivePersistValue a (PublicKeyHash_Ed25519 x) = toPrimitivePersistValue a $ toBase58Text x
  toPrimitivePersistValue a (PublicKeyHash_Secp256k1 x) = toPrimitivePersistValue a $ toBase58Text x
  toPrimitivePersistValue a (PublicKeyHash_P256 x) = toPrimitivePersistValue a $ toBase58Text x
  fromPrimitivePersistValue a = toPublicKeyHash . fromPrimitivePersistValue a
    where
      toPublicKeyHash = either (error . show) id . tryFromBase58 publicKeyHashConstructorDecoders . T.encodeUtf8

instance PersistField URI where
  persistName _ = "URI"
  toPersistValues = primToPersistValue
  fromPersistValues vs = first (fromMaybe (error "Invalid URI") . Uri.mkURI) <$> fromPersistValues vs
  dbType p x = dbType p ("" :: Text)

instance PrimitivePersistField URI where
  toPrimitivePersistValue x v = toPrimitivePersistValue x (Uri.render v)
  fromPrimitivePersistValue x v = fromMaybe (error "Invalid URI") $ Uri.mkURI $ fromPrimitivePersistValue x v

instance PersistField LedgerIdentifier where
  persistName _ = "LedgerIdentifier"
  toPersistValues = primToPersistValue
  fromPersistValues vs = first LedgerIdentifier <$> fromPersistValues vs
  dbType p x = dbType p ("" :: Text)

instance PrimitivePersistField LedgerIdentifier where
  toPrimitivePersistValue x v = toPrimitivePersistValue x (unLedgerIdentifier v)
  fromPrimitivePersistValue x v = LedgerIdentifier $ fromPrimitivePersistValue x v

instance PersistField DerivationPath where
  persistName _ = "DerivationPath"
  toPersistValues = primToPersistValue
  fromPersistValues vs = first DerivationPath <$> fromPersistValues vs
  dbType p x = dbType p ("" :: Text)

instance PrimitivePersistField DerivationPath where
  toPrimitivePersistValue x v = toPrimitivePersistValue x (unDerivationPath v)
  fromPrimitivePersistValue x v = DerivationPath $ fromPrimitivePersistValue x v


instance ToField LedgerIdentifier where
  toField = toField . unLedgerIdentifier
instance FromField LedgerIdentifier where
  fromField f = fmap LedgerIdentifier . fromField f

instance ToField DerivationPath where
  toField = toField . unDerivationPath
instance FromField DerivationPath where
  fromField f = fmap DerivationPath . fromField f

instance ToField SigningCurve where
  toField = toField . show
instance FromField SigningCurve where
  fromField f = maybe (fail "Invalid value for SigningCurve") pure . readMaybe <=< fromField f

instance ToField Ballot where
  toField = toField . show
instance FromField Ballot where
  fromField f = maybe (fail "Invalid value for Ballot") pure . readMaybe <=< fromField f

instance ToField TestChainStatus where
  toField = toField . show
instance FromField TestChainStatus where
  fromField f = maybe (fail "Invalid value for TestChainStatus") pure . readMaybe <=< fromField f

instance ToField PublicKeyHash where
  toField a = toField (toPublicKeyHashText a)
instance FromField PublicKeyHash where
  -- TODO: Write a real Conversion for this.
  fromField f b = either (error . show) id . tryFromBase58 publicKeyHashConstructorDecoders . T.encodeUtf8 <$> fromField f b

instance ToField RightKind where
  toField = toField . show
instance FromField RightKind where
  fromField f b = maybe (fail "Invalid value for RightKind") pure . readMaybe =<< fromField f b


instance ToField URI where
  toField = toField . Uri.render
instance FromField URI where
  fromField f b = fromMaybe (error "Invalid URI") . Uri.mkURI <$> fromField f b


instance ToField (WithInfinity UTCTime) where
  toField = \case
    UpperInfinity -> Plain "'infinity'::timestamp"
    Bounded x -> toField x
    LowerInfinity -> Plain "'-infinity'::timestamp"

instance Field1 (a :. b) (a' :. b) a a' where
  _1 a2fb (a :. b) = (:. b) <$> a2fb a

instance Field2 (a :. b) (a :. b') b b' where
  _2 a2fb (a :. b) = (a :.) <$> a2fb b

mkRhyolitePersist (Just "migrateSchema") [groundhog|
  - embedded: ProtoInfo
  - entity: ProtocolIndex
    autoKey: null
    keys:
      - name: ProtocolIndexKey
        default: true
    constructors:
      - name: ProtocolIndex
        uniques:
          - name: ProtocolIndexKey
            type: primary
            fields:
              - _protocolIndex_chainId
              - _protocolIndex_hash

  - primitive: VotingPeriodKind
  - primitive: Ballot
  - embedded: Ballots
  - entity: Amendment
    autoKey: null
    constructors:
      - name: Amendment
        uniques:
          - name: Amendment_period
            type: primary
            fields: [_amendment_period, _amendment_chainId, _amendment_votingPeriod]
  - embedded: PeriodVote
  - entity: PeriodProposal
    constructors:
      - name: PeriodProposal
        uniques:
          - name: PeriodProposal_hash
            type: constraint
            fields: [_periodProposal_hash, _periodProposal_chainId, _periodProposal_votingPeriod]
  - entity: BakerProposal
    autoKey: null
    constructors:
      - name: BakerProposal
        fields:
          - name: _bakerProposal_proposal
            reference:
              table: PeriodProposal
              onDelete: cascade
        uniques:
          - name: BakerProposal_key
            type: primary
            fields: [_bakerProposal_pkh, _bakerProposal_proposal]
  - entity: BakerVote
    autoKey: null
    constructors:
      - name: BakerVote
        fields:
          - name: _bakerVote_proposal
            reference:
              table: PeriodProposal
              onDelete: cascade
        uniques:
          - name: BakerVote_key
            type: primary
            fields: [_bakerVote_pkh, _bakerVote_proposal]
  - entity: PeriodTestingVote
    autoKey: null
    constructors:
      - name: PeriodTestingVote
        fields:
          - name: _periodTestingVote_proposal
            reference:
              table: PeriodProposal
              onDelete: cascade
  - primitive: TestChainStatus
  - entity: PeriodTesting
    autoKey: null
    constructors:
      - name: PeriodTesting
        fields:
          - name: _periodTesting_proposal
            reference:
              table: PeriodProposal
              onDelete: cascade
  - entity: PeriodPromotionVote
    autoKey: null
    constructors:
      - name: PeriodPromotionVote
        fields:
          - name: _periodPromotionVote_proposal
            reference:
              table: PeriodProposal
              onDelete: cascade
  - primitive: SigningCurve
  - entity: ConnectedLedger
    autoKey: null
    constructors:
      - name: ConnectedLedger
        fields:
          - name: _connectedLedger_forceConnectivityCheck
            type: Bool
            default: "False"
  - embedded: SecretKey
  - entity: LedgerAccount
    autoKey: null
  - entity: Accusation
    autoKey: null
    constructors:
      - name: Accusation
        uniques:
          - name: Accusation_hash
            type: primary
            fields: [_accusation_hash, _accusation_blockHash]
    keys:
      - name: Accusation_hash
        default: true
  - entity: BlockTodo
    autoKey: null
    constructors:
      - name: BlockTodo
        uniques:
          - name: BlockTodo_hash
            type: primary
            fields: [_blockTodo_hash]
    keys:
      - name: BlockTodo_hash
        default: true
  - embedded: DeletableRow
  - entity: BakerDaemon
    constructors:
      - name: BakerDaemon
  - entity: BakerDaemonInternal
    autoKey: null
    keys:
      - name: BakerDaemonInternalId
        default: true
    constructors:
      - name: BakerDaemonInternal
        uniques:
          - name: BakerDaemonInternalId
            type: primary
            fields: [_bakerDaemonInternal_id]
  - embedded: BakerDaemonInternalData
  - entity: Node
    constructors:
      - name: Node
  - entity: NodeExternal
    autoKey: null
    keys:
      - name: NodeExternalId
        default: true
    constructors:
      - name: NodeExternal
        uniques:
          - name: NodeExternalId
            type: primary
            fields: [_nodeExternal_id]
  - embedded: NodeExternalData
  - entity: NodeInternal
    autoKey: null
    keys:
      - name: NodeInternalId
        default: true
    constructors:
      - name: NodeInternal
        uniques:
          - name: NodeInternalId
            type: primary
            fields: [_nodeInternal_id]
  - entity: ProcessData
  - primitive: ProcessState
  - primitive: ProcessControl
  - entity: NodeDetails
    autoKey: null
    keys:
      - name: NodeDetailsId
        default: true
    constructors:
      - name: NodeDetails
        uniques:
          - name: NodeDetailsId
            type: primary
            fields: [_nodeDetails_id]
  - embedded: NodeDetailsData
  - entity: PublicNodeConfig
    constructors:
    - name: PublicNodeConfig
      uniques:
        - name: _publicnodeconfig_uniqueness
          type: constraint
          fields: [_publicNodeConfig_source]
  - entity: PublicNodeHead
    constructors:
    - name: PublicNodeHead
      uniques:
        - name: _publicnodehead_uniqueness
          type: constraint
          fields: [_publicNodeHead_source, _publicNodeHead_chain]
  - embedded: BakeEfficiency
  - embedded: NetworkStat
  - entity: Baker
    autoKey: null
    keys:
      - name: BakerKey
        default: true
    constructors:
      - name: Baker
        uniques:
          - name: BakerKey
            type: primary
            fields: [_baker_publicKeyHash]
  - embedded: BakerData
  - entity: BakerDetails
    autoKey: null
    keys:
     - name: BakerDetailsKey
       default: true
    constructors:
     - name: BakerDetails
       uniques:
        - name: BakerDetailsKey
          type: primary
          fields: [_bakerDetails_publicKeyHash]
  - entity: BakerRightsCycleProgress
    constructors:
      - name: BakerRightsCycleProgress
        uniques:
          - name: _bakerRightsCycleProgress_branch
            type: constraint
            fields: [_bakerRightsCycleProgress_chainId, _bakerRightsCycleProgress_publicKeyHash, _bakerRightsCycleProgress_branch]
  - entity: BakerRight
    constructors:
      - name: BakerRight
        uniques:
          - name: _bakerRights_right
            type: constraint
            fields: [_bakerRight_branch, _bakerRight_level, _bakerRight_right]
  - embedded: VeryBlockLike
  - entity: Notificatee
    constructors:
      - name: Notificatee
        uniques:
          - name: _notificatee_uniqueness
            type: constraint
            fields: [_notificatee_email]
  - primitive: SmtpProtocol
  - entity: MailServerConfig
    constructors:
      - name: MailServerConfig
        uniques:
          - name: _mailserverconfig_uniqueness
            type: constraint
            fields:
              - _mailServerConfig_hostName
              - _mailServerConfig_portNumber
              - _mailServerConfig_smtpProtocol
              - _mailServerConfig_userName
              - _mailServerConfig_password
        fields:
          - name: _mailServerConfig_enabled
            type: Bool
            default: "True"
  - primitive: RightKind
  - primitive: UpgradeCheckError
  - primitive: PublicNode
  - primitive: NamedChain
  - entity: ErrorLog

  - entity: ErrorLogBadNodeHead
    autoKey: null
    keys:
      - name: ErrorLogBadNodeHeadId
        default: true
    constructors:
      - name: ErrorLogBadNodeHead
        uniques:
          - name: ErrorLogBadNodeHeadId
            type: primary
            fields: [_errorLogBadNodeHead_log]
  - entity: ErrorLogBakerNoHeartbeat
    autoKey: null
    keys:
      - name: ErrorLogBakerNoHeartbeatId
        default: true
    constructors:
      - name: ErrorLogBakerNoHeartbeat
        uniques:
          - name: ErrorLogBakerNoHeartbeatId
            type: primary
            fields: [_errorLogBakerNoHeartbeat_log]
  - entity: ErrorLogBakerLedgerDisconnected
    autoKey: null
    keys:
      - name: ErrorLogBakerLedgerDisconnectedId
        default: true
    constructors:
      - name: ErrorLogBakerLedgerDisconnected
        uniques:
          - name: ErrorLogBakerLedgerDisconnectedId
            type: primary
            fields: [_errorLogBakerLedgerDisconnected_log]
  - entity: ErrorLogInaccessibleNode
    autoKey: null
    keys:
      - name: ErrorLogInaccessibleNodeId
        default: true
    constructors:
      - name: ErrorLogInaccessibleNode
        uniques:
          - name: ErrorLogInaccessibleNodeId
            type: primary
            fields: [_errorLogInaccessibleNode_log]
  - entity: ErrorLogBakerAccused
    autoKey: null
    keys:
      - name: ErrorLogBakerAccusedId
        default: true
    constructors:
      - name: ErrorLogBakerAccused
        uniques:
          - name: ErrorLogBakerAccusedId
            type: primary
            fields: [_errorLogBakerAccused_log]
  - entity: ErrorLogBakerDeactivated
    autoKey: null
    keys:
      - name: ErrorLogBakerDeactivatedId
        default: true
    constructors:
      - name: ErrorLogBakerDeactivated
        uniques:
          - name: ErrorLogBakerDeactivatedId
            type: primary
            fields: [_errorLogBakerDeactivated_log]
  - entity: ErrorLogBakerDeactivationRisk
    autoKey: null
    keys:
      - name: ErrorLogBakerDeactivationRiskId
        default: true
    constructors:
      - name: ErrorLogBakerDeactivationRisk
        uniques:
          - name: ErrorLogBakerDeactivationRiskId
            type: primary
            fields: [_errorLogBakerDeactivationRisk_log]
  - entity: ErrorLogInsufficientFunds
    autoKey: null
    keys:
      - name: ErrorLogInsufficientFundsId
        default: true
    constructors:
      - name: ErrorLogInsufficientFunds
        uniques:
          - name: ErrorLogInsufficientFundsId
            type: primary
            fields: [_errorLogInsufficientFunds_log]
  - entity: ErrorLogNodeVersionMismatch
    autoKey: null
    keys:
      - name: ErrorLogNodeVersionMismatchId
        default: true
    constructors:
      - name: ErrorLogNodeVersionMismatch
        uniques:
          - name: ErrorLogNodeVersionMismatchId
            type: primary
            fields: [_errorLogNodeVersionMismatch_log]
  - entity: ErrorLogNodeWrongChain
    autoKey: null
    keys:
      - name: ErrorLogNodeWrongChainId
        default: true
    constructors:
      - name: ErrorLogNodeWrongChain
        uniques:
          - name: ErrorLogNodeWrongChainId
            type: primary
            fields: [_errorLogNodeWrongChain_log]
  - entity: ErrorLogNodeInvalidPeerCount
    autoKey: null
    keys:
      - name: ErrorLogNodeInvalidPeerCountId
        default: true
    constructors:
      - name: ErrorLogNodeInvalidPeerCount
        uniques:
          - name: ErrorLogNodeInvalidPeerCountId
            type: primary
            fields: [_errorLogNodeInvalidPeerCount_log]
  - entity: ErrorLogNetworkUpdate
    autoKey: null
    keys:
      - name: ErrorLogNetworkUpdateId
        default: true
    constructors:
      - name: ErrorLogNetworkUpdate
        uniques:
          - name: ErrorLogNetworkUpdateId
            type: primary
            fields: [_errorLogNetworkUpdate_log]
  - entity: ErrorLogBakerMissed
    autoKey: null
    keys:
      - name: ErrorLogBakerMissedId
        default: true
    constructors:
      - name: ErrorLogBakerMissed
        uniques:
          - name: ErrorLogBakerMissedId
            type: primary
            fields: [_errorLogBakerMissed_log]
  - entity: ErrorLogVotingReminder
    autoKey: null
    keys:
      - name: ErrorLogVotingReminderId
        default: true
    constructors:
      - name: ErrorLogVotingReminder
        uniques:
          - name: ErrorLogVotingReminderId
            type: primary
            fields: [_errorLogVotingReminder_log]
  - primitive: InternalNodeFailureReason
  - entity: ErrorLogInternalNodeFailed
    autoKey: null
    keys:
      - name: ErrorLogInternalNodeFailedId
        default: true
    constructors:
      - name: ErrorLogInternalNodeFailed
        uniques:
          - name: ErrorLogInternalNodeFailedId
            type: primary
            fields: [_errorLogInternalNodeFailed_log]
  - entity: RawCacheEntry
    constructors:
     - name: RawCacheEntry
       uniques:
        - name: _rawCacheEntry_uniqueness
          type: constraint
          fields:
           - _rawCacheEntry_chainId
           - _rawCacheEntry_key
  - entity: TelegramConfig
    constructors:
    - name: TelegramConfig
      uniques:
      - name: _telegramConfig_uniqueness
        type: constraint
        fields: [_telegramConfig_botApiKey]
  - entity: TelegramMessageQueue
  - entity: TelegramRecipient
  - entity: SnapshotMeta
  - entity: UpstreamVersion
  - embedded: RightNotificationLimit
  - entity: RightNotificationSettings
    autoKey: null
    constructors:
    - name: RightNotificationSettings
      uniques:
      - name: RightNotificationSettingsId
        type: primary
        fields: [_rightNotificationSettings_rightKind]
  - entity: CacheBakingRights
    autoKey: null
    constructors:
      - name: CacheBakingRights
        uniques:
          - name: CacheBakingRights_context
            type: primary
            fields: [_cacheBakingRights_context, _cacheBakingRights_level]
  - entity: CacheEndorsingRights
    autoKey: null
    constructors:
      - name: CacheEndorsingRights
        uniques:
          - name: CacheEndorsingRights_context
            type: primary
            fields: [_cacheEndorsingRights_context, _cacheEndorsingRights_level]
|]

fmap concat $ traverse (uncurry makeDefaultKeyIdInt64)
  [ (''BakerDaemon, 'BakerDaemonKey)
  , (''BakerRightsCycleProgress, 'BakerRightsCycleProgressKey)
  , (''BakerRight, 'BakerRightKey)
  , (''ErrorLog, 'ErrorLogKey)
  , (''RawCacheEntry, 'RawCacheEntryKey)
  , (''MailServerConfig, 'MailServerConfigKey)
  , (''Node, 'NodeKey)
  , (''Notificatee, 'NotificateeKey)
  , (''PeriodProposal, 'PeriodProposalKey)
  , (''ProcessData, 'ProcessDataKey)
  , (''PublicNodeConfig, 'PublicNodeConfigKey)
  , (''PublicNodeHead, 'PublicNodeHeadKey)
  , (''SnapshotMeta, 'SnapshotMetaKey)
  , (''TelegramConfig, 'TelegramConfigKey)
  , (''TelegramRecipient, 'TelegramRecipientKey)
  , (''TelegramMessageQueue, 'TelegramMessageQueueKey)
  , (''UpstreamVersion, 'UpstreamVersionKey)
  ]

instance DefaultKeyId ProtocolIndex where
  toIdData _ (ProtocolIndexKeyKey chainId protoHash) = (chainId, protoHash)
  fromIdData _ (chainId, protoHash) = ProtocolIndexKeyKey chainId protoHash

instance DefaultKeyId Accusation where
  toIdData _ (Accusation_hashKey oh bh) = (oh, bh)
  fromIdData _ = uncurry Accusation_hashKey

instance DefaultKeyId Baker where
  toIdData _ (BakerKeyKey pkh) = pkh
  fromIdData _ = BakerKeyKey

instance DefaultKeyId BakerDetails where
  toIdData _ (BakerDetailsKeyKey pkh) = pkh
  fromIdData _ = BakerDetailsKeyKey

instance DefaultKeyId NodeExternal where
  toIdData _ (NodeExternalIdKey nid) = nid
  fromIdData _ = NodeExternalIdKey
instance DefaultKeyId NodeInternal where
  toIdData _ (NodeInternalIdKey nid) = nid
  fromIdData _ = NodeInternalIdKey

instance DefaultKeyId ErrorLogBakerMissed where
  toIdData _ (ErrorLogBakerMissedIdKey eid) = eid :: Id ErrorLog
  fromIdData _ = ErrorLogBakerMissedIdKey :: Id ErrorLog -> Key ErrorLogBakerMissed (Unique ErrorLogBakerMissedId)
instance DefaultKeyId ErrorLogBadNodeHead where
  toIdData _ (ErrorLogBadNodeHeadIdKey eid) = eid
  fromIdData _ = ErrorLogBadNodeHeadIdKey
instance DefaultKeyId ErrorLogBakerNoHeartbeat where
  toIdData _ (ErrorLogBakerNoHeartbeatIdKey eid) = eid
  fromIdData _ = ErrorLogBakerNoHeartbeatIdKey
instance DefaultKeyId ErrorLogBakerLedgerDisconnected where
  toIdData _ (ErrorLogBakerLedgerDisconnectedIdKey eid) = eid
  fromIdData _ = ErrorLogBakerLedgerDisconnectedIdKey
instance DefaultKeyId ErrorLogInaccessibleNode where
  toIdData _ (ErrorLogInaccessibleNodeIdKey eid) = eid
  fromIdData _ = ErrorLogInaccessibleNodeIdKey
instance DefaultKeyId ErrorLogBakerAccused where
  toIdData _ (ErrorLogBakerAccusedIdKey eid) = eid
  fromIdData _ = ErrorLogBakerAccusedIdKey
instance DefaultKeyId ErrorLogBakerDeactivated where
  toIdData _ (ErrorLogBakerDeactivatedIdKey eid) = eid
  fromIdData _ = ErrorLogBakerDeactivatedIdKey
instance DefaultKeyId ErrorLogBakerDeactivationRisk where
  toIdData _ (ErrorLogBakerDeactivationRiskIdKey eid) = eid
  fromIdData _ = ErrorLogBakerDeactivationRiskIdKey
instance DefaultKeyId ErrorLogInsufficientFunds where
  toIdData _ (ErrorLogInsufficientFundsIdKey eid) = eid
  fromIdData _ = ErrorLogInsufficientFundsIdKey
instance DefaultKeyId ErrorLogNodeVersionMismatch where
  toIdData _ (ErrorLogNodeVersionMismatchIdKey eid) = eid
  fromIdData _ = ErrorLogNodeVersionMismatchIdKey
instance DefaultKeyId ErrorLogNodeWrongChain where
  toIdData _ (ErrorLogNodeWrongChainIdKey eid) = eid
  fromIdData _ = ErrorLogNodeWrongChainIdKey
instance DefaultKeyId ErrorLogNodeInvalidPeerCount where
  toIdData _ (ErrorLogNodeInvalidPeerCountIdKey eid) = eid
  fromIdData _ = ErrorLogNodeInvalidPeerCountIdKey
instance DefaultKeyId ErrorLogNetworkUpdate where
  toIdData _ (ErrorLogNetworkUpdateIdKey eid) = eid
  fromIdData _ = ErrorLogNetworkUpdateIdKey
instance DefaultKeyId ErrorLogVotingReminder where
  toIdData _ (ErrorLogVotingReminderIdKey eid) = eid
  fromIdData _ = ErrorLogVotingReminderIdKey
instance DefaultKeyId ErrorLogInternalNodeFailed where
  toIdData _ (ErrorLogInternalNodeFailedIdKey eid) = eid
  fromIdData _ = ErrorLogInternalNodeFailedIdKey

fmap concat $ traverse (\n ->
  let u = mkName (nameBase n <> "Id") in
  [d| instance DefaultKeyIsUnique $(conT n) where
        type DefaultKeyUnique $(conT n) = $(conT u)
        defaultKeyToKey = id
      |])
  $
  [ ''NodeExternal
  , ''NodeInternal
  ] ++ errorLogNames

fmap concat $ traverse (\n ->
  let c = mkName (nameBase n <> "Constructor") in
  [d| instance HasSingleConstructor $(conT n) where
        type SingleConstructor $(conT n) = $(conT c)
        singleConstructor _ = $(conE c)
      |])
  $
  [ ''Baker
  , ''Node
  , ''NodeExternal
  , ''NodeInternal
  ] ++ errorLogNames

type LogTagConstraints e =
  ( Eq (IdData e)
  , Ord (IdData e)
  , Show (IdData e)
  , DefaultKey e ~ Key e (Unique (DefaultKeyUnique e))
  , DefaultKeyId e
  , HasSingleConstructor e
  , IdData e ~ Id ErrorLog
  , IsUniqueKey (Key e (Unique (DefaultKeyUnique e)))
  , PersistEntity e
  )

nodeLogAssume :: NodeLogTag e -> (LogTagConstraints e => x) -> x
nodeLogAssume = \case
  NodeLogTag_InaccessibleNode -> id
  NodeLogTag_NodeWrongChain -> id
  NodeLogTag_NodeInvalidPeerCount -> id
  NodeLogTag_BadNodeHead -> id
  NodeLogTag_VersionMismatch -> id

bakerLogAssume :: BakerLogTag e -> (LogTagConstraints e => x) -> x
bakerLogAssume = \case
  BakerLogTag_BakerLedgerDisconnected -> id
  BakerLogTag_BakerMissed -> id
  BakerLogTag_BakerDeactivated -> id
  BakerLogTag_BakerDeactivationRisk -> id
  BakerLogTag_BakerAccused -> id
  BakerLogTag_InsufficientFunds -> id
  BakerLogTag_VotingReminder -> id

logAssume :: LogTag e -> (LogTagConstraints e => x) -> x
logAssume = \case
  LogTag_NetworkUpdate -> id
  LogTag_Node nTag -> nodeLogAssume nTag
  LogTag_Baker bTag -> bakerLogAssume bTag
  LogTag_InternalNodeFailed -> id
  LogTag_BakerNoHeartbeat -> id

data Related b c r where
  Related :: (HasSingleConstructor r, PersistEntity r, PersistField x) => Field b c x -> ForeignKey r x -> Related b c r

data ForeignKey r x where
  ForeignKey_AutoId :: forall r. EntityWithId r => ForeignKey r (Id r)
  ForeignKey_UniqueId :: forall r u. EntityWithIdBy u r => ForeignKey r (Id r)
  ForeignKey_UniqueIdData :: forall r u. EntityWithIdBy u r => ForeignKey r (IdData r)
  ForeignKey_Field :: forall r x. Field r (SingleConstructor r) x -> ForeignKey r x

logDep :: LogTag e -> [Some (Related e (SingleConstructor e))]
logDep = \case
  LogTag_NetworkUpdate -> []
  LogTag_Node nTag -> bothNodes $ nodeLogDep nTag
  LogTag_Baker bTag -> pure $ Some $ bakerLogDep bTag
  LogTag_InternalNodeFailed -> [Some (Related ErrorLogInternalNodeFailed_nodeField ForeignKey_UniqueId) ]
  LogTag_BakerNoHeartbeat -> []
  where
    bothNodes :: forall e. Related e (SingleConstructor e) Node -> [Some (Related e (SingleConstructor e))]
    bothNodes = \case
      Related fld fk -> case fk of
        ForeignKey_AutoId -> [Some (Related fld $ ForeignKey_UniqueIdData @NodeExternal), Some (Related fld $ ForeignKey_UniqueIdData @NodeInternal)]
        ForeignKey_Field fld2 -> case fld2 of {}

nodeLogDep :: NodeLogTag e -> Related e (SingleConstructor e) Node
nodeLogDep = \case
  NodeLogTag_InaccessibleNode -> depNodeAlert ErrorLogInaccessibleNode_nodeField
  NodeLogTag_NodeWrongChain -> depNodeAlert ErrorLogNodeWrongChain_nodeField
  NodeLogTag_NodeInvalidPeerCount -> depNodeAlert ErrorLogNodeInvalidPeerCount_nodeField
  NodeLogTag_BadNodeHead -> depNodeAlert ErrorLogBadNodeHead_nodeField
  NodeLogTag_VersionMismatch -> depNodeAlert ErrorLogNodeVersionMismatch_nodeField
  where
    depNodeAlert f = Related f ForeignKey_AutoId

bakerLogDep :: BakerLogTag e -> Related e (SingleConstructor e) Baker
bakerLogDep = \case
  BakerLogTag_BakerLedgerDisconnected -> depBakerAlert' ErrorLogBakerLedgerDisconnected_bakerField
  BakerLogTag_BakerMissed -> depBakerAlert' ErrorLogBakerMissed_bakerField
  BakerLogTag_BakerDeactivated -> depBakerAlert ErrorLogBakerDeactivated_publicKeyHashField
  BakerLogTag_BakerDeactivationRisk -> depBakerAlert ErrorLogBakerDeactivationRisk_publicKeyHashField
  BakerLogTag_BakerAccused -> depBakerAlert' ErrorLogBakerAccused_bakerField
  BakerLogTag_InsufficientFunds -> depBakerAlert' ErrorLogInsufficientFunds_bakerField
  BakerLogTag_VotingReminder -> depBakerAlert' ErrorLogVotingReminder_bakerField
  where
    depBakerAlert' f = Related f ForeignKey_UniqueId
    depBakerAlert f = Related f $ ForeignKey_Field Baker_publicKeyHashField

embeddedSecretKeyEquals
  :: (ProjectionDb r db, ProjectionRestriction r (RestrictionHolder v c), FieldLike r SecretKey, EntityConstr v c, DbDescriptor db)
  => r -> SecretKey -> Cond db (RestrictionHolder v c)
embeddedSecretKeyEquals f sk =
         f ~> SecretKey_ledgerIdentifierSelector ==. _secretKey_ledgerIdentifier sk
  GH.&&. f ~> SecretKey_signingCurveSelector ==. _secretKey_signingCurve sk
  GH.&&. f ~> SecretKey_derivationPathSelector ==. _secretKey_derivationPath sk

instance ArgDict c NotifyTag where
  type ConstraintsFor NotifyTag c =
    ( c (Id Baker, Maybe BakerData)
    , c BakerDetails
    , c (Id BakerRightsCycleProgress, BakerRightsCycleProgress, [BakerRight])
    , c (Id ErrorLogBadNodeHead)
    , c (Id ErrorLogBakerAccused)
    , c (Id ErrorLogBakerDeactivated)
    , c (Id ErrorLogBakerDeactivationRisk)
    , c (Id ErrorLogBakerLedgerDisconnected)
    , c (Id ErrorLogBakerMissed)
    , c (Id ErrorLogBakerNoHeartbeat)
    , c (Id ErrorLogInaccessibleNode)
    , c (Id ErrorLogInsufficientFunds)
    , c (Id ErrorLogInternalNodeFailed)
    , c (Id ErrorLogNetworkUpdate)
    , c (Id ErrorLogNodeInvalidPeerCount)
    , c (Id ErrorLogNodeVersionMismatch)
    , c (Id ErrorLogNodeWrongChain)
    , c (Id ErrorLogVotingReminder)
    , c (Id UpstreamVersion, UpstreamVersion)
    , c (Id MailServerConfig, MailServerConfig)
    , c (Id ProtocolIndex)
    , c (Id Node, Maybe NodeExternalData)
    , c (Id Node, Maybe ProcessData)
    , c (Id Node, Maybe NodeDetailsData)
    , c (Id Notificatee)
    , c (Id PublicNodeConfig, PublicNodeConfig)
    , c (Id PublicNodeHead, Maybe PublicNodeHead)
    , c SnapshotMeta
    , c (Id TelegramConfig, TelegramConfig)
    , c (Id TelegramRecipient, Maybe TelegramRecipient)
    , c (Maybe ConnectedLedger)
    , c (SecretKey, Maybe (PublicKeyHash, Tez))
    , c (SecretKey, Maybe (PublicKeyHash, Tez))
    , c (SecretKey, Maybe SetupState)
    , c (SecretKey, Maybe VoteState)
    , c (RightKind, Maybe RightNotificationLimit)
    , c (VotingPeriodKind, Maybe Amendment)
    , c (Id PeriodProposal, Maybe (PeriodProposal, Maybe Bool))
    , c (Maybe PeriodTestingVote)
    , c (Maybe PeriodTesting)
    , c (Maybe PeriodPromotionVote)
    , c (Maybe BakerVote)
    , c (PublicKeyHash, Bool)
    )
  argDict = \case
    NotifyTag_Baker -> Dict
    NotifyTag_BakerDetails -> Dict
    NotifyTag_BakerRightsProgress -> Dict
    NotifyTag_ErrorLog t' -> case t' of
      LogTag_NetworkUpdate -> Dict
      LogTag_BakerNoHeartbeat -> Dict
      LogTag_InternalNodeFailed -> Dict
      LogTag_Node t -> case t of
        NodeLogTag_InaccessibleNode -> Dict
        NodeLogTag_NodeWrongChain -> Dict
        NodeLogTag_NodeInvalidPeerCount -> Dict
        NodeLogTag_BadNodeHead -> Dict
        NodeLogTag_VersionMismatch -> Dict
      LogTag_Baker t -> case t of
        BakerLogTag_BakerLedgerDisconnected -> Dict
        BakerLogTag_BakerMissed -> Dict
        BakerLogTag_BakerDeactivated -> Dict
        BakerLogTag_BakerDeactivationRisk -> Dict
        BakerLogTag_BakerAccused -> Dict
        BakerLogTag_InsufficientFunds -> Dict
        BakerLogTag_VotingReminder -> Dict
    NotifyTag_ProtocolIndex -> Dict
    NotifyTag_UpstreamVersion -> Dict
    NotifyTag_MailServerConfig -> Dict
    NotifyTag_NodeExternal -> Dict
    NotifyTag_NodeInternal -> Dict
    NotifyTag_NodeDetails -> Dict
    NotifyTag_Notificatee -> Dict
    NotifyTag_PublicNodeConfig -> Dict
    NotifyTag_PublicNodeHead -> Dict
    NotifyTag_SnapshotMeta -> Dict
    NotifyTag_TelegramConfig -> Dict
    NotifyTag_TelegramRecipient -> Dict
    NotifyTag_ConnectedLedger -> Dict
    NotifyTag_ShowLedger -> Dict
    NotifyTag_Prompting -> Dict
    NotifyTag_VotePrompting -> Dict
    NotifyTag_RightNotificationSettings -> Dict
    NotifyTag_Amendment -> Dict
    NotifyTag_Proposals -> Dict
    NotifyTag_PeriodTestingVote -> Dict
    NotifyTag_PeriodTesting -> Dict
    NotifyTag_PeriodPromotionVote -> Dict
    NotifyTag_BakerVote -> Dict
    NotifyTag_BakerRegistered -> Dict

fmap concat $ for [''NotifyTag] $ \t -> concat <$> sequence
  [ deriveJSONGADT t
--  , deriveArgDict t -- weird bug
  , deriveGEq t
  , deriveGCompare t
  , deriveGShow t
  ]
