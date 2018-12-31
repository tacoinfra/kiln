{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

{-# OPTIONS_GHC -Wall -Werror #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Backend.Schema
  ( module Backend.Schema

  -- Re-exports
  , toId
  ) where

import Control.Lens (Field1, Field2)
import Data.Time (UTCTime)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short (fromShort, toShort)
import Data.Fixed (Fixed (MkFixed), HasResolution, Micro)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import Data.Version (Version)
import qualified Data.Version as Version
import Data.Word (Word64)
import Database.Groundhog.Core
import qualified Database.Groundhog.Expression as GH
import Database.Groundhog.Generic
import Database.Groundhog.Instances ()
import Database.Groundhog.Postgresql (AutoKeyField (..), PersistBackend, executeRaw, get, update, (==.))
import qualified Database.Groundhog.Postgresql.Array as Groundhog
import Database.Groundhog.TH (groundhog)
import Database.PostgreSQL.Simple (Binary (..), Only (..), fromBinary, (:.)(..) )
import Database.PostgreSQL.Simple.FromField hiding (Binary)
import Database.PostgreSQL.Simple.ToField (ToField (toField), Action(Plain))
import Database.PostgreSQL.Simple.Types (PGArray (..))
import qualified Formatting as Fmt
import Rhyolite.Backend.Account ()
import Rhyolite.Backend.Listen (NotificationType (..), NotifyMessage (..), getSchemaName, notifyChannel)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Backend.Schema.Class (DefaultKeyId, toIdData, fromIdData)
import Rhyolite.Backend.Schema.TH (makeDefaultKeyIdInt64, mkRhyolitePersist)
import Rhyolite.Schema (Id, Json (..), SchemaName (..))
import Text.Read (readMaybe)
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.Base58Check (HashedValue (..), tryFromBase58)
import Tezos.NodeRPC.Sources (PublicNode (..))
import Tezos.NodeRPC.Types
import Tezos.Types

import Backend.Version (parseVersion)
import Common.AppendIntervalMap (WithInfinity(..))
import Common.Schema
import ExtraPrelude

stripOnly :: (Coercible (f (Only a)) (f a)) => f (Only a) -> f a
stripOnly = coerce

data Notify
  = Notify_Client !(Id Client)
  | Notify_Baker !Baker
  | Notify_BakerDetails !BakerDetails
  | Notify_BakerRightsProgress !(Id BakerRightsCycleProgress) !BakerRightsCycleProgress ![BakerRight]
  | Notify_ErrorLogBadNodeHead !(Id ErrorLogBadNodeHead)
  | Notify_ErrorLogBakerNoHeartbeat !(Id ErrorLogBakerNoHeartbeat)
  | Notify_ErrorLogInaccessibleNode !(Id ErrorLogInaccessibleNode)
  | Notify_ErrorLogMultipleBakersForSameBaker !(Id ErrorLogMultipleBakersForSameBaker)
  | Notify_ErrorLogNodeWrongChain !(Id ErrorLogNodeWrongChain)
  | Notify_ErrorLogBakerMissed !(Id ErrorLogBakerMissed)
  | Notify_ErrorLogBakerDeactivated !(Id ErrorLogBakerDeactivated)
  | Notify_ErrorLogBakerDeactivationRisk !(Id ErrorLogBakerDeactivationRisk)
  | Notify_UpstreamVersion !(Id UpstreamVersion) !UpstreamVersion
  | Notify_MailServerConfig !(Id MailServerConfig) !MailServerConfig
  | Notify_NodeExternal !(Id Node) !(Maybe NodeExternalData)
  | Notify_NodeDetails !(Id Node) !(Maybe NodeDetailsData)
  | Notify_Notificatee !(Id Notificatee)
  | Notify_Parameters !(Id Parameters) Parameters
  | Notify_PublicNodeConfig !(Id PublicNodeConfig) PublicNodeConfig
  | Notify_PublicNodeHead !(Id PublicNodeHead) !(Maybe PublicNodeHead)
  | Notify_TelegramConfig !(Id TelegramConfig) !TelegramConfig
  | Notify_TelegramRecipient !(Id TelegramRecipient) (Maybe TelegramRecipient)
  deriving (Eq, Ord, Typeable, Generic, Show)
instance ToJSON Notify
instance FromJSON Notify

class HasDefaultNotify f where
  mkDefaultNotify :: f -> Notify

instance HasDefaultNotify (Id Client) where
  mkDefaultNotify = Notify_Client
instance HasDefaultNotify Baker where
  mkDefaultNotify = Notify_Baker
instance HasDefaultNotify (Id ErrorLogBadNodeHead) where
  mkDefaultNotify = Notify_ErrorLogBadNodeHead
instance HasDefaultNotify (Id ErrorLogBakerNoHeartbeat) where
  mkDefaultNotify = Notify_ErrorLogBakerNoHeartbeat
instance HasDefaultNotify (Id ErrorLogInaccessibleNode) where
  mkDefaultNotify = Notify_ErrorLogInaccessibleNode
instance HasDefaultNotify (Id ErrorLogMultipleBakersForSameBaker) where
  mkDefaultNotify = Notify_ErrorLogMultipleBakersForSameBaker
instance HasDefaultNotify (Id ErrorLogNodeWrongChain) where
  mkDefaultNotify = Notify_ErrorLogNodeWrongChain
instance HasDefaultNotify (Id ErrorLogBakerDeactivated) where
  mkDefaultNotify = Notify_ErrorLogBakerDeactivated
instance HasDefaultNotify (Id ErrorLogBakerDeactivationRisk) where
  mkDefaultNotify = Notify_ErrorLogBakerDeactivationRisk
instance HasDefaultNotify (Id Notificatee) where
  mkDefaultNotify = Notify_Notificatee
instance HasDefaultNotify (Id ErrorLogBakerMissed) where
  mkDefaultNotify = Notify_ErrorLogBakerMissed
instance HasDefaultNotify BakerDetails where
  mkDefaultNotify = Notify_BakerDetails

class HasDefaultNotifyUnique f where
  mkDefaultNotifyUnique :: Id f -> f -> Notify

instance HasDefaultNotifyUnique MailServerConfig where
  mkDefaultNotifyUnique = Notify_MailServerConfig
instance HasDefaultNotifyUnique TelegramConfig where
  mkDefaultNotifyUnique = Notify_TelegramConfig

notify :: (PersistBackend m) => Notify -> m ()
notify n = do
  schemaName <- getSchemaName
  let
    cmd = "NOTIFY " <> notifyChannel <> ", ?"
    notification = NotifyMessage { _notifyMessage_schemaName = SchemaName . T.pack $ schemaName
                                 , _notifyMessage_notificationType = NotificationType_Update
                                 , _notifyMessage_entityName = ""
                                 , _notifyMessage_value = toJSON n
                                 }
  void $ executeRaw False cmd [PersistString $ T.unpack $ T.decodeUtf8 $ LBS.toStrict $ Aeson.encode notification]


type EntityWithId a = (DefaultKeyId a, DefaultKey a ~ Key a BackendSpecific, PersistEntity a, PrimitivePersistField (Key a BackendSpecific))

getId :: (PersistBackend m, EntityWithId a) => Id a -> m (Maybe a)
getId = get . fromId

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
  notify $ mkDefaultNotify tid

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
  notify $ mkDefaultNotifyUnique tid newRow

insertNotify :: (HasDefaultNotify (Id a), EntityWithId a, AutoKey a ~ Key a BackendSpecific, PersistBackend m) => a -> m (Id a)
insertNotify a = do
  primaryKey <- insert' a
  notify $ mkDefaultNotify primaryKey
  pure primaryKey

insertNotifyUnique :: (HasDefaultNotifyUnique a, EntityWithId a, AutoKey a ~ Key a BackendSpecific, PersistBackend m) => a -> m (Id a)
insertNotifyUnique a = do
  primaryKey <- insert' a
  notify $ mkDefaultNotifyUnique primaryKey a
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

instance FromField Word64 where
  fromField f b = fromInteger <$> fromField f b -- is this sign-correct?

-- TODO: Move all of this into postgresql-simple
instance ToField (Fixed a) where
  toField (MkFixed x) = toField x

instance HasResolution a => PrimitivePersistField (Fixed a) where
  toPrimitivePersistValue p (MkFixed x) = toPrimitivePersistValue p (fromInteger x :: Int64)
  fromPrimitivePersistValue p x = MkFixed (toInteger (fromPrimitivePersistValue p x :: Int64))

instance HasResolution a => PersistField (Fixed a) where
  persistName _ = "Fixed"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbInt64 False Nothing Nothing

instance PrimitivePersistField Tez where
  toPrimitivePersistValue p (Tez x) = toPrimitivePersistValue p x
  fromPrimitivePersistValue p v = Tez $ fromPrimitivePersistValue p v

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

instance FromField Micro where
  fromField f b = MkFixed . toInteger @Int64 <$> fromField f b

instance NeverNull (HashedValue a)
instance NeverNull (Json BakedEvent)
-- instance NeverNull (Json BlockInfo)
instance NeverNull (Json CacheDelegateInfo)
instance NeverNull Cycle
instance NeverNull Fitness
instance NeverNull NetworkStat
instance NeverNull PublicKeyHash
instance NeverNull RawLevel
instance NeverNull Tez
instance NeverNull TezosWord64
instance NeverNull Version
instance NeverNull VeryBlockLike
instance NeverNull (Json VeryBlockLike)

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


instance ToField PublicKeyHash where
  toField a = toField (toPublicKeyHashText a)
instance FromField PublicKeyHash where
  -- TODO: Write a real Conversion for this.
  fromField f b = either (error . show) id . tryFromBase58 publicKeyHashConstructorDecoders . T.encodeUtf8 <$> fromField f b

instance ToField ClientWorker where
  toField = toField . show
instance FromField ClientWorker where
  fromField f b = maybe (fail "Invalid value for ClientWorker") pure . readMaybe =<< fromField f b

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
  - entity: Client
    constructors:
      - name: Client
        uniques:
          - name: _client_uniqueness
            type: constraint
            fields: [_client_address]
  - entity: ClientInfo
    constructors:
      - name: ClientInfo
        uniques:
          - name: _clientInfo_uniqueness
            type: constraint
            fields: [_clientInfo_client]
        fields:
          - name: _clientInfo_client
            reference:
              table: Client
              onDelete: cascade
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
  - entity: Parameters
    constructors:
      - name: Parameters
        uniques:
          - name: _parameters_uniqueness
            type: constraint
            fields: [_parameters_chain]
  - embedded: ProtoInfo
  - entity: PendingReward
    constructors:
      - name: PendingReward
        uniques:
          - name: _pendingReward_uniqueness
            type: constraint
            fields: [_pendingReward_baker, _pendingReward_hash]
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
  - primitive: ClientWorker
  - primitive: UpgradeCheckError
  - primitive: PublicNode
  - entity: ErrorLog
  - entity: ErrorLogBadNodeHead
  - entity: ErrorLogBakerNoHeartbeat
  - entity: ErrorLogInaccessibleNode
  - entity: ErrorLogMultipleBakersForSameBaker
  - entity: ErrorLogBakerDeactivated
  - entity: ErrorLogBakerDeactivationRisk
  - entity: ErrorLogNodeWrongChain
  - entity: ErrorLogBakerMissed
  - entity: CachedProtocolConstants
    constructors:
     - name: CachedProtocolConstants
       uniques:
        - name: _cachedprotocolconstants_uniqueness
          type: constraint
          fields:
           - _cachedProtocolConstants_protocol
  - entity: GenericCacheEntry
    constructors:
     - name: GenericCacheEntry
       uniques:
        - name: _genericCacheEntry_uniqueness
          type: constraint
          fields:
           - _genericCacheEntry_chainId
           - _genericCacheEntry_key
  - entity: TelegramConfig
    constructors:
    - name: TelegramConfig
      uniques:
      - name: _telegramConfig_uniqueness
        type: constraint
        fields: [_telegramConfig_botApiKey]
  - entity: TelegramMessageQueue
  - entity: TelegramRecipient
  - entity: UpstreamVersion
|]

fmap concat $ traverse (uncurry makeDefaultKeyIdInt64)
  [ (''CachedProtocolConstants, 'CachedProtocolConstantsKey)
  , (''Client, 'ClientKey)
  , (''ClientInfo, 'ClientInfoKey)
  , (''BakerRightsCycleProgress, 'BakerRightsCycleProgressKey)
  , (''BakerRight, 'BakerRightKey)
  , (''ErrorLog, 'ErrorLogKey)
  , (''ErrorLogBakerMissed, 'ErrorLogBakerMissedKey)
  , (''ErrorLogBadNodeHead, 'ErrorLogBadNodeHeadKey)
  , (''ErrorLogBakerNoHeartbeat, 'ErrorLogBakerNoHeartbeatKey)
  , (''ErrorLogInaccessibleNode, 'ErrorLogInaccessibleNodeKey)
  , (''ErrorLogMultipleBakersForSameBaker, 'ErrorLogMultipleBakersForSameBakerKey)
  , (''ErrorLogBakerDeactivated, 'ErrorLogBakerDeactivatedKey)
  , (''ErrorLogBakerDeactivationRisk, 'ErrorLogBakerDeactivationRiskKey)
  , (''ErrorLogNodeWrongChain, 'ErrorLogNodeWrongChainKey)
  , (''GenericCacheEntry, 'GenericCacheEntryKey)
  , (''MailServerConfig, 'MailServerConfigKey)
  , (''Node, 'NodeKey)
  , (''Notificatee, 'NotificateeKey)
  , (''Parameters, 'ParametersKey)
  , (''PendingReward, 'PendingRewardKey)
  , (''PublicNodeConfig, 'PublicNodeConfigKey)
  , (''PublicNodeHead, 'PublicNodeHeadKey)
  , (''TelegramConfig, 'TelegramConfigKey)
  , (''TelegramRecipient, 'TelegramRecipientKey)
  , (''TelegramMessageQueue, 'TelegramMessageQueueKey)
  , (''UpstreamVersion, 'UpstreamVersionKey)
  ]

instance DefaultKeyId Baker where
  toIdData _ (BakerKeyKey pkh) = pkh
  fromIdData _ = BakerKeyKey

instance DefaultKeyId BakerDetails where
  toIdData _ (BakerDetailsKeyKey pkh) = pkh
  fromIdData _ = BakerDetailsKeyKey

