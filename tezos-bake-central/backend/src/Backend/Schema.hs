{-# LANGUAGE ConstraintKinds #-}
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
  ) where

import Control.Lens (Field1, Field2)
import Data.Time (UTCTime)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short (fromShort, toShort)
import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Sum (EqTag)
import Data.Dependent.Sum (OrdTag)
import Data.Dependent.Sum (ShowTag)
import Data.Dependent.Sum (compareTagged)
import Data.Dependent.Sum (eqTagged)
import Data.Dependent.Sum (showTaggedPrec)
import Data.Fixed (Fixed (MkFixed), HasResolution, Micro)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import Data.Some (Some(..))
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
import Database.PostgreSQL.Simple.FromField hiding (Binary, Field)
import Database.PostgreSQL.Simple.ToField (ToField (toField), Action(Plain))
import Database.PostgreSQL.Simple.Types (PGArray (..))
import qualified Formatting as Fmt
import Language.Haskell.TH (conE)
import Language.Haskell.TH (conT)
import Language.Haskell.TH (mkName)
import Language.Haskell.TH (nameBase)
import Rhyolite.Backend.Account ()
import Rhyolite.Backend.Listen (NotificationType (..), NotifyMessage (..), getSchemaName, notifyChannel)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Backend.Schema.Class (DefaultKeyId, toIdData, fromIdData)
import Rhyolite.Backend.Schema.Class (DefaultKeyIsUnique)
import Rhyolite.Backend.Schema.Class (DefaultKeyUnique)
import Rhyolite.Backend.Schema.Class (defaultKeyToKey)
import Rhyolite.Backend.Schema.Class (HasSingleConstructor)
import Rhyolite.Backend.Schema.Class (SingleConstructor)
import Rhyolite.Backend.Schema.Class (singleConstructor)
import Rhyolite.Backend.Schema.TH (makeDefaultKeyIdInt64, mkRhyolitePersist)
import Rhyolite.Schema (Id, Json (..), SchemaName (..))
import Rhyolite.Schema (IdData)
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

stripOnly :: Coercible (f (Only a)) (f a) => f (Only a) -> f a
stripOnly = coerce

data Notify
  = Notify_Client !(Id Client)
  | Notify_Baker !(Id Baker) !(Maybe BakerData)
  | Notify_BakerDetails !BakerDetails
  | Notify_BakerRightsProgress !(Id BakerRightsCycleProgress) !BakerRightsCycleProgress ![BakerRight]
  | Notify_ErrorLog !(DSum LogTag Id)
  | Notify_UpstreamVersion !(Id UpstreamVersion) !UpstreamVersion
  | Notify_MailServerConfig !(Id MailServerConfig) !MailServerConfig
  | Notify_NodeExternal !(Id Node) !(Maybe NodeExternalData)
  | Notify_NodeInternal !(Id Node) !(Maybe NodeInternalData)
  | Notify_NodeDetails !(Id Node) !(Maybe NodeDetailsData)
  | Notify_Notificatee !(Id Notificatee)
  | Notify_Parameters !(Id Parameters) Parameters
  | Notify_PublicNodeConfig !(Id PublicNodeConfig) PublicNodeConfig
  | Notify_PublicNodeHead !(Id PublicNodeHead) !(Maybe PublicNodeHead)
  | Notify_TelegramConfig !(Id TelegramConfig) !TelegramConfig
  | Notify_TelegramRecipient !(Id TelegramRecipient) (Maybe TelegramRecipient)
  deriving (Typeable, Generic)
deriving instance EqTag NodeLogTag Id => Eq Notify
deriving instance OrdTag NodeLogTag Id => Ord Notify
deriving instance ShowTag NodeLogTag Id => Show Notify
instance ToJSON Notify
instance FromJSON Notify

class HasDefaultNotify f where
  mkDefaultNotify :: f -> Notify

instance HasDefaultNotify (DSum LogTag Id) where
  mkDefaultNotify = Notify_ErrorLog
instance HasDefaultNotify (DSum NodeLogTag Id) where
  mkDefaultNotify (t :=> v) = mkDefaultNotify $ LogTag_Node t :=> v
instance HasDefaultNotify (DSum BakerLogTag Id) where
  mkDefaultNotify (t :=> v) = mkDefaultNotify $ LogTag_Baker t :=> v

instance HasDefaultNotify (Id Client) where
  mkDefaultNotify = Notify_Client
instance HasDefaultNotify (Id ErrorLogBadNodeHead) where
  mkDefaultNotify = mkDefaultNotify . (NodeLogTag_BadNodeHead :=>)
instance HasDefaultNotify (Id ErrorLogBakerNoHeartbeat) where
  mkDefaultNotify = mkDefaultNotify . (LogTag_BakerNoHeartbeat :=>)
instance HasDefaultNotify (Id ErrorLogInaccessibleNode) where
  mkDefaultNotify = mkDefaultNotify . (NodeLogTag_InaccessibleNode :=>)
instance HasDefaultNotify (Id ErrorLogMultipleBakersForSameBaker) where
  mkDefaultNotify = mkDefaultNotify . (BakerLogTag_MultipleBakersForSameBaker :=>)
instance HasDefaultNotify (Id ErrorLogNodeWrongChain) where
  mkDefaultNotify = mkDefaultNotify . (NodeLogTag_NodeWrongChain :=>)
instance HasDefaultNotify (Id ErrorLogNodeInvalidPeerCount) where
  mkDefaultNotify = mkDefaultNotify . (NodeLogTag_NodeInvalidPeerCount :=>)
instance HasDefaultNotify (Id ErrorLogNetworkUpdate) where
  mkDefaultNotify = mkDefaultNotify . (LogTag_NetworkUpdate :=>)
instance HasDefaultNotify (Id ErrorLogBakerDeactivated) where
  mkDefaultNotify = mkDefaultNotify . (BakerLogTag_BakerDeactivated :=>)
instance HasDefaultNotify (Id ErrorLogBakerDeactivationRisk) where
  mkDefaultNotify = mkDefaultNotify . (BakerLogTag_BakerDeactivationRisk :=>)
instance HasDefaultNotify (Id Notificatee) where
  mkDefaultNotify = Notify_Notificatee
instance HasDefaultNotify (Id ErrorLogBakerMissed) where
  mkDefaultNotify = mkDefaultNotify . (BakerLogTag_BakerMissed :=>)
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

type EntityWithIdBy u a = (DefaultKeyId a, DefaultKey a ~ Key a (Unique u), PersistEntity a, IsUniqueKey (Key a (Unique u)))
getIdBy :: (PersistBackend m, EntityWithIdBy u a) => Id a -> m (Maybe a)
getIdBy = getBy . fromId

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

instance ToField NamedChain where
  toField v = toField (show v)

instance FromField NamedChain where
  fromField f b = read <$> fromField f b

instance FromField NodeInternalState where
  fromField f b = read <$> fromField f b

instance ToField NodeInternalState where
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
  - embedded: DeletableRow
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
  - primitive: NodeInternalState
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
  - embedded: NodeInternalData
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
  - primitive: ClientWorker
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
  - entity: ErrorLogMultipleBakersForSameBaker
    autoKey: null
    keys:
      - name: ErrorLogMultipleBakersForSameBakerId
        default: true
    constructors:
      - name: ErrorLogMultipleBakersForSameBaker
        uniques:
          - name: ErrorLogMultipleBakersForSameBakerId
            type: primary
            fields: [_errorLogMultipleBakersForSameBaker_log]
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
  , (''GenericCacheEntry, 'GenericCacheEntryKey)
  , (''MailServerConfig, 'MailServerConfigKey)
  , (''Node, 'NodeKey)
  , (''Notificatee, 'NotificateeKey)
  , (''Parameters, 'ParametersKey)
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

instance DefaultKeyId NodeExternal where
  toIdData _ (NodeExternalIdKey nid) = nid
  fromIdData _ = NodeExternalIdKey
instance DefaultKeyId NodeInternal where
  toIdData _ (NodeInternalIdKey nid) = nid
  fromIdData _ = NodeInternalIdKey

instance DefaultKeyId ErrorLogBakerMissed where
  toIdData _ (ErrorLogBakerMissedIdKey eid) = (eid :: Id ErrorLog)
  fromIdData _ = ErrorLogBakerMissedIdKey :: Id ErrorLog -> Key ErrorLogBakerMissed (Unique ErrorLogBakerMissedId)
instance DefaultKeyId ErrorLogBadNodeHead where
  toIdData _ (ErrorLogBadNodeHeadIdKey eid) = eid
  fromIdData _ = ErrorLogBadNodeHeadIdKey
instance DefaultKeyId ErrorLogBakerNoHeartbeat where
  toIdData _ (ErrorLogBakerNoHeartbeatIdKey eid) = eid
  fromIdData _ = ErrorLogBakerNoHeartbeatIdKey
instance DefaultKeyId ErrorLogInaccessibleNode where
  toIdData _ (ErrorLogInaccessibleNodeIdKey eid) = eid
  fromIdData _ = ErrorLogInaccessibleNodeIdKey
instance DefaultKeyId ErrorLogMultipleBakersForSameBaker where
  toIdData _ (ErrorLogMultipleBakersForSameBakerIdKey eid) = eid
  fromIdData _ = ErrorLogMultipleBakersForSameBakerIdKey
instance DefaultKeyId ErrorLogBakerDeactivated where
  toIdData _ (ErrorLogBakerDeactivatedIdKey eid) = eid
  fromIdData _ = ErrorLogBakerDeactivatedIdKey
instance DefaultKeyId ErrorLogBakerDeactivationRisk where
  toIdData _ (ErrorLogBakerDeactivationRiskIdKey eid) = eid
  fromIdData _ = ErrorLogBakerDeactivationRiskIdKey
instance DefaultKeyId ErrorLogNodeWrongChain where
  toIdData _ (ErrorLogNodeWrongChainIdKey eid) = eid
  fromIdData _ = ErrorLogNodeWrongChainIdKey
instance DefaultKeyId ErrorLogNodeInvalidPeerCount where
  toIdData _ (ErrorLogNodeInvalidPeerCountIdKey eid) = eid
  fromIdData _ = ErrorLogNodeInvalidPeerCountIdKey
instance DefaultKeyId ErrorLogNetworkUpdate where
  toIdData _ (ErrorLogNetworkUpdateIdKey eid) = eid
  fromIdData _ = ErrorLogNetworkUpdateIdKey

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
  , HasDefaultNotify (Id e)
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

bakerLogAssume :: BakerLogTag e -> (LogTagConstraints e => x) -> x
bakerLogAssume = \case
  BakerLogTag_MultipleBakersForSameBaker -> id
  BakerLogTag_BakerMissed -> id
  BakerLogTag_BakerDeactivated -> id
  BakerLogTag_BakerDeactivationRisk -> id

logAssume :: LogTag e -> (LogTagConstraints e => x) -> x
logAssume = \case
  LogTag_NetworkUpdate -> id
  LogTag_Node nTag -> nodeLogAssume nTag
  LogTag_Baker bTag -> bakerLogAssume bTag
  LogTag_BakerNoHeartbeat -> id

instance EqTag LogTag Id where
  eqTagged t _ = logAssume t (==)
instance OrdTag LogTag Id where
  compareTagged t _ = logAssume t compare
instance ShowTag LogTag Id where
  showTaggedPrec t = logAssume t showsPrec

instance EqTag NodeLogTag Id where
  eqTagged t _ = nodeLogAssume t (==)
instance OrdTag NodeLogTag Id where
  compareTagged t _ = nodeLogAssume t compare
instance ShowTag NodeLogTag Id where
  showTaggedPrec t = nodeLogAssume t showsPrec

instance EqTag BakerLogTag Id where
  eqTagged t _ = bakerLogAssume t (==)
instance OrdTag BakerLogTag Id where
  compareTagged t _ = bakerLogAssume t compare
instance ShowTag BakerLogTag Id where
  showTaggedPrec t = bakerLogAssume t showsPrec

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
  LogTag_Baker bTag -> pure $ This $ bakerLogDep bTag
  LogTag_BakerNoHeartbeat -> []
  where
    bothNodes :: forall e. Related e (SingleConstructor e) Node -> [Some (Related e (SingleConstructor e))]
    bothNodes = \case
      Related fld fk -> case fk of
        ForeignKey_AutoId -> [This (Related fld $ ForeignKey_UniqueIdData @NodeExternal), This (Related fld $ ForeignKey_UniqueIdData @NodeInternal)]
        ForeignKey_Field fld2 -> case fld2 of {}

nodeLogDep :: NodeLogTag e -> Related e (SingleConstructor e) Node
nodeLogDep = \case
  NodeLogTag_InaccessibleNode -> depNodeAlert ErrorLogInaccessibleNode_nodeField
  NodeLogTag_NodeWrongChain -> depNodeAlert ErrorLogNodeWrongChain_nodeField
  NodeLogTag_NodeInvalidPeerCount -> depNodeAlert ErrorLogNodeInvalidPeerCount_nodeField
  NodeLogTag_BadNodeHead -> depNodeAlert ErrorLogBadNodeHead_nodeField
  where
    depNodeAlert f = Related f ForeignKey_AutoId

bakerLogDep :: BakerLogTag e -> Related e (SingleConstructor e) Baker
bakerLogDep = \case
  BakerLogTag_MultipleBakersForSameBaker -> depBakerAlert ErrorLogMultipleBakersForSameBaker_publicKeyHashField
  BakerLogTag_BakerMissed -> depBakerAlert' ErrorLogBakerMissed_bakerField
  BakerLogTag_BakerDeactivated -> depBakerAlert ErrorLogBakerDeactivated_publicKeyHashField
  BakerLogTag_BakerDeactivationRisk -> depBakerAlert ErrorLogBakerDeactivationRisk_publicKeyHashField
  where
    depBakerAlert' f = Related f $ ForeignKey_UniqueId
    depBakerAlert f = Related f $ ForeignKey_Field Baker_publicKeyHashField
