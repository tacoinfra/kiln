{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Backend.Schema where

import Data.Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (Coercible, coerce)
import Data.Fixed (Fixed (MkFixed), HasResolution, Micro)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Typeable
import Data.Version (Version)
import qualified Data.Version as Version
import Data.Word (Word64)
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Instances ()
import Database.Groundhog.Postgresql ()
import qualified Database.Groundhog.Postgresql.Array as Groundhog
import Database.Groundhog.TH
import Database.PostgreSQL.Simple (Binary (..), Only (..), fromBinary)
import Database.PostgreSQL.Simple.FromField hiding (Binary)
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Database.PostgreSQL.Simple.Types (PGArray (..))
import qualified Formatting as Fmt
import Rhyolite.Backend.Account ()
import Rhyolite.Backend.Schema (fromId)
import Rhyolite.Backend.Schema.Class (DefaultKeyId)
import Rhyolite.Backend.Schema.TH (makeDefaultKeyIdInt64, mkRhyolitePersist)
import Rhyolite.Schema (Id, Json (..))
import Text.URI (URI)
import qualified Text.URI as Uri

import Tezos.Base58Check (HashedValue (..), tryFromBase58)
import Tezos.NodeRPC.Types
import Tezos.Types

import Backend.Version (parseVersion)
import Common.Schema

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
  dbType p (Tez x) = dbType p x

instance PersistField PeriodSequence where
  persistName _ = "PeriodSequence"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType p (PeriodSequence x) = dbType p (Json x)

instance PrimitivePersistField PeriodSequence where
  toPrimitivePersistValue p (PeriodSequence x) = toPrimitivePersistValue p (Json x)
  fromPrimitivePersistValue p x = PeriodSequence $ unJson $ fromPrimitivePersistValue p x

instance FromField Micro where
  fromField f b = MkFixed . toInteger @Int64 <$> fromField f b

instance NeverNull (HashedValue a ByteString)
instance NeverNull (Json BakedEvent)
-- instance NeverNull (Json BlockInfo)
instance NeverNull Fitness
instance NeverNull NetworkStat
instance NeverNull PublicKeyHash
instance NeverNull RawLevel
instance NeverNull Tez
instance NeverNull TezosWord64
instance NeverNull Version

-- unsafeParseBinary :: TezosBinary a => ByteString -> a
-- unsafeParseBinary = either error id . eitherBinary "unsafeParseBinary"

stripOnly :: (Coercible (f (Only a)) (f a)) => f (Only a) -> f a
stripOnly = coerce

type EntityWithId a = (DefaultKeyId a, DefaultKey a ~ Key a BackendSpecific, PersistEntity a, PrimitivePersistField (Key a BackendSpecific))

getId :: (PersistBackend m, EntityWithId a) => Id a -> m (Maybe a)
getId = get . fromId


parseVersionOrError :: Text -> Version
parseVersionOrError = fromMaybe (error "Invalid version") . parseVersion

-- instance TezosBinary a => PersistField (Base16ByteString a) where
--   persistName _ = "Base16ByteString"
--   toPersistValues = primToPersistValue . encodeBinary . unbase16ByteString
--   fromPersistValues = (fmap.first) (Base16ByteString . unsafeParseBinary) . primFromPersistValue
--   dbType p x = dbType p (encodeBinary x)

instance FromField (HashedValue t ByteString) where
  fromField f b = HashedValue . fromBinary <$> fromField f b

instance ToField (HashedValue t ByteString) where
  toField (HashedValue a) = toField $ Binary a


instance PrimitivePersistField a => PersistField (HashedValue t a) where
  persistName _ = "HashedValue"
  toPersistValues = primToPersistValue . unHashedValue
  fromPersistValues = (fmap.first) HashedValue . primFromPersistValue
  dbType p (HashedValue x) = dbType p x

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

instance PrimitivePersistField (HashedValue t ByteString) where
  toPrimitivePersistValue x (HashedValue v) = toPrimitivePersistValue x v
  fromPrimitivePersistValue x v = HashedValue $ fromPrimitivePersistValue x v

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
    n' = LT.toStrict $ Fmt.format (Fmt.left 4 '0') $ Fmt.format Fmt.hex 10

unArray :: Groundhog.Array a -> [a]
unArray (Groundhog.Array a) = a

-- prefix fitness arrays with length so that they naturally order correctly
toDBFitness :: ToJSON a => FitnessF a -> [Text]
toDBFitness (FitnessF x) = ((leftPad $ length x) :) .  toList . fmap (T.decodeUtf8 . LBS.toStrict . encode) $ x
fromDBFitness :: FromJSON a => [Text] -> FitnessF a
fromDBFitness = FitnessF . Seq.fromList . fmap ( fromJust . decode . LBS.fromStrict . T.encodeUtf8 ) . tail

instance (ToJSON a, FromJSON a) => PrimitivePersistField (FitnessF a) where
  toPrimitivePersistValue p x = toPrimitivePersistValue p ( Groundhog.Array $ toDBFitness x)
  fromPrimitivePersistValue p = fromDBFitness . unArray . fromPrimitivePersistValue p

instance (FromJSON a, ToJSON a) => PersistField (FitnessF a) where
  persistName _ = "Fitness"
  toPersistValues = toPersistValues . Groundhog.Array . toDBFitness
  fromPersistValues vs = first (fromDBFitness . unArray) <$> fromPersistValues vs
  dbType p x = dbType p (Groundhog.Array $ toDBFitness x) -- p (Json (Seq.empty :: Seq.Seq (Base16ByteString a)))

instance (ToJSON a, Typeable a) => ToField (FitnessF a) where
  toField v = toField $ PGArray $ toDBFitness v
instance (FromJSON a, Typeable a) => FromField (FitnessF a) where
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
  toPersistValues = toPersistValues . Uri.render
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

instance ToField EndpointType where
  toField = toField . show
instance FromField EndpointType where
  fromField f b = read <$> fromField f b

instance ToField ClientWorker where
  toField = toField . show
instance FromField ClientWorker where
  fromField f b = read <$> fromField f b

instance ToField URI where
  toField = toField . Uri.render
instance FromField URI where
  fromField f b = fromMaybe (error "Invalid URI") . Uri.mkURI <$> fromField f b

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
        uniques:
          - name: _node_uniqueness
            type: constraint
            fields: [_node_address]
  - entity: TzScan
    constructors:
    - name: TzScan
      uniques:
        - name: _tzscan_uniqueness
          type: constraint
          fields: [_tzScan_chainId]
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
            fields: [_pendingReward_delegate, _pendingReward_hash]
  - entity: Delegate
    constructors:
      - name: Delegate
        uniques:
          - name: _delegate_uniqueness
            type: constraint
            fields: [_delegate_publicKeyHash]
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
  - primitive: EndpointType
  - primitive: ClientWorker
  - primitive: UpgradeCheckError
  - entity: ErrorLog
  - entity: ErrorLogBakerNoHeartbeat
  - entity: ErrorLogInaccessibleEndpoint
  - entity: ErrorLogMultipleBakersForSameDelegate
  - entity: ErrorLogNodeOnFork
  - entity: ErrorLogUpgradeNotice
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
|]

fmap concat $ traverse (uncurry makeDefaultKeyIdInt64)
  [ (''CachedProtocolConstants, 'CachedProtocolConstantsKey)
  , (''Client, 'ClientKey)
  , (''ClientInfo, 'ClientInfoKey)
  , (''Delegate, 'DelegateKey)
  , (''ErrorLog, 'ErrorLogKey)
  , (''ErrorLogBakerNoHeartbeat, 'ErrorLogBakerNoHeartbeatKey)
  , (''ErrorLogInaccessibleEndpoint, 'ErrorLogInaccessibleEndpointKey)
  , (''ErrorLogMultipleBakersForSameDelegate, 'ErrorLogMultipleBakersForSameDelegateKey)
  , (''ErrorLogNodeOnFork, 'ErrorLogNodeOnForkKey)
  , (''ErrorLogUpgradeNotice, 'ErrorLogUpgradeNoticeKey)
  , (''GenericCacheEntry, 'GenericCacheEntryKey)
  , (''MailServerConfig, 'MailServerConfigKey)
  , (''Node, 'NodeKey)
  , (''Notificatee, 'NotificateeKey)
  , (''Parameters, 'ParametersKey)
  , (''PendingReward, 'PendingRewardKey)
  , (''TzScan, 'TzScanKey)
  ]
