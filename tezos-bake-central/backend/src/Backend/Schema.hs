{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Backend.Schema where

import Control.Arrow
import Data.ByteString (ByteString)
import Data.Coerce (Coercible, coerce)
import Data.Fixed
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding as T
import Data.Word (Word64)
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.Instances ()
import Database.Groundhog.Postgresql ()
import Database.Groundhog.TH
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Rhyolite.Backend.Account ()
import Rhyolite.Backend.Schema ()
import Rhyolite.Backend.Schema.TH (makeDefaultKeyIdInt64, mkRhyolitePersist)
import Rhyolite.Schema (Json (..))

import Common.Base16ByteString
import Common.Fitness
import Common.Json (TezosWord64 (..))
import Common.PublicKeyHash
import Common.Schema
import Common.TaggedHash
import Common.Tez
import Common.TezosBinary

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

instance PrimitivePersistField Tezzies where
  toPrimitivePersistValue p (Tezzies x) = toPrimitivePersistValue p x
  fromPrimitivePersistValue p v = Tezzies $ fromPrimitivePersistValue p v

instance ToField Tezzies where
  toField (Tezzies n) = toField n

instance PersistField Tezzies where
  persistName _ = "Tezzies"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType p (Tezzies x) = dbType p x

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

instance FromField Tezzies where
  fromField f b = Tezzies <$> fromField f b -- is this sign-correct?

instance NeverNull (HashedValue a ByteString)
instance NeverNull (Json BakedEvent)
instance NeverNull (Json BlockInfo)
instance NeverNull Fitness
instance NeverNull NetworkStat
instance NeverNull PublicKeyHash
instance NeverNull TezosWord64
instance NeverNull Tezzies

unsafeParseBinary :: TezosBinary a => ByteString -> a
unsafeParseBinary = either error id . eitherBinary "unsafeParseBinary"

stripOnly :: (Coercible (f (Only a)) (f a)) => f (Only a) -> f a
stripOnly = coerce


instance TezosBinary a => PersistField (Base16ByteString a) where
  persistName _ = "Base16ByteString"
  toPersistValues = primToPersistValue . encodeBinary . unbase16ByteString
  fromPersistValues = (fmap.first) (Base16ByteString . unsafeParseBinary) . primFromPersistValue
  dbType p x = dbType p (encodeBinary x)

instance FromField a => FromField (HashedValue t a) where
  fromField f b = HashedValue <$> fromField f b

instance PrimitivePersistField a => PersistField (HashedValue t a) where
  persistName _ = "HashedValue"
  toPersistValues = primToPersistValue . unHashedValue
  fromPersistValues = (fmap.first) HashedValue . primFromPersistValue
  dbType p (HashedValue x) = dbType p x


instance FromField TezosWord64 where
  fromField f b = TezosWord64 <$> fromField f b

instance PrimitivePersistField TezosWord64 where
  toPrimitivePersistValue x (TezosWord64 v) = toPrimitivePersistValue x v
  fromPrimitivePersistValue x v = TezosWord64 $ fromPrimitivePersistValue x v

instance PrimitivePersistField (HashedValue t ByteString) where
  toPrimitivePersistValue x (HashedValue v) = toPrimitivePersistValue x v
  fromPrimitivePersistValue x v = HashedValue $ fromPrimitivePersistValue x v

instance PersistField TezosWord64 where
  persistName _ = "TezosWord64"
  toPersistValues = primToPersistValue . unTezosWord64
  fromPersistValues = (fmap.first) TezosWord64 . primFromPersistValue
  dbType p (TezosWord64 x) = dbType p x

instance PersistField PublicKeyHash where
  persistName _ = "PublicKeyHash"
  toPersistValues (PublicKeyHash_Ed25519 x) = primToPersistValue $ toBase58Text x
  toPersistValues (PublicKeyHash_Secp256k1 x) = primToPersistValue $ toBase58Text x
  fromPersistValues = (fmap.first) toPublicKeyHash . primFromPersistValue
    where
      toPublicKeyHash = either (error . show) id . tryFromBase58 publicKeyHashConstructorDecoders . T.encodeUtf8
  dbType p _ = dbType p ("" :: Text)

instance PrimitivePersistField PublicKeyHash where
  toPrimitivePersistValue a (PublicKeyHash_Ed25519 x) = toPrimitivePersistValue a $ toBase58Text x
  toPrimitivePersistValue a (PublicKeyHash_Secp256k1 x) = toPrimitivePersistValue a $ toBase58Text x
  fromPrimitivePersistValue a = toPublicKeyHash . fromPrimitivePersistValue a
    where
      toPublicKeyHash = either (error . show) id . tryFromBase58 publicKeyHashConstructorDecoders . T.encodeUtf8

instance ToField PublicKeyHash where
  toField a = toField (toPublicKeyHashText a)

instance FromField PublicKeyHash where
  -- TODO: Write a real Conversion for this.
  fromField f b = either (error . show) id . tryFromBase58 publicKeyHashConstructorDecoders . T.encodeUtf8 <$> fromField f b


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
  - embedded: NetworkStat
  - entity: Parameters
    constructors:
      - name: Parameters
        uniques:
          - name: _parameters_uniqueness
            type: constraint
            fields: [_parameters_node]
        fields:
          - name: _parameters_node
            reference:
              table: Node
              onDelete: cascade
  - embedded: ProtoInfo
  - entity: PendingReward
    constructors:
      - name: PendingReward
        uniques:
          - name: _pendingReward_uniqueness
            type: constraint
            fields: [_pendingReward_client, _pendingReward_hash]
  - entity: DelegateStats
    constructors:
      - name: DelegateStats
        uniques:
          - name: _delegateStats_uniqueness
            type: constraint
            fields: [_delegateStats_publicKeyHash]
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
|]

fmap concat $ traverse (uncurry makeDefaultKeyIdInt64)
  [ (''Client, 'ClientKey)
  , (''ClientInfo, 'ClientInfoKey)
  , (''DelegateStats, 'DelegateStatsKey)
  , (''MailServerConfig, 'MailServerConfigKey)
  , (''Node, 'NodeKey)
  , (''Notificatee, 'NotificateeKey)
  , (''Parameters, 'ParametersKey)
  , (''PendingReward, 'PendingRewardKey)
  ]
