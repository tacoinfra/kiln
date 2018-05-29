{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import Data.Fixed
import Data.Int (Int64)
import Data.Text.Encoding as T
import Data.Word
import Database.Groundhog.Instances ()
import Database.Groundhog.Postgresql ()
import Database.Groundhog.TH
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Rhyolite.Backend.Account ()
import Rhyolite.Backend.Schema ()
import Rhyolite.Backend.Schema.TH

import Common.Schema
-- import Tezos.BakeMonitor.Types
import Common.Tez
import Common.TezosBinary

import Database.Groundhog.Core
import Database.Groundhog.Generic
import Rhyolite.Schema (Json (..))

import Common.Base16ByteString
import Common.PublicKeyHash
import Common.TaggedHash

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

instance NeverNull Tezzies

instance FromField Micro where
  fromField f b = MkFixed . toInteger @Int64 <$> fromField f b

instance FromField Tezzies where
  fromField f b = Tezzies <$> fromField f b -- is this sign-correct?

instance NeverNull (HashedValue a ByteString)
instance NeverNull (Json BlockInfo)
instance NeverNull (Json BakedEvent)
instance NeverNull PublicKeyHash
instance NeverNull NetworkStat

unsafeParseBinary :: TezosBinary a => ByteString -> a
unsafeParseBinary = either error id . eitherBinary "unsafeParseBinary"

instance TezosBinary a => PersistField (Base16ByteString a) where
  persistName _ = "Base16ByteString"
  toPersistValues = primToPersistValue . encodeBinary . unbase16ByteString
  fromPersistValues = (fmap.first) (Base16ByteString . unsafeParseBinary) . primFromPersistValue
  dbType p x = dbType p (encodeBinary x)

instance PrimitivePersistField a => PersistField (HashedValue t a) where
  persistName _ = "HashedValue"
  toPersistValues = primToPersistValue . unHashedValue
  fromPersistValues = (fmap.first) HashedValue . primFromPersistValue
  dbType p (HashedValue x) = dbType p x

instance PersistField PublicKeyHash where
  persistName _ = "PublicKeyHash"
  toPersistValues (PublicKeyHash_Ed25519 x) = primToPersistValue $ toBase58Text x
  toPersistValues (PublicKeyHash_Secp256k1 x) = primToPersistValue $ toBase58Text x
  fromPersistValues = (fmap.first) toPublicKeyHash . primFromPersistValue
    where
      toPublicKeyHash = either (error . show) id . tryFromBase58 publicKeyHashConstructorDecoders . T.encodeUtf8
  dbType p (PublicKeyHash_Ed25519 x) = dbType p $ toBase58Text x
  dbType p (PublicKeyHash_Secp256k1 x) = dbType p $ toBase58Text x

-- instance PersistField Operation

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

fmap concat $ mapM (uncurry makeDefaultKeyIdInt64)
  [ (''Client, 'ClientKey)
  , (''ClientInfo, 'ClientInfoKey)
  , (''Node, 'NodeKey)
  , (''Parameters, 'ParametersKey)
  , (''PendingReward, 'PendingRewardKey)
  , (''Notificatee, 'NotificateeKey)
  , (''MailServerConfig, 'MailServerConfigKey)
  ]
