{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Backend.Schema where

import Database.Groundhog.Instances ()
import Database.Groundhog.Postgresql ()
import Data.Fixed
import Data.Int(Int64)
import Data.Word
import Focus.Backend.Account ()
import Focus.Backend.DB.Groundhog (groundhog, mkFocusPersist)
import Focus.Backend.Schema.TH
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import Common.Schema
import Tezos.BakeMonitor.Types

import Database.Groundhog.Core
import Database.Groundhog.Generic
import Data.Proxy
import Focus.Schema (Json(..))

instance FromField Word64 where
  fromField f b = fromInteger <$> fromField f b -- is this sign-correct?

-- TODO: Move all of this into focus
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

mkFocusPersist (Just "migrateSchema") [groundhog|
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
  - entity: Parameters
    constructors:
      - name: Parameters
        uniques:
          - name: _parameters_uniqeness
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
          - name: _pendingReward_uniqeness
            type: constraint
            fields: [_pendingReward_client, _pendingReward_hash]
|]

fmap concat $ mapM (uncurry makeDefaultKeyIdInt64)
  [ (''Client, 'ClientKey)
  , (''ClientInfo, 'ClientInfoKey)
  , (''Node, 'NodeKey)
  , (''Parameters, 'ParametersKey)
  , (''PendingReward, 'PendingRewardKey)
  ]
