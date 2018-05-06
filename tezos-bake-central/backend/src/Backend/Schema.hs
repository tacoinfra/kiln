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
import Focus.Backend.Account ()
import Focus.Backend.DB.Groundhog (groundhog, mkFocusPersist)
import Focus.Backend.Schema.TH

import Common.Schema
import Tezos.BakeMonitor.Types

import Database.Groundhog.Core
import Database.Groundhog.Generic
import Data.Proxy

instance HasResolution a => PrimitivePersistField (Fixed a) where
  toPrimitivePersistValue p x = toPrimitivePersistValue p $ (floor :: Fixed a -> Int64) $ x * (fromInteger $ resolution (Proxy :: Proxy a))
  fromPrimitivePersistValue p x = (fromIntegral x' :: Fixed a) / (fromInteger $ resolution (Proxy :: Proxy a))
    where
      x' :: Int64 = fromPrimitivePersistValue p x

instance HasResolution a => PersistField (Fixed a) where
  persistName _ = "Fixed"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbInt64 False Nothing Nothing

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
|]

fmap concat $ mapM (uncurry makeDefaultKeyIdInt64)
  [ (''Client, 'ClientKey)
  , (''ClientInfo, 'ClientInfoKey)
  , (''Node, 'NodeKey)
  , (''Parameters, 'ParametersKey)
  ]
