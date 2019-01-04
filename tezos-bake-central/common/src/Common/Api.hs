{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Common.Api where

import Data.Dependent.Sum (DSum)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Rhyolite.App (HasRequest, PrivateRequest, PublicRequest)
import Rhyolite.Request.Class (Request)
import Rhyolite.Request.TH (makeRequestForDataInstance)
import Rhyolite.Schema (Email)
import Text.URI (URI)

import Tezos.NodeRPC.Sources (PublicNode)
import Tezos.Types

import Common.App (AlertNotificationMethod, Bake, MailServerView)
import Common.Schema (LogTag)

instance (Request (PublicRequest Bake), Request (PrivateRequest Bake)) => HasRequest Bake where
  data PublicRequest Bake a where
    PublicRequest_AddExternalNode
      :: URI
      -> Maybe Text
      -> Maybe Int
      -> PublicRequest Bake ()
    PublicRequest_AddInternalNode
      :: PublicRequest Bake ()
    PublicRequest_RemoveNode
      :: URI
      -> PublicRequest Bake ()
    PublicRequest_AddClient
      :: URI -- address of client to subscribe to
      -> Maybe Text
      -> PublicRequest Bake () -- TODO: perhaps give an Id Client
    PublicRequest_RemoveClient
      :: URI -- address of client to unsubscribe from
      -> PublicRequest Bake ()
    -- TODO think harder about update versus initial set
    PublicRequest_SetMailServerConfig
      :: MailServerView
      -> [Email]
      -> Maybe Text -- password
      -> PublicRequest Bake ()
    PublicRequest_SendTestEmail
      :: Email
      -> PublicRequest Bake ()
    PublicRequest_AddBaker
      :: PublicKeyHash
      -> Maybe Text
      -> PublicRequest Bake ()
    PublicRequest_RemoveBaker
      :: PublicKeyHash
      -> PublicRequest Bake ()
    PublicRequest_CheckForUpgrade
      :: PublicRequest Bake ()
    PublicRequest_SetPublicNodeConfig
      :: PublicNode
      -> Bool
      -> PublicRequest Bake ()
    PublicRequest_AddTelegramConfig
      :: Text
      -> PublicRequest Bake ()
    PublicRequest_SetAlertNotificationMethodEnabled
      :: AlertNotificationMethod -- which one
      -> Bool -- whether is enabled
      -> PublicRequest Bake Bool -- True: success, False: no config to enable
    PublicRequest_ResolveAlert
      :: DSum LogTag Identity
      -> PublicRequest Bake ()

  data PrivateRequest Bake a where
    PrivateRequest_NoOp :: PrivateRequest Bake ()

fmap concat $ sequence
  [ makeRequestForDataInstance ''PublicRequest ''Bake
  , makeRequestForDataInstance ''PrivateRequest ''Bake
  ]
