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

import Data.Text (Text)
import Data.Version (Version)
import Rhyolite.App (HasRequest, PrivateRequest, PublicRequest)
import Rhyolite.Request.Class (Request)
import Rhyolite.Request.TH (makeRequestForDataInstance)
import Rhyolite.Schema (Email)

import Tezos.Types

import Common.App (Bake, MailServerView)
import Common.Schema (ClientAddress, UpgradeCheckError)

instance (Request (PublicRequest Bake), Request (PrivateRequest Bake)) => HasRequest Bake where
  data PublicRequest Bake a where
    PublicRequest_AddNode
      :: ClientAddress
      -> Maybe CryptoboxPublicKeyHash
      -> PublicRequest Bake ()
    PublicRequest_RemoveNode
      :: ClientAddress
      -> PublicRequest Bake ()
    PublicRequest_AddClient
      :: ClientAddress -- address of client to subscribe to
      -> PublicRequest Bake () -- TODO: perhaps give an Id Client
    PublicRequest_RemoveClient
      :: ClientAddress -- address of client to unsubscribe from
      -> PublicRequest Bake ()
    PublicRequest_SetMailServerConfig
      :: MailServerView
      -> Text -- password
      -> PublicRequest Bake ()
    PublicRequest_AddNotificatee
      :: Email
      -> PublicRequest Bake ()
    PublicRequest_RemoveNotificatee
      :: Email
      -> PublicRequest Bake ()
    PublicRequest_SendTestEmail
      :: Email
      -> PublicRequest Bake ()
    PublicRequest_AddDelegate
      :: PublicKeyHash
      -> PublicRequest Bake ()
    PublicRequest_RemoveDelegate
      :: PublicKeyHash
      -> PublicRequest Bake ()
    PublicRequest_CheckForUpgrade
      :: PublicRequest Bake (Either UpgradeCheckError Version)
  data PrivateRequest Bake a where
    PrivateRequest_NoOp :: PrivateRequest Bake ()

makeRequestForDataInstance ''PublicRequest ''Bake
makeRequestForDataInstance ''PrivateRequest ''Bake
