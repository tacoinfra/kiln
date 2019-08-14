{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-orphans #-}

module Common.Api where

import Data.Dependent.Sum (DSum)
import Data.Functor.Const
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Rhyolite.App (HasRequest, PrivateRequest, PublicRequest)
import Rhyolite.Request.TH (makeRequestForDataInstance)
import Rhyolite.Schema (Email, Id)
import Text.URI (URI)

import Tezos.NodeRPC.Sources (PublicNode)
import Tezos.Operation(Ballot)
import Tezos.Types

import Common.App (AlertNotificationMethod, Bake, MailServerView, WorkerType)
import Common.Schema

instance HasRequest Bake where
  data PublicRequest Bake a where
    PublicRequest_AddExternalNode
      :: URI
      -> Maybe Text
      -> Maybe Int
      -> PublicRequest Bake ()
    PublicRequest_AddInternalNode
      :: Maybe NodeProcessState
      -> PublicRequest Bake ()
    PublicRequest_RemoveNode
      :: Either URI ()
      -> PublicRequest Bake ()
    PublicRequest_UpdateInternalWorker
      :: WorkerType
      -> Bool -- Desired running state
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
    PublicRequest_PollLedgerDevice :: PublicRequest Bake ()
    PublicRequest_ShowLedger :: SecretKey -> PublicRequest Bake ()
    PublicRequest_ImportSecretKey :: SecretKey -> PublicRequest Bake ()
    PublicRequest_SetupLedgerToBake :: SecretKey -> PublicRequest Bake ()
    PublicRequest_RegisterKeyAsDelegate :: SecretKey -> Tez -> PublicRequest Bake ()
    PublicRequest_SetHWM :: SecretKey -> RawLevel -> PublicRequest Bake ()
    PublicRequest_AddBaker
      :: PublicKeyHash
      -> Maybe Text
      -> PublicRequest Bake ()
    PublicRequest_RemoveBaker
      :: PublicKeyHash
      -> PublicRequest Bake ()
    PublicRequest_CheckForUpgrade
      :: PublicRequest Bake ()
    PublicRequest_DismissUpgradeAlert
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
    PublicRequest_ResolveAlerts
      :: [DSum LogTag (Const (Id ErrorLog))]
      -> PublicRequest Bake ()
    PublicRequest_SetRightNotificationSettings
      :: RightKind
      -> Maybe RightNotificationLimit
      -> PublicRequest Bake ()
    PublicRequest_DoVote
      :: SecretKey
      -> Id PeriodProposal
      -> Maybe Ballot -- When 'Nothing', vote for proposal rather than submitting a ballot
      -> PublicRequest Bake ()

  data PrivateRequest Bake a where
    PrivateRequest_NoOp :: PrivateRequest Bake ()

makeRequestForDataInstance ''PublicRequest ''Bake
makeRequestForDataInstance ''PrivateRequest ''Bake
