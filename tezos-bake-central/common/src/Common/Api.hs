{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-orphans -Wno-redundant-constraints #-}

module Common.Api where

import qualified Data.Aeson as Aeson
import Data.Aeson.GADT (deriveJSONGADT)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Sum (DSum)
import Data.Functor.Const
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Database.Id.Class
import Rhyolite.Schema (Email)
import Text.URI (URI)

import Tezos.Types

import Common.App (AlertNotificationMethod, LiquidityBakingToggleVote, MailServerView, WorkerType)
import Common.Schema

data PublicRequest a where
  PublicRequest_AddExternalNode
    :: URI
    -> Maybe Text
    -> Maybe Int
    -> PublicRequest ()
  PublicRequest_AddInternalNode
    :: Maybe (NodeProcessState, SnapshotImportSource)
    -> PublicRequest (Either AddInternalNodeError ())
  PublicRequest_RemoveNode
    :: Either URI ()
    -> PublicRequest ()
  PublicRequest_UpdateInternalWorker
    :: WorkerType
    -> Bool -- Desired running state
    -> PublicRequest ()
  PublicRequest_CancelSnapshotImport :: PublicRequest ()
  PublicRequest_CancelSnapshotDownload :: PublicRequest ()
  -- TODO think harder about update versus initial set
  PublicRequest_SetMailServerConfig
    :: MailServerView
    -> [Email]
    -> Maybe Text -- password
    -> PublicRequest ()
  PublicRequest_SendTestEmail
    :: Email
    -> PublicRequest ()
  PublicRequest_PollLedgerDevice :: PublicRequest ()
  PublicRequest_ShowLedgerBatch :: [SecretKey] -> PublicRequest ()
  PublicRequest_ShowLedger :: SecretKey -> PublicRequest ()
  PublicRequest_SetLiquidityBakingToggle :: PublicKeyHash -> LiquidityBakingToggleVote -> PublicRequest ()
  PublicRequest_ImportSecretKey :: SecretKey -> PublicRequest ()
  PublicRequest_SetupLedgerToBake :: SecretKey -> PublicRequest ()
  PublicRequest_RegisterKeyAsDelegate :: SecretKey -> PublicRequest ()
  PublicRequest_SetHWM :: SecretKey -> RawLevel -> PublicRequest ()
  PublicRequest_AddBaker
    :: PublicKeyHash
    -> Maybe Text
    -> PublicRequest ()
  PublicRequest_RemoveBaker
    :: PublicKeyHash
    -> PublicRequest ()
  PublicRequest_CheckForUpgrade
    :: PublicRequest ()
  PublicRequest_DismissUpgradeAlert
    :: PublicRequest ()
  PublicRequest_AddTelegramConfig
    :: Text
    -> PublicRequest ()
  PublicRequest_SetAlertNotificationMethodEnabled
    :: AlertNotificationMethod -- which one
    -> Bool -- whether is enabled
    -> PublicRequest  Bool -- True: success, False: no config to enable
  PublicRequest_ResolveAlert
    :: DSum LogTag Identity
    -> PublicRequest ()
  PublicRequest_ResolveAlerts
    :: [DSum LogTag (Const (Id ErrorLog))]
    -> PublicRequest ()
  PublicRequest_SetRightNotificationSettings
    :: RightKind
    -> Maybe RightNotificationLimit
    -> PublicRequest ()
  PublicRequest_DoVote
    :: SecretKey
    -> Id PeriodProposal
    -> Maybe Ballot -- When 'Nothing', vote for proposal rather than submitting a ballot
    -> PublicRequest ()

data PrivateRequest a where
  PrivateRequest_NoOp :: PrivateRequest ()

deriveJSONGADT ''PublicRequest
deriveArgDict ''PublicRequest
deriveJSONGADT  ''PrivateRequest
deriveArgDict ''PrivateRequest
