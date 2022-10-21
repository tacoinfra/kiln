{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Backend.Common.Node where

import Control.Monad.Logger (MonadLoggerIO, logError)
import Data.Functor.Infix hiding ((<&>))
import Data.Pool (Pool)
import Data.Some (Some(..))
import Data.Universe
import Database.Groundhog.Core (EntityConstr, Field)
import Database.Groundhog.Postgresql
import Database.Id.Class
import Database.Id.Groundhog
import Rhyolite.Backend.DB (getTime, project1, runDb)
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, queryQ)
import Text.URI (URI)

import Backend.Schema
import Common.App
import Common.Schema
import ExtraPrelude

getInternalNode :: PersistBackend m => m (Maybe (Id Node, DeletableRow (Id ProcessData)))
getInternalNode = project1 (NodeInternal_idField, NodeInternal_dataField) CondEmpty

removeNodeDbImpl :: forall m. (SqlDb (PhantomDb m), PersistBackend m, PostgresRaw m) => Either URI () -> m ()
removeNodeDbImpl = \case
  Left addr -> do
    nids :: [Id Node] <- project NodeExternal_idField (NodeExternal_dataField ~> DeletableRow_dataSelector ~> NodeExternalData_addressSelector ==. addr)
    for_ nids $ \nid -> do
      update [NodeExternal_dataField ~> DeletableRow_deletedSelector =. True] (NodeExternal_idField ==. nid)
      notify NotifyTag_NodeExternal (nid, Nothing)
      clearErrors nid
  Right () -> do
    getInternalNode >>= \case
      Nothing -> pure ()
      Just (nid, nodeData) -> do
        update
          [ NodeInternal_dataField ~> DeletableRow_deletedSelector =. True
          ]
          CondEmpty
        let pid = _deletableRow_data nodeData
        update
          [ ProcessData_controlField =. ProcessControl_Stop
          , ProcessData_errorLogField =. (Nothing :: Maybe Text)
          ] (AutoKeyField ==. fromId pid)
        clearErrors nid
        notify NotifyTag_NodeInternal (nid, Nothing)
  where
    clearErrors nid = do
      let
        deleteLogs :: forall cstr m' t.
                      ( Monad m', PersistBackend m'
                      , IdData t ~ Id ErrorLog, HasDefaultNotify (Id t), EntityConstr t cstr)
                   => NodeLogTag t
                   -> Field t cstr (Id Node)
                   -> m' [Id ErrorLog]
        deleteLogs tag field = do
          ids <- errorLogIdForNodeLogTag tag <$$> select (field ==. nid)
          for_ ids $ notifyDefault . Id @t
          pure ids

        onTag :: Some NodeLogTag -> m [Id ErrorLog]
        onTag (Some tag) = case tag of
          NodeLogTag_InaccessibleNode -> deleteLogs tag ErrorLogInaccessibleNode_nodeField
          NodeLogTag_NodeInsufficientPeers -> deleteLogs tag ErrorLogNodeInsufficientPeers_nodeField
          NodeLogTag_NodeWrongChain -> deleteLogs tag ErrorLogNodeWrongChain_nodeField
          NodeLogTag_BadNodeHead -> deleteLogs tag ErrorLogBadNodeHead_nodeField
          NodeLogTag_NodeInvalidPeerCount -> deleteLogs tag ErrorLogNodeInvalidPeerCount_nodeField

      internalNodeLogIds <- do
        -- TODO: Groundhog doesn't typecheck
        -- ids <- _errorLogInternalNodeFailed_log <$$> select (ErrorLogInternalNodeFailed_nodeField ==. (Id nid :: Id NodeInternal))
        ids :: [Id ErrorLog] <- stripOnly <$> [queryQ|SELECT log FROM "ErrorLogInternalNodeFailed" WHERE "node#id" = ?nid|]
        for_ ids $ notifyDefault . Id @ErrorLogInternalNodeFailed
        pure ids

      nodeLogIds <- fmap concat $ for universe onTag

      now <- getTime
      update [ErrorLog_stoppedField =. Just now] (AutoKeyField `in_` fmap fromId (nodeLogIds <> internalNodeLogIds))

startNodeDaemon :: PersistBackend m => m ()
startNodeDaemon = updateNodeDaemon ProcessControl_Run

stopNodeDaemon :: PersistBackend m => m ()
stopNodeDaemon = updateNodeDaemon ProcessControl_Stop

updateNodeDaemon :: PersistBackend m => ProcessControl -> m ()
updateNodeDaemon control = do
  (getInternalNode >>=) $ traverse_ $ \(nid, nodeData) -> do
    let pid = _deletableRow_data nodeData
    update
      [ ProcessData_controlField =. control
      , ProcessData_errorLogField =. (Nothing :: Maybe Text)
      ] (AutoKeyField ==. fromId pid)
    processData <- getId $ _deletableRow_data nodeData
    notify NotifyTag_NodeInternal (nid, processData)

isNodeSynced :: MonadLoggerIO m => Pool Postgresql -> Id Node -> m Bool
isNodeSynced db nodeId = do
  synced <- listToMaybe . stripOnly <$> runDb (Identity db)
    [queryQ|
      select count(el.id) = 0 from "ErrorLogBadNodeHead" ebh
      join "ErrorLog" el on el.id = ebh.log
      where ebh.node = ?nodeId and el.stopped is null
    |]
  case synced of
    Just s  -> pure s
    Nothing -> do
      $(logError) $ "Couldn't get the count of alerts for node with id " <> tshow nodeId
      pure False
