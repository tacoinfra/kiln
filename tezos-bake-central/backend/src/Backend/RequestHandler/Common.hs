{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Backend.RequestHandler.Common where

import Data.List.NonEmpty (nonEmpty)
import Data.Functor.Infix hiding ((<&>))
import Data.Some (Some(..))
import Data.Universe
import Database.Groundhog.Core (EntityConstr, Field)
import Database.Groundhog.Postgresql
import Database.Id.Class
import Database.Id.Groundhog
import Rhyolite.Backend.DB (getTime, project1)
import Tezos.Types (PublicKeyHash)
import Text.URI (URI)

import Backend.Schema
import Common.App
import Common.Schema
import ExtraPrelude

getInternalNode :: PersistBackend m => m (Maybe (Id Node, DeletableRow (Id ProcessData)))
getInternalNode = project1 (NodeInternal_idField, NodeInternal_dataField) CondEmpty

addBakerImpl :: (Monad m, PersistBackend m) => PublicKeyHash -> Maybe Text -> m ()
addBakerImpl pkh alias = do
  existingIds :: [Id Baker] <- fmap toId <$> project BakerKey (Baker_publicKeyHashField ==. pkh)
  let newVal = BakerData
        { _bakerData_alias = alias
        }
  case nonEmpty existingIds of
    Nothing -> void $ insert $ Baker
      { _baker_publicKeyHash = pkh
      , _baker_data = DeletableRow
        { _deletableRow_data = newVal
        , _deletableRow_deleted = False
        }
      }
    Just bIds -> for_ bIds $ \bId ->
      update [ Baker_dataField ~> DeletableRow_deletedSelector =. False
             , Baker_dataField ~> DeletableRow_dataSelector ~> BakerData_aliasSelector =. alias
             ]
             (BakerKey ==. fromId bId)
  notify NotifyTag_Baker (Id pkh, Just newVal)

removeNodeDbImpl :: forall m. (SqlDb (PhantomDb m), PersistBackend m) => Either URI () -> m ()
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
        update [ProcessData_controlField =. ProcessControl_Stop] (AutoKeyField ==. fromId pid)
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
          NodeLogTag_NodeWrongChain -> deleteLogs tag ErrorLogNodeWrongChain_nodeField
          NodeLogTag_BadNodeHead -> deleteLogs tag ErrorLogBadNodeHead_nodeField
          NodeLogTag_NodeInvalidPeerCount -> deleteLogs tag ErrorLogNodeInvalidPeerCount_nodeField

      ids <- fmap concat $ for universe onTag
      now <- getTime
      update [ErrorLog_stoppedField =. Just now] (AutoKeyField `in_` fmap fromId ids)
