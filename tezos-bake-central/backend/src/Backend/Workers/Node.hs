{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Workers.Node where

import Control.Lens (ifor, ifor_, ix, to, (.~), (<&>), (^.), (^?), _Just, _Right)
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT, throwError, catchError)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Control.Monad.Reader (MonadReader, runReaderT)
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity (..))
import Data.LCA.Online.Polymorphic
import Data.Map (Map)
import Data.Pool (Pool)
import Data.Semigroup((<>), Max(..))
import Database.Groundhog.Postgresql
import Say (say, sayErr, sayShow)
import qualified Network.HTTP.Client as Http (Manager)
import Data.Traversable (for)

import Rhyolite.Backend.Listen (NotificationType (..), insertAndNotify, insertAndNotify_, notifyEntityId,
                                updateAndNotify)
import Rhyolite.Backend.DB (RunDb, getTime, openDb, runDb, selectMap)
import Rhyolite.Backend.DB.PsqlSimple (In (..), Only (..), PostgresRaw, Values (..), executeQ, queryQ)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Concurrent (worker)
import Rhyolite.Schema (Id (..), Json (..))

import Tezos.NodeRPC
import Tezos.Types

import Common.Schema
import Backend.Errors
import Backend.Schema
import Backend.Config (AppConfig (..), HasAppConfig, getAppConfig)

type Branch' a = Path BlockHash a
type Branch = Branch' (Maybe (Max Fitness))
type NodePool = Map ClientAddress NodeStatus

data NodeStatus = NodeStatus
  { nId :: Id Node
  , node :: Node
  , head :: Branch
  }

-- nodeWorker = do
--   history <- MVar 
-- at startup:
--    load all known nodes

nodeWorker
  :: Int -- delay between checking for updates, in microseconds
  -> AppConfig
  -> Http.Manager
  -> Pool Postgresql
  -> IO (IO ())
nodeWorker delay appConfig httpMgr db = do
  worker delay $ do
    say "Update node cycle."
    nodes :: [(Id Node, ClientAddress)] <- runNoLoggingT $ runDb (Identity db) $ flip runReaderT appConfig $ do
      fmap (first toId) <$>
        project (AutoKeyField, Node_addressField) (Node_deletedField ==. False)

    let nodeError :: ClientAddress -> RpcError -> ExceptT RpcError IO ()
        nodeError nodeAddr _ = ExceptT ( fmap Right ( runNoLoggingT ( runDb (Identity db) ( flip runReaderT appConfig ( reportInaccessibleEndpointError EndpointType_Node nodeAddr )))))
    for nodes $ \(nodeId :: Id Node, nodeAddr) -> runExceptT $ flip catchError (nodeError nodeAddr) $ flip runReaderT (NodeRPCContext httpMgr nodeAddr) $ do
      say $ "Updating node at " <> nodeAddr
      protoInfo <- nodeRPC $ RProtoConstants headId
      headBlockInfo <- nodeRPC $ RBlock headId
      runNoLoggingT $ runDb (Identity db) $ flip runReaderT appConfig $ do
        clearInaccessibleEndpointError EndpointType_Node nodeAddr
        [queryQ| SELECT id FROM "Parameters" WHERE node = ?nodeId |] >>= \case
          (Only (pid :: Id Parameters): _) ->
            updateAndNotify pid [Parameters_protoInfoField =. protoInfo]
          _ ->
            insertAndNotify_ Parameters {_parameters_node = nodeId, _parameters_protoInfo = protoInfo}

        updateAndNotify nodeId
          [ Node_headLevelField =. Just (headBlockInfo ^. block_header . blockHeader_level)
          , Node_headBlockHashField =. Just (headBlockInfo ^. block_hash)
          , Node_fitnessField =. Just (headBlockInfo ^. block_header . blockHeader_fitness)
          ]


-- when a new node is learned:
--    begin monitoring it.


