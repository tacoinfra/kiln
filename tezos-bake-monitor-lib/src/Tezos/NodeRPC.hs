{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tezos.NodeRPC (module X) where

import Tezos.NodeRPC.Class as X
import Tezos.NodeRPC.Network as X (HasNodeRPC, NodeRPCContext (..), nodeRPC, nodeRPCContext)
import Tezos.NodeRPC.Types as X

-- runNodeRPCT :: (HasNodeRPC r , AsRpcError e) => NodeRPCContext -> ReaderT (ExceptT m) a -> m a
-- runNodeRPCT c xs = runReaderT (runExceptT xs) c
