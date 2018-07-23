{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tezos.NodeRPC
  ( module Tezos.NodeRPC.Types
  , module Tezos.NodeRPC.Network
  ) where

import Tezos.NodeRPC.Types
import Tezos.NodeRPC.Network
  ( nodeRPC
  , HasNodeRPC , nodeRPCContext , NodeRPCContext(..)
  )

-- runNodeRPCT :: (HasNodeRPC r , AsRpcError e) => NodeRPCContext -> ReaderT (ExceptT m) a -> m a
-- runNodeRPCT c xs = runReaderT (runExceptT xs) c
