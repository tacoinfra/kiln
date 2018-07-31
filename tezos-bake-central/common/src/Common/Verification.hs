{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Verification where

import Control.Lens (Prism')
import Control.Lens.TH (makePrisms)
import Data.Either.Validation
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import Common.Schema
import Tezos.Types
import Tezos.NodeRPC.Types

data ForkInfo = ForkInfo
  { _forkInfo_node :: Node
  , _forkInfo_forkStatus :: Either ForkStatus ()
  , _forkInfo_time :: UTCTime
  , _forkInfo_hash :: BlockHash
  } deriving (Eq, Ord, Show)

data ForkStatus
  = ForkStatus_TooNew
  | ForkStatus_TooOld
  | ForkStatus_Forked
  | ForkStatus_BadNode RpcError
  deriving (Eq, Ord, Show)

makePrisms ''ForkStatus

instance AsRpcError ForkStatus where
  asRpcError = _ForkStatus_BadNode

class AsForkStatus e where
  asForkStatus :: Prism' e ForkStatus

instance AsForkStatus ForkStatus where
  asForkStatus = id

showForkStatus :: ForkStatus -> Text
showForkStatus = T.pack . \case
  -- ForkStatus_Good -> "good"
  ForkStatus_TooNew -> "new"
  ForkStatus_TooOld -> "block not in chain"
  ForkStatus_Forked -> "forked"
  ForkStatus_BadNode _ -> "no response from node"

onBadForkState :: (ForkInfo -> a) -> ForkInfo -> Validation a ()
onBadForkState k fi = case _forkInfo_forkStatus fi of
  Left ForkStatus_TooOld -> Failure $ k fi {_forkInfo_forkStatus = Left ForkStatus_TooOld}
  Left ForkStatus_Forked -> Failure $ k fi {_forkInfo_forkStatus = Left ForkStatus_Forked}
  _ -> Success ()

showBadFork :: ForkInfo -> Error
showBadFork (ForkInfo node status bakedTime bakedHash) = Error bakedTime $ T.concat
          [ "node: ", maybe "" toBase58Text $ _node_identity node
          , "@", _node_address node
          , " BAKER STATE:" , either showForkStatus (const "good") status
          , " for block:", toBase58Text bakedHash
          , " @ ",  T.pack $ show bakedTime
          , "\n"
          ]

validateForkyBlocks :: Applicative f => ([ForkInfo] -> f ()) -> [ForkInfo] -> f ()
validateForkyBlocks f xs = case traverse (onBadForkState pure) xs of
  Success _ -> f []
  Failure bad -> f bad

