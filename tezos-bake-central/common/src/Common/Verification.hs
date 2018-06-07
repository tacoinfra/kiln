{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Verification where

import Data.Either.Validation
import Data.Text (Text)
import qualified Data.Text as T

import Common.Schema
import Common.TaggedHash (toBase58Text)

data ForkInfoF e = ForkInfo
  { _forkInfo_node :: Node
  , _forkInfo_forkStatus :: ForkStatusF e
  , _forkInfo_baked :: Baked
  }

data ForkStatusF e
  = ForkStatus_Good
  | ForkStatus_TooNew
  | ForkStatus_TooOld
  | ForkStatus_Forked
  | ForkStatus_BadNode e

showForkStatus :: ForkStatusF e -> Text
showForkStatus = T.pack . \case
  ForkStatus_Good -> "good"
  ForkStatus_TooNew -> "new"
  ForkStatus_TooOld -> "block not in chain"
  ForkStatus_Forked -> "forked"
  ForkStatus_BadNode _ -> "no response from node"

onBadForkState :: (ForkInfoF e -> a) -> ForkInfoF e -> Validation a ()
onBadForkState k fi = case _forkInfo_forkStatus fi of
  ForkStatus_TooOld -> Failure $ k fi
  ForkStatus_Forked -> Failure $ k fi
  _ -> Success ()

showBadFork :: ForkInfoF e -> [Error]
showBadFork (ForkInfo node status baked) = pure $ Error (_event_time baked) $ T.concat
          [ "node: ", _node_address node
          , " BAKER STATE:" , showForkStatus status
          , " for block:", toBase58Text $ _bakedEvent_hash $ _event_detail baked
          , " @ ",  T.pack $ show $ _event_time baked
          , "\n"
          ]

validateForkyBlocks :: Applicative f => ([Error] -> f ()) -> [ForkInfoF e] -> f ()
validateForkyBlocks f xs = case traverse (onBadForkState (showBadFork)) xs of
  Success _ -> pure ()
  Failure bad -> f bad


