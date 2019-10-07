{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Tezos.V005.NodeRPC.CrossCompat where
  
import Control.Lens (Getter, to, (^.), lens, (.~), view)
import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value(Object), (.:))
import qualified Tezos.V004.Types as V004
import qualified Tezos.V005.Types as V005
import Tezos.V005.Block (BlockLike(..), HasProtocolHash(..), HasChainId(..), HasBlockHeaderFull(..), HasBlockMetadata(..))
import Tezos.V005.BalanceUpdate (HasBalanceUpdates(..))

-- These kinds of unions here only work with these specific schema changes
-- between V004 and V005 don't have a subset relationship. If a version
-- change just added/ removed fields, you could easily end up parsing into
-- the smaller set because it succeeds unless you order the alternatives in
-- decending order of field set size.
--
-- If we're already apologising for shortcomings here know that this method
-- should be replaced with some method in the NodeRPC that forces you to
-- know the protocol before asking for a block. This would force everyone
-- to do a block header call before getting a block, but this is probably
-- better long term so we don't have any annoying confusing errors.
--
-- Also these strings of Parser alternatives have really bad error
-- messages, because you end up just getting the error message of the last
-- parser if it bottoms out of all of them. If we continue with this encoding
-- we probably want a better combinator than <|>

-- See the comments at the top of V005.Account to see what changed.

data AccountCrossCompat
  = AccountV004 V004.Account
  | AccountV005 V005.Account
  deriving (Eq, Show)

-- The user needs a way to get the delegate PKH out regardless of version.
-- This forces both accounts to agree on their PKH type. In this case they
-- currently do agree and this is probably OK for now.
accountCrossCompat_delegatePkh :: Getter AccountCrossCompat (Maybe V004.PublicKeyHash)
accountCrossCompat_delegatePkh = to $ \case
  AccountV004 a -> a ^. V004.account_delegate . V004.accountDelegate_value
  AccountV005 a -> a ^. V005.account_delegate

instance FromJSON AccountCrossCompat where
  parseJSON jv
    = AccountV005 <$> parseJSON jv
    <|> AccountV004 <$> parseJSON jv

instance ToJSON AccountCrossCompat where
  toJSON a = case a of
    AccountV004 a4 -> toJSON a4
    AccountV005 a5 -> toJSON a5

data BlockCrossCompat
  = BlockV004 V004.Block
  | BlockV005 V005.Block
  deriving (Eq, Show)

blockCrossCata :: (V004.Block -> a) -> (V005.Block -> a) -> BlockCrossCompat -> a
blockCrossCata f4 f5 = \case
  BlockV004 b4 -> f4 b4
  BlockV005 b5 -> f5 b5

instance HasProtocolHash BlockCrossCompat where
  protocolHash = lens
    (blockCrossCata (view protocolHash) (view protocolHash))
    (\b ph -> blockCrossCata
      (BlockV004 . (protocolHash .~ ph))
      (BlockV005 . (protocolHash .~ ph))
      b)
      
instance HasChainId BlockCrossCompat where
  chainIdL = lens
    (blockCrossCata (view chainIdL) (view chainIdL))
    (\b ph -> blockCrossCata
      (BlockV004 . (chainIdL .~ ph))
      (BlockV005 . (chainIdL .~ ph))
      b)

instance HasBlockMetadata BlockCrossCompat where
  blockMetadata = lens
    (blockCrossCata (view blockMetadata) (view blockMetadata))
    (\b ph -> blockCrossCata
      (BlockV004 . (blockMetadata .~ ph))
      (BlockV005 . (blockMetadata .~ ph))
      b)
      
instance HasBlockHeaderFull BlockCrossCompat where
  blockHeaderFull = lens
    (blockCrossCata (view blockHeaderFull) (view blockHeaderFull))
    (\b ph -> blockCrossCata
      (BlockV004 . (blockHeaderFull .~ ph))
      (BlockV005 . (blockHeaderFull .~ ph))
      b)
      
instance BlockLike BlockCrossCompat where
  hash = lens
    (blockCrossCata (view hash) (view hash))
    (\b ph -> blockCrossCata (BlockV004 . (hash .~ ph)) (BlockV005 . (hash .~ ph)) b)
  predecessor = lens
    (blockCrossCata (view predecessor) (view predecessor))
    (\b ph -> blockCrossCata (BlockV004 . (predecessor .~ ph)) (BlockV005 . (predecessor .~ ph)) b)
  level = lens
    (blockCrossCata (view level) (view level))
    (\b ph -> blockCrossCata (BlockV004 . (level .~ ph)) (BlockV005 . (level .~ ph)) b)
  fitness = lens
    (blockCrossCata (view fitness) (view fitness))
    (\b ph -> blockCrossCata (BlockV004 . (fitness .~ ph)) (BlockV005 . (fitness .~ ph)) b)
  timestamp = lens
    (blockCrossCata (view timestamp) (view timestamp))
    (\b ph -> blockCrossCata (BlockV004 . (timestamp .~ ph)) (BlockV005 . (timestamp .~ ph)) b)

instance HasBalanceUpdates BlockCrossCompat where
  balanceUpdates f b = blockCrossCata (fmap BlockV004 . balanceUpdates f) (fmap BlockV005 . balanceUpdates f) b
      
instance FromJSON BlockCrossCompat where
  parseJSON jv@(Object o) = do
    pv :: String <- o .: "protocol"
    case pv of
      -- TODO: This ought to be better.
      "PsBabyM1eUXZseaJdmXFApDSBqj8YBfwELoxZHHW77EMcAbbwAS" -> BlockV005 <$> parseJSON jv -- V005 Bugfix
      "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU" -> BlockV005 <$> parseJSON jv -- V005 / Babylon
      "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd" -> BlockV004 <$> parseJSON jv -- V004 / Athens
      "PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP" -> BlockV004 <$> parseJSON jv -- V003
      "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt" -> BlockV004 <$> parseJSON jv -- V002
      "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY" -> BlockV004 <$> parseJSON jv -- V001
      -- We parse anything that we don't know on V005. This may catch some weird genesis blocks
      -- and it'll catch newer protocols if we are too slow to update.
      _ -> BlockV005 <$> parseJSON jv

  parseJSON _ = mzero
    
instance ToJSON BlockCrossCompat where
  toJSON a = case a of
    BlockV004 b4 -> toJSON b4
    BlockV005 b5 -> toJSON b5
    
-- MichelinePrimitive
-- This added a new primitive "APPLY" into this enum. I've made the executive decision
-- to just treat all blocks as V005 since the only time we'd care is if we were creating
-- a new Michelson expression, using APPLY and somehow wanting to write pre-babylon...
