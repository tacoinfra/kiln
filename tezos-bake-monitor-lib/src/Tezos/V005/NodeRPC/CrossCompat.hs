{-# LANGUAGE LambdaCase #-}
module Tezos.V005.NodeRPC.CrossCompat where
  
import Control.Lens (Getter, to, (^.))
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import qualified Tezos.V004.Types as V004
import qualified Tezos.V005.Types as V005

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

data Account
  = AccountV004 V004.Account
  | AccountV005 V005.Account
  deriving (Eq, Show)

-- The user needs a way to get the delegate PKH out regardless of version.
-- This forces both accounts to agree on their PKH type. In this case they
-- currently do agree and this is probably OK for now.
account_delegatePkh :: Getter Account (Maybe V004.PublicKeyHash)
account_delegatePkh = to $ \case
  AccountV004 a -> a ^. V004.account_delegate . V004.accountDelegate_value
  AccountV005 a -> a ^. V005.account_delegate

instance FromJSON Account where
  parseJSON jv
    = AccountV005 <$> parseJSON jv
    <|> AccountV004 <$> parseJSON jv

instance ToJSON Account where
  toJSON a = case a of
    AccountV004 a4 -> toJSON a4
    AccountV005 a5 -> toJSON a5

-- We don't actually need this cross compat because there is nothing at the
-- top level RPC that just gets out an operation. I'll delete this when I add
-- the cross compat block.
data Operation
  = OperationV004 V004.Operation
  | OperationV005 V005.Operation
  deriving (Eq, Show)

instance FromJSON Operation where
  parseJSON jv
    = OperationV004 <$> parseJSON jv
    <|> OperationV005 <$> parseJSON jv

instance ToJSON Operation where
  toJSON a = case a of
    OperationV004 a4 -> toJSON a4
    OperationV005 a5 -> toJSON a5

-- TODO: Add a cross compat block and add it to the NodeRPC returns

-- MichelinePrimitive
-- This added a new primitive "APPLY" into this enum. I've made the executive decision
-- to just treat all blocks as V005 since the only time we'd care is if we were creating
-- a new Michelson expression, using APPLY and somehow wanting to write pre-babylon...
