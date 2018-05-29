{-# LANGUAGE DeriveGeneric #-}

module Common.Script where

import Data.Void
import Data.Semigroup
import Common.Micheline
import GHC.Generics
import Data.Typeable
import Common.TezosBinary
import Data.Attoparsec.ByteString ((<?>))

data Script = Script -- ^ parameters: Script_repr.expr option ;
  { _script_code :: Node Micheline_V1_Prim
  , _script_storage :: Node Micheline_V1_Prim
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance TezosBinary Script where
  parseBinary = (Script <$> parseBinary <*> parseBinary) <?> "Script"

  encodeBinary (Script c s) = encodeBinary c <> encodeBinary s

