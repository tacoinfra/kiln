{-# LANGUAGE DeriveGeneric #-}

module Common.Script where

import Common.Micheline
import Common.TezosBinary
import Data.Attoparsec.ByteString ((<?>))
import Data.Semigroup
import Data.Typeable
import GHC.Generics

-- parameters: Script_repr.expr option ;
data Script = Script
  { _script_code :: Node Micheline_V1_Prim
  , _script_storage :: Node Micheline_V1_Prim
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance TezosBinary Script where
  parseBinary = (Script <$> parseBinary <*> parseBinary) <?> "Script"

  encodeBinary (Script c s) = encodeBinary c <> encodeBinary s
