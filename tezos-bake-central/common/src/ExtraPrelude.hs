module ExtraPrelude
  ( Generic
  , MonoidalMap
  , Text
  , Typeable

  , (<=<)
  , (<>)
  , (>=>)
  , ($>)
  , for
  , for_
  , fromMaybe
  , isJust
  , isRight
  , toList
  , void
  ) where

import Control.Monad ((<=<), (>=>))
import Data.Either (isRight)
import Data.Foldable (for_, toList)
import Data.Functor (void, ($>))
import Data.Map.Monoidal (MonoidalMap)
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Traversable (for)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
