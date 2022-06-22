module ExtraPrelude
  ( CallStack
  , Coercible
  , Compose (..)
  , Const (..)
  , First (..)
  , Generic
  , HasCallStack
  , Identity (..)
  , Iso
  , Lens
  , Lens'
  , MonadError
  , MonadIO (liftIO)
  , MonadReader (ask)
  , MonoidalMap
  , NonEmpty (..)
  , Option (..)
  , Prism
  , Prism'
  , Proxy (..)
  , Semigroup
  , Set
  , Text
  , Typeable

  , _1
  , _2
  , _3
  , _Just
  , _Left
  , _Nothing
  , _Right
  , asks
  , bool
  , callStack
  , catMaybes
  , coerce
  , def
  , find
  , first
  , fix
  , fold
  , foldM
  , for
  , for_
  , fromMaybe
  , guard
  , ifor
  , ifor_
  , isJust
  , isLeft
  , isNothing
  , isRight
  , itraverse
  , join
  , liftA2
  , liftA3
  , listToMaybe
  , on
  , prettyCallStack
  , preview
  , runExceptT
  , runReaderT
  , second
  , throwError
  , toList
  , traverse_
  , unless
  , view
  , views
  , void
  , when

  , (?~)
  , (.~)
  , (***)
  , (&)
  , (&&&)
  , (%~)
  , (^?)
  , (^.)
  , (<&>)
  , (<<<)
  , (<=<)
  , (<>)
  , (<|>)
  , (>=>)
  , (>>>)
  , ($>)
  , (<<$>>)

  , safeSucc
  , tshow
  , when'
  , whenJust
  , whenM
  ) where

import Control.Applicative (Const (..), liftA2, liftA3, (<|>))
import Control.Arrow ((***), (&&&))
import Control.Category ((<<<), (>>>))
import Control.Lens (Iso, Lens, Lens', Prism, Prism', ifor, ifor_, itraverse, preview, view,
                     views, _1, _2, _3, _Just, _Left, _Nothing, _Right,
                     (?~), (.~), (%~), (^.), (^?), (<&>))
import Control.Monad (foldM, guard, join, when, unless, (<=<), (>=>))
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), asks, runReaderT)
import Data.Bifunctor (first, second)
import Data.Bool (bool)
import Data.Coerce (Coercible, coerce)
import Data.Default (def)
import Data.Either (isLeft, isRight)
import Data.Foldable (find, fold, for_, toList, traverse_)
import Data.Function (fix, on, (&))
import Data.Functor (void, ($>))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Monoidal (MonoidalMap)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Semigroup (First (..), Option (..), Semigroup, (<>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import qualified Data.Text as T

safeSucc :: (Eq a, Enum a, Bounded a) => a -> Maybe a
safeSucc a = if a /= maxBound then Just (succ a) else Nothing

tshow :: Show a => a -> Text
tshow = T.pack . show

whenJust :: (Applicative m, Monoid a) => Maybe t -> (t -> m a) -> m a
whenJust Nothing _ = pure mempty
whenJust (Just x) f = f x

whenM :: (Applicative m, Monoid b) => Bool -> m b -> m b
whenM x true = if x then true else pure mempty

when' :: (Monad m, Monoid b) => m Bool -> m b -> m b
when' x true = x >>= \v -> if v then true else pure mempty

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
