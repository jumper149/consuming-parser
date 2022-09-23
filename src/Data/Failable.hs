module Data.Failable where

import Control.Monad.Error.Class qualified as C
import Data.Kind
import GHC.Generics

type Failable :: Type -> Type -> Type -- TODO: This is redundant.
data Failable :: Type -> Type -> Type where
  Failed :: e -> Failable e a
  Succeeding :: a -> Failable e a
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving stock (Functor)

instance Applicative (Failable e) where
  pure = Succeeding
  f <*> x =
    case f of
      Failed e -> Failed e
      Succeeding g -> fmap g x

instance Monad (Failable e) where
  a >>= f =
    case a of
      Failed e -> Failed e
      Succeeding b -> f b

instance C.MonadError e (Failable e) where
  throwError = Failed
  catchError e f =
    case e of
      Failed err -> f err
      val@(Succeeding _) -> val

chain :: (e -> e -> e) -> Failable e a -> Failable e a -> Failable e a
chain combineError x y =
  case x of
    val@(Succeeding _) -> val
    Failed xErr ->
      case y of
        val@(Succeeding _) -> val
        Failed yErr -> Failed (xErr `combineError` yErr)
