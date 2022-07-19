{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Failable where

import Control.Alternative
import qualified Control.Monad.Error.Class as C
import Data.Kind
import GHC.Generics
import qualified Prelude

-- * Failable

data Failable :: Type -> Type -> Type where
    Failed :: e -> Failable e a
    Succeeding :: a -> Failable e a
  deriving stock (Prelude.Eq, Generic, Prelude.Ord, Prelude.Read, Prelude.Show)
  deriving stock (Prelude.Functor)

instance Prelude.Applicative (Failable e) where
    pure = Succeeding
    f <*> x =
      case f of
        Failed e -> Failed e
        Succeeding g -> Prelude.fmap g x

instance Prelude.Monad (Failable e) where
    a >>= f =
      case a of
        Failed e -> Failed e
        Succeeding b -> f b

instance Prelude.Semigroup e => Alternative (Failable e) where
    x <|> y =
      case x of
        val@(Succeeding _) -> val
        Failed xErr ->
          case y of
            val@(Succeeding _) -> val
            Failed yErr -> Failed (xErr Prelude.<> yErr)

instance C.MonadError e (Failable e) where
    throwError = Failed
    catchError e f =
      case e of
        Failed err -> f err
        val@(Succeeding _) -> val
