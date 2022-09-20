{-# LANGUAGE NoImplicitPrelude #-}

module Control.Alternative where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import Data.Kind
import qualified Prelude

type Alternative :: (Type -> Type) ->Constraint
class Alternative m where
  (<|>) :: m a -> m a -> m a

instance (Prelude.Monad (t m), MonadTransControl t, Prelude.Monad m, Alternative m) => Alternative (Elevator t m) where
  (<|>) x y = (restoreT Prelude.. Prelude.pure Prelude.=<<) Prelude.$ liftWith Prelude.$ \runT -> runT x <|> runT y
