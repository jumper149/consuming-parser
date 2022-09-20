{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Trans.Failable where

import Control.Alternative
import qualified Control.Monad.Error.Class as C
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Data.Failable
import qualified Data.Functor.Compose
import Data.Kind
import qualified Prelude

type FailableT :: Type -> (Type -> Type) -> Type -> Type
newtype FailableT e m a = FailableT {runFailableT :: m (Failable e a)}

deriving via Data.Functor.Compose.Compose m (Failable e) instance Prelude.Functor m => Prelude.Functor (FailableT e m)
deriving via Data.Functor.Compose.Compose m (Failable e) instance Prelude.Applicative m => Prelude.Applicative (FailableT e m)

instance Prelude.Monad m => Prelude.Monad (FailableT e m) where
  ma >>= fma =
    FailableT
      ( runFailableT ma
          Prelude.>>= ( \case
                          Failed e -> Prelude.pure (Failed e)
                          Succeeding x -> runFailableT (fma x)
                      )
      )

instance MonadTrans (FailableT e) where
  lift ma = FailableT (Prelude.fmap Prelude.pure ma)

instance MonadTransControl (FailableT e) where
  type StT (FailableT e) a = Failable e a
  liftWith f = FailableT (Prelude.fmap Prelude.pure (f runFailableT))
  restoreT = FailableT

instance (Prelude.Applicative m, Prelude.Semigroup e) => Alternative (FailableT e m) where
  FailableT x <|> FailableT y =
    FailableT (Prelude.fmap (<|>) x Prelude.<*> y)

instance Prelude.Monad m => C.MonadError e (FailableT e m) where
  throwError e = FailableT (Prelude.pure (C.throwError e))
  catchError e f =
    FailableT
      ( runFailableT e
          Prelude.>>= ( \case
                          Failed err -> runFailableT (f err)
                          val@(Succeeding _) -> Prelude.pure val
                      )
      )
