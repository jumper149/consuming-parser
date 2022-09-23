{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Trans.Failable where

import Control.Alternative
import Control.Monad.Error.Class qualified as C
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Data.Failable
import Data.Functor.Compose qualified
import Data.Kind

type FailableT :: Type -> (Type -> Type) -> Type -> Type
newtype FailableT e m a = FailableT {runFailableT :: m (Failable e a)}

deriving via Data.Functor.Compose.Compose m (Failable e) instance Functor m => Functor (FailableT e m)
deriving via Data.Functor.Compose.Compose m (Failable e) instance Applicative m => Applicative (FailableT e m)

instance Monad m => Monad (FailableT e m) where
  ma >>= fma =
    FailableT
      ( runFailableT ma
          >>= ( \case
                  Failed e -> pure (Failed e)
                  Succeeding x -> runFailableT (fma x)
              )
      )

instance MonadTrans (FailableT e) where
  lift ma = FailableT (fmap pure ma)

instance MonadTransControl (FailableT e) where
  type StT (FailableT e) a = Failable e a
  liftWith f = FailableT (fmap pure (f runFailableT))
  restoreT = FailableT

instance (Applicative m, Semigroup e) => Alternative (FailableT e m) where
  FailableT x <|> FailableT y =
    FailableT (fmap (<|>) x <*> y)

instance Monad m => C.MonadError e (FailableT e m) where
  throwError e = FailableT (pure (C.throwError e))
  catchError e f =
    FailableT
      ( runFailableT e
          >>= ( \case
                  Failed err -> runFailableT (f err)
                  val@(Succeeding _) -> pure val
              )
      )

deriving via
  FailableT e ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    Monad (t2 m) => C.MonadError e (ComposeT (FailableT e) t2 m)
