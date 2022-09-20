{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser where

import qualified Control.Alternative
import qualified Control.Monad.Error.Class as C
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import qualified Control.Monad.Trans.State as T
import Data.Failable
import Data.Kind
import Control.Monad.Trans.Failable
import GHC.Generics

import qualified Data.Functor
import qualified Prelude

-- * Consumption

data Consumption :: Type where
    Unknown :: Consumption
    Consuming :: Consumption

type family a && b where
    'Unknown && x = 'Unknown
    'Consuming && x = x
    x && 'Unknown = 'Unknown
    x && 'Consuming = x
    x && x = x

type family a || b where
    'Unknown || x = x
    'Consuming || x = 'Consuming
    x || 'Unknown = x
    x || 'Consuming = 'Consuming
    x || x = x

-- * Error

data Error :: Type -> Type where
    ErrorCustom :: e -> Error e
    ErrorInputEmpty :: Error e
    ErrorInputLeft :: Error e
    ErrorAppend :: Error e -> Error e -> Error e
  deriving stock (Prelude.Eq, Generic, Prelude.Ord, Prelude.Read, Prelude.Show)

instance Prelude.Semigroup (Error e) where
    (<>) = ErrorAppend

-- * Parser

-- | This parser backtracks on failure.
-- When `c` is `True`, then this parser is guarenteed to consume input on success.
type ParserT :: Consumption -- ^ c
             -> Type -- ^ t
             -> Type -- ^ e
             -> (Type -> Type) -- ^ m
             -> Type -- ^ a
             -> Type
newtype ParserT c t e m a = ParserT { unParserT :: T.StateT [t] (FailableT (Error e) m) a }
  deriving (MonadTrans, MonadTransControl) via (ComposeT (T.StateT [t]) (FailableT (Error e)))
  deriving Prelude.Functor

parse :: ParserT c t e m a -> [t] -> m (Failable (Error e) (a, [t]))
parse parser tokens = runFailableT (T.runStateT (unParserT parser) tokens)

--combine :: Prelude.Monad m => ParserT Consuming a e m [b] -> ParserT Consuming b e m c -> ParserT Consuming a e m c
--combine x y = ParserT Prelude.$ T.StateT Prelude.$ \ as -> do
--  (bs, as') <- restoreT (parse x as)
--  (c, bs') <- restoreT (parse y bs)
--  case bs' of
--    [] -> Prelude.pure (c, as')
--    _rest -> C.throwError ErrorInputLeft

consume :: Prelude.Monad m => ParserT 'Consuming t e m t
consume = do
  tokens <- ParserT @'Consuming T.get
  case tokens of
   [] -> throw ErrorInputEmpty
   t:ts -> do
     ParserT @'Unknown (T.put ts)
     pure t

end :: Prelude.Monad m => ParserT 'Unknown t e m ()
end = do
  tokens <- ParserT @'Unknown T.get
  case tokens of
    [] -> pure ()
    _rest -> throw ErrorInputLeft

(<$>) :: Prelude.Functor m => (a -> b) -> ParserT c t e m a -> ParserT c t e m b
(<$>) = (Data.Functor.<$>)

(<&>) :: Prelude.Functor m => ParserT c t e m a -> (a -> b) -> ParserT c t e m b
(<&>) = (Data.Functor.<&>)

($>) :: Prelude.Functor m => ParserT c t e m a -> b -> ParserT c t e m b
($>) = (Data.Functor.$>)

(<$) :: Prelude.Functor m => a -> ParserT c t e m b -> ParserT c t e m a
(<$) = (Data.Functor.<$)

pure :: Prelude.Monad m => a -> ParserT 'Unknown t e m a
pure = ParserT Prelude.. Prelude.pure

(<*>) :: Prelude.Monad m => ParserT c1 t e m (a -> b) -> ParserT c2 t e m a -> ParserT (c1 || c2) t e m b
ParserT x <*> ParserT y = ParserT (x Prelude.<*> y)

(<**>) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m (a -> b) -> ParserT (c2 || c1) t e m b
(<**>) = Prelude.flip (<*>)

(*>) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m b -> ParserT (c1 || c2) t e m b
x *> y = (\ _a b -> b) <$> x <*> y

(<*) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m b -> ParserT (c1 || c2) t e m a
x <* y = (\ a _b -> a) <$> x <*> y

(>>=) :: Prelude.Monad m => ParserT c1 t e m a -> (a -> ParserT c2 t e m b) -> ParserT (c1 || c2) t e m b
x >>= f = ParserT (unParserT x Prelude.>>= unParserT Prelude.. f)

(=<<) :: Prelude.Monad m => (a -> ParserT c1 t e m b) -> ParserT c2 t e m a -> ParserT (c2 || c1) t e m b
(=<<) = Prelude.flip (>>=)

(>>) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m b -> ParserT (c1 || c2) t e m b
x >> y = x >>= Prelude.const y

(<<) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m b -> ParserT (c2 || c1) t e m a
(<<) = Prelude.flip (>>)

(<|>) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m a -> ParserT (c1 && c2) t e m a
ParserT x <|> ParserT y = ParserT (descend (Ascend x Control.Alternative.<|> Ascend y))

throw :: Prelude.Monad m => Error e -> ParserT c t e m a
throw e = ParserT (descend (C.throwError e))

catch :: Prelude.Monad m => ParserT c1 t e m a -> (Error e -> ParserT c2 t e m a) -> ParserT (c1 && c2) t e m a
catch throwing catching = ParserT (descend (C.catchError (Ascend (unParserT throwing)) (Ascend Prelude.. unParserT Prelude.. catching)))
