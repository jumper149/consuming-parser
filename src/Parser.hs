{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser where

import Control.Alternative qualified
import Control.Monad.Error.Class qualified as C
import Control.Monad.Identity qualified as Identity
import Control.Monad.Trans.Class qualified as C
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Failable
import Control.Monad.Trans.State qualified as T
import Data.Failable
import Data.Kind
import GHC.Generics
import Prelude qualified

-- * Consumption

type Consumption :: Type -- TODO: This is redundant.
data Consumption :: Type where
  Unknown :: Consumption
  Consuming :: Consumption

type (&&) :: Consumption -> Consumption -> Consumption
type family a && b where
  Unknown && _ = Unknown
  Consuming && x = x
  _ && Unknown = Unknown
  x && Consuming = x
  x && x = x

type (||) :: Consumption -> Consumption -> Consumption
type family a || b where
  Unknown || x = x
  Consuming || _ = Consuming
  x || Unknown = x
  _ || Consuming = Consuming
  x || x = x

-- * Error

type Error :: Type -> Type -- TODO: This is redundant.
data Error :: Type -> Type where
  ErrorCustom :: e -> Error e
  ErrorInputEmpty :: Error e
  ErrorInputLeft :: Error e
  ErrorUnexpectedToken :: Error e
  ErrorTerminal :: Error e
  ErrorAppend :: Error e -> Error e -> Error e
  deriving stock (Prelude.Eq, Generic, Prelude.Ord, Prelude.Read, Prelude.Show)

instance Prelude.Semigroup (Error e) where
  (<>) = ErrorAppend

-- * Parser

-- | This parser backtracks on failure.
-- When `c` is `True`, then this parser is guarenteed to consume input on success.
type ParserT ::
  Consumption -> -- c
  Type -> -- t
  Type -> -- e
  (Type -> Type) -> -- m
  Type -> -- a
  Type
newtype ParserT c t e m a = ParserT {unParserT :: T.StateT [t] (FailableT (Error e) m) a}

parseT :: ParserT c t e m a -> [t] -> m (Failable (Error e) (a, [t]))
parseT parser tokens = runFailableT (T.runStateT (unParserT parser) tokens)

type Parser ::
  Consumption -> -- c
  Type -> -- t
  Type -> -- e
  Type -> -- a
  Type
type Parser c t e a = ParserT c t e Identity.Identity a

parse :: Parser c t e a -> [t] -> Failable (Error e) (a, [t])
parse parser tokens = Identity.runIdentity (parseT parser tokens)

-- combine :: Prelude.Monad m => ParserT Consuming a e m [b] -> ParserT Consuming b e m c -> ParserT Consuming a e m c
-- combine x y = ParserT Prelude.$ T.StateT Prelude.$ \ as -> do
--  (bs, as') <- restoreT (parse x as)
--  (c, bs') <- restoreT (parse y bs)
--  case bs' of
--    [] -> Prelude.pure (c, as')
--    _rest -> C.throwError ErrorInputLeft

consume :: Prelude.Monad m => ParserT Consuming t e m t
consume = do
  tokens <- ParserT @Consuming T.get
  case tokens of
    [] -> throw ErrorInputEmpty
    t : ts -> do
      ParserT @Unknown (T.put ts)
      pure t

end :: Prelude.Monad m => ParserT Unknown t e m ()
end = do
  tokens <- ParserT @Unknown T.get
  case tokens of
    [] -> pure ()
    _rest -> throw ErrorInputLeft

lift :: Prelude.Monad m => m a -> ParserT c t e m a
lift x = ParserT (C.lift (C.lift x))

(<$>) :: Prelude.Functor m => (a -> b) -> ParserT c t e m a -> ParserT c t e m b
f <$> ParserT x = ParserT (f Prelude.<$> x)

(<&>) :: Prelude.Functor m => ParserT c t e m a -> (a -> b) -> ParserT c t e m b
(<&>) = Prelude.flip (<$>)

($>) :: Prelude.Functor m => ParserT c t e m a -> b -> ParserT c t e m b
x $> y = Prelude.const y <$> x

(<$) :: Prelude.Functor m => a -> ParserT c t e m b -> ParserT c t e m a
x <$ y = Prelude.const x <$> y

pure :: Prelude.Monad m => a -> ParserT Unknown t e m a
pure = ParserT Prelude.. Prelude.pure

(<*>) :: Prelude.Monad m => ParserT c1 t e m (a -> b) -> ParserT c2 t e m a -> ParserT (c1 || c2) t e m b
ParserT x <*> ParserT y = ParserT (x Prelude.<*> y)

(<**>) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m (a -> b) -> ParserT (c2 || c1) t e m b
(<**>) = Prelude.flip (<*>)

(*>) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m b -> ParserT (c1 || c2) t e m b
x *> y = (\_a b -> b) <$> x <*> y

(<*) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m b -> ParserT (c1 || c2) t e m a
x <* y = (\a _b -> a) <$> x <*> y

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

forget :: ParserT c t e m a -> ParserT Unknown t e m a
forget (ParserT x) = ParserT x
