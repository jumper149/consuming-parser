{-# LANGUAGE RebindableSyntax #-}

module Parser.Core where

import Parser.Consumption
import Parser.Error

import Control.Applicative qualified
import Control.Monad.Error.Class qualified as C
import Control.Monad.Identity qualified
import Control.Monad.State.Class qualified as C
import Control.Monad.Trans.Class qualified as C
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Except qualified as T
import Control.Monad.Trans.State qualified as T
import Data.Kind
import Prelude qualified

-- * Parser

-- | A monad transformer for a monadic parser.
--
-- This parser backtracks on failure.
--
-- When `c` is 'Consuming', then this parser is guarenteed to consume input on success.
-- When `c` is 'Unknown', the parser might or might not consume input on success.
type ParserT ::
  Consumption -> -- c
  Type -> -- t
  Type -> -- e
  (Type -> Type) -> -- m
  Type -> -- a
  Type
newtype ParserT c t e m a = ParserT {unParserT :: ComposeT (T.StateT [t]) (T.ExceptT (Trace e)) m a}

parseT :: ParserT c t e m a -> [t] -> m (Prelude.Either (Trace e) (a, [t]))
parseT parser tokens = runComposeT (`T.runStateT` tokens) T.runExceptT (unParserT parser)

-- | A pure monadic parser using `ParserT`.
type Parser ::
  Consumption -> -- c
  Type -> -- t
  Type -> -- e
  Type -> -- a
  Type
type Parser c t e a = ParserT c t e Control.Monad.Identity.Identity a

parse :: Parser c t e a -> [t] -> Prelude.Either (Trace e) (a, [t])
parse parser tokens = Control.Monad.Identity.runIdentity (parseT parser tokens)

-- * Core primitives

token :: Prelude.Monad m => ParserT Consuming t e m t
token = do
  tokens <- ParserT @Consuming C.get
  case tokens of
    [] -> throw ErrorInputEmpty
    t : ts -> do
      _ <- ParserT @Unknown (C.put ts)
      pure t

end :: Prelude.Monad m => ParserT Unknown t e m ()
end = do
  tokens <- ParserT @Unknown C.get
  case tokens of
    [] -> pure ()
    _rest -> throw ErrorInputLeft

lift :: Prelude.Monad m => m a -> ParserT c t e m a
lift = ParserT Prelude.. C.lift

-- | '(<$>)' can be expressed using '(>>=)' and 'pure', but then it would enforce the constraint @'Prelude.Monad' m@
(<$>) :: Prelude.Functor m => (a -> b) -> ParserT c t e m a -> ParserT c t e m b
f <$> ParserT x = ParserT (f Prelude.<$> x)

pure :: Prelude.Monad m => a -> ParserT Unknown t e m a
pure = ParserT Prelude.. Prelude.pure

(>>=) :: Prelude.Monad m => ParserT c1 t e m a -> (a -> ParserT c2 t e m b) -> ParserT (c1 || c2) t e m b
x >>= f = ParserT (unParserT x Prelude.>>= unParserT Prelude.. f)

(<|>) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m a -> ParserT (c1 && c2) t e m a
ParserT (ComposeT x) <|> ParserT (ComposeT y) = ParserT (ComposeT (descend ((Control.Applicative.<|>) (Ascend x) (Ascend y))))

throw :: Prelude.Monad m => Error e -> ParserT c t e m a
throw e = ParserT (C.throwError (TracePoint e))

catch :: Prelude.Monad m => ParserT c1 t e m a -> (Trace e -> ParserT c2 t e m a) -> ParserT (c1 && c2) t e m a
catch throwing catching = ParserT (C.catchError (unParserT throwing) (unParserT Prelude.. catching))

forget :: ParserT c t e m a -> ParserT Unknown t e m a
forget = ParserT Prelude.. unParserT

-- ** Convenient functions

-- *** Functor

(<&>) :: Prelude.Functor m => ParserT c t e m a -> (a -> b) -> ParserT c t e m b
(<&>) = Prelude.flip (<$>)

($>) :: Prelude.Functor m => ParserT c t e m a -> b -> ParserT c t e m b
x $> y = y <$ x

(<$) :: Prelude.Functor m => a -> ParserT c t e m b -> ParserT c t e m a
x <$ y = Prelude.const x <$> y

-- *** Applicative

(<*>) :: Prelude.Monad m => ParserT c1 t e m (a -> b) -> ParserT c2 t e m a -> ParserT (c1 || c2) t e m b
x <*> y = x >>= \f -> f <$> y

(<**>) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m (a -> b) -> ParserT (c2 || c1) t e m b
(<**>) = Prelude.flip (<*>)

(*>) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m b -> ParserT (c1 || c2) t e m b
x *> y = (\_a b -> b) <$> x <*> y

(<*) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m b -> ParserT (c1 || c2) t e m a
x <* y = Prelude.const <$> x <*> y

-- *** Monad

(=<<) :: Prelude.Monad m => (a -> ParserT c1 t e m b) -> ParserT c2 t e m a -> ParserT (c2 || c1) t e m b
(=<<) = Prelude.flip (>>=)

(>>) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m b -> ParserT (c1 || c2) t e m b
x >> y = x >>= Prelude.const y

(<<) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m b -> ParserT (c2 || c1) t e m a
(<<) = Prelude.flip (>>)

-- * Composition

-- compose :: Prelude.Monad m => ParserT Consuming a e m [b] -> ParserT Consuming b e m c -> ParserT Consuming a e m c
-- compose x y = ParserT Prelude.$ T.StateT Prelude.$ \ as -> do
-- (bs, as') <- restoreT (parse x as)
-- (c, bs') <- restoreT (parse y bs)
-- case bs' of
--   [] -> Prelude.pure (c, as')
--   _rest -> C.throwError ErrorInputLeft
