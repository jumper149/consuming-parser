{-# LANGUAGE QualifiedDo #-}

module Parser.Combinators where

import Parser.Core qualified as P
import Parser.Core.Consumption qualified as P
import Parser.Core.Error qualified as P

import Data.Bifunctor
import Data.List.NonEmpty qualified as NE

-- * Primitives

satisfy :: Monad m => (t -> Bool) -> P.ParserT P.Consuming t e m t
satisfy p =
  P.do
    x <- P.token
    if p x
      then P.pure x
      else P.throw P.ErrorUnexpectedToken
    P.<|> P.throw P.ErrorSatisfy

match :: (Eq t, Monad m) => t -> P.ParserT P.Consuming t e m ()
match t = void (satisfy (== t)) P.<|> P.throw P.ErrorEqual

oneOf :: (Foldable f, Eq t, Monad m) => f t -> P.ParserT P.Consuming t e m t
oneOf ts = satisfy (`elem` ts) P.<|> P.throw P.ErrorOneOf

rest :: Monad m => P.ParserT P.Unknown t e m [t]
rest =
  P.do
    x <- P.token
    xs <- rest
    P.pure $ x : xs
    P.<|> P.pure []

-- * Combinators

void :: Monad m => P.ParserT c t e m a -> P.ParserT c t e m ()
void p = p P.>> P.pure ()

optional :: Monad m => P.ParserT P.Consuming t e m a -> P.ParserT P.Unknown t e m (Maybe a)
optional p = P.catch (Just P.<$> p) (\_ -> P.pure Nothing)

asum :: Monad m => NE.NonEmpty (P.ParserT c t e m a) -> P.ParserT c t e m a
asum (p NE.:| ps) =
  case ps of
    [] -> p
    q : qs -> asum $ (p P.<|> q) NE.:| qs

many :: Monad m => P.ParserT P.Consuming t e m a -> P.ParserT P.Unknown t e m [a]
many p = NE.toList P.<$> some p P.<|> P.pure []

some :: Monad m => P.ParserT P.Consuming t e m a -> P.ParserT P.Consuming t e m (NE.NonEmpty a)
some p = P.do
  x <- p
  xs <- many p
  P.pure (x NE.:| xs)

manyTill ::
  Monad m =>
  P.ParserT P.Consuming t e m a ->
  P.ParserT P.Consuming t e m b ->
  P.ParserT P.Consuming t e m ([a], b)
manyTill p c = ([],) P.<$> c `P.catch` \_err -> first NE.toList P.<$> someTill p c

someTill ::
  Monad m =>
  P.ParserT P.Consuming t e m a ->
  P.ParserT P.Consuming t e m b ->
  P.ParserT P.Consuming t e m (NE.NonEmpty a, b)
someTill p c = P.do
  x <- p
  (xs, y) <- manyTill p c
  P.pure (x NE.:| xs, y)
