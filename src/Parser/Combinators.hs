{-# LANGUAGE QualifiedDo #-}

module Parser.Combinators where

import Data.List.NonEmpty qualified as NE
import Parser qualified as P

void :: Monad m => P.ParserT c t e m a -> P.ParserT c t e m ()
void p = p P.>> P.pure ()

optional :: Monad m => P.ParserT P.Consuming t e m a -> P.ParserT P.Unknown t e m (Maybe a)
optional p = P.catch (Just P.<$> p) (\_ -> P.pure Nothing)

terminal :: (Eq t, Monad m) => t -> P.ParserT P.Consuming t e m ()
terminal t =
  P.do
    x <- P.token
    if x == t
      then P.pure ()
      else P.throw P.ErrorUnexpectedToken
    P.<|> P.throw P.ErrorTerminal

many :: Monad m => P.ParserT P.Consuming t e m a -> P.ParserT P.Unknown t e m [a]
many p = NE.toList P.<$> some p P.<|> P.pure []

some :: Monad m => P.ParserT P.Consuming t e m a -> P.ParserT P.Consuming t e m (NE.NonEmpty a)
some p = P.do
  x <- p
  xs <- many p
  P.pure (x NE.:| xs)

manyUntil ::
  Monad m =>
  P.ParserT P.Consuming t e m a ->
  P.ParserT P.Unknown t e m b ->
  P.ParserT P.Unknown t e m ([a], b)
manyUntil p c = ([],) P.<$> c `P.catch` \_err -> (\(xs, y) -> (NE.toList xs, y)) P.<$> someUntil p c

someUntil ::
  Monad m =>
  P.ParserT P.Consuming t e m a ->
  P.ParserT P.Unknown t e m b ->
  P.ParserT P.Consuming t e m (NE.NonEmpty a, b)
someUntil p c = P.do
  x <- p
  (xs, y) <- manyUntil p c
  P.pure ((x NE.:| xs), y)
