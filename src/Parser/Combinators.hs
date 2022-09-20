{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parser.Combinators where

import Data.List.NonEmpty qualified
import Parser
import Prelude qualified

void :: Prelude.Monad m => ParserT c t e m a -> ParserT c t e m ()
void p = p >> pure ()

optional :: Prelude.Monad m => ParserT 'Consuming t e m a -> ParserT 'Unknown t e m (Prelude.Maybe a)
optional p = catch (Prelude.Just <$> p) (\_ -> pure Prelude.Nothing)

many :: Prelude.Monad m => ParserT 'Consuming t e m a -> ParserT 'Unknown t e m [a]
many p = Data.List.NonEmpty.toList <$> some p <|> pure []

some :: Prelude.Monad m => ParserT 'Consuming t e m a -> ParserT 'Consuming t e m (Data.List.NonEmpty.NonEmpty a)
some p = do
  x <- p
  xs <- many p
  pure (x Data.List.NonEmpty.:| xs)
