{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}

module Parser.Combinators where

import qualified Data.List.NonEmpty
import Parser
import qualified Prelude

optional :: Prelude.Monad m => ParserT 'Consuming t e m a -> ParserT 'Unknown t e m (Prelude.Maybe a)
optional p = catch (Prelude.Just <$> p) (\ _ -> pure Prelude.Nothing)

many :: Prelude.Monad m => ParserT 'Consuming t e m a -> ParserT 'Unknown t e m [a]
many p = Data.List.NonEmpty.toList <$> some p <|> pure []

some :: Prelude.Monad m => ParserT 'Consuming t e m a -> ParserT 'Consuming t e m (Data.List.NonEmpty.NonEmpty a)
some p = do
    x <- p
    xs <- many p
    pure (x Data.List.NonEmpty.:| xs)
