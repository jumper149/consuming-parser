{-# LANGUAGE QualifiedDo #-}

module Parser.Token.Char where

import Data.List.NonEmpty qualified as NonEmpty
import Parser qualified as P
import Parser.Combinators qualified as P
import Parser.Error qualified as P

newline :: Monad m => P.ParserT P.Consuming Char () m ()
newline =
  P.token P.>>= \case
    '\n' -> P.pure ()
    _ -> P.throw $ P.ErrorCustom ()

digit :: Monad m => P.ParserT P.Consuming Char () m Char
digit = P.do
  x <- P.token
  case x of
    '1' -> P.pure '1'
    '2' -> P.pure '2'
    '3' -> P.pure '3'
    '4' -> P.pure '4'
    '5' -> P.pure '5'
    '6' -> P.pure '6'
    '7' -> P.pure '7'
    '8' -> P.pure '8'
    '9' -> P.pure '9'
    '0' -> P.pure '0'
    _ -> P.throw $ P.ErrorCustom ()

number :: Monad m => P.ParserT P.Consuming Char () m String
number = (NonEmpty.toList P.<$> P.some digit) P.<* P.end

floating :: Monad m => P.ParserT P.Consuming Char () m String
floating = P.do
  wholes <- NonEmpty.toList P.<$> P.some digit
  P.equal '.'
  fractions <- NonEmpty.toList P.<$> P.some digit
  P.end
  P.pure $ wholes ++ "." ++ fractions

numberParser :: Monad m => P.ParserT P.Consuming Char () m (Either String String)
numberParser = (Left P.<$> number) P.<|> (Right P.<$> floating)
