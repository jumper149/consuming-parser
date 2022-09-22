{-# LANGUAGE QualifiedDo #-}

module Parser.Token.Char where

import Parser qualified as P
import Parser.Combinators qualified as P
import Parser.Consumption qualified as P
import Parser.Error qualified as P

import Data.List.NonEmpty qualified as NonEmpty

newline :: Monad m => P.ParserT P.Consuming Char () m ()
newline = P.equal '\n' P.<|> P.throw (P.ErrorCustom ())

digit :: Monad m => P.ParserT P.Consuming Char () m Char
digit = P.do
  x <- P.token
  case x of
    '1' -> P.pure x
    '2' -> P.pure x
    '3' -> P.pure x
    '4' -> P.pure x
    '5' -> P.pure x
    '6' -> P.pure x
    '7' -> P.pure x
    '8' -> P.pure x
    '9' -> P.pure x
    '0' -> P.pure x
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
