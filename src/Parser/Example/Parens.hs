{-# LANGUAGE QualifiedDo #-}

module Parser.Example.Parens where

import Parser.Combinators qualified as P
import Parser.Core qualified as P
import Parser.Core.Consumption qualified as P
import Parser.Core.Error qualified as P

import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.Tree qualified as C

throwOn :: Monad m => P.Error e -> P.ParserT P.Consuming t e m a -> P.ParserT P.Consuming t e m a
throwOn e p = p P.<|> P.throw e

openingParser :: Monad m => P.ParserT P.Consuming Char String m ()
openingParser = P.equal '(' P.<|> P.throw (P.ErrorCustom "No opening parens.")

closingParser :: Monad m => P.ParserT P.Consuming Char String m ()
closingParser = P.equal ')' P.<|> P.throw (P.ErrorCustom "No closing parens.")

type Tree :: Type
data Tree
  = Node
  | Fingers (NE.NonEmpty Tree)
  deriving stock (Show)

nodeParser :: Monad m => P.ParserT P.Consuming Char String m Tree
nodeParser =
  throwOn (P.ErrorCustom "No node.") $ P.do
    openingParser
    closingParser
    P.pure Node

fingersParser :: Monad m => P.ParserT P.Consuming Char String m Tree
fingersParser =
  throwOn (P.ErrorCustom "No fingers.") $ P.do
    openingParser
    insideResult <- P.some treeParser
    closingParser
    P.pure $ Fingers insideResult

treeParser :: Monad m => P.ParserT P.Consuming Char String m Tree
treeParser = throwOn (P.ErrorCustom "No tree.") $ nodeParser P.<|> fingersParser

fullParser :: Monad m => P.ParserT P.Consuming Char String m Tree
fullParser = P.do
  result <- treeParser
  P.end P.<|> P.throw (P.ErrorCustom "Additional input detected.")
  P.pure result

drawTree :: Tree -> String
drawTree tree = C.drawTree (convertTree tree)
 where
  convertTree Node = C.Node @String "x" []
  convertTree (Fingers (x NE.:| xs)) = C.Node @String "x" $ convertTree <$> x : xs
