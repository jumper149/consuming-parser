{-# LANGUAGE QualifiedDo #-}

module Parser.Example.Parens where

import qualified Data.Tree as Containers
import qualified Data.List.NonEmpty as NonEmpty
import qualified Parser as P
import qualified Parser.Combinators as P

throwOn :: Monad m => P.Error e -> P.ParserT 'P.Consuming t e m a -> P.ParserT 'P.Consuming t e m a
throwOn e p = p P.<|> P.throw e

openingParser :: Monad m => P.ParserT 'P.Consuming Char String m ()
openingParser = P.do
    x <- P.consume
    case x of
      '(' -> P.pure ()
      _ -> P.throw (P.ErrorCustom "No opening parens.")

closingParser :: Monad m => P.ParserT 'P.Consuming Char String m ()
closingParser = P.do
    x <- P.consume
    case x of
      ')' -> P.pure ()
      _ -> P.throw (P.ErrorCustom "No closing parens.")

data Tree = Node
          | Fingers (NonEmpty.NonEmpty Tree)
  deriving Show

nodeParser :: Monad m => P.ParserT 'P.Consuming Char String m Tree
nodeParser = throwOn (P.ErrorCustom "No node.")
  (P.do
    openingParser
    closingParser
    P.pure Node
  )

fingersParser :: Monad m => P.ParserT 'P.Consuming Char String m Tree
fingersParser = throwOn (P.ErrorCustom "No fingers.")
  (P.do
    openingParser
    insideResult <- P.some treeParser
    closingParser
    P.pure (Fingers insideResult)
  )

treeParser :: Monad m => P.ParserT 'P.Consuming Char String m Tree
treeParser = throwOn (P.ErrorCustom "No tree.") (nodeParser P.<|> fingersParser)

fullParser :: Monad m => P.ParserT 'P.Consuming Char String m Tree
fullParser = P.do
    result <- treeParser
    P.end P.<|> P.throw (P.ErrorCustom "Additional input detected.")
    P.pure result

drawTree :: Tree -> String
drawTree tree = Containers.drawTree (convertTree tree)
 where
  convertTree Node = Containers.Node "x" []
  convertTree (Fingers (x NonEmpty.:| xs)) = Containers.Node "x" (convertTree <$> (x : xs))
