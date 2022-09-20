{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Parser.Example.Parens where

import qualified Data.Tree as Containers
import qualified Data.List.NonEmpty as NonEmpty
import Parser
import Parser.Combinators
import qualified Prelude
import Data.String (fromString)

throwOn :: Prelude.Monad m => Error e -> ParserT 'Prelude.True t e m a -> ParserT 'Prelude.True t e m a
throwOn e p = p <|> throw e

openingParser :: Prelude.Monad m => ParserT 'Prelude.True Prelude.Char Prelude.String m ()
openingParser = do
    x <- consume
    case x of
      '(' -> pure ()
      _ -> throw (ErrorCustom "No opening parens.")

closingParser :: Prelude.Monad m => ParserT 'Prelude.True Prelude.Char Prelude.String m ()
closingParser = do
    x <- consume
    case x of
      ')' -> pure ()
      _ -> throw (ErrorCustom "No closing parens.")

data Tree = Node
          | Fingers (NonEmpty.NonEmpty Tree)
  deriving Prelude.Show

nodeParser :: Prelude.Monad m => ParserT 'Prelude.True Prelude.Char Prelude.String m Tree
nodeParser = throwOn (ErrorCustom "No node.")
  (do
    openingParser
    closingParser
    pure Node
  )

fingersParser :: Prelude.Monad m => ParserT 'Prelude.True Prelude.Char Prelude.String m Tree
fingersParser = throwOn (ErrorCustom "No fingers.")
  (do
    openingParser
    insideResult <- some treeParser
    closingParser
    pure (Fingers insideResult)
  )

treeParser :: Prelude.Monad m => ParserT 'Prelude.True Prelude.Char Prelude.String m Tree
treeParser = throwOn (ErrorCustom "No tree.") (nodeParser <|> fingersParser)

fullParser :: Prelude.Monad m => ParserT 'Prelude.True Prelude.Char Prelude.String m Tree
fullParser = do
    result <- treeParser
    end <|> throw (ErrorCustom "Additional input detected.")
    pure result

drawTree :: Tree -> Prelude.String
drawTree tree = Containers.drawTree (convertTree tree)
 where
  convertTree Node = Containers.Node "x" []
  convertTree (Fingers (x NonEmpty.:| xs)) = Containers.Node "x" (convertTree Prelude.<$> (x : xs))
