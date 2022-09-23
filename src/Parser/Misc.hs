module Parser.Misc where

import Parser.Core.Error qualified as P
import Parser.Core.Index qualified as P
import Parser.Core.State qualified as P

import Data.Tree

displayResult :: (Show a, Show e, Show t) => Either (P.Trace e) (a, P.State t) -> String
displayResult x =
  case x of
    Left trace -> "Failed to parse:\n" ++ displayTrace trace
    Right (val, state) -> "Parsed value: " ++ show val ++ "\nFinal state: " ++ show state ++ "\n"

displayTrace :: Show e => P.Trace e -> String
displayTrace = drawTree . traceToTree
 where
  traceToTree :: Show e => P.Trace e -> Tree String
  traceToTree = \case
    P.TracePoint index err -> Node {rootLabel = displayIndex index ++ ": " ++ displayError err, subForest = []}
    P.TraceAppend t1 t2 ->
      case traceToTree t2 of
        Node {rootLabel, subForest} -> Node {rootLabel, subForest = subForest ++ [traceToTree t1]}

displayError :: Show e => P.Error e -> String
displayError = \case
  P.ErrorCustom e -> show e
  P.ErrorInputEmpty -> "InputEmpty"
  P.ErrorInputLeft -> "InputLeft"
  P.ErrorUnexpectedToken -> "UnexpectedToken"
  P.ErrorSatisfy -> "Satisfy"
  P.ErrorEqual -> "Equal"
  P.ErrorOneOf -> "OneOf"

displayIndex :: P.Index -> String
displayIndex = show . indexToInteger
 where
  indexToInteger :: P.Index -> Integer
  indexToInteger = \case
    P.Zero -> 0
    P.Successor n -> succ $ indexToInteger n
