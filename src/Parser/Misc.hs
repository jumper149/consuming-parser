module Parser.Misc where

import Data.Failable
import Parser.Error qualified as P

displayResult :: (Show a, Show e, Show t) => Failable (P.Error e) (a, [t]) -> String
displayResult x =
  case x of
    Failed err -> "Failed to parse:\n" ++ displayError err
    Succeeding (val, tokens) -> "Parsed value: " ++ show val ++ "\nRemaining Tokens: " ++ show tokens ++ "\n"

displayError :: Show e => P.Error e -> String
displayError = \case
  P.ErrorCustom e -> show e ++ "\n"
  P.ErrorInputEmpty -> "InputEmpty\n"
  P.ErrorInputLeft -> "InputLeft\n"
  P.ErrorUnexpectedToken -> "UnexpectedToken\n"
  P.ErrorSatisfy -> "Satisfy\n"
  P.ErrorEqual -> "Equal\n"
  P.ErrorOneOf -> "OneOf\n"
  P.ErrorAppend e1 e2 ->
    let str1 = displayError e1
        str2 = displayError e2
     in str2 ++ unlines (("  " ++) <$> lines str1)
