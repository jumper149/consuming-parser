module Parser.Misc where

import Parser.Error qualified as P

displayResult :: (Show a, Show e, Show t) => Either (P.Trace e) (a, [t]) -> String
displayResult x =
  case x of
    Left err -> "Failed to parse:\n" ++ displayTrace err
    Right (val, tokens) -> "Parsed value: " ++ show val ++ "\nRemaining Tokens: " ++ show tokens ++ "\n"

displayTrace :: Show e => P.Trace e -> String
displayTrace = \case
  P.TracePoint err -> displayError err ++ "\n"
  P.TraceAppend t1 t2 ->
    let str1 = displayTrace t1
        str2 = displayTrace t2
     in str2 ++ unlines (("  " ++) <$> lines str1)

displayError :: Show e => P.Error e -> String
displayError = \case
  P.ErrorCustom e -> show e
  P.ErrorInputEmpty -> "InputEmpty"
  P.ErrorInputLeft -> "InputLeft"
  P.ErrorUnexpectedToken -> "UnexpectedToken"
  P.ErrorSatisfy -> "Satisfy"
  P.ErrorEqual -> "Equal"
  P.ErrorOneOf -> "OneOf"
