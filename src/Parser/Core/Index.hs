module Parser.Core.Index where

import Data.Kind
import GHC.Generics

type Index :: Type
data Index
  = Zero
  | Successor Index
  deriving stock (Prelude.Eq, Generic, Prelude.Ord, Prelude.Read, Prelude.Show)
