module Parser.Error where

import Data.Kind
import GHC.Generics

type Error :: Type -> Type -- TODO: This is redundant.
data Error :: Type -> Type where
  ErrorCustom :: e -> Error e
  ErrorInputEmpty :: Error e
  ErrorInputLeft :: Error e
  ErrorSatisfy :: Error e
  ErrorUnexpectedToken :: Error e
  ErrorEqual :: Error e
  ErrorOneOf :: Error e
  ErrorAppend :: Error e -> Error e -> Error e
  deriving stock (Prelude.Eq, Generic, Prelude.Ord, Prelude.Read, Prelude.Show)

instance Prelude.Semigroup (Error e) where
  (<>) = ErrorAppend
