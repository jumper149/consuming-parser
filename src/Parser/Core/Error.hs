module Parser.Core.Error where

import Parser.Core.Index

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
  deriving stock (Prelude.Eq, Generic, Prelude.Ord, Prelude.Read, Prelude.Show)

type Trace :: Type -> Type -- TODO: This is redundant.
data Trace :: Type -> Type where
  TracePoint :: Index -> Error e -> Trace e
  TraceAppend :: Trace e -> Trace e -> Trace e
  deriving stock (Prelude.Eq, Generic, Prelude.Ord, Prelude.Read, Prelude.Show)

instance Semigroup (Trace e) where
  (<>) = TraceAppend

instance Monoid (Trace e) where
  mempty = error "This instance is only implemented to be able to derive an `Alternative` instance."
