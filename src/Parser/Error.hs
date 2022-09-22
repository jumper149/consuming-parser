{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser.Error where

import Data.Kind
import GHC.Generics
import Prelude qualified

type Error :: Type -> Type -- TODO: This is redundant.
data Error :: Type -> Type where
  ErrorCustom :: e -> Error e
  ErrorInputEmpty :: Error e
  ErrorInputLeft :: Error e
  ErrorSatisfy :: Error e
  ErrorUnexpectedToken :: Error e
  ErrorTerminal :: Error e
  ErrorAppend :: Error e -> Error e -> Error e
  deriving stock (Prelude.Eq, Generic, Prelude.Ord, Prelude.Read, Prelude.Show)

instance Prelude.Semigroup (Error e) where
  (<>) = ErrorAppend
