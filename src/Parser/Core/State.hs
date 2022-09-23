module Parser.Core.State where

import Parser.Core.Index

import Data.Kind
import GHC.Generics

type State :: Type -> Type
data State t = MkState
  { stateTokens :: [t]
  , statePosition :: Index
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
