{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Parser.TypeError where

import Parser.Core qualified as P

import Control.Applicative
import Control.Monad.Trans.Class
import GHC.TypeLits
import Prelude

type UnsupportedParametricFunctor :: ErrorMessage -> ErrorMessage
type UnsupportedParametricFunctor shownType =
  Text "‘" :<>: ShowType P.ParserT :<>: Text "’ doesn't support the class"
    :$$: Text "  ‘" :<>: shownType :<>: Text "’"
    :$$: Text "Try to use specialised primitives"

instance TypeError (UnsupportedParametricFunctor (ShowType Functor)) => Functor (P.ParserT c t e m)
instance TypeError (UnsupportedParametricFunctor (ShowType Applicative)) => Applicative (P.ParserT c t e m)
instance TypeError (UnsupportedParametricFunctor (ShowType Monad)) => Monad (P.ParserT c t e m)
instance TypeError (UnsupportedParametricFunctor (ShowType MonadTrans)) => MonadTrans (P.ParserT c t e)
instance TypeError (UnsupportedParametricFunctor (ShowType Alternative)) => Alternative (P.ParserT c t e m)
