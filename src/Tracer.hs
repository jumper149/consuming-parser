{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tracer where

import Data.Kind
import GHC.Generics

type Trace :: Type -> Type -- TODO: This is redundant.
data Trace :: Type -> Type where
  Footprint :: e -> Trace e
  ReadTrace :: Trace e -> Trace e -> Trace e
  deriving stock (Eq, Generic, Ord, Read, Show)

type Tracer :: Type -> [Trace e] -> Type -> Type
newtype Tracer e ts a = MkTracer { runTracer :: Either (Trace e) a }

pure' :: a -> Tracer e '[] a
pure' = MkTracer . Right

bind' :: Tracer e ts1 a -> (a -> Tracer e ts2 b) -> Tracer e (ts1 |?> ts2) b
bind' x f = MkTracer $ runTracer x >>= runTracer . f

alt' :: Tracer e ts1 a -> Tracer e ts2 a -> Tracer e (ts1 |!> ts2) a
alt' x y =
  case runTracer x of
    Right a -> MkTracer $ Right a
    Left t1 ->
      case runTracer y of
        Right a -> MkTracer $ Right a
        Left t2 -> MkTracer $ Left $ ReadTrace t1 t2

example :: Tracer Bool _ Char
example = bind' (alt' x1 x2) x3
 where
     x1 :: Tracer Bool '[Footprint True] Char
     x1 = MkTracer $ Left $ Footprint True
     x2 :: Tracer Bool '[Footprint False] Char
     x2 = MkTracer $ Left $ Footprint False
     x3 :: a -> Tracer Bool '[Footprint False] Char
     x3 _ = MkTracer $ Left $ Footprint False

type (.!>) :: [Trace e] -> Trace e -> [Trace e]
type family a .!> b where
    '[] .!> _y = '[]
    (x ': xs) .!> y = ReadTrace x y ': (xs .!> y)

type (|!>) :: [Trace e] -> [Trace e] -> [Trace e]
type family a |!> b where
    _xs |!> '[] = '[]
    xs |!> (y ': ys) = (xs .!> y) ++ (xs |!> ys)

type (++) :: [a] -> [a] -> [a]
type family a ++ b where
    '[] ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)

type (|?>) :: [a] -> [a] -> [a]
type family a |?> b where
    xs |?> ys = xs ++ ys
