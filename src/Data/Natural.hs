{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Natural where

import Data.Kind

type Natural :: Type
data Natural
  = Z
  | S Natural

pattern Zero :: Natural
pattern Zero = Z

pattern One :: Natural
pattern One = S Zero

pattern Two :: Natural
pattern Two = S One

pattern Three :: Natural
pattern Three = S Two

pattern Four :: Natural
pattern Four = S Three

pattern Five :: Natural
pattern Five = S Four

pattern Six :: Natural
pattern Six = S Five

pattern Seven :: Natural
pattern Seven = S Six

pattern Eight :: Natural
pattern Eight = S Seven

pattern Nine :: Natural
pattern Nine = S Eight

type Plus :: Natural -> Natural -> Natural
type family Plus n m where
  Plus Z n = n
  Plus (S j) k = S (Plus j k)

type Minus :: Natural -> Natural -> Natural
type family Minus n m where
  Minus j Z = j
  Minus (S j) (S k) = Minus j k

type Comparison :: Type
data Comparison
  = LessThan
  | Equal
  | GreaterThan

type Compare :: Natural -> Natural -> Comparison
type family Compare n m where
  Compare Z Z = Equal
  Compare Z _ = LessThan
  Compare _ Z = GreaterThan
  Compare (S j) (S k) = Compare j k
