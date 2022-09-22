{-# LANGUAGE TypeFamilies #-}

module Parser.Consumption where

import Data.Kind

type Consumption :: Type
data Consumption
  = Unknown
  | Consuming

type (&&) :: Consumption -> Consumption -> Consumption
type family a && b where
  Unknown && _ = Unknown
  Consuming && x = x
  _ && Unknown = Unknown
  x && Consuming = x
  x && x = x

type (||) :: Consumption -> Consumption -> Consumption
type family a || b where
  Unknown || x = x
  Consuming || _ = Consuming
  x || Unknown = x
  _ || Consuming = Consuming
  x || x = x
