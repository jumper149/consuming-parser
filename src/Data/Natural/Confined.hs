{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Natural.Confined where

import Data.Kind
import Data.Natural qualified
import Data.Proxy
import Data.Type.Equality

type Proof :: Data.Natural.Natural -> Data.Natural.Natural -> Data.Natural.Natural -> Type
data Proof n m x where
  Equal :: Proxy x
        -> Data.Natural.Compare j x :~: Data.Natural.Equal
        -> Data.Natural.Compare x k :~: Data.Natural.Equal
        -> Proof j k x
  EqualLower :: Proxy x
             -> Data.Natural.Compare j x :~: Data.Natural.Equal
             -> Data.Natural.Compare j k :~: Data.Natural.LessThan
             -> Proof j k x
  EqualUpper :: Proxy x
             -> Data.Natural.Compare j k :~: Data.Natural.LessThan
             -> Data.Natural.Compare x k :~: Data.Natural.LessThan
             -> Proof j k x
  Between :: Proxy x
          -> Data.Natural.Compare j x :~: Data.Natural.LessThan
          -> Data.Natural.Compare x k :~: Data.Natural.LessThan
          -> Proof j k x
