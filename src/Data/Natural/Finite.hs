{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Natural.Finite where

import Data.Kind
import Data.Natural qualified
import Data.Type.Equality
import Data.Proxy

type Finite :: Data.Natural.Natural -> Type
data Finite n where
  Z :: Finite (Data.Natural.S k)
  S :: Finite k -> Finite (Data.Natural.S k)

instance Show (Finite n) where
    show Z = "Z"
    show (S k) = "S " <> show k

pattern Zero :: Finite (Data.Natural.S n)
pattern Zero = Z

pattern One :: Finite (Data.Natural.S (Data.Natural.S n))
pattern One = S Zero

pattern Two :: Finite (Data.Natural.S (Data.Natural.S (Data.Natural.S n)))
pattern Two = S One

pattern Three :: Finite (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S n))))
pattern Three = S Two

pattern Four :: Finite (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S n)))))
pattern Four = S Three

pattern Five :: Finite (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S n))))))
pattern Five = S Four

pattern Six :: Finite (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S n)))))))
pattern Six = S Five

pattern Seven :: Finite (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S n))))))))
pattern Seven = S Six

pattern Eight :: Finite (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S n)))))))))
pattern Eight = S Seven

pattern Nine :: Finite (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S (Data.Natural.S n))))))))))
pattern Nine = S Eight

weaken :: Finite n -> Finite (Data.Natural.S n)
weaken = \case
  Z -> Z
  S k -> S (weaken k)

strengthen :: Finite (Data.Natural.S n) -> Maybe (Finite n)
strengthen x =
  case testEquality (Proxy @n) (Proxy @Data.Natural.Z) of
    Nothing -> Just Z
    Just _ -> Nothing
