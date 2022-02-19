{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parser where

import Control.Alternative
import qualified Control.Monad.Error.Class as C
import qualified Control.Monad.Trans.State as T
import Control.Monad.Trans.Elevator
import Data.Failable
import qualified Data.Functor.Identity
import Control.Monad.Trans.Failable
import GHC.Generics
import qualified Prelude

-- * Error

data Error :: * -> * where
    ErrorCustom :: e -> Error e
    ErrorAppend :: Error e -> Error e -> Error e
  deriving stock (Prelude.Eq, Generic, Prelude.Ord, Prelude.Read, Prelude.Show)

instance Prelude.Semigroup (Error e) where
    (<>) = ErrorAppend

-- * Parser

newtype Parser t e a = Parser { unParser :: T.StateT [t] (FailableT (Error e) Data.Functor.Identity.Identity) a }
  deriving newtype (Prelude.Functor, Prelude.Applicative, Prelude.Monad)
  deriving (Alternative, C.MonadError (Error e)) via Elevator (T.StateT [t]) (FailableT (Error e) Data.Functor.Identity.Identity)

parse :: Parser t e a -> [t] -> Failable (Error e) (a, [t])
parse parser tokens = Data.Functor.Identity.runIdentity (runFailableT (T.runStateT (unParser parser) tokens))
