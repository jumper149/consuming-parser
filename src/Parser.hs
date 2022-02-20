{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser where

import Control.Alternative
import qualified Control.Monad.Error.Class as C
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import qualified Control.Monad.Trans.State as T
import Data.Failable
import Control.Monad.Trans.Failable
import GHC.Generics
import qualified Prelude

-- * Error

data Error :: * -> * where
    ErrorCustom :: e -> Error e
    ErrorInputEmpty :: Error e
    ErrorInputLeft :: Error e
    ErrorAppend :: Error e -> Error e -> Error e
  deriving stock (Prelude.Eq, Generic, Prelude.Ord, Prelude.Read, Prelude.Show)

instance Prelude.Semigroup (Error e) where
    (<>) = ErrorAppend

-- * Parser

newtype ParserT t e m a = ParserT { unParserT :: T.StateT [t] (FailableT (Error e) m) a }
  deriving newtype (Prelude.Functor, Prelude.Applicative, Prelude.Monad)
  deriving (Alternative, C.MonadError (Error e)) via Elevator (T.StateT [t]) (FailableT (Error e) m)
  deriving (MonadTrans, MonadTransControl) via (ComposeT (T.StateT [t]) (FailableT (Error e)))

parse :: ParserT t e m a -> [t] -> m (Failable (Error e) (a, [t]))
parse parser tokens = runFailableT (T.runStateT (unParserT parser) tokens)

combine :: Prelude.Monad m => ParserT a e m [b] -> ParserT b e m c -> ParserT a e m c
combine x y = ParserT Prelude.$ T.StateT Prelude.$ \ as -> do
  (bs, as') <- restoreT (parse x as)
  (c, bs') <- restoreT (parse y bs)
  case bs' of
    [] -> Prelude.pure (c, as')
    _rest -> C.throwError ErrorInputLeft

consume :: Prelude.Monad m => ParserT t e m t
consume = do
  tokens <- ParserT T.get
  case tokens of
    [] -> C.throwError ErrorInputEmpty
    t:ts -> do
      ParserT (T.put ts)
      Prelude.pure t

end :: Prelude.Monad m => ParserT t e m ()
end = do
  tokens <- ParserT T.get
  case tokens of
    [] -> Prelude.pure ()
    _rest -> C.throwError ErrorInputLeft
