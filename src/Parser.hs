{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser where

import qualified Control.Alternative
import qualified Control.Monad.Error.Class as C
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import qualified Control.Monad.Trans.State as T
import Data.Failable
import Data.Kind
import Data.Type.Bool
import Control.Monad.Trans.Failable
import GHC.Generics
import qualified Prelude

-- * Error

data Error :: Type -> Type where
    ErrorCustom :: e -> Error e
    ErrorInputEmpty :: Error e
    ErrorInputLeft :: Error e
    ErrorAppend :: Error e -> Error e -> Error e
  deriving stock (Prelude.Eq, Generic, Prelude.Ord, Prelude.Read, Prelude.Show)

instance Prelude.Semigroup (Error e) where
    (<>) = ErrorAppend

-- * Parser

type ParserT :: Prelude.Bool -- ^ c
             -> Type -- ^ t
             -> Type -- ^ e
             -> (Type -> Type) -- ^ m
             -> Type -- ^ a
             -> Type
newtype ParserT c t e m a = ParserT { unParserT :: T.StateT [t] (FailableT (Error e) m) a }
  deriving (MonadTrans, MonadTransControl) via (ComposeT (T.StateT [t]) (FailableT (Error e)))

parse :: ParserT c t e m a -> [t] -> m (Failable (Error e) (a, [t]))
parse parser tokens = runFailableT (T.runStateT (unParserT parser) tokens)

--combine :: Prelude.Monad m => ParserT 'Prelude.True a e m [b] -> ParserT 'Prelude.True b e m c -> ParserT 'Prelude.True a e m c
--combine x y = ParserT Prelude.$ T.StateT Prelude.$ \ as -> do
--  (bs, as') <- restoreT (parse x as)
--  (c, bs') <- restoreT (parse y bs)
--  case bs' of
--    [] -> Prelude.pure (c, as')
--    _rest -> C.throwError ErrorInputLeft

consume :: Prelude.Monad m => ParserT 'Prelude.True t e m t
consume = do
  tokens <- ParserT @'Prelude.True T.get
  case tokens of
   [] -> throw ErrorInputEmpty
   t:ts -> do
     ParserT @'Prelude.False (T.put ts)
     pure t

end :: Prelude.Monad m => ParserT 'Prelude.False t e m ()
end = do
  tokens <- ParserT @'Prelude.False T.get
  case tokens of
    [] -> pure ()
    _rest -> throw ErrorInputLeft

map :: Prelude.Functor m => (a -> b) -> ParserT c t e m a -> ParserT c t e m b
map f = ParserT Prelude.. Prelude.fmap f Prelude.. unParserT

pure :: Prelude.Monad m => a -> ParserT 'Prelude.False t e m a
pure = ParserT Prelude.. Prelude.pure

(<*>) :: Prelude.Monad m => ParserT c1 t e m (a -> b) -> ParserT c2 t e m a -> ParserT (c1 || c2) t e m b
ParserT x <*> ParserT y = ParserT (x Prelude.<*> y)

(>>=) :: Prelude.Monad m => ParserT c1 t e m a -> (a -> ParserT c2 t e m b) -> ParserT (c1 || c2) t e m b
x >>= f = ParserT (unParserT x Prelude.>>= unParserT Prelude.. f)

(>>) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m b -> ParserT (c1 || c2) t e m b
x >> y = x >>= Prelude.const y

(<|>) :: Prelude.Monad m => ParserT c1 t e m a -> ParserT c2 t e m a -> ParserT (c1 && c2) t e m a
ParserT x <|> ParserT y = ParserT (descend (Ascend x Control.Alternative.<|> Ascend y))

throw :: Prelude.Monad m => Error e -> ParserT 'Prelude.False t e m a
throw e = ParserT (descend (C.throwError e))

catch :: Prelude.Monad m => ParserT c1 t e m a -> (Error e -> ParserT c2 t e m a) -> ParserT (c1 && c2) t e m a
catch throwing catching = ParserT (descend (C.catchError (Ascend (unParserT throwing)) (Ascend Prelude.. unParserT Prelude.. catching)))
