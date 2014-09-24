{-# LANGUAGE
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
module Control.Monad.AnyCont (
    MonadAnyCont(..),
    ScopeAnyCont(..),
    AnyContT(..),
    MonadTransAnyCont(..),
    runAnyContT,
    withAnyContT,
    mapAnyContT,
    throwACE
  ) where

import Control.Monad.Trans.AnyCont
import Control.Monad.AnyCont.Class
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad.Error.Class

import  Control.Monad.Trans.Except 

instance MonadState s m => MonadState s (AnyContT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadError e m => MonadError e (AnyContT (ExceptT e m)) where
  throwError = lift . throwE
  x `catchError` h = anyContT $ \f -> (runAnyContT x f) `catchE` (\e -> runAnyContT (h e) f)

throwACE :: Monad m => e -> (AnyContT (ExceptT e m)  a)
throwACE = lift . throwE 