{-# LANGUAGE InstanceSigs #-}

module StateT where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

{-
instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \st -> fmap updater $ runStateT m st
    where updater ~(x, s) = (f x, s)

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \ s -> return (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> v = StateT $ \ s -> do
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')
-}
instance Monad m => Monad (StateT s m) where
  return :: a -> StateT s m a
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  m >>= k  = StateT $ \s -> do
    ~(x, s') <- runStateT m s
    runStateT (k x) s'


instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \st -> do
    a <- m
    return (a, st)


get :: Monad m => StateT s m s
get = state $ \s -> (s, s)

put :: Monad m => s -> StateT s m ()
put s = state $ \_ -> ((), s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s)

{-
Реализуйте функции evalStateT и execStateT.
-}

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT state s = do
	(x, _) <- runStateT state s
	return x


execStateT :: Monad m => StateT s m a -> s -> m s
execStateT state s = do
	(_, s') <- runStateT state s
	return s'

{-
Нетрудно понять, что монада State более «сильна», чем монада Reader: вторая тоже, в некотором смысле, предоставляет доступ к глобальному состоянию, 
но только, в отличие от первой, не позволяет его менять. Покажите, как с помощью StateT можно эмулировать ReaderT:

GHCi> evalStateT (readerToStateT $ asks (+2)) 4
6
GHCi> runStateT  (readerToStateT $ asks (+2)) 4
(6,4)
-}

readerToStateT :: Monad m => ReaderT r m a -> StateT r m a
readerToStateT reader = StateT $ \s -> do
	x <- runReaderT reader s
	return (x, s)