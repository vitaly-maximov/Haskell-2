{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module LoggT where

import Control.Monad.Trans.Class

import Data.Functor.Identity
import Control.Monad.State

import Control.Monad.Reader

{-
В этой и следующих задачах мы продолжаем работу с трансформером LoggT разработанным на первом уроке этой недели:

data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

write2log :: Monad m => String -> LoggT m ()

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
Теперь мы хотим сделать этот трансформер mtl-совместимым.

Избавьтесь от необходимости ручного подъема операций вложенной монады State, сделав трансформер LoggT, 
примененный к монаде с интерфейсом MonadState, представителем этого (MonadState) класса типов:

instance MonadState s m => MonadState s (LoggT m) where
  get   = undefined
  put   = undefined
  state = undefined

logSt' :: LoggT (State Integer) Integer      
logSt' = do 
  modify (+1)                   -- no lift!
  a <- get                      -- no lift!
  write2log $ show $ a * 10
  put 42                        -- no lift!
  return $ a * 100
Проверка:

GHCi> runState (runLoggT logSt') 2
(Logged "30" 300,42)
-}

data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

logTst :: LoggT Identity Integer
logTst = do 
  x <- LoggT $ Identity $ Logged "AAA" 30
  y <- return 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z
  
failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42

instance Monad m => Functor (LoggT m) where
    fmap = liftM

instance Monad m => Applicative (LoggT m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (LoggT m) where
  return = LoggT . return . Logged ""

  -- (>>=) :: LoggT m a -> (a -> LoggT m b) -> LoggT m b
  (LoggT m) >>= k  = LoggT $ do
    (Logged l1 v1) <- m
    (Logged l2 v2) <- runLoggT $ k v1
    return $ Logged (l1 ++ l2) v2
  
  fail = LoggT . fail

logTst' :: Logg Integer   
logTst' = do 
  write2log "AAA"
  write2log "BBB"
  return 42

stLog :: StateT Integer Logg Integer
stLog = do 
  modify (+1)
  a <- get
  lift $ write2log $ show $ a * 10
  put 42
  return $ a * 100


write2log :: Monad m => String -> LoggT m ()
write2log = LoggT . return . (flip Logged) ()

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

{---}

logSt' :: LoggT (State Integer) Integer      
logSt' = do 
  modify (+1)                   -- no lift!
  a <- get                      -- no lift!
  write2log $ show $ a * 10
  put 42                        -- no lift!
  return $ a * 100

{---}
{-
instance MonadState s m => MonadState s (LoggT m) where
  get   = LoggT $ do
    x <- get
    return $ Logged "" x

  put s = LoggT $ do
    put s
    return $ Logged "" ()

  state f = LoggT $ do
    x <- state f
    return $ Logged "" x
-}

instance MonadTrans LoggT where
    lift m = LoggT $ do
        x <- m
        return $ Logged "" x

instance MonadState s m => MonadState s (LoggT m) where
    get = lift get
    put = lift . put
    state = lift .state

{-
Избавьтесь от необходимости ручного подъема операций вложенной монады Reader, сделав трансформер LoggT, примененный к монаде с интерфейсом MonadReader, представителем этого (MonadReader) класса типов:

instance MonadReader r m => MonadReader r (LoggT m) where
  ask    = undefined
  local  = undefined
  reader = undefined
Для упрощения реализации функции local имеет смысл использовать вспомогательную функцию, поднимающую стрелку между двумя «внутренними представлениями» трансформера LoggT в стрелку между двумя LoggT:

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = undefined
Тест:

logRdr :: LoggT (Reader [(Int,String)]) ()      
logRdr = do 
  Just x <- asks $ lookup 2                      -- no lift!
  write2log x
  Just y <- local ((3,"Jim"):) $ asks $ lookup 3 -- no lift!
  write2log y
Ожидаемый результат:

GHCi> runReader (runLoggT logRdr) [(1,"John"),(2,"Jane")]
Logged "JaneJim" ()
-}

logRdr :: LoggT (Reader [(Int,String)]) ()      
logRdr = do 
  Just x <- asks $ lookup 2                      -- no lift!
  write2log x
  Just y <- local ((3,"Jim"):) $ asks $ lookup 3 -- no lift!
  write2log y

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = LoggT . f . runLoggT

instance MonadReader r m => MonadReader r (LoggT m) where
  ask    = lift ask
  local = mapLoggT . local
  reader = lift . reader

{-
Чтобы избавится от необходимости ручного подъема операции write2log, обеспечивающей стандартный интерфейс вложенного трансформера LoggT, можно поступить по аналогии с другими трансформерами библиотеки mtl. А именно, разработать класс типов MonadLogg, выставляющий этот стандартный интерфейс

class Monad m => MonadLogg m where
  w2log :: String -> m ()
  logg :: Logged a -> m a
(Замечание: Мы переименовываем функцию write2log в w2log, поскольку хотим держать всю реализацию в одном файле исходного кода. При следовании принятой в библиотеках transformers/mtl идеологии они имели бы одно и то же имя, но были бы определены в разных модулях. При работе с transformers мы импортировали бы свободную функцию c квалифицированным именем Control.Monad.Trans.Logg.write2log, а при использовании mtl работали бы с методом класса типов MonadLogg с полным именем Control.Monad.Logg.write2log. )

Этот интерфейс, во-первых, должен выставлять сам трансформер LoggT, обернутый вокруг произвольной монады:

instance Monad m => MonadLogg (LoggT m) where
  w2log = undefined
  logg  = undefined
Реализуйте этого представителя, для проверки используйте:

logSt'' :: LoggT (State Integer) Integer      
logSt'' = do 
  x <- logg $ Logged "BEGIN " 1
  modify (+x)
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100
Результат должен быть таким:

GHCi> runState (runLoggT logSt'') 2
(Logged "BEGIN 30 END" 300,42)
Во-вторых, интерфейс MonadLogg должен выставлять любой стандартный трансформер, обернутый вокруг монады, выставляющей этот интерфейс:

instance MonadLogg m => MonadLogg (StateT s m) where
  w2log = undefined
  logg  = undefined

instance MonadLogg m => MonadLogg (ReaderT r m) where
  w2log = undefined
  logg  = undefined
  
-- etc...
Реализуйте двух этих представителей, для проверки используйте:

rdrStLog :: ReaderT Integer (StateT Integer Logg) Integer      
rdrStLog = do 
  x <- logg $ Logged "BEGIN " 1
  y <- ask
  modify (+ (x+y))
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100
Результат должен быть таким:

GHCi> runLogg $ runStateT (runReaderT rdrStLog 4) 2
Logged "BEGIN 70 END" (700,42)
-}

logSt'' :: LoggT (State Integer) Integer      
logSt'' = do 
  x <- logg $ Logged "BEGIN " 1
  modify (+x)
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100

rdrStLog :: ReaderT Integer (StateT Integer Logg) Integer      
rdrStLog = do 
  x <- logg $ Logged "BEGIN " 1
  y <- ask
  modify (+ (x+y))
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100

class Monad m => MonadLogg m where
  w2log :: String -> m ()
  logg :: Logged a -> m a

instance Monad m => MonadLogg (LoggT m) where
  w2log = write2log
  logg = LoggT . return 

instance MonadLogg m => MonadLogg (StateT s m) where
  w2log = lift . w2log
  logg  = lift . logg

instance MonadLogg m => MonadLogg (ReaderT r m) where
  w2log = lift . w2log
  logg  = lift . logg