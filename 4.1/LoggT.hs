module LoggT where

import Control.Monad
import Control.Monad.Identity

import Control.Monad.Trans.State
import Control.Monad.Trans.Class

{-
Сделайте на основе типа данных

data Logged a = Logged String a deriving (Eq,Show)
трансформер монад LoggT :: (* -> *) -> * -> * с одноименным конструктором данных и меткой поля runLoggT:

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }
Для этого реализуйте для произвольной монады m представителя класса типов Monad для LoggT m :: * -> *:

instance Monad m => Monad (LoggT m) where
  return x = undefined
  m >>= k  = undefined
  fail msg = undefined

Для проверки используйте функции:

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
которые при правильной реализации монады должны вести себя так:

GHCi> runIdentity (runLoggT logTst)
Logged "AAABBB" 42
GHCi> runLoggT $ failTst [5,5]
[Logged "A" 42,Logged "A" 42]
GHCi> runLoggT $ failTst [5,6]
[Logged "A" 42]
GHCi> runLoggT $ failTst [7,6]
[]	
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

{-
instance Monad m => Functor (LoggT m) where
	fmap = liftM

instance Monad m => Applicative (LoggT m) where
	pure = return
	(<*>) = ap
-}

instance Monad m => Monad (LoggT m) where
  return = LoggT . return . Logged ""

  -- (>>=) :: LoggT m a -> (a -> LoggT m b) -> LoggT m b
  (LoggT m) >>= k  = LoggT $ do
  	(Logged l1 v1) <- m
  	(Logged l2 v2) <- runLoggT $ k v1
  	return $ Logged (l1 ++ l2) v2
  
  fail = LoggT . fail


{-
Напишите функцию write2log обеспечивающую трансформер LoggT стандартным логгирующим интерфейсом:

write2log :: Monad m => String -> LoggT m ()
write2log = undefined
Эта функция позволяет пользователю осуществлять запись в лог в процессе вычисления в монаде LoggT m для любой монады m. 
Введите для удобства упаковку для LoggT Identity и напишите функцию запускающую вычисления в этой монаде

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = undefined
Тест

logTst' :: Logg Integer   
logTst' = do 
  write2log "AAA"
  write2log "BBB"
  return 42
должен дать такой результат:

GHCi> runLogg logTst'
Logged "AAABBB" 42
А тест (подразумевающий импорт Control.Monad.Trans.State и Control.Monad.Trans.Class)

stLog :: StateT Integer Logg Integer
stLog = do 
  modify (+1)
  a <- get
  lift $ write2log $ show $ a * 10
  put 42
  return $ a * 100
— такой:

GHCi> runLogg $ runStateT stLog 2
Logged "30" (300,42)
-}

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

{-
В последнем примере предыдущей задачи функция lift :: (MonadTrans t, Monad m) => m a -> t m a 
позволяла поднять вычисление из внутренней монады (в примере это был Logg) во внешний трансформер (StateT Integer). 
Это возможно, поскольку для трансформера StateT s реализован представитель класса типов MonadTrans из Control.Monad.Trans.Class.

Сделайте трансформер LoggT представителем этого класса MonadTrans, 
так чтобы можно было поднимать вычисления из произвольной внутренней монады в наш трансформер:

instance MonadTrans LoggT where
  lift = undefined

logSt :: LoggT (State Integer) Integer
logSt = do 
  lift $ modify (+1)
  a <- lift get
  write2log $ show $ a * 10
  lift $ put 42
  return $ a * 100
Проверка:

GHCi> runState (runLoggT logSt) 2
(Logged "30" 300,42)
-}

logSt :: LoggT (State Integer) Integer
logSt = do 
  lift $ modify (+1)
  a <- lift get
  write2log $ show $ a * 10
  lift $ put 42
  return $ a * 100

instance MonadTrans LoggT where
  -- lift :: m a -> LoggT m a
  lift m = LoggT $ do
    a <- m
    return $ Logged "" a