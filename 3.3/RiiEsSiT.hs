module RiiEsSiT where

{-
Модифицируйте монаду EsSi из предыдущей задачи, обернув ее в трансформер ReaderT с окружением, 
представляющим собой пару целых чисел, задающих нижнюю и верхнюю границы для вычислений. 
Функции go теперь не надо будет передавать эти параметры, они будут браться из окружения. 
Сделайте получившуюся составную монаду трансформером:

type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))
  
Реализуйте также функцию для запуска этого трансформера

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a 
                -> (Integer,Integer)  
                -> Integer 
                -> m (Either String a, Integer)
и модифицируйте код функции go, изменив её тип на

go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
так, чтобы для шага вычисления последовательности с отладочным выводом текущего элемента последовательности на экран

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n
мы получили бы

GHCi> runRiiEsSiT (forever $ go tickCollatz') (1,200) 27
82
41
124
62
31
94
47
142
71
214
(Left "Upper bound",214)
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a 
                 -> (Integer,Integer) 
                 -> Integer 
                 -> m (Either String a, Integer)
runRiiEsSiT rii minmax n = runStateT (runExceptT (runReaderT rii minmax)) n

go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go state = do
    (min, max) <- asks id
    lift $ lift state
    x <- lift $ lift get    
    when (x <= min) (lift $ throwE "Lower bound")
    when (x >= max) (lift $ throwE "Upper bound")