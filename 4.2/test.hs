module Test where

{-
На предыдущих видео-степах мы реализовали ленивую версию монады State, которая откладывает сопоставление с образцом внутренней пары до момента использования её элементов. Такая стратегия приводит к лучшей определенности вычислений в некоторых сценариях; примеры аналогичны приводившимся для монады Writer. Однако, есть сценарии, где такая стратегия неэффективна и приводит к утечкам памяти. Следующий пример при попытке вычисления test будет приводить к ошибке времени исполнения из-за переполнения памяти, несмотря на использование оператора ($!) внутри tick:

tick :: State Int Int
tick = do 
  n <- get
  put $! (n+1)
  return n

test :: Int
test = snd $ runState (mapM_ (const tick) [1..10^7]) 0
Ваша задача: превратить ленивую версию монады State в строгую, чтобы исполнение приведенного выше примера не приводило бы к переполнению (ни в приведенном варианте, ни при замене mapM_ на traverse_).
-}

tick :: State Int Int
tick = do 
  n <- get
  put $! (n+1)
  return n

test :: Int
test = snd $ runState (mapM_ (const tick) [1..10^7]) 0

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
  fmap f m = State $ \st -> updater $ runState m st
    where updater ~(x, s) = (f x, s)

instance Applicative (State s) where
  pure x =  State $ \s -> (x, s)

  f <*> v = State $ \s ->
    let (g, s')  = runState f s
        (x, s'') = runState v $! s'
    in (g x, s'')

instance Monad (State s) where
  return a = State $ \s -> (a, s)

  m >>= k = State $ \s -> 
    let (x, s') = runState m s
    in runState (k x) $! s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)