{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Co where

-- Пожалуйста, не удаляйте эти импорты. Они нужны для тестирующей системы.
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable

import Control.Monad.Cont
import Control.Applicative

{-
Чтобы закончить наш курс ярко, предлагаем вам с помощью этой задачи в полной мере почувствовать на себе всю мощь continuation-passing style. 
Чтобы успешно решить эту задачу, вам нужно хорошо понимать, как работает CPS и монада ContT (а этого, как известно, никто не понимает). 
Кстати, это была подсказка.

Сопрограмма (корутина, coroutine) это обобщение понятия подпрограммы (по-простому говоря, функции). У функции, в отличие от сопрограммы, 
есть одна точка входа (то, откуда она начинает работать), а точек выхода может быть несколько, 
но выйти через них функция может только один раз за время работы; у сопрограммы же точек входа и выхода может быть несколько. 
Проще всего объяснить на примере:

coroutine1 = do
  tell "1"
  yield
  tell "2"

coroutine2 = do
  tell "a"
  yield
  tell "b"
GHCi> execWriter (runCoroutines coroutine1 coroutine2)
"1a2b"
Здесь используется специальное действие yield, которое передает управление другой сопрограмме. 
Когда другая сопрограмма возвращает управление (с помощью того же yield или завершившись), 
первая сопрограмма продолжает работу с того места, на котором остановилась в прошлый раз.

В общем случае, одновременно могут исполняться несколько сопрограмм, причем при передаче управления, 
они могут обмениваться значениями. В этой задаче достаточно реализовать сопрограммы в упрощенном виде: 
одновременно работают ровно две сопрограммы и значениями обмениваться они не могут.

Реализуйте трансформер CoroutineT, функцию yield для передачи управления и функцию runCoroutines для запуска. 
Учтите, что одна сопрограмма может завершиться раньше другой; другая должна при этом продолжить работу:

coroutine3, coroutine4 :: CoroutineT (Writer String) ()
coroutine3 = do
  tell "1"
  yield
  yield
  tell "2"

coroutine4 = do
  tell "a"
  yield
  tell "b"
  yield
  tell "c"
  yield
  tell "d"
  yield
GHCi> execWriter (runCoroutines coroutine3 coroutine4)
"1ab2cd"
-}


newtype CoroutineT m a = CoroutineT { runCoroutineT :: ContT (Maybe (CoroutineT m ())) m a }

runCoroutines :: Monad m => CoroutineT m () -> CoroutineT m () -> m ()
runCoroutines co1 co2 = do
	c1 <- runCoroutine co1
	c2 <- runCoroutine co2
	case (c1, c2) of
		(Nothing, Nothing) -> return ()
		(Just co3, Nothing) -> runCoroutines co3 coNothing
		(Nothing, Just co4) -> runCoroutines co4 coNothing
		(Just co3, Just co4) -> runCoroutines co3 co4
	where		
		runCoroutine co = runContT (runCoroutineT co) doNothing
		doNothing _ = return Nothing
		coNothing = CoroutineT $ ContT $ doNothing

yield :: Monad m => CoroutineT m ()
yield = CoroutineT $ ContT $ \ c -> do
	return $ Just $ CoroutineT $ ContT $ \ _ -> c ()

instance (Monad m) => Functor (CoroutineT m) where
	fmap = liftM

instance (Monad m) => Applicative (CoroutineT m) where
	pure = return
	(<*>) = ap

instance (Monad m) => Monad (CoroutineT m) where
	return = CoroutineT . return
	(CoroutineT c) >>= k = CoroutineT $ do
		x <- c
		runCoroutineT $ k x

instance MonadTrans CoroutineT where
	lift m = CoroutineT $ ContT $ \ c -> do
		x <- m
		y <- c x
		return $ y

instance (MonadWriter w m) => MonadWriter w (CoroutineT m) where
	tell = lift . tell

coroutine1 :: CoroutineT (Writer String) ()
coroutine1 = do
  tell "1"
  yield
  tell "2"

coroutine2 :: CoroutineT (Writer String) ()
coroutine2 = do
  tell "a"
  yield
  tell "b"

coroutine3, coroutine4 :: CoroutineT (Writer String) ()
coroutine3 = do
  tell "1"
  yield
  yield
  tell "2"

coroutine4 = do
  tell "a"
  yield
  tell "b"
  yield
  tell "c"
  yield
  tell "d"
  yield

coroutine5 :: CoroutineT (Writer String) ()
coroutine5 = do
  tell "1"
  tell "2"
  tell "3"
  tell "4"