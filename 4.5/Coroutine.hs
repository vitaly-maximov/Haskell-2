{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Coroutine where

-- Пожалуйста, не удаляйте эти импорты. Они нужны для тестирующей системы.
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable

{-
Чтобы закончить наш курс ярко, предлагаем вам с помощью этой задачи в полной мере почувствовать на себе всю мощь continuation-passing style. 
Чтобы успешно решить эту задачу, вам нужно хорошо понимать, как работает CPS и монада ContT (а этого, как известно, никто не понимает). 
Кстати, это была подсказка.

Сопрограмма (корутина, coroutine) это обобщение понятия подпрограммы (по-простому говоря, функции). 
У функции, в отличие от сопрограммы, есть одна точка входа (то, откуда она начинает работать), 
а точек выхода может быть несколько, но выйти через них функция может только один раз за время работы; 
у сопрограммы же точек входа и выхода может быть несколько. Проще всего объяснить на примере:

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

В общем случае, одновременно могут исполняться несколько сопрограмм, причем при передаче управления, они могут обмениваться значениями. 
В этой задаче достаточно реализовать сопрограммы в упрощенном виде: одновременно работают ровно две сопрограммы и 
значениями обмениваться они не могут.

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

coroutine1 = do
  tell "1"
  yield
  tell "2"

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


newtype CoroutineT m a = CoroutineT { runCoroutineT :: ?? }

runCoroutines :: Monad m => CoroutineT m () -> CoroutineT m () -> m ()
runCoroutines = undefined

yield :: Monad m => CoroutineT m ()
yield = undefined