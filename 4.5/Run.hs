{-# LANGUAGE FlexibleContexts #-}

module Run where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable

{-
Вам дан список вычислений с состоянием (State s a) и начальное состояние. 
Требуется выполнить все эти вычисления по очереди (очередное вычисление получает на вход состояние, оставшееся от предыдущего) 
и вернуть список результатов. Но это ещё не всё. Ещё вам дан предикат, определяющий, разрешено некоторое состояние или нет; 
после выполнения очередного вычисления вы должны с помощью этого предиката проверить текущее состояние, и, если оно не разрешено, 
завершить вычисление, указав номер вычисления, которое его испортило.

При этом, завершаясь с ошибкой, мы можем как сохранить накопленное до текущего момента состояние, так и выкинуть его. 
В первом случае наша функция будет иметь такой тип:

runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
Во втором — такой:

runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
Пример:

GHCi> runLimited1 (< 3) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
(Left 2,3)
GHCi> runLimited2 (< 3) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
Left 2
— после выполнения вычисления с индексом 2 (нумерация с нуля) в состоянии оказалась тройка, что плохо.

GHCi> runLimited1 (< 100) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
(Right [(),(),(),()],4)
GHCi> runLimited2 (< 100) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
Right ([(),(),(),()],4)
— всё хорошо, предикат всё время выполнялся. Вычисления ничего не возвращали (имели тип State Int ()), 
так что списки получились такие неинтересные.

Если, прочитав про список, с каждым элементом которого надо совершить какое-то действие, вы сразу же подумали про traverse, 
то правильно сделали. Вопрос только в том, какое именно действие надо совершить с каждым элементом списка, но тут тоже всё довольно просто: 
надо выполнить вычисление с состоянием, описываемое элементом списка, а затем проверить состояние и, в случае проблемы, кинуть исключение:

limited p fs = traverse limit1 (zip [0..] fs)
  where
    limit1 (i, f) = do
      a <- state (runState f)
      stateIsBad <- gets (not . p)
      when stateIsBad $ throwError i
      pure a
(Прежде чем проходить по списку, мы ещё пронумеровали его элементы.)

Собственно, остался сущий пустяк — запустить наше новое вычисление.

runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs s = run1 (limited p fs) s

runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
runLimited2 p fs s = run2 (limited p fs) s
А теперь задание. Реализуйте функции run1 и run2.
-}

limited :: (MonadError Int m, MonadState s m) => (s -> Bool) -> [State s a] -> m [a]
limited p fs = traverse limit1 (zip [0..] fs)
  where
    limit1 (i, f) = do
      a <- state (runState f)
      stateIsBad <- gets (not . p)
      when stateIsBad $ throwError i
      pure a


runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs s = run1 (limited p fs) s

runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
runLimited2 p fs s = run2 (limited p fs) s

run1 :: ExceptT Int (State s) [a] -> s -> (Either Int [a], s)
run1 m s = runState (runExceptT m) s

run2 :: ExceptT Int (State s) [a] -> s -> Either Int ([a], s)
run2 m s = case run1 m s of
    (Left e, _) -> Left e
    (Right x, s) -> Right (x, s)
