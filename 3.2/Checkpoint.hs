module Checkpoint where

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Monad (Cont r) where
	return x = Cont $ \c -> c x

	--(>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
	(Cont x) >>= k = Cont $ \c -> x (\a -> runCont (k a) c)


{-
Возможность явно работать с продолжением обеспечивает доступ к очень гибкому управлению исполнением. 
В этом задании вам предстоит реализовать вычисление, которое анализирует и модифицирует значение, 
возвращаемое кодом, написанным после него.

В качестве примера рассмотрим следующую функцию:

addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2     {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3     {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4         {- x4 = x1 + 30 -}
Эта функция принимает значение x1, совершает с ним последовательность операций (несколько раз прибавляет 10) и 
после каждой операции «сохраняет» промежуточный результат. При запуске такой функции используется дополнительный 
предикат, который является критерием «корректности» результата, и в случае, если возвращенное функцией значение 
этому критерию не удовлетворяет, вернется последнее удовлетворяющее ему значение из «сохраненных»:

GHCi> evalCont (checkpointed (< 100) $ addTens 1)
31
GHCi> evalCont (checkpointed  (< 30) $ addTens 1)
21
GHCi> evalCont (checkpointed  (< 20) $ addTens 1)
11
GHCi> evalCont (checkpointed  (< 10) $ addTens 1)
1
(Если ни возвращенное, ни сохраненные значения не подходят, результатом должно быть первое из сохраненных значений; 
если не было сохранено ни одного значения, то результатом должно быть возвращенное значение.)

Обратите внимание на то, что функция checkpoint передается в Checkpointed вычисление как параметр, 
поскольку её поведение зависит от предиката, который будет известен только непосредственно при запуске.
-}

type Checkpointed a = (a -> (Cont a a)) -> Cont a a


addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2     {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3     {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4         {- x4 = x1 + 30 -}

checkpointed :: (a -> Bool) -> Checkpointed a -> Cont r a
checkpointed predicate point = do
	return $ runCont (point (\a -> Cont (\c -> let b = c a in if predicate b then b else a))) id
