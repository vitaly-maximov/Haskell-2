module Divide where

import Control.Applicative

{-
Функция

divideList :: Fractional a => [a] -> a
divideList []     = 1
divideList (x:xs) = (/) x (divideList xs)
сворачивает список посредством деления. Модифицируйте ее, реализовав divideList' :: (Show a, Fractional a) => [a] -> (String,a), такую что последовательность вычислений отражается в логе:

GHCi> divideList [3,4,5]
3.75
GHCi> divideList' [3,4,5]
("<-3.0/<-4.0/<-5.0/1.0",3.75)
Используйте аппликативный функтор пары, сохраняя близкую к исходной функции структуру реализации

divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = _
divideList' (x:xs) = (/) <$> _ <*> _
-}

divideList' :: (Show a, Fractional a) => [a] -> (String, a)
divideList' [] = ("1.0", 1)
divideList' (x : xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> divideList' xs