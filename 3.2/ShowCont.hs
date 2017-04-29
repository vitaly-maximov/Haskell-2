module ShowCont where

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

{-
Реализуйте функцию showCont, запускающую вычисление и возвращающую его результат в виде строки.
-}

showCont :: Show a => Cont String a -> String
showCont (Cont c) = c show