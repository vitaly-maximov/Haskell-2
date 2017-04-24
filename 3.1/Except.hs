module Except where

{-
Реализуйте функцию withExcept :: (e -> e') -> Except e a -> Except e' a, 
позволящую, если произошла ошибка, применить к ней заданное преобразование.
-}

newtype Except e a = Except { runExcept :: Either e a }

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f (Except (Left e)) = Except $ Left $ f e
withExcept _ (Except (Right x)) = Except $ Right x
