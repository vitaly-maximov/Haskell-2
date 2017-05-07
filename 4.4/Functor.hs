{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Functor where

{-
Предположим мы хотим реализовать следующую облегченную версию функтора, используя многопараметрические классы типов:

class Functor' c e where
  fmap' :: (e -> e) -> c -> c
Добавьте в определение этого класса типов необходимые функциональные зависимости и реализуйте его представителей для списка и Maybe так, 
чтобы обеспечить работоспособность следующих вызовов

GHCi> fmap' succ "ABC"
"BCD"
GHCi> fmap' (^2) (Just 42)
Just 1764
-}


class Functor' c e | c -> e where
  fmap' :: (e -> e) -> c -> c

instance Functor' (Maybe a) a where
    fmap' f Nothing = Nothing
    fmap' f (Just x) = Just $ f x

instance Functor' [a] a where
    fmap' f [] = []
    fmap' f (x : xs) = f x : fmap' f xs

