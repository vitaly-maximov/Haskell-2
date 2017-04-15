{-# LANGUAGE TypeOperators #-}

module Cmps where

{-
Сделайте тип

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 
представителем класса типов Foldable при условии, что аргументы композиции являются представителями Foldable.

GHCi> maximum $ Cmps [Nothing, Just 2, Just 3]
3
GHCi> length $ Cmps [[1,2], [], [3,4,5,6,7]]
7
-}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    foldr f ini (Cmps x) = foldr (\ y acc -> foldr f acc y) ini x