{-# LANGUAGE TypeOperators #-}
module Cmps where

{-
Сделайте тип

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 
представителем класса типов Traversable при условии, что аргументы композиции являются представителями Traversable.

GHCi> sequenceA (Cmps [Just (Right 2), Nothing])
Right (Cmps {getCmps = [Just 2,Nothing]})
GHCi> sequenceA (Cmps [Just (Left 2), Nothing])
Left 2
-}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

instance (Functor f, Functor g) => Functor (f |.| g) where
    fmap f (Cmps x) = Cmps $ fmap (fmap f) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    pure = Cmps . pure . pure
    Cmps f <*> Cmps x = Cmps $ fmap (<*>) f <*> x

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    foldr f ini (Cmps x) = foldr (\ y acc -> foldr f acc y) ini x

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
    traverse f (Cmps x) = Cmps <$> traverse (traverse f) x
