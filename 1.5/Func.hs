{-# LANGUAGE TypeOperators #-}
module Func where

import Control.Applicative

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
    fmap f (Cmps x) = Cmps $ fmap (fmap f) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    pure = Cmps . pure . pure
    Cmps f <*> Cmps x = Cmps $ fmap (<*>) f <*> x

{-
Напишите универсальные функции

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
позволяющие избавляться от синтаксического шума для композиции нескольких функторов:

GHCi> pure 42 :: ([] |.| [] |.| []) Int
Cmps {getCmps = [Cmps {getCmps = [[42]]}]}
GHCi> unCmps3 (pure 42 :: ([] |.| [] |.| []) Int)
[[[42]]]
GHCi> unCmps3 (pure 42 :: ([] |.| Maybe |.| []) Int)
[Just [42]]
GHCi> unCmps4 (pure 42 :: ([] |.| [] |.| [] |.| []) Int)
[[[[42]]]]
-}

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 (Cmps x) = fmap getCmps x

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 (Cmps x) = fmap unCmps3 x
