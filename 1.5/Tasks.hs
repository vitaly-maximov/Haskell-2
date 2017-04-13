{-# LANGUAGE TypeOperators #-}
module Tasks where

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }
    deriving (Eq, Show)

type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (2, ('a', False))

b :: B t
b = Cmps (False, id, Left "hello")

c :: C
c = Cmps (\flag n -> 5)