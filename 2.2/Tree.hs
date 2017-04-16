module Tree where

{-
Сделайте двоичное дерево

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)
представителем класса типов Traversable (а также всех других необходимых классов типов).

GHCi> traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 3 Nil)
Right (Branch (Branch Nil 1 Nil) 3 Nil)
GHCi> traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 2 Nil)
Left 2
GHCi> sequenceA $ Branch (Branch Nil [1,2] Nil) [3] Nil
[Branch (Branch Nil 1 Nil) 3 Nil,Branch (Branch Nil 2 Nil) 3 Nil]
-}

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

instance Traversable Tree where
    traverse _ Nil = pure Nil
    traverse f (Branch l x r) = Branch <$> traverse f l <*> f x <*> traverse f r