module Tree where

{-
Для реализации свертки двоичных деревьев нужно выбрать алгоритм обхода узлов дерева (см., например, http://en.wikipedia.org/wiki/Tree_traversal).

Сделайте двоичное дерево

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)
представителем класса типов Foldable, реализовав симметричную стратегию (in-order traversal). Реализуйте также три другие стандартные стратегии (pre-order traversal, post-order traversal и level-order traversal), сделав типы-обертки

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)
представителями класса Foldable.

GHCi> tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
GHCi> foldr (:) [] tree
[1,2,3,4]
GHCi> foldr (:) [] $ PreO tree
[3,1,2,4]
GHCi> foldr (:) [] $ PostO tree
[2,1,4,3]
GHCi> foldr (:) [] $ LevelO tree
[3,1,4,2]
-}

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

instance Foldable Preorder where
    foldr f ini (PreO Nil) = ini
    foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))

instance Foldable Postorder where
    foldr f ini (PostO Nil) = ini
    foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) (PostO r)) (PostO l)

instance Foldable Levelorder where    
    foldr f ini (LevelO root) = level [root] where
        level [] = ini
        level (Nil : nodes) = level nodes
        level ((Branch l x r) : nodes) = f x $ level (nodes ++ [l, r])
