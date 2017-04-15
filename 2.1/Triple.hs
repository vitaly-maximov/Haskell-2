module Triple where

{-
Сделайте тип

data Triple a = Tr a a a  deriving (Eq,Show)
представителем класса типов Foldable:

GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
"!!abcdefg"
-}

data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where
    -- foldl :: (b -> a -> b) -> b -> Triple a -> a
    foldl f ini (Tr x y z) = f (f (f ini x) y) z

    -- foldr :: (a -> b -> b) -> b -> Triple a -> b
    foldr f ini (Tr x y z) = f x (f y (f z ini))