module Triple where

{-
Сделайте тип

data Triple a = Tr a a a  deriving (Eq,Show)
представителем класса типов Traversable:

GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
"!!abcdefg"
GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 14 16)
Right (Tr 12 14 16)
GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 8 4)
Left 8
GHCi> sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9))
Tr (Tr 1 4 7) (Tr 2 5 8) (Tr 3 6 9)
-}

data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
    pure x = Tr x x x
    (Tr x1 y1 z1) <*> (Tr x2 y2 z2) = Tr (x1 x2) (y1 y2) (z1 z2)

instance Foldable Triple where
    foldr f ini (Tr x y z) = f x (f y (f z ini))

instance Traversable Triple where
    -- traverse :: (Applicative f) => (a -> f b) -> Triple a -> Triple (f b)
    traverse f (Tr x y z) = Tr <$> (f x) <*> (f y) <*> (f z)