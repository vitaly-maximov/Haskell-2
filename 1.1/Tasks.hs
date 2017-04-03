module Tasks where

{-
GHCi> getArr2 (fmap length (Arr2 take)) 10 "abc"
3
GHCi> getArr3 (tail <$> tail <$> Arr3 zipWith) (+) [1,2,3,4] [10,20,30,40,50]
[33,44]
-}

newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap g (Arr2 f) = Arr2 (\ e1 e2 -> g $ f e1 e2)

instance Functor (Arr3 e1 e2 e3) where
  fmap g (Arr3 f) = Arr3 (\ e1 e2 e3 -> g $ f e1 e2 e3)