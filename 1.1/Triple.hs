module Triple where

import Control.Applicative

data Triple a = Tr a a a  deriving (Eq,Show)

{-
GHCi> (^2) <$> Tr 1 (-2) 3
Tr 1 4 9
GHCi> Tr (^2) (+2) (*3) <*> Tr 2 3 4
Tr 4 5 12
-}

instance Functor Triple where
	--fmap :: (a -> b) -> Triple a -> Triple b
	fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
	--pure :: a -> Triple a
	pure x = Tr x x x

	-- (<*>) :: Triple (a -> b) -> Triple a -> Triple b
	Tr f1 f2 f3 <*> Tr x y z = Tr (f1 x) (f2 y) (f3 z)