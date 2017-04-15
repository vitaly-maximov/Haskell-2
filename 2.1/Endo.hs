module Endo where

import Data.Monoid

{-
Реализуйте функцию

mkEndo :: Foldable t => t (a -> a) -> Endo a
принимающую контейнер функций и последовательно сцепляющую элементы этого контейнера с помощью композиции, порождая в итоге эндоморфизм.

GHCi> e1 = mkEndo [(+5),(*3),(^2)]
GHCi> appEndo e1 2
17
GHCi> e2 = mkEndo (42,(*3))
GHCi> appEndo e2 2
6
-}

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo 