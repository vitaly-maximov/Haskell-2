module Prs where

import Control.Applicative

{-
Предположим, тип парсера определен следующим образом:

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }
Сделайте этот парсер представителем класса типов Functor. Реализуйте также парсер anyChr :: Prs Char, удачно разбирающий и возвращающий любой первый символ любой непустой входной строки.

GHCi> runPrs anyChr "ABC"
Just ('A',"BC")
GHCi> runPrs anyChr ""
Nothing
GHCi> runPrs (digitToInt <$> anyChr) "BCD"
Just (11,"CD")
-}

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap f (Prs g) = Prs h where
  	h s = case g s of
  		Nothing -> Nothing
  		Just (x, str) -> Just (f x, str)

anyChr :: Prs Char
anyChr = Prs f where
	f [] = Nothing
	f (x : xs) = Just (x, xs)

{-
Сделайте парсер

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }
из предыдущей задачи аппликативным функтором с естественной для парсера семантикой:

GHCi> runPrs ((,,) <$> anyChr <*> anyChr <*> anyChr) "ABCDE"
Just (('A','B','C'),"DE")
GHCi> runPrs (anyChr *> anyChr) "ABCDE"
Just ('B',"CDE")
Представитель для класса типов Functor уже реализован.
-}

instance Applicative Prs where
	-- pure :: a -> Prs a
	pure x = Prs (\s -> Just (x, s))

	-- (<*>) :: Prs (a -> b) -> Prs a -> Prs b
	Prs p1 <*> Prs p2 = Prs p where
		p s = case p1 s of
			Nothing -> Nothing
			Just (f, s') -> case p2 s' of
				Nothing -> Nothing
				Just (x, s'') -> Just (f x, s'')