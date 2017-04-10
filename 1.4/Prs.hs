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

{-
Сделайте парсер

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }
представителем класса типов Alternative с естественной для парсера семантикой:

GHCi> runPrs (char 'A' <|> char 'B') "ABC"
Just ('A',"BC")
GHCi> runPrs (char 'A' <|> char 'B') "BCD"
Just ('B',"CD")
GHCi> runPrs (char 'A' <|> char 'B') "CDE"
Nothing
Представители для классов типов Functor и Applicative уже реализованы. Функцию char :: Char -> Prs Char включать в решение не нужно, но полезно реализовать для локального тестирования.
-}

instance Alternative Prs where
  empty = undefined
  (<|>) = undefined

{-
Реализуйте для парсера

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }
парсер-комбинатор many1 :: Prs a -> Prs [a], который отличается от many только тем, что он терпит неудачу в случае, когда парсер-аргумент неудачен на начале входной строки.

> runPrs (many1 $ char 'A') "AAABCDE"
Just ("AAA","BCDE")
> runPrs (many1 $ char 'A') "BCDE"
Nothing
Функцию char :: Char -> Prs Char включать в решение не нужно, но полезно реализовать для локального тестирования.
-}

many1 :: Prs a -> Prs [a]
many1 = undefined

{-
Реализуйте парсер nat :: Prs Int для натуральных чисел, так чтобы парсер

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat
обладал таким поведением

GHCi> runPrs mult "14*3"
Just (42,"")
GHCi> runPrs mult "64*32"
Just (2048,"")
GHCi> runPrs mult "77*0"
Just (0,"")
GHCi> runPrs mult "2*77AAA"
Just (154,"AAA")
Функции char :: Char -> Prs Char и mult :: Prs Int включать в решение не нужно, но полезно реализовать для локального тестирования.
-}

nat :: Prs Int
nat = undefined