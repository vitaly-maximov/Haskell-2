module PrsEP where

import Control.Applicative

{-
Реализуем улучшенную версию парсера PrsE

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0
Этот парсер получил дополнительный целочисленный параметр в аргументе и в возвращаемом значении. 
С помощью этого параметра мы сможем отслеживать и передвигать текущую позицию в разбираемой строке и 
сообщать о ней пользователю в случае ошибки:

GHCi> charEP c = satisfyEP (== c)
GHCi> runPrsEP (charEP 'A') 0 "ABC"
(1,Right ('A',"BC"))
> runPrsEP (charEP 'A') 41 "BCD"
(42,Left "pos 42: unexpected B")
> runPrsEP (charEP 'A') 41 ""
(42,Left "pos 42: unexpected end of input")
Вспомогательная функция parseEP дает возможность вызывать парсер более удобным образом по сравнению с runPrsEP, скрывая технические детали:

GHCi> parseEP (charEP 'A') "ABC"
Right ('A',"BC")
GHCi> parseEP (charEP 'A') "BCD"
Left "pos 1: unexpected B"
GHCi> parseEP (charEP 'A') ""
Left "pos 1: unexpected end of input"
Реализуйте функцию satisfyEP :: (Char -> Bool) -> PrsEP Char, обеспечивающую описанное выше поведение.
-}

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

charEP c = satisfyEP (== c)

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP predicate = PrsEP parser where
    parser pos [] = (pos + 1, Left $ "pos " ++ show (pos + 1) ++ ": unexpected end of input")
    parser pos (x : xs) | predicate x = (pos + 1, Right (x, xs))
    parser pos (x : _) = (pos + 1, Left $ "pos " ++ show (pos + 1) ++ ": unexpected " ++ [x])

{-
Сделайте парсер

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0
представителем классов типов Functor и Applicative, обеспечив следующее поведение:

GHCi> runPrsEP (pure 42) 0 "ABCDEFG"
(0,Right (42,"ABCDEFG"))
GHCi> charEP c = satisfyEP (== c)
GHCi> anyEP = satisfyEP (const True)
GHCi> testP = (,) <$> anyEP <* charEP 'B' <*> anyEP
GHCi> runPrsEP testP 0 "ABCDE"
(3,Right (('A','C'),"DE"))
GHCi> parseEP testP "BCDE"
Left "pos 2: unexpected C"
GHCi> parseEP testP ""
Left "pos 1: unexpected end of input"
GHCi> parseEP testP "B"
Left "pos 2: unexpected end of input"
-}

anyEP = satisfyEP (const True)

testP = (,) <$> anyEP <* charEP 'B' <*> anyEP

instance Functor PrsEP where
    fmap f (PrsEP p) = PrsEP p' where
        p' pos s = case p pos s of
            (pos', Left e) -> (pos', Left e)
            (pos', Right (x, s')) -> (pos', Right (f x, s'))

instance Applicative PrsEP where
    pure x = PrsEP (\ pos s -> (pos, Right (x, s)))
    (PrsEP f) <*> (PrsEP g) = PrsEP p where
        p pos s = case f pos s of
            (pos', Left e) -> (pos', Left e)
            (pos', Right (x, s')) -> case g pos' s' of
                (pos'', Left e) -> (pos'', Left e)
                (pos'', Right (y, s'')) -> (pos'', Right (x y, s''))

{-
Сделайте парсер

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0
представителем класса типов Alternative, обеспечив следующее поведение для пары неудачных альтернатив: сообщение об ошибке возвращается из той альтернативы, которой удалось распарсить входную строку глубже.

GHCi> runPrsEP empty 0 "ABCDEFG"
(0,Left "pos 0: empty alternative")
GHCi> charEP c = satisfyEP (== c)
GHCi> tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c
GHCi> parseEP (tripleP "ABC" <|> tripleP "ADC") "ABE"
Left "pos 3: unexpected E"
GHCi> parseEP (tripleP "ABC" <|> tripleP "ADC") "ADE"
Left "pos 3: unexpected E"
GHCi> parseEP (tripleP "ABC" <|> tripleP "ADC") "AEF"
Left "pos 2: unexpected E"
-}

tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c

instance Alternative PrsEP where
    empty = PrsEP (\ pos s -> (pos, Left $ "pos " ++ show pos ++ ": empty alternative"))
    (PrsEP f) <|> (PrsEP g) = PrsEP p where
        p pos s = case f pos s of
            (pos', Right (x, s')) -> (pos', Right (x, s'))
            (pos', Left e) -> case g pos s of
                (pos'', Left e') | pos'' <= pos' -> (pos', Left e)
                (pos'', Left e') -> (pos'', Left e')
                (pos'', Right r) -> (pos'', Right r)

