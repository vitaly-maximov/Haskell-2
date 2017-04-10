module PrsE where

import Control.Applicative

{-
Рассмотрим более продвинутый парсер, позволяющий возвращать пользователю причину неудачи при синтаксическом разборе:

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }
Реализуйте функцию satisfyE :: (Char -> Bool) -> PrsE Char таким образом, чтобы функция

charE :: Char -> PrsE Char
charE c = satisfyE (== c)
обладала бы следующим поведением:

GHCi> runPrsE (charE 'A') "ABC"
Right ('A',"BC")
GHCi> runPrsE (charE 'A') "BCD"
Left "unexpected B"
GHCi> runPrsE (charE 'A') ""
Left "unexpected end of input"
-}

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE predicate = PrsE parser where
    parser "" = Left "unexpected end of input"
    parser (x : xs) = case predicate x of
        False -> Left ("unexpected " ++ [x])
        True -> Right (x, xs)

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

{-
Сделайте парсер

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }
из предыдущей задачи функтором и аппликативным функтором:

GHCi> let anyE = satisfyE (const True)
GHCi> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "ABCDE"
Right (('A','C'),"DE")
GHCi> runPrsE ((,) <$> anyE <* charE 'C' <*> anyE) "ABCDE"
Left "unexpected B"
GHCi> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "AB"
Left "unexpected end of input"
-}

instance Functor PrsE where
    -- fmap :: (a -> b) -> PrsE a -> PrsE b
    fmap f (PrsE a) = PrsE b where
        b s = case a s of
            Left e -> Left e
            Right (x, s') -> Right (f x, s')

instance Applicative PrsE where
    -- pure :: a -> PrsE a
    pure x = PrsE (\s -> Right (x, s))

    -- (<*>) :: PrsE (a -> b) -> PrsE a -> PrsE b
    (PrsE p1) <*> (PrsE p2) = PrsE p where
        p s = case p1 s of
            Left e1 -> Left e1
            Right (f, s') -> case p2 s' of
                Left e2 -> Left e2
                Right (x, s'') -> Right (f x, s'')
