module PrsE where

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

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE = undefined

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

