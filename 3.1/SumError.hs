module SumError where

import Control.Applicative

{-
Используя tryRead из прошлого задания, реализуйте функцию trySum, которая получает список чисел, 
записанных в виде строк, и суммирует их. В случае неудачи, функция должна возвращать информацию об ошибке 
вместе с номером элемента списка (нумерация с единицы), вызвавшим ошибку.

Для хранения информации об ошибке и номере проблемного элемента используем новый тип данных:

data SumError = SumError Int ReadError
  deriving Show
GHCi> runExcept $ trySum ["10", "20", "30"]
Right 60
GHCi> runExcept $ trySum ["10", "20", ""]
Left (SumError 3 EmptyInput)
GHCi> runExcept $ trySum ["10", "two", "30"]
Left (SumError 2 (NoParse "two"))
Подсказка: функция withExcept в этом задании может быть чрезвычайно полезна. 
Постарайтесь максимально эффективно применить знания, полученные на прошлой неделе.
-}

newtype Except e a = Except { runExcept :: Either e a }

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f (Except (Left e)) = Except $ Left $ f e
withExcept _ (Except (Right x)) = Except $ Right x

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: Read a => String -> Except ReadError a
tryRead [] = Except $ Left $ EmptyInput
tryRead s = case reads s of
	[(x, [])] -> Except $ Right $  x
	_ -> Except $ Left $ NoParse s

data SumError = SumError Int ReadError
  deriving Show

trySum :: [String] -> Except SumError Integer
trySum = Except . snd . foldl (\ (n, sum) x -> (n + 1, (+) <$> sum <*> tryReadNth n x)) (1, Right 0) where
    tryReadNth n x = runExcept $ withExcept (SumError n) (tryRead x)

{-
trySum = except . snd . foldl (\ (n, sum) x -> (n + 1, (+) <$> sum <*> tryReadNth n x)) (1, Right 0) where
    tryReadNth n x = runExcept $ withExcept (SumError n) (tryRead x)
-}