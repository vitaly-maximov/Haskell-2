module ListIndexError where

import Control.Monad.Trans.Except

{-
В модуле Control.Monad.Trans.Except библиотеки transformers имеется реализация монады Except с интерфейсом, 
идентичным представленному в видео-степах, но с более общими типами. 
Мы изучим эти типы в следующих модулях, однако использовать монаду Except из библиотеки transformers мы можем уже сейчас.

Введём тип данных для представления ошибки обращения к списку по недопустимому индексу:

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex 
  deriving (Eq, Show)
Реализуйте оператор (!!!) :: [a] -> Int -> Except ListIndexError a доступа к элементам массива по индексу, 
отличающийся от стандартного (!!) поведением в исключительных ситуациях. 
В этих ситуациях он должен выбрасывать подходящее исключение типа ListIndexError.

GHCi> runExcept $ [1..100] !!! 5
Right 6
GHCi> (!!!!) xs n = runExcept $ xs !!! n
GHCi> [1,2,3] !!!! 0
Right 1
GHCi> [1,2,3] !!!! 42
Left (ErrIndexTooLarge 42)
GHCi> [1,2,3] !!!! (-3)
Left ErrNegativeIndex
-}

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
	deriving (Eq, Show)

infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a
_ !!! i | i < 0 = throwE ErrNegativeIndex
xs !!! i = case drop i xs of
    [] -> throwE $ ErrIndexTooLarge i
    x : _  -> return x



