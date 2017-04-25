module SimpleError where

import Control.Applicative
import Data.Foldable (msum)
import Data.Monoid

{-
Тип данных для представления ошибки обращения к списку по недопустимому индексу

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex 
  deriving (Eq, Show)
не очень естественно делать представителем класса типов Monoid. Однако, 
если мы хотим обеспечить накопление информации об ошибках, моноид необходим. 
К счастью, уже знакомая нам функция withExcept :: (e -> e') -> Except e a -> Except e' a 
позволяет изменять тип ошибки при вычислении в монаде Except.

Сделайте тип данных

newtype SimpleError = Simple { getSimple :: String } 
  deriving (Eq, Show)
представителем необходимых классов типов и реализуйте преобразователь для типа данных ошибки 
lie2se :: ListIndexError -> SimpleError так, чтобы обеспечить следующее поведение

GHCi> toSimple = runExcept . withExcept lie2se
GHCi> xs = [1,2,3]
GHCi> toSimple $ xs !!! 42
Left (Simple {getSimple = "[index (42) is too large]"})
GHCi> toSimple $ xs !!! (-2)
Left (Simple {getSimple = "[negative index]"})
GHCi> toSimple $ xs !!! 2
Right 3
GHCi> import Data.Foldable (msum)
GHCi> toSimpleFromList = runExcept . msum . map (withExcept lie2se)
GHCi> toSimpleFromList [xs !!! (-2), xs !!! 42]
Left (Simple {getSimple = "[negative index][index (42) is too large]"})
GHCi> toSimpleFromList [xs !!! (-2), xs !!! 2]
Right 3
-}

newtype Except e a = Except { runExcept :: Either e a }

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f (Except (Left e)) = Except $ Left $ f e
withExcept _ (Except (Right x)) = Except $ Right x

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
	deriving (Eq, Show)

infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a
_ !!! i | i < 0 = Except $ Left $ ErrNegativeIndex
xs !!! i = case drop i xs of
    [] -> Except $ Left $ ErrIndexTooLarge i
    x : _  -> Except $ Right $  x

{- new -}

newtype SimpleError = Simple { getSimple :: String } 
  deriving (Eq, Show)

toSimple = runExcept . withExcept lie2se
toSimpleFromList = runExcept . msum . map (withExcept lie2se)

instance Monoid SimpleError where
	mempty = Simple ""
	(Simple a) `mappend` (Simple b) = Simple (a ++ b)

lie2se :: ListIndexError -> SimpleError
lie2se ErrNegativeIndex = Simple "[negative index]"
lie2se (ErrIndexTooLarge i) = Simple $ "[index (" ++ show i ++ ") is too large]"