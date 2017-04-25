module Validate where

import Control.Applicative

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
Стандартная семантика Except как аппликативного функтора и монады: выполнять цепочку вычислений до первой ошибки. 
Реализация представителей классов Alternative и MonadPlus наделяет эту монаду альтернативной☺ семантикой: 
попробовать несколько вычислений, вернуть результат первого успешного, а в случае неудачи — все возникшие ошибки.

Довольно часто возникает необходимость сделать нечто среднее. 
К примеру, при проверке корректности заполнения анкеты или при компиляции программы для общего успеха необходимо, 
чтобы ошибок совсем не было, но в то же время, нам хотелось бы не останавливаться после первой же ошибки, 
а продолжить проверку, чтобы отобразить сразу все проблемы. Except такой семантикой не обладает, 
но никто не может помешать нам сделать свой тип данных (назовем его Validate), 
представители которого будут обеспечивать требую семантику, позволяющую сохранить список всех произошедших ошибок:

newtype Validate e a = Validate { getValidate :: Either [e] a }
Реализуйте функцию validateSum :: [String] -> Validate SumError Integer:

GHCi> getValidate $ validateSum ["10", "20", "30"]
Right 60
GHCi> getValidate $ validateSum ["10", "", "30", "oops"]
Left [SumError 2 EmptyInput,SumError 4 (NoParse "oops")]
Эта функция практически ничем не отличается от уже реализованной ранее trySum, если использовать функцию-адаптер 
collectE :: Except e a -> Validate e a и представителей каких-нибудь классов типов для Validate.
-}

newtype Validate e a = Validate { getValidate :: Either [e] a }

collectE :: Except e a -> Validate e a
collectE (Except (Left e)) = Validate $ Left [e]
collectE (Except (Right x)) = Validate $ Right x

instance Functor (Validate e) where
	fmap f (Validate (Left e)) = Validate $ Left e
	fmap f (Validate (Right x)) = Validate $ Right (f x)

instance Applicative (Validate e) where
	pure x = Validate $ Right x
	(Validate (Left xs)) <*> (Validate (Left ys)) = Validate $ Left (xs ++ ys)
	(Validate (Left e)) <*> _ = (Validate (Left e))
	_ <*> (Validate (Left e)) = (Validate (Left e))
	(Validate (Right f)) <*> (Validate (Right x)) = Validate $ Right (f x)

validateSum :: [String] -> Validate SumError Integer
validateSum = snd . foldl (\ (n, sum) x -> (n + 1, (+) <$> sum <*> tryReadNth n x)) (1, Validate $ Right 0) where
    tryReadNth n x = collectE $ withExcept (SumError n) (tryRead x)