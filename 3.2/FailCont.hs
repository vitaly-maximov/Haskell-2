module FailCont where

import Control.Applicative
import Control.Monad

newtype Except e a = Except { runExcept :: Either e a }

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: Read a => String -> Except ReadError a
tryRead [] = Except $ Left $ EmptyInput
tryRead s = case reads s of
	[(x, [])] -> Except $ Right $  x
	_ -> Except $ Left $ NoParse s

{-
Вычисление в монаде Cont передает результат своей работы в функцию-продолжение. 
А что, если наши вычисления могут завершиться с ошибкой? В этом случае мы могли бы 
явно возвращать значение типа Either и каждый раз обрабатывать два возможных исхода, 
что не слишком удобно. Более разумный способ решения этой проблемы предоставляют трансформеры монад, 
но с ними мы познакомимся немного позже.

Определите тип данных FailCont для вычислений, которые получают два продолжения и вызывают одно из них в случае успеха, 
а другое — при неудаче. Сделайте его представителем класса типов Monad и реализуйте вспомогательные функции toFailCont 
и evalFailCont, используемые в следующем коде:

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2
(Здесь используется функция tryRead из предыдущего урока; определять её заново не надо.)

GHCi> evalFailCont $ addInts "15" "12"
Right 27
GHCi> runFailCont (addInts "15" "") print (putStrLn . ("Oops: " ++) . show)
Oops: EmptyInput
-}

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2


newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }

toFailCont :: Except e a -> FailCont r e a
toFailCont ex = case runExcept ex of
	Left e -> FailCont $ \ _ fail -> fail e
	Right x -> FailCont $ \ ok _ -> ok x

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont (FailCont fc) = fc (\ x -> Right x) (\ e -> Left e)

instance Monad (FailCont r e) where
	return x = FailCont $ \ok _ -> ok x

	-- (>>=) :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b
	(FailCont fc) >>= k = FailCont $ \ ok fail -> fc (\ x -> runFailCont (k x) ok fail) fail

instance Functor (FailCont r e) where
	fmap = liftM

instance Applicative (FailCont r e) where
	pure = return
	(<*>) = ap

{-
Реализуйте функцию callCFC для монады FailCont по аналогии с callCC.
-}

-- (a -> ((b -> r) -> (e -> r) -> r) -> ((a -> r) -> (e -> r) -> r) -> ((a -> r) -> (e -> r) -> r)
callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC f = FailCont $ \ ok fail -> runFailCont (f $ \ x -> FailCont $ \ _ _ -> ok x) ok fail