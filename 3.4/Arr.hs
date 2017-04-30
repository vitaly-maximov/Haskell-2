module Arr where

import Control.Applicative

{-
В задачах из предыдущих модулей мы сталкивались с типами данных

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }
задающих вычисления с двумя и тремя окружениями соответственно. Можно расширить их до трансформеров:

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }
Напишите «конструирующие» функции

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
обеспечивающие следующее поведение

GHCi> (getArr2T $ arr2 (+)) 33 9 :: [Integer]
[42]
GHCi> (getArr3T $ arr3 foldr) (*) 1 [1..5] :: Either String Integer
Right 120
GHCi> import Data.Functor.Identity
GHCi> runIdentity $ (getArr2T $ arr2 (+)) 33 9
42
-}

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f2 = Arr2T $ \ x y -> return $ f2 x y

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f3 = Arr3T $ \ x y z -> return $ f3 x y z

{-
Сделайте трансформеры

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }
представителями класса типов Functor в предположении, что m является функтором:

GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2,e1+e2]
GHCi> (getArr2T $ succ <$> a2l) 10 100
[11,101,111]
GHCi> a3e = Arr3T $ \e1 e2 e3 -> Right (e1+e2+e3)
GHCi> (getArr3T $ sqrt <$> a3e) 2 3 4
Right 3.0
-}

instance Functor m => Functor (Arr2T e1 e2 m) where
    fmap f (Arr2T g) = Arr2T $ \ e1 e2 -> fmap f $ g e1 e2

instance Functor m => Functor (Arr3T e1 e2 e3 m) where
    fmap f (Arr3T g) = Arr3T $ \ e1 e2 e3 -> fmap f $ g e1 e2 e3

{-
Сделайте трансформеры

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }
представителями класса типов Applicative в предположении, что m является аппликативным функтором:

GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2]
GHCi> a2fl = Arr2T $ \e1 e2 -> [(e1*e2+),const 7]
GHCi> getArr2T (a2fl <*> a2l) 2 10
[22,30,7,7]
GHCi> a3fl = Arr3T $ \e1 e2 e3 -> [(e2+),(e3+)]
GHCi> a3l = Arr3T $ \e1 e2 e3 -> [e1,e2]
GHCi> getArr3T (a3fl <*> a3l) 3 5 7
[8,10,10,12]
-}

instance Applicative m => Applicative (Arr2T e1 e2 m) where
    pure x = Arr2T $ \ e1 e2 -> pure x
    (Arr2T f) <*> (Arr2T x) = Arr2T $ \ e1 e2 -> f e1 e2 <*> x e1 e2

instance Applicative m => Applicative (Arr3T e1 e2 e3 m) where
    pure x = Arr3T $ \ e1 e2 e3 -> pure x
    (Arr3T f) <*> (Arr3T x) = Arr3T $ \ e1 e2 e3 -> f e1 e2 e3 <*> x e1 e2 e3

{-
Сделайте трансформеры

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }
представителями класса типов Monad в предположении, что m является монадой:

GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2]
GHCi> getArr2T (do {x <- a2l; y <- a2l; return (x + y)}) 3 5
[6,8,8,10]
GHCi> a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)
GHCi> getArr3T (do {x <- a3m; y <- a3m; return (x * y)}) 2 3 4
Just 81
-}

instance Monad m => Monad (Arr2T e1 e2 m) where
    v >>= k = Arr2T $ \ e1 e2 -> do
        x <- getArr2T v e1 e2
        getArr2T (k x) e1 e2

{-
instance Monad m => Monad (Arr3T e1 e2 e3 m) where
    v >>= k = Arr3T $ \ e1 e2 e3 -> do
        x <- getArr3T v e1 e2 e3
        getArr3T (k x) e1 e2 e3
-}

{-
Разработанная нами реализация интерфейса монады для трансформера Arr3T (как и для Arr2T и ReaderT) имеет не очень хорошую особенность. 
При неудачном сопоставлении с образцом вычисления в этой монаде завершаются аварийно, с выводом сообщения об ошибке в диагностический поток:

GHCi> a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)
GHCi> getArr3T (do {9 <- a3m; y <- a3m; return y}) 2 3 4
Just 9
GHCi> getArr3T (do {10 <- a3m; y <- a3m; return y}) 2 3 4
*** Exception: Pattern match failure in do expression at :12:15-16
Для обычного ридера такое поведение нормально, однако у трансформера внутренняя монада может уметь обрабатывать ошибки более щадащим образом. 
Переопределите функцию fail класса типов Monad для Arr3T так, чтобы обработка неудачного сопоставления с образцом осуществлялась бы во 
внутренней монаде:

GHCi> getArr3T (do {10 <- a3m; y <- a3m; return y}) 2 3 4
Nothing
-}

instance Monad m => Monad (Arr3T e1 e2 e3 m) where
    fail s = Arr3T $ \ e1 e2 e3 -> fail s
    v >>= k = Arr3T $ \ e1 e2 e3 -> do
        x <- getArr3T v e1 e2 e3
        getArr3T (k x) e1 e2 e3

{-
Сделайте трансформер

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
представителями класса типов MonadTrans:

GHCi> a2l = Arr2T $ \e1 e2 -> [e1,e2]
GHCi> getArr2T (do {x <- a2l; y <- lift [10,20,30]; return (x+y)}) 3 4
[13,23,33,14,24,34]
Реализуйте также «стандартный интерфейс» для этой монады — функцию

asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
работающую как asks для ReaderT, но принимающую при этом функцию от обоих наличных окружений:

GHCi> getArr2T (do {x <- asks2 const; y <- asks2 (flip const); z <- asks2 (,); return (x,y,z)}) 'A' 'B'
('A','B',('A','B'))
-}

class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans (Arr2T e1 e2) where
    lift x = Arr2T $ \ _ _ -> x

asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f2 = Arr2T $ \ e1 e2 -> return $ f2 e1 e2