module OddC where

import Control.Monad

{-
Для типа данных

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)
(контейнер-последовательность, который по построению может содержать только нечетное число элементов) реализуйте функцию

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
конкатенирующую три таких контейнера в один:

GHCi> tst1 = Bi 'a' 'b' (Un 'c')
GHCi> tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
GHCi> tst3 = Bi 'i' 'j' (Un 'k')
GHCi> concat3OC tst1 tst2 tst3
Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
Обратите внимание, что соображения четности запрещают конкатенацию двух контейнеров OddC.

Постарайтесь решить эту и две следующих задачи, не пользуясь сведением к стандартным спискам.
-}

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

tst1 = Bi 'a' 'b' (Un 'c')
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a) (Un b) z = Bi a b z
concat3OC (Un a) (Bi b c y) z = Bi a b $ concat3OC (Un c) y z
concat3OC (Bi a b x) y z = Bi a b $ concat3OC x y z

{-
Для типа данных

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)
реализуйте функцию

concatOC :: OddC (OddC a) -> OddC a
Она должна обеспечивать для типа OddC поведение, аналогичное поведению функции concat для списков:

GHCi> concatOC $ Un (Un 42)
Un 42
GHCi> tst1 = Bi 'a' 'b' (Un 'c')
GHCi> tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
GHCi> tst3 = Bi 'i' 'j' (Un 'k')
GHCi> concatOC $ Bi tst1 tst2 (Un tst3)
Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
-}

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a) = a
concatOC (Bi a b z) = concat3OC a b $ concatOC z

{-
Сделайте тип данных

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)
представителем классов типов Functor, Applicative и Monad. Семантика должна быть подобной семантике представителей этих классов типов для списков: монада OddC должна иметь эффект вычисления с произвольным нечетным числом результатов:

GHCi> tst1 = Bi 10 20 (Un 30)
GHCi> tst2 = Bi 1 2 (Bi 3 4 (Un 5))
GHCi> do {x <- tst1; y <- tst2; return (x + y)}
Bi 11 12 (Bi 13 14 (Bi 15 21 (Bi 22 23 (Bi 24 25 (Bi 31 32 (Bi 33 34 (Un 35)))))))
GHCi> do {x <- tst2; y <- tst1; return (x + y)}
Bi 11 21 (Bi 31 12 (Bi 22 32 (Bi 13 23 (Bi 33 14 (Bi 24 34 (Bi 15 25 (Un 35)))))))
Функцию fail можно не реализовывать, полагаясь на реализацию по умолчанию.
-}

tst1M = Bi 10 20 (Un 30)
tst2M = Bi 1 2 (Bi 3 4 (Un 5))

tst1R = do {x <- tst1M; y <- tst2M; return (x + y)}
tst2R = do {x <- tst2M; y <- tst1M; return (x + y)}

instance Functor OddC where
    fmap = liftM

instance Applicative OddC where
    pure = return
    (<*>) = ap

instance Monad OddC where
    -- return :: a -> OddC a
    return x = Un x

    -- (>>=) :: OddC a -> (a -> OddC b) -> OddC b
    (Un x) >>= f = f x
    (Bi x y z) >>= f = concat3OC (f x) (f y) $ z >>= f
