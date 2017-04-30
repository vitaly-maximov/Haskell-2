module Separate where

import Control.Monad.Trans.Writer
import Control.Monad.Writer

{-
Реализуйте функцию separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a].

Эта функция принимает два предиката и список и записывает в один лог элементы списка, 
удовлетворяющие первому предикату, в другой лог — второму предикату, а возвращающает список элементов, 
ни одному из них не удовлетворяющих.

GHCi> (runWriter . runWriterT) $ separate (<3) (>7) [0..10]
(([3,4,5,6,7],[0,1,2]),[8,9,10])
-}

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate _ _ [] = return []
separate p1 p2 (x : xs) = do
    when (p1 x) (Control.Monad.Trans.Writer.tell [x])    
    when (p2 x) (lift $ Control.Monad.Writer.tell [x])    
    ys <- separate p1 p2 xs
    return $ case (p1 x) || (p2 x) of
        True -> ys
        False -> x : ys


