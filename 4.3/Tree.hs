module Tree where

import Control.Monad.Writer
import Control.Monad.Trans.State

{-
Те из вас, кто проходил первую часть нашего курса, конечно же помнят, последнюю задачу из него. 
В тот раз всё закончилось монадой State, но сейчас с неё все только начинается!

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
Вам дано значение типа Tree (), иными словами, вам задана форма дерева. 
От вас требуется сделать две вещи: во-первых, пронумеровать вершины дерева, обойдя их in-order обходом 
(левое поддерево, вершина, правое поддерево); во-вторых, подсчитать количество листьев в дереве.

GHCi> numberAndCount (Leaf ())
(Leaf 1,1)
GHCi> numberAndCount (Fork (Leaf ()) () (Leaf ()))
(Fork (Leaf 1) 2 (Leaf 3),2)
Конечно, можно решить две подзадачи по-отдельности, но мы сделаем это всё за один проход. 
Если бы вы писали решение на императивном языке, вы бы обошли дерево, 
поддерживая в одной переменной следующий доступный номер для очередной вершины, 
а в другой — количество встреченных листьев, причем само значение второй переменной, по сути, в процессе обхода не требуется. 
Значит, вполне естественным решением будет завести состояние для первой переменной, а количество листьев накапливать в «логе»-моноиде.

Вот так выглядит код, запускающий наше вычисление и извлекающий результат:

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)
  where
    go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
    go = undefined
Вам осталось только описать само вычисление — функцию go.
-}

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
    deriving Show

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)
  where
    go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
    go tree = do
        case tree of
            Leaf _ -> do
                n <- inc
                lift $ tell 1
                return $ Leaf n
            Fork l x r -> do
                l' <- go l
                n <- inc
                r' <- go r
                return $ Fork l' n r'
        where
            inc = do
                n <- get
                put $ n + 1
                return n

