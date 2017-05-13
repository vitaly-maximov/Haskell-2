{-# LANGUAGE FlexibleContexts #-}

module Tree where

import Control.Monad.Except
import Control.Monad.State

{-
В очередной раз у вас есть дерево строковых представлений чисел:

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
и функция tryRead:

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: (Read a, MonadError ReadError m) => String -> m a
 
Просуммируйте числа в дереве, а если хотя бы одно прочитать не удалось, верните ошибку:

GHCi> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16")
Left (NoParse "oops")
GHCi> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16")
Right 34

﻿Подумайте, что общего у этой задачи с похожей, которую вы решали ранее? В чем отличия? При чем здесь библиотека mtl?
-}

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

data ReadError = EmptyInput | NoParse String
  deriving Show


tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead [] = throwError EmptyInput
tryRead s = case reads s of
    (x, "") : _ -> return x
    _ -> throwError $ NoParse s

--ExceptT ReadError (State (Integer)) ()
treeSum :: Tree String -> Either ReadError Integer
treeSum tree = evalState (runExceptT (sum tree)) 0
    where
        --add :: Integer -> String -> ExceptT ReadError (State Integer) ()
        add a s = do
            b <- tryRead s
            put $ a + b
            return 0

        --sum :: Tree String -> ExceptT ReadError (State Integer) ()
        sum tree = do            
            acc <- get
            case tree of
                Leaf s -> add acc s
                Fork l s r -> do
                    add acc s
                    sum l
                    sum r
            result <- get
            return result