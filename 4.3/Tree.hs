module Tree where

import Control.Monad.Trans.Except
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Foldable

{-
С деревом мы недавно встречались:

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
Вам на вход дано дерево, содержащее целые числа, записанные в виде строк. Ваша задача обойти дерево in-order 
(левое поддерево, вершина, правое поддерево) и просуммировать числа до первой строки, 
которую не удаётся разобрать функцией tryRead из прошлого задания (или до конца дерева, если ошибок нет). 
Если ошибка произошла, её тоже надо вернуть.
Обходить деревья мы уже умеем, так что от вас требуется только функция go, подходящая для такого вызова:

treeSum t = let (err, s) = runWriter . runExceptT $ traverse_ go t
            in (maybeErr err, getSum s)
  where
    maybeErr :: Either ReadError () -> Maybe ReadError
    maybeErr = either Just (const Nothing)

GHCi> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16")
(Just (NoParse "oops"),3)
GHCi> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16")
(Nothing,34)
-}

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

treeSum t = let (err, s) = runWriter . runExceptT $ traverse_ go t
            in (maybeErr err, getSum s)
  where
    maybeErr :: Either ReadError () -> Maybe ReadError
    maybeErr = either Just (const Nothing)

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
tryRead [] = throwE EmptyInput
tryRead s = case reads s of
    (x, "") : _ -> return x
    _ -> throwE $ NoParse s

instance Foldable Tree where
    foldr f ini (Leaf x) = f x ini
    foldr f ini (Fork l x r) = foldr f (f x (foldr f ini r)) l

go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
go s = do    
    x <- tryRead s
    lift $ tell $ Sum x
    return ()
