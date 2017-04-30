module MyRWT where

import Control.Monad.Trans

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import Data.Char

{-
Превратите монаду MyRW в трансформер монад MyRWT:

logFirstAndRetSecond :: MyRWT IO String
logFirstAndRetSecond = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2
GHCi> runMyRWT logFirstAndRetSecond ["abc","defg","hij"]
First is "abc"
Second is "DEFG"
("DEFG","abc")   
-}

logFirstAndRetSecond :: MyRWT IO String
logFirstAndRetSecond = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2


type MyRWT m = ReaderT [String] (WriterT String m)

runMyRWT :: Monad m => MyRWT m a -> [String] -> m (a, String)
runMyRWT rwt env = runWriterT (runReaderT rwt env)

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks

myTell :: Monad m => String -> MyRWT m ()
myTell = lift . tell

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift
