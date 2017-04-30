module MyRW where

import Control.Monad.Trans.Reader
import Control.Monad.Writer

import Data.Char

{-
Наша абстракция пока что недостаточно хороша, поскольку пользователь всё ещё должен помнить такие детали, 
как, например, то, что asks нужно вызывать напрямую, а tell — только с помощью lift.

Нам хотелось бы скрыть такие детали реализации, обеспечив унифицированный интерфейс доступа к возможностям нашей монады, 
связанным с чтением окружения, и к возможностям, связанным с записью в лог. Для этого реализуйте функции myAsks и myTell, 
позволяющие записать logFirstAndRetSecond следующим образом:

logFirstAndRetSecond :: MyRW String
logFirstAndRetSecond = do
  el1 <- myAsks head
  el2 <- myAsks (map toUpper . head . tail)
  myTell el1
  return el2
-}

type MyRW = ReaderT [String] (Writer String)

runMyRW :: MyRW a -> [String] -> (a, String)
runMyRW r env = runWriter (runReaderT r env)

logFirstAndRetSecond :: MyRW String
logFirstAndRetSecond = do
  el1 <- myAsks head
  el2 <- myAsks (map toUpper . head . tail)
  myTell el1
  return el2


myAsks :: ([String] -> a) -> MyRW a
myAsks = asks

myTell :: String -> MyRW ()
myTell = lift . tell
    