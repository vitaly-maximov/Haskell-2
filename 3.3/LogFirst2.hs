module LogFirst2 where

import Data.Char
--import Data.List

import MyRWT hiding (logFirstAndRetSecond)

logFirstAndRetSecond :: MyRWT Maybe String
logFirstAndRetSecond = do
  xs <- myAsks id
  case xs of
    (el1 : el2 : _) -> myTell el1 >> return (map toUpper el2)
    _ -> myLift Nothing

{-
С помощью трансформера монад MyRWT мы можем написать безопасную версию logFirstAndRetSecond:

logFirstAndRetSecond :: MyRWT Maybe String
logFirstAndRetSecond = do
  xs <- myAsk
  case xs of
    (el1 : el2 : _) -> myTell el1 >> return (map toUpper el2)
    _ -> myLift Nothing

GHCi> runMyRWT logFirstAndRetSecond ["abc","defg","hij"]
Just ("DEFG","abc")
GHCi> runMyRWT logFirstAndRetSecond ["abc"]
Nothing
Реализуйте безопасную функцию veryComplexComputation, записывающую в лог через запятую 
первую строку четной длины и первую строку нечетной длины, а возвращающую пару из второй строки четной и второй строки нечетной длины, 
приведенных к верхнему регистру:    

GHCi> runMyRWT veryComplexComputation ["abc","defg","hij"]
Nothing
GHCi> runMyRWT veryComplexComputation ["abc","defg","hij","kl"]
Just (("KL","HIJ"),"defg,abc")
Подсказка: возможно, полезно будет реализовать функцию myWithReader.
-}

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
    xs <- myAsks id
    let es = filter (even . length) xs
    let os = filter (odd . length) xs
    if (hasTwo es) || (hasTwo os) then
        myLift Nothing
    else do 
        myTell $ (head es) ++ "," ++ (head os);        
        myLift $ Just $ (upSecond es, upSecond os)
    where
        hasTwo = null . drop 1
        upSecond = map toUpper . head . tail
        