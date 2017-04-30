module Test where

--import Control.Monad.Trans.Writer
import Control.Monad.Writer

test :: [Int] -> Writer [Int] [Int]
test xs = do
    ys <- if null xs then return [] else test $ tail xs
    return (head xs : ys)

test2 :: IO (Int, Int)
test2 = do
    putStrLn "hello"
    return (5, 4)

test3 = show test2