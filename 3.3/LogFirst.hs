module LogFirst where

import Control.Monad.Trans.Writer
import Control.Monad.Reader
import Data.Char

{-
Перепишите функцию logFirstAndRetSecond из предыдущего видео, используя трансформер WriterT из модуля Control.Monad.Trans.Writer 
библиотеки transformers, и монаду Reader в качестве базовой.

GHCi> runReader (runWriterT logFirstAndRetSecond) strings
("DEFG","abc")
-}

logFirstAndRetSecond :: WriterT String (Reader [String]) String
logFirstAndRetSecond = do
    el1 <- lift $ asks head
    el2 <- lift $ asks (map toUpper . head . tail)
    tell el1
    return el2