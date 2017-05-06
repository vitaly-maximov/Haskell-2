module Try where

import Control.Monad.Trans.Except

{-
Вспомним функцию tryRead:

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: Read a => String -> Except ReadError a
 
Измените её так, чтобы она работала в трансформере ExceptT.
-}

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
tryRead [] = throwE EmptyInput
tryRead s = case reads s of
    (x, "") : _ -> return x
    _ -> throwE $ NoParse s
    