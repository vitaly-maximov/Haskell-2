module Test where

data ReadError = EmptyInput | NoParse String
  deriving Show

newtype Except e a = Except { runExcept :: Either e a }

tryRead :: Read a => String -> Except ReadError a
tryRead [] = Except $ Left $ EmptyInput
tryRead s = case reads s of
	[(x, [])] -> Except $ Right $  x
	_ -> Except $ Left $ NoParse s