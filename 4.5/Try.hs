{-# LANGUAGE FlexibleContexts #-}

module Try where

import Control.Monad.Except

{-
Функция tryRead обладает единственным эффектом: в случае ошибки она должна прерывать вычисление. 
Это значит, что её можно использовать в любой монаде, предоставляющей возможность завершать вычисление с ошибкой, 
но сейчас это не так, поскольку её тип это делать не позволяет:

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
 
Измените её так, чтобы она работала в любой монаде, позволяющей сообщать об исключительных ситуациях типа ReadError. 
Для этого к трансформеру ExceptT в библиотеке mtl прилагается класс типов MonadError 
(обратите внимание на название класса — это так сделали специально, чтобы всех запутать), находящийся в модуле Control.Monad.Except.
-}

data ReadError = EmptyInput | NoParse String
  deriving Show


tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead [] = throwError EmptyInput
tryRead s = case reads s of
    (x, "") : _ -> return x
    _ -> throwError $ NoParse s