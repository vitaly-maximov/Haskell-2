module Password where

{-
Следующий код

import Control.Monad.Trans.Maybe
import Data.Char (isNumber, isPunctuation)

askPassword0 :: MaybeT IO ()
askPassword0 = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword0
  liftIO $ putStrLn "Storing in database..."

getValidPassword0 :: MaybeT IO String
getValidPassword0 = do
  s <- liftIO getLine
  guard (isValid0 s)
  return s

isValid0 :: String -> Bool
isValid0 s = length s >= 8
            && any isNumber s
            && any isPunctuation s
используя трансформер MaybeT и свойства функции msum, отвергает ввод пользовательского пароля, 
до тех пор пока он не станет удовлетворять заданным критериям. Это можно проверить, вызывая его в интерпретаторе

GHCi> runMaybeT askPassword0
Используя пользовательский тип ошибки и трансформер ExceptT вместо MaybeT, модифицируйте приведенный выше код так, 
чтобы он выдавал пользователю сообщение о причине, по которой пароль отвергнут.

data PwdError = PwdError String

type PwdErrorMonad = ExceptT PwdError IO

askPassword :: PwdErrorMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."
  
getValidPassword :: PwdErrorMonad String
getValidPassword = undefined
Ожидаемое поведение:

GHCi> runExceptT askPassword
Enter your new password:
qwerty
Incorrect input: password is too short!
qwertyuiop
Incorrect input: password must contain some digits!
qwertyuiop123
Incorrect input: password must contain some punctuation!
qwertyuiop123!!!
Storing in database...
GHCi>
-}

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (msum)
import Data.Char (isNumber, isPunctuation)

newtype PwdError = PwdError String

instance Monoid PwdError where
    mempty = PwdError mempty
    mappend _ _ = mempty

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

getValidPassword :: PwdErrorIOMonad String
getValidPassword = action `catchE` handler 
    where
        action = do
            s <- liftIO getLine
            if (length s < 8) then
                throwE $ PwdError "Incorrect input: password is too short!"
            else if (not $ any isNumber s) then
                throwE $ PwdError "Incorrect input: password must contain some digits!"
            else if (not $ any isPunctuation s) then
                throwE $ PwdError "Incorrect input: password must contain some punctuation!"
            else
                return s
        
        handler (PwdError e) = do
            liftIO $ putStrLn e
            throwE $ PwdError e




