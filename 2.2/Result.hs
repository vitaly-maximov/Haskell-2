module Result where

{-
Сделайте тип данных

data Result a = Ok a | Error String deriving (Eq,Show)
представителем класса типов Traversable (и всех других необходимых классов типов).

GHCi> traverse (\x->[x+2,x-2]) (Ok 5)
[Ok 7,Ok 3]
GHCi> traverse (\x->[x+2,x-2]) (Error "!!!")
[Error "!!!"]
-}

data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
    fmap f (Ok x) = Ok (f x)
    fmap _ (Error s) = (Error s)

instance Foldable Result where
    foldr f ini (Ok x) = f x ini
    foldr _ ini (Error s) = ini

instance Traversable Result where
    traverse f (Ok x) = Ok <$> f x
    traverse _ (Error s) = pure $ Error s