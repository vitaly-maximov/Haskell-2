module Parser where

{-
Сделайте парсер

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }
из первого модуля курса представителем класса типов Monad:

GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ABC"
Right (('A','B'),"C")
GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ACD"
Left "unexpected C"
GHCi> runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "BCD"
Left "unexpected B"
-}

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

{- from 1.4 -}

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE predicate = PrsE parser where
    parser "" = Left "unexpected end of input"
    parser (x : xs) = case predicate x of
        False -> Left ("unexpected " ++ [x])
        True -> Right (x, xs)

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

instance Functor PrsE where
    -- fmap :: (a -> b) -> PrsE a -> PrsE b
    fmap f (PrsE a) = PrsE b where
        b s = case a s of
            Left e -> Left e
            Right (x, s') -> Right (f x, s')

instance Applicative PrsE where
    -- pure :: a -> PrsE a
    pure x = PrsE (\s -> Right (x, s))

    -- (<*>) :: PrsE (a -> b) -> PrsE a -> PrsE b
    (PrsE p1) <*> (PrsE p2) = PrsE p where
        p s = case p1 s of
            Left e1 -> Left e1
            Right (f, s') -> case p2 s' of
                Left e2 -> Left e2
                Right (x, s'') -> Right (f x, s'')

{- new -}

instance Monad PrsE where
    -- (>>=) :: PrsE a -> (a -> PrsE b) -> PrsE b
    (PrsE p) >>= f = PrsE p' where
        p' s = case p s of
            Left e -> Left e
            Right (x, s') -> runPrsE (f x) s'


