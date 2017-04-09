module IgnoreBraces where

{-
Используя аппликативный интерфейс Parsec, реализуйте функцию ignoreBraces, которая принимает три аргумента-парсера. Первый парсер разбирает текст, интерпретируемый как открывающая скобка, второй — как закрывающая, а третий разбирает весь входной поток, расположенный между этими скобками. Возвращаемый парсер возвращает результат работы третьего парсера, скобки игнорируются.

GHCi> test = ignoreBraces (string "[[") (string "]]") (many1 letter)
GHCi> parseTest test "[[ABC]]DEF"
"ABC"
-}

import Text.Parsec
import Control.Applicative

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces open close parse = open *> parse <* close