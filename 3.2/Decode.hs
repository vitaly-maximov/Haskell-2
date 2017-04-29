module Decode where

{-
CPS-преобразование часто кладут в основу предметно-ориентированных языков (DSL).

Реализуйте комбинаторы, которые позволят записывать числа вот в таком забавном формате:

GHCi> decode one hundred twenty three as a number
123
GHCi> decode one hundred twenty one as a number
121
GHCi> decode one hundred twenty as a number
120
GHCi> decode one hundred as a number
100
GHCi> decode three hundred as a number
300
GHCi> decode two thousand seventeen as a number
2017
-}

decode = ($ 0)
as = flip ($)
a = flip ($)
number = id

one = flip ($) . (+1)
two = flip ($) . (+2)
three = flip ($) . (+3)
seventeen = flip ($) . (+17)
twenty = flip ($) . (+20)
hundred = flip ($) . (*100)
thousand = flip ($) . (*1000)