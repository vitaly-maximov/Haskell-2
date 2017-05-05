module Game where

{-
Представьте, что друг принес вам игру. В этой игре герой ходит по полю. За один ход он может переместиться на одну клетку 
вверх, вниз, влево и вправо (стоять на месте нельзя). На поле его поджидают различные опасности, 
такие как пропасти (chasm) и ядовитые змеи (snake). Если игрок наступает на клетку с пропастью или со змеёй, он умирает.

data Tile = Floor | Chasm | Snake
  deriving Show

data DeathReason = Fallen | Poisoned
  deriving (Eq, Show)
Карта задается функцией, отображающей координаты клетки в тип этой самой клетки:

type Point = (Integer, Integer)
type GameMap = Point -> Tile
Ваша задача состоит в том, чтобы реализовать функцию

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
принимающую карту, количество шагов и начальную точку, а возвращающую список всех возможных исходов (с повторениями), 
если игрок сделает заданное число шагов из заданной точки. Заодно реализуйте функцию

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
показывающую, сколькими способами игрок может умереть данным способом, сделав заданное число шагов из заданной точки.

Например, для такого поля:

map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise                        = Chasm
 | 0 1 2 3 4 5
--------------
0| o o o o o o
1| o       s o
2| o   s     o
3| o         o
4| o         o
5| o o o o o o
ожидаются такие ответы:

GHCi> waysToDie Poisoned map1 1 (4,2)
1  -- можно пойти к змее наверх
GHCi> waysToDie Poisoned map1 2 (4,2)
2  -- можно пойти к змее наверх или к змее влево
GHCi> waysToDie Poisoned map1 3 (4,2)
5  -- за три шага к левой змее, по-прежнему можно дойти одним способом,
   -- а к правой — уже четырьмя (вверх, влево-вверх-вправо,
   --                            влево-вправо-вверх, вниз-вверх-вверх)
GHCi> waysToDie Poisoned map1 4 (4,2)
13
Гарантируется, что изначально игрок стоит на пустой клетке.

Подсказка: не забывайте, в каком уроке эта задача.
-}

data Tile = Floor | Chasm | Snake
  deriving Show

data DeathReason = Fallen | Poisoned
  deriving (Eq, Show)

type Point = (Integer, Integer)
type GameMap = Point -> Tile

map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise                        = Chasm


moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves _ stepsCount position | stepsCount == 0 = [Right position]
moves gameMap stepsCount (x, y) = up ++ down ++ left ++ right 
    where
        up = process (x, y - 1)
        down = process (x, y + 1)
        left = process (x - 1, y)
        right = process (x + 1, y)

        process position = case gameMap position of
            Floor -> moves gameMap (stepsCount - 1) position
            Chasm -> [Left Fallen]
            Snake -> [Left Poisoned]

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie deathReason gameMap stepsCount position = length $ filter (== Left deathReason) $ moves gameMap stepsCount position
