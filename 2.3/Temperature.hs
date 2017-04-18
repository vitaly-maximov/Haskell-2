module Temperature where

{-
Расширьте интерфейс для работы с температурами из предыдущего видео Кельвинами и реализуйте функцию

k2c :: Temperature Kelvin -> Temperature Celsius
обеспечивающую следующее поведение

GHCi> k2c 0
Temperature (-273.15)
GHCi> k2c 0 == Temperature (-273.15)
True
GHCi> k2c 273.15
Temperature 0.0
-}

newtype Temperature a = Temperature Double
  deriving (Num,Show)

data Celsius
data Fahrenheit 
data Kelvin

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c (Temperature k) = Temperature (k - 273.15)