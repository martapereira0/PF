
import Data.Char
--length >= 8
--Uma ou mais letras maiúsculas, minúsculas e um algarismo.


forte :: String -> Bool
forte x = length x >= 8 && maiuscula x >=1 && minuscula x >= 1 && algarismo x >= 1


maiuscula :: String -> Int
maiuscula s = length [x | x <- s, isUpper x]


minuscula :: String -> Int
minuscula s = length [x | x <- s, isLower x]

algarismo :: String -> Int
algarismo n = length [x | x <- n, isDigit x ]
