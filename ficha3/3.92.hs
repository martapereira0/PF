import Data.Char

forte :: String -> Bool
forte s | length s >= 8 && maiuscula s >= 1 && minuscula s >= 1 && algarismo s >= 1 = True
        | otherwise = False

maiuscula :: String -> Int
maiuscula s = length [x | x <- s, isUpper x] -- o x vai ser cada caracter da string e por isso a lista vai ter o tamanho da String

minuscula :: String -> Int
minuscula s = length [x | x <- s,  isLower x]

algarismo :: String -> Int
algarismo s = length [x | x <- s,  isDigit x]
