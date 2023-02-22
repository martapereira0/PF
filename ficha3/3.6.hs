import Data.Char

-- converter letra para inteiro 0..25
letraInt :: Char -> Int
letraInt x = ord x - ord 'A'

-- converter inteiro 0..25 para letra
intLetra :: Int -> Char
intLetra n = chr (n + ord 'A')
