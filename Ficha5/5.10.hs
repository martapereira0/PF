--a)
binom :: Integer -> Integer -> Integer
binom n k = div (product [1..n]) (product [1..k] * (product [1..n-k]))

linha :: Integer -> [Integer] -- a primeira linha é a 0
linha n = [binom n k | k <- [0..n]]

pascal :: [[Integer]]
pascal = [linha n | n <- [0..]]

{-b)
binom1 :: Integer -> Integer -> Integer
binom1 m q  | m > q = binom1 m-1 q-1 + binom1 m q
            | otherwise = 1

nextlinha :: Integer -> [Integer] -- a primeira linha é a 0
nextlinha n = [binom1 n+1 k+1 | k <- [0..n]]

pascal1 :: [[Integer]]
pascal1 = [linha n | n <- [0..]]-}

novalinha :: [Integer] -> [Integer]
novalinha [] = []
novalinha (x:xs) |
