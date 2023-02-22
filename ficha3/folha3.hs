--3.2)
perfeito :: Integer -> Integer
perfeito n = sum [x | x <- [1..(n-1)], n`mod`x == 0]

perfeitos :: Integer -> [Integer]
perfeitos n = [y | y <- [1..n], perfeito y == y]

--3.3)
pitagoricos :: Integer -> [(Integer,Integer,Integer)]
pitagoricos n = [(x,y,z) | x <- [1..n],y <- [1..n],z <- [1..n], x^2 + y^2 == z^2]

--3.4)
--divisores próprios são todos os divisores de um numero menores que ele

divprop :: Integer -> [Integer]
divprop n = [ x | x <- [1..n], n`mod`x ==0]

primo :: Integer -> Bool
primo n | divprop n == [1,n] = True
        | otherwise = False

--3.5)
binom :: Integer -> Integer -> Integer
binom n k = div num den
    where
      num = product [1..n]
      den = product [1..k] * product [1..(n-k)]

linha :: Integer -> [Integer]
linha n = [binom n k | k <- [0..n]]

pascal :: Integer -> [[Integer]]
pascal n = [linha x | x <-[0..n]]

soma :: (Int,Int) -> Int
soma (x,y) = x+y

--Teste 2018
distancia :: Float -> Float -> Float -> Float -> Float
distancia x1 y1 x2 y2 = sqrt((x2-x1)^2 + (y2-y1)^2)


declives :: Float -> Float -> Float -> Float -> Float
declives x1 y1 x2 y2 = (y2 - y1) / (x2 - x1)

colineares ::  Float -> Float -> Float -> Float -> Float -> Float -> Bool
colineares x1 y1 x2 y2 x3 y3  | declives x1 y1 x2 y2 == declives x2 y2 x3 y3 = True
                              | otherwise = False

niguais :: Int -> a -> [a]
niguais 0 _ = []
niguais n x = x : niguais (n-1) x

niguais' :: Int -> a -> [a]
niguais' n x =  [ x | y <- [1..n]]


merge' :: Ord a => [a] -> [a] -> [a]
merge' [] [] = []
merge' [] l2 = l2
merge' l1 [] = l1
merge' (x:xs) (y:ys)  | x <= y = x : merge' xs (y:ys)
                      | otherwise = y : merge' (x:xs) ys

length_zip :: [a] -> [(Int,a)]
length_zip xs = zip [length xs,length xs-1..1] xs
