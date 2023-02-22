primos :: [Integer]
primos = crivo [2..]

crivo :: [Integer] -> [Integer]
crivo (p:xs) = p : crivo [x| x <- xs, mod x p /= 0]

goldbach :: Integer -> (Integer,Integer)
goldbach n = head [(x,y)| x <-takeWhile (\z -> z<=n) primos , y <- takeWhile (\z -> z<=n) primos, x+y==n]
