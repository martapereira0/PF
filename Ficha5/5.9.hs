-- fazer lista infinita para o numerador e denominador e depois fazer zipWith (/) [] []
lista1 :: Int -> [Double]
lista1 n = [4.0*(-1)^x | x <- list]
    where
      list = [0..]

denom2 :: Int -> [Double]
denom2 x = 1 : [a*(a+1)*(a+2) | a <- lista]
    where
      lista = [2.0,4.0..]

aproxPi1 :: Int -> Double
aproxPi1 x = sum (zipWith (/) num denom)
    where
      num = take x (lista1 x)
      denom = take x [1.0,3.0..]

aproxPi2 :: Int -> Double
aproxPi2 n = sum (zipWith (/) num denom)
    where
      num = take n (3 : lista1 n)
      denom = take n (denom2 n)
