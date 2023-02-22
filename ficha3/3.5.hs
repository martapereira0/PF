binom :: Integer -> Integer -> Integer
binom n k = div num den
    where
      num = product [1..n]
      den = product [1..k] * product [1..(n-k)]


linha :: Integer -> [Integer]
linha n = [binom n k | k <- [0..n]]

pascal :: Integer -> [[Integer]]
pascal 0 = [[1]]
pascal l = [linha n| n <- [0..(l)]]
