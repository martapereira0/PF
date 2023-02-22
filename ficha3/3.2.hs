divprop :: Integer -> [Integer]
divprop x = [a | a <-[1..x-1], mod x a == 0]

perfeitos :: Integer -> [Integer]
perfeitos n = [x | x <- [1..n-1], sum (divprop x ) == x ]
