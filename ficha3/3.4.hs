divprop :: Integer -> [Integer]
divprop n = [x | x <-[1..n], mod n x == 0]

primo :: Integer -> Bool
primo n = divprop n == [1,n] -- um nº primo tem exatamente dois divisores, o 1 e ele próprio
