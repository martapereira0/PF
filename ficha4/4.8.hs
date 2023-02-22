--a)
addpoly :: [Int] -> [Int] -> [Int]
addpoly [] [] = []
addpoly (x:xs) [] = (x:xs)
addpoly [] (y:ys) = (y:ys)
addpoly (x:xs) (y:ys) = x+y : addpoly xs ys

--b)
multPoly :: [Int] -> [Int] -> [Int]
multPoly [] [] = []
multPoly (x:xs) [] = []
multPoly [] (x:xs) = []
multPoly (a0:xs) (b0:zs) = a0*b0 : restantes
    where
      restantes = addpoly (addpoly resto1 resto2) resto3
      resto1 = [a0*z | z <- zs]
      resto2 = [b0*x | x <- xs]
      resto3 = 0 : multPoly xs zs
