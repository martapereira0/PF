--Todos os números são divisores de 0!
mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc 0 x = x
mdc x y
    | x > y = mdc (x-y) y
    | otherwise = mdc x (y-x)
