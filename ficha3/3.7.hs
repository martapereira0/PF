myand :: [Bool] -> Bool
myand [] = True
myand (False:xs) = False -- se tiver pelo menos um False então nem todos os valores são True
myand (True:xs) = myand xs

myor :: [Bool] -> Bool
myor [] = False
myor (True:xs) = True
myor (False:xs) = myor xs

myconcat :: [[a]] -> [a]
myconcat [] = [] --caso base
myconcat (x:xs) = x ++ myconcat xs

myreplicate :: Int -> a -> [a]
myreplicate n x | n<=0 = []
                | otherwise = n : myreplicate (n-1) x

myselection :: [a] -> Int -> a
myselection (x:_) 0 = x --1º elemento da lista
myselection (_:xs) n = myselection xs (n-1)

myelem :: Eq a => a -> [a] -> Bool
myelem n [] = False
myelem n (x:xs) | x == n = True
                | otherwise  myelem n xs
