intercalar :: a -> [a] -> [a] -- intercalar um valor entre os elementos de uma lista
intercalar _ [] = []
intercalar c [x] = [x]
intercalar c (x:xs) = x : c : intercalar c xs
