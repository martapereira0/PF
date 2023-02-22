--a) inserir um elemento numa lista ordenada na posição correta de forma a manter a ordenação
insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs)
    | n <= x = n:x:xs
    |otherwise = x : (insert n xs)

--b) ordenar uma lista pelo método de inserção
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)
