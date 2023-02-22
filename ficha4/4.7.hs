--a) juntar duas listas ordenadas numa só mantendo a ordenação
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] (x:xs) = (x:xs)
merge (x:xs) [] = (x:xs)
merge (x:xs) (y:ys) = sort (unir (x:xs) (y:ys))


unir :: [a] -> [a] -> [a] --unir as listas
unir (x:xs) (y:ys) = (x:xs) ++ (y:ys)

--inserir um elemento numa lista ordenada na posição correta de forma a manter a ordenação
insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs)
    | n <= x = n:x:xs
    |otherwise = x : (insert n xs)

--ordenar uma lista pelo método de inserção
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)

--b)
metades :: [a] -> ([a],[a])
metades l = (left,right)
    where
      left = take n l
      right = drop n l
      n = div (length l) 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x] --lista com apenas 1 elemento
msort l = merge (msort (left)) (msort (right)) -- l é a lista
  where
    left = take n l
    right = drop n l
    n = div (length l) 2
