--a)
minimium :: Ord a => [a] -> a
minimium [x] = x
minimium (x:xs)= min x (minimium xs) -- a função min é uma função pré-definida que diz o minimo entre dois numeros
--b)
delete :: Eq a => a -> [a] -> [a]
delete n [] = []
delete n (x:xs) | n == x = xs
                | otherwise = x : delete n xs

--c)
ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort xs = a : ssort (delete a xs)
      where a = minimium xs
